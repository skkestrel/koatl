use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Display;

use slotmap::SlotMap;
use slotmap::new_key_type;

use crate::types::Type;
use crate::util::{RefHash, TlErrBuilder, TlErrs, TlResult};
use parser::ast::*;

new_key_type! { pub struct ScopeKey;}
new_key_type! { pub struct DeclarationKey;}

impl ScopeKey {
    pub fn bind<'res>(self, state: &'res mut ResolveState<'_>) -> &'res mut Scope {
        &mut state.scopes[self]
    }
}

impl DeclarationKey {
    pub fn bind<'src, 'res>(
        self,
        state: &'res mut ResolveState<'src>,
    ) -> &'res mut Declaration<'src> {
        &mut state.declarations[self]
    }
}

#[allow(dead_code)]
pub struct ResolveState<'src> {
    pub source: &'src str,
    pub allow_top_level_await: bool,

    pub export_stars: Vec<SIdent<'src>>,

    pub resolutions: HashMap<RefHash, DeclarationKey>,
    pub functions: HashMap<RefHash, FnInfo>,
    pub patterns: HashMap<RefHash, PatternInfo>,

    pub declarations: SlotMap<DeclarationKey, Declaration<'src>>,
    pub scopes: SlotMap<ScopeKey, Scope>,

    pub root_scope: ScopeKey,
    pub err_scope: ScopeKey,

    errors: TlErrs,

    placeholder_stack: Vec<PlaceholderGuard>,
    fn_stack: Vec<FnInfo>,
    scope_stack: Vec<ScopeKey>,
}

fn top_scope<'src, 'state>(
    scope_stack: &'state Vec<ScopeKey>,
    scopes: &'state mut SlotMap<ScopeKey, Scope>,
) -> &'state mut Scope {
    &mut scopes[*scope_stack.last().unwrap()]
}

fn top_scope_and_key<'src, 'state>(
    scope_stack: &'state Vec<ScopeKey>,
    scopes: &'state mut SlotMap<ScopeKey, Scope>,
) -> (&'state mut Scope, ScopeKey) {
    let key = *scope_stack.last().unwrap();
    (&mut scopes[key], key)
}

#[allow(dead_code)]
impl<'src> ResolveState<'src> {
    fn new(source: &'src str) -> Self {
        let mut root_scope = Scope::new();
        root_scope.is_global = true;

        let err_scope = Scope::new();

        let mut scopes = SlotMap::with_key();
        let root_scope_key = scopes.insert(root_scope);
        let err_scope_key = scopes.insert(err_scope);

        ResolveState {
            allow_top_level_await: false,
            export_stars: Vec::new(),
            source,

            resolutions: HashMap::new(),
            functions: HashMap::new(),
            patterns: HashMap::new(),

            declarations: SlotMap::with_key(),
            scopes,

            root_scope: root_scope_key,
            err_scope: err_scope_key,

            errors: TlErrs::new(),

            placeholder_stack: Vec::new(),
            fn_stack: Vec::new(),
            scope_stack: Vec::new(),
        }
    }

    fn top_scope(&mut self) -> &mut Scope {
        top_scope(&self.scope_stack, &mut self.scopes)
    }

    fn top_scope_and_key(&mut self) -> (&mut Scope, ScopeKey) {
        top_scope_and_key(&self.scope_stack, &mut self.scopes)
    }

    fn resolve(&mut self, ident: &SIdent<'src>, lhs: bool) -> TlResult<DeclarationKey> {
        if let Some(found) = self.scope_stack.find_decl(self, ident) {
            if found.fn_local {
                return Ok(found.decl);
            }

            let Some(fn_ctx) = self.fn_stack.last_mut() else {
                return Err(simple_err("Internal error: no function context", ident.span).into());
            };

            fn_ctx.captures.insert(found.decl.clone().into());

            return Ok(found.decl);
        }

        let (scope, scope_key) = top_scope_and_key(&self.scope_stack, &mut self.scopes);

        if lhs {
            if scope.is_class || scope.is_global {
                let decl =
                    self.declarations
                        .insert_declaration(ident.clone(), scope_key, DeclType::Let);

                scope.locals.push(decl.clone());

                return Ok(decl);
            }
        }

        if !lhs {
            let decl =
                self.declarations
                    .insert_declaration(ident.clone(), self.root_scope, DeclType::Let);

            self.scopes[self.root_scope].locals.push(decl.clone());

            return Ok(decl);
        }

        return Err(simple_err(
            "Undeclared identifier; either declare with 'let' or mark as 'global'.",
            ident.span,
        ));
    }

    fn scoped<F, O>(&mut self, scope_key: ScopeKey, f: F) -> ScopedOutput<O>
    where
        F: FnOnce(&mut ResolveState<'src>) -> O,
    {
        self.scope_stack.push(scope_key);

        let result = f(self);

        let new_key = self.scope_stack.pop();

        if scope_key != new_key.unwrap() {
            panic!("Internal error: scope stack mismatch");
        }

        if !self.scopes[scope_key].lifted_decls.is_empty() {
            panic!("Internal error: lifted_decls should be empty at the end of scope processing");
        }

        ScopedOutput {
            value: result,
            scope_key,
        }
    }

    fn placeholder_guarded<F>(&mut self, span: Span, f: F) -> Indirect<SExpr<'src>>
    where
        F: FnOnce(&mut ResolveState<'src>) -> Indirect<SExpr<'src>>,
    {
        let mut temp_scope = Scope::new();
        temp_scope.is_fn = true;

        let temp_scope_key = self.scopes.insert(temp_scope);

        let ph_decl = self.declarations.insert_declaration(
            Ident("x".into()).spanned(span),
            temp_scope_key,
            DeclType::Let,
        );

        // Don't set it as a function argument, since placeholder should never shadow
        // a real variable called "x".
        // (Setting is_fn_arg will cause the python transpiler to use the *exact* name)

        // self.declarations[ph_decl].is_fn_arg = true;

        self.placeholder_stack
            .push(PlaceholderGuard::new(ph_decl, span));

        self.fn_stack.push(FnInfo::new());

        let result = f(self);

        let mut ph_fn_ctx = self.fn_stack.pop().unwrap();
        let placeholder_ctx = self.placeholder_stack.pop().unwrap();

        if placeholder_ctx.activated {
            if ph_fn_ctx.is_async || ph_fn_ctx.is_generator || ph_fn_ctx.is_do {
                self.errors.extend(simple_err(
                    "Await, bind, and yield are not allowed in placeholder functions",
                    placeholder_ctx.span,
                ));
            }

            ph_fn_ctx.is_placeholder = true;

            ph_fn_ctx.arg_names.push(placeholder_ctx.arg_decl);

            self.scopes[temp_scope_key]
                .locals
                .push(placeholder_ctx.arg_decl);

            let placeholder_pattern = SPatternInner::Capture(Some(Ident("x".into()).spanned(span)))
                .spanned(span)
                .indirect();

            self.patterns.insert(
                placeholder_pattern.as_ref().into(),
                PatternInfo {
                    default: true,
                    decls: vec![placeholder_ctx.arg_decl.clone()],
                },
            );

            let fn_node = SExprInner::Fn(vec![SArgDefItem::Arg(placeholder_pattern, None)], result)
                .spanned(span)
                .indirect();

            self.functions.insert(fn_node.as_ref().into(), ph_fn_ctx);

            return fn_node;
        } else {
            // placeholder not activated, so forward the function properties to the nearest
            self.scopes.remove(temp_scope_key);
            self.declarations.remove(ph_decl);

            if ph_fn_ctx.is_async {
                self.set_async(span);
            }
            if ph_fn_ctx.is_do {
                self.set_do(span);
            }
            if ph_fn_ctx.is_generator {
                self.set_generator(span);
            }

            if let Some(cur_fn_ctx) = self.fn_stack.last_mut() {
                cur_fn_ctx.captures.extend(ph_fn_ctx.captures);
            }

            return result;
        }
    }

    fn declare_in_top_scope(&mut self, name: SIdent<'src>, modifier: DeclType) -> DeclarationKey {
        let (class, global) = {
            let scope = self.top_scope();
            (scope.is_class, scope.is_global)
        };

        match modifier {
            DeclType::Global => {
                if global || class {
                    self.errors.extend(simple_err(
                        "Global declarations are not allowed at the module or class level",
                        name.span,
                    ));
                }
            }
            DeclType::Export => {
                if !global {
                    self.errors.extend(simple_err(
                        "Exports are only allowed at the module level",
                        name.span,
                    ));
                }
            }
            DeclType::Let | DeclType::Const => {
                if class || global {
                    if let Some(found) = self.scope_stack.find_decl(self, &name) {
                        self.errors.extend(
                            TlErrBuilder::new()
                                .message("Cannot shadow declarations in a class or global scope")
                                .span(name.span)
                                .context(
                                    "Previous declaration here",
                                    self.declarations[found.decl].loc,
                                )
                                .build(),
                        );
                    }
                }
            }
        };

        let key = *self.scope_stack.last().unwrap();

        let decl = self
            .declarations
            .insert_declaration(name.clone(), key, modifier);

        self.scopes[key].locals.push(decl);

        decl
    }

    fn set_async(&mut self, span: Span) -> () {
        if let Some(fn_ctx) = self.fn_stack.last_mut() {
            fn_ctx.is_async = true;
        } else if !self.allow_top_level_await {
            self.errors.extend(simple_err(
                "Await is only allowed in a function context",
                span,
            ));
        }
    }

    fn set_do(&mut self, span: Span) -> () {
        if let Some(fn_ctx) = self.fn_stack.last_mut() {
            fn_ctx.is_do = true;
        } else {
            self.errors.extend(simple_err(
                "Bind operator is only allowed in a function context",
                span,
            ));
        }
    }

    fn set_generator(&mut self, span: Span) -> () {
        if let Some(fn_ctx) = self.fn_stack.last_mut() {
            fn_ctx.is_generator = true;
        } else {
            self.errors.extend(simple_err(
                "Yield is only allowed in a function context",
                span,
            ));
        }
    }
}

fn simple_err(message: &str, span: Span) -> TlErrs {
    TlErrBuilder::new().message(message).span(span).build()
}

#[allow(dead_code)]
struct PlaceholderGuard {
    activated: bool,
    span: Span,
    arg_decl: DeclarationKey,
}

impl PlaceholderGuard {
    fn new(decl: DeclarationKey, span: Span) -> Self {
        Self {
            activated: false,
            arg_decl: decl,
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FnInfo {
    pub is_do: bool,
    pub is_async: bool,
    pub is_generator: bool,
    pub is_placeholder: bool,

    pub arg_names: Vec<DeclarationKey>,
    pub captures: HashSet<DeclarationKey>,
}

impl FnInfo {
    pub fn new() -> Self {
        Self {
            is_do: false,
            is_async: false,
            is_generator: false,
            is_placeholder: false,
            arg_names: Vec::new(),
            captures: HashSet::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Declaration<'src> {
    pub name: Ident<'src>,
    pub scope: ScopeKey,
    pub loc: Span,
    pub typ: Type,

    pub is_fn_arg: bool,
    pub is_exported: bool,
    pub is_const: bool,
}

trait DeclSlotMapExt<'src> {
    fn insert_declaration(
        &mut self,
        name: SIdent<'src>,
        scope: ScopeKey,
        modifier: DeclType,
    ) -> DeclarationKey;
}

impl<'src> DeclSlotMapExt<'src> for SlotMap<DeclarationKey, Declaration<'src>> {
    fn insert_declaration(
        &mut self,
        name: SIdent<'src>,
        scope: ScopeKey,
        modifier: DeclType,
    ) -> DeclarationKey {
        let decl = Declaration {
            name: name.value,
            scope,
            loc: name.span,
            typ: Type::Unprocessed,
            is_fn_arg: false,

            is_const: matches!(modifier, DeclType::Const),
            is_exported: matches!(modifier, DeclType::Export),
        };

        self.insert(decl)
    }
}

impl Display for Declaration<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Decl \"{}{}{}: {:?}\"",
            if self.is_exported { "export " } else { "" },
            if self.is_const { "const " } else { "" },
            self.name,
            self.typ
        )
    }
}

#[derive(Debug)]
pub struct Scope {
    pub locals: Vec<DeclarationKey>,

    pub is_class: bool,
    pub is_global: bool,
    pub is_fn: bool,

    /**
     * This enables special treatment for defining
     * recursive functions; lifted_decls are added to the
     * containing scope when processing the function body.
     */
    lifted_decls: Vec<DeclarationKey>,
}

impl Display for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Scope {{ {} locals, is_class: {}, is_global: {}, is_fn: {} }}",
            self.locals.len(),
            self.is_class,
            self.is_global,
            self.is_fn
        )
    }
}

impl Scope {
    fn new() -> Scope {
        Self {
            locals: Vec::new(),
            lifted_decls: Vec::new(),

            is_fn: false,
            is_class: false,
            is_global: false,
        }
    }
}

#[allow(dead_code)]
struct FindResult {
    decl: DeclarationKey,
    scope_local: bool,
    fn_local: bool,
}

trait ScopeStackExt {
    fn find_decl<'src>(
        &self,
        state: &ResolveState<'src>,
        ident: &SIdent<'src>,
    ) -> Option<FindResult>;
}

impl ScopeStackExt for Vec<ScopeKey> {
    fn find_decl<'src>(
        &self,
        state: &ResolveState<'src>,
        ident: &SIdent<'src>,
    ) -> Option<FindResult> {
        let mut fn_local = true;
        let mut scope_local = true;
        for scope in self.iter().rev().map(|s| &state.scopes[*s]) {
            if let Some(decl) = scope
                .locals
                .iter()
                .rev()
                .find(|d| state.declarations[**d].name == ident.value)
            {
                return Some(FindResult {
                    decl: *decl,
                    scope_local,
                    fn_local,
                });
            }

            if scope.is_fn {
                fn_local = false;
            }

            scope_local = false;
        }

        None
    }
}

pub struct PatternInfo {
    pub default: bool,
    pub decls: Vec<DeclarationKey>,
}

#[allow(dead_code)]
struct ScopedOutput<T> {
    pub value: T,
    pub scope_key: ScopeKey,
}

fn error_ident<'src>(state: &mut ResolveState<'src>, span: Span) -> Indirect<SExpr<'src>> {
    let ident = Ident("_".into()).spanned(span);
    let expr = Expr::Ident(ident.clone()).spanned(span).indirect();

    let decl = state
        .declarations
        .insert_declaration(ident.clone(), state.err_scope, DeclType::Let);

    state.resolutions.insert(expr.as_ref().into(), decl);

    expr
}

fn traverse_placeholder<'src>(state: &mut ResolveState<'src>, span: Span) -> Indirect<SExpr<'src>> {
    if let Some(placeholder_ctx) = state.placeholder_stack.last_mut() {
        placeholder_ctx.activated = true;

        let ident = Ident("x".into()).spanned(span);
        let expr = Expr::Ident(ident).spanned(span).indirect();
        state
            .resolutions
            .insert(expr.as_ref().into(), placeholder_ctx.arg_decl.clone());

        return expr;
    }

    panic!("Internal error: Placeholder context stack is empty");
}

struct PatternMeta<'src> {
    pub default: bool,
    pub captures: Vec<Ident<'src>>,
}

trait SPatternExt<'src> {
    fn traverse(
        self,
        state: &mut ResolveState<'src>,
    ) -> (Indirect<SPattern<'src>>, PatternMeta<'src>);
}

impl<'src> SPatternExt<'src> for Indirect<SPattern<'src>> {
    fn traverse(
        self,
        state: &mut ResolveState<'src>,
    ) -> (Indirect<SPattern<'src>>, PatternMeta<'src>) {
        let mut default = false;
        let mut captures = vec![];
        let span = self.span;

        let pattern: SPatternInner = match self.value {
            Pattern::Capture(cap) => {
                // Make sure that capture patterns do not start with an uppercase letter to
                // prevent unexpectedly shadowing a type

                if cap
                    .as_ref()
                    .is_some_and(|x| char::is_uppercase(x.value.0.chars().nth(0).unwrap_or('_')))
                {
                    state.errors.extend(simple_err(
                        "Capture patterns must start with a lowercase letter; to match a type, add '()'",
                        self.span,
                    ));
                }

                if let Some(cap) = cap.clone() {
                    captures.push(cap.value.clone());
                }

                default = true;

                Pattern::Capture(cap)
            }
            Pattern::As(pattern, name) => {
                let (pattern, meta) = pattern.traverse(state);
                captures.extend(meta.captures);
                captures.push(name.value.clone());
                default |= meta.default;
                Pattern::As(pattern, name)
            }
            Pattern::Or(items) => {
                let mut iter = items.into_iter();
                let Some(initial) = iter.next() else {
                    return (
                        Pattern::Capture(None).spanned(span).indirect(),
                        PatternMeta {
                            default: false,
                            captures: vec![],
                        },
                    );
                };

                let mut items = vec![];

                let (pattern, meta) = initial.traverse(state);
                items.push(pattern);
                default = meta.default;
                captures.extend(meta.captures);
                captures.sort_by_key(|x| x.0.clone());

                for item in iter {
                    let span = item.span;

                    if default {
                        state.errors.extend(simple_err(
                            "Default pattern makes remaining patterns unreachable",
                            span,
                        ));
                    }

                    default |= meta.default;
                    let (pattern, mut meta) = item.traverse(state);

                    meta.captures.sort_by_key(|x| x.0.clone());

                    if captures != meta.captures {
                        state
                            .errors
                            .extend(simple_err("Or patterns must bind the same names", span));
                    }

                    items.push(pattern);
                }

                Pattern::Or(items)
            }
            Pattern::Value(v) => Pattern::Value(v.traverse(state)),
            Pattern::Literal(v) => Pattern::Literal(v),
            Pattern::Sequence(items) => {
                let items = items
                    .into_iter()
                    .map(|item| match item {
                        PatternSequenceItem::Item(inner) => {
                            let (pattern, meta) = inner.traverse(state);
                            captures.extend(meta.captures);
                            PatternSequenceItem::Item(pattern)
                        }
                        PatternSequenceItem::Spread(inner) => {
                            if let Some(inner) = inner.clone() {
                                captures.push(inner.value.clone());
                            }
                            PatternSequenceItem::Spread(inner)
                        }
                    })
                    .collect::<Vec<_>>();

                Pattern::Sequence(items)
            }
            Pattern::Mapping(items) => {
                let items = items
                    .into_iter()
                    .map(|item| match item {
                        PatternMappingItem::Ident(id) => {
                            captures.push(id.value.clone());
                            PatternMappingItem::Ident(id)
                        }
                        PatternMappingItem::Item(key, value) => {
                            let (pattern, meta) = value.traverse(state);
                            captures.extend(meta.captures);

                            PatternMappingItem::Item(key, pattern)
                        }
                        PatternMappingItem::Spread(inner) => {
                            if let Some(inner) = inner.clone() {
                                captures.push(inner.value.clone());
                            }

                            PatternMappingItem::Spread(inner)
                        }
                    })
                    .collect::<Vec<_>>();

                Pattern::Mapping(items)
            }
            Pattern::Class(cls, items) => {
                let items = items
                    .into_iter()
                    .map(|item| match item {
                        PatternClassItem::Item(inner) => {
                            let (pattern, meta) = inner.traverse(state);
                            captures.extend(meta.captures);
                            PatternClassItem::Item(pattern)
                        }
                        PatternClassItem::Kw(key, inner) => {
                            let (pattern, meta) = inner.traverse(state);
                            captures.extend(meta.captures);
                            PatternClassItem::Kw(key, pattern)
                        }
                    })
                    .collect::<Vec<_>>();

                Pattern::Class(cls.traverse(state), items)
            }
        };

        (
            pattern.spanned(span).indirect(),
            PatternMeta { default, captures },
        )
    }
}

fn traverse_list_items<'src>(
    state: &mut ResolveState<'src>,
    items: Vec<SListItem<'src>>,
) -> Vec<SListItem<'src>> {
    items
        .into_iter()
        .map(|i| -> SListItem<'src> {
            match i {
                ListItem::Item(i) => ListItem::<STree>::Item(i.traverse(state)),
                ListItem::Spread(i) => ListItem::<STree>::Spread(i.traverse(state)),
            }
        })
        .collect::<Vec<_>>()
}

fn traverse_call_items<'src>(
    state: &mut ResolveState<'src>,
    items: Vec<SCallItem<'src>>,
) -> Vec<SCallItem<'src>> {
    items
        .into_iter()
        .map(|item| match item {
            CallItem::Arg(i) => CallItem::Arg(i.traverse_deep_guarded(state)),
            CallItem::Kwarg(name, i) => {
                CallItem::Kwarg(name.clone(), i.traverse_deep_guarded(state))
            }
            CallItem::ArgSpread(i) => CallItem::ArgSpread(i.traverse_deep_guarded(state)),
            CallItem::KwargSpread(i) => CallItem::KwargSpread(i.traverse_deep_guarded(state)),
        })
        .collect::<Vec<_>>()
}

fn pattern_scoped<'src>(
    state: &mut ResolveState<'src>,
    pattern: Indirect<SPattern<'src>>,
) -> (Indirect<SPattern<'src>>, ScopeKey, PatternMeta<'src>) {
    let scope = Scope::new();
    let scope_key = state.scopes.insert(scope);

    let (pattern, meta) = pattern.traverse(state);

    let decls = meta
        .captures
        .iter()
        .map(|x| {
            state.declarations.insert_declaration(
                x.clone().spanned(pattern.span),
                scope_key,
                DeclType::Let,
            )
        })
        .collect::<Vec<_>>();

    state.patterns.insert(
        pattern.as_ref().into(),
        PatternInfo {
            default: meta.default,
            decls: decls.clone(),
        },
    );

    state.scopes[scope_key].locals.extend(decls.clone());

    (pattern, scope_key, meta)
}

trait SExprExt<'src> {
    fn traverse(self, state: &mut ResolveState<'src>) -> Indirect<SExpr<'src>>;
    fn traverse_expecting_scope(self, state: &mut ResolveState<'src>) -> Indirect<SExpr<'src>>;
    fn traverse_guarded(self, state: &mut ResolveState<'src>) -> Indirect<SExpr<'src>>;
    fn traverse_deep_guarded(self, state: &mut ResolveState<'src>) -> Indirect<SExpr<'src>>;
    fn traverse_full(
        self,
        state: &mut ResolveState<'src>,
        expect_scope: bool,
    ) -> Indirect<SExpr<'src>>;
}

impl<'src> SExprExt<'src> for Indirect<SExpr<'src>> {
    fn traverse_deep_guarded(self, state: &mut ResolveState<'src>) -> Indirect<SExpr<'src>> {
        match self.value {
            Expr::Placeholder => traverse_placeholder(state, self.span),
            _ => self.traverse_guarded(state),
        }
    }

    fn traverse_guarded(self, state: &mut ResolveState<'src>) -> Indirect<SExpr<'src>> {
        state.placeholder_guarded(self.span, |state| self.traverse(state))
    }

    fn traverse_expecting_scope(self, state: &mut ResolveState<'src>) -> Indirect<SExpr<'src>> {
        state.placeholder_guarded(self.span, |state| self.traverse_full(state, true))
    }

    fn traverse(self: Self, state: &mut ResolveState<'src>) -> Indirect<SExpr<'src>> {
        self.traverse_full(state, false)
    }

    fn traverse_full(
        self: Self,
        state: &mut ResolveState<'src>,
        expect_scope: bool,
    ) -> Indirect<SExpr<'src>> {
        let span = self.span;

        let t = match self.value {
            Expr::Literal(_) => return self,
            Expr::Ident(ident) => {
                let decl = match state.resolve(&ident, false) {
                    Ok(decl) => decl,
                    Err(errs) => {
                        state.errors.extend(errs);
                        return error_ident(state, span);
                    }
                };

                let expr = Expr::Ident(ident).spanned(span).indirect();
                state.resolutions.insert(expr.as_ref().into(), decl.clone());

                return expr;
            }
            Expr::Checked(expr, pattern) => {
                let pattern = if let Some(pattern) = pattern {
                    let (pattern, meta) = pattern.traverse(state);

                    if !meta.captures.is_empty() {
                        state.errors.extend(
                            simple_err(
                                "Non-'_' captures in a 'matches' are only allowed in 'if ... matches ...', or 'if ... matches not ...' constructions",
                                pattern.span,
                            )
                        );
                    }

                    if meta.default {
                        None
                    } else {
                        state.patterns.insert(
                            pattern.as_ref().into(),
                            PatternInfo {
                                default: meta.default,
                                decls: vec![],
                            },
                        );
                        Some(pattern)
                    }
                } else {
                    None
                };

                Expr::Checked(expr.traverse_guarded(state), pattern)
            }
            Expr::Decorated(deco, expr) => Expr::Call(
                deco.traverse(state),
                vec![CallItem::Arg(expr.traverse(state))],
            ),
            Expr::Matches(x, pattern) => {
                let (pattern, meta) = pattern.traverse(state);
                if !meta.captures.is_empty() {
                    state.errors.extend(simple_err(
                        "Non-'_' capture patterns are not allowed in bare 'matches'-expressions",
                        pattern.span,
                    ));
                }

                state.patterns.insert(
                    pattern.as_ref().into(),
                    PatternInfo {
                        default: meta.default,
                        decls: vec![],
                    },
                );

                Expr::Matches(x.traverse(state), pattern)
            }
            Expr::Placeholder => return traverse_placeholder(state, span),
            Expr::Block(stmts) => {
                let new_stmts = if expect_scope {
                    let mut new_stmts = Vec::new();
                    for stmt in stmts {
                        let stmt = stmt.traverse(state);
                        new_stmts.push(stmt);
                    }
                    new_stmts
                } else {
                    let scope = state.scopes.insert(Scope::new());
                    state
                        .scoped(scope, |state| {
                            let mut new_stmts = Vec::new();
                            for stmt in stmts {
                                let stmt = stmt.traverse(state);
                                new_stmts.push(stmt);
                            }
                            new_stmts
                        })
                        .value
                };

                Expr::Block(new_stmts)
            }
            Expr::If(cond, then, else_) => 'block: {
                let cond_span = cond.span;

                match &cond.value {
                    Expr::Matches(..) => {
                        let Expr::Matches(expr, pattern) = cond.value else {
                            unreachable!();
                        };

                        let (pattern, then_scope, _meta) = pattern_scoped(state, pattern);

                        let then = state
                            .scoped(then_scope, |state| then.traverse_expecting_scope(state))
                            .value;

                        let else_ = else_.map(|else_| else_.traverse(state));

                        break 'block SExprInner::If(
                            Expr::Matches(expr.traverse(state), pattern)
                                .spanned(cond_span)
                                .indirect(),
                            then,
                            else_,
                        );
                    }
                    Expr::Unary(UnaryOp::Not, inner) => match &inner.value {
                        Expr::Matches(_, pattern) => {
                            let (pattern, else_scope, meta) =
                                pattern_scoped(state, pattern.clone());

                            if !meta.captures.is_empty() {
                                let Expr::Unary(UnaryOp::Not, inner) = cond.value else {
                                    unreachable!();
                                };

                                let Expr::Matches(expr, _) = inner.value else {
                                    unreachable!();
                                };

                                let then = then.traverse(state);

                                let else_locals = state.scopes[else_scope].locals.clone();

                                state.top_scope().locals.extend(else_locals);

                                break 'block SExprInner::If(
                                    SExprInner::Unary(
                                        UnaryOp::Not,
                                        SExprInner::Matches(expr.traverse(state), pattern)
                                            .spanned(cond_span)
                                            .indirect(),
                                    )
                                    .spanned(cond_span)
                                    .indirect(),
                                    then,
                                    else_.map(|x| x.traverse(state)),
                                );
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                }

                Expr::If(
                    cond.traverse(state),
                    then.traverse(state),
                    else_.map(|else_| else_.traverse(state)),
                )
            }
            Expr::Match(subject, cases) => {
                let subject = subject.traverse(state);
                let cases = cases
                    .into_iter()
                    .map(|case| {
                        let (pattern, scope) = if let Some(pattern) = case.pattern {
                            let (pattern, scope, _meta) = pattern_scoped(state, pattern);
                            (Some(pattern), scope)
                        } else {
                            (None, state.scopes.insert(Scope::new()))
                        };

                        SMatchCase {
                            pattern,
                            guard: case.guard.map(|g| g.traverse(state)),
                            body: state
                                .scoped(scope, |state| case.body.traverse_expecting_scope(state))
                                .value,
                        }
                    })
                    .collect::<Vec<_>>();

                Expr::Match(subject, cases)
            }
            Expr::Class(bases, body) => {
                let bases = traverse_call_items(state, bases);

                let mut scope = Scope::new();
                scope.is_class = true;
                let scope = state.scopes.insert(scope);

                let body = state
                    .scoped(scope, |state| body.traverse_expecting_scope(state))
                    .value;

                Expr::Class(bases, body)
            }
            Expr::Fn(arg_def_items, body) => {
                let mut decls = vec![];

                let mut scope = Scope::new();
                scope.is_fn = true;
                let scope = state.scopes.insert(scope);

                let items = arg_def_items
                    .into_iter()
                    .map(|item| match item {
                        ArgDefItem::Arg(arg, default) => {
                            let (pattern, meta) = arg.traverse(state);

                            let mut arg_decls = vec![];

                            for capture in meta.captures {
                                let cap = capture.spanned(pattern.span);

                                let decl = state.declarations.insert_declaration(
                                    cap,
                                    scope,
                                    DeclType::Let,
                                );
                                state.declarations[decl].is_fn_arg = true;

                                arg_decls.push(decl);
                            }

                            state.patterns.insert(
                                pattern.as_ref().into(),
                                PatternInfo {
                                    default: meta.default,
                                    decls: arg_decls.iter().map(|x| x.clone()).collect(),
                                },
                            );

                            decls.extend(arg_decls);

                            ArgDefItem::Arg(pattern, default.map(|x| x.traverse(state)))
                        }
                        ArgDefItem::ArgSpread(arg) => {
                            let decl = state.declarations.insert_declaration(
                                arg.clone(),
                                scope,
                                DeclType::Let,
                            );
                            state.declarations[decl].is_fn_arg = true;
                            decls.push(decl);

                            ArgDefItem::ArgSpread(arg)
                        }
                        ArgDefItem::KwargSpread(arg) => {
                            let decl = state.declarations.insert_declaration(
                                arg.clone(),
                                scope,
                                DeclType::Let,
                            );
                            state.declarations[decl].is_fn_arg = true;
                            decls.push(decl);

                            ArgDefItem::KwargSpread(arg)
                        }
                    })
                    .collect::<Vec<_>>();

                let mut fn_info = FnInfo::new();
                for (i, decl) in decls.iter().enumerate() {
                    if let Some(found) = decls[..i]
                        .iter()
                        .find(|x| state.declarations[**x].name == state.declarations[*decl].name)
                    {
                        state.errors.extend(
                            TlErrBuilder::new()
                                .message("Duplicate declaration in function arguments")
                                .context("First declared here", state.declarations[*found].loc)
                                .span(state.declarations[*decl].loc)
                                .build(),
                        );
                    }

                    fn_info.arg_names.push(*decl);
                }

                state.scopes[scope].locals.extend(decls);

                state.fn_stack.push(fn_info);

                let n_scopes = state.scope_stack.len();

                let (n_fndef_lifted, n_classdef_lifted) = {
                    // lift decls
                    let fndef_scope = state.top_scope();
                    let n_fndef_lifted = fndef_scope.lifted_decls.len();

                    fndef_scope
                        .locals
                        .extend(fndef_scope.lifted_decls.drain(..));

                    let n_classdef_lifted = if n_scopes >= 2 {
                        let classdef_scope = &mut state.scopes[state.scope_stack[n_scopes - 2]];
                        let n = classdef_scope.lifted_decls.len();

                        classdef_scope
                            .locals
                            .extend(classdef_scope.lifted_decls.drain(..));

                        n
                    } else {
                        0
                    };

                    (n_fndef_lifted, n_classdef_lifted)
                };

                let body = state.scoped(scope, |state| body.traverse(state)).value;

                {
                    // put back
                    let fndef_scope = state.top_scope();
                    let nlocals = fndef_scope.locals.len();

                    fndef_scope
                        .lifted_decls
                        .extend(fndef_scope.locals.drain(nlocals - n_fndef_lifted..));

                    if n_classdef_lifted > 0 {
                        let classdef_scope = &mut state.scopes[state.scope_stack[n_scopes - 2]];
                        let nlocals = classdef_scope.locals.len();

                        classdef_scope
                            .lifted_decls
                            .extend(classdef_scope.locals.drain(nlocals - n_classdef_lifted..));
                    }
                }

                let fn_ctx = state.fn_stack.pop().unwrap();
                let expr = Expr::Fn(items, body).spanned(span).indirect();

                state.functions.insert(expr.as_ref().into(), fn_ctx);

                return expr;
            }
            Expr::Fstr(spanned, items) => Expr::Fstr(
                spanned,
                items
                    .into_iter()
                    .map(|(expr, suffix)| {
                        (
                            FmtExpr {
                                expr: expr.expr.traverse(state),
                                fmt: expr.fmt,
                            },
                            suffix,
                        )
                    })
                    .collect::<Vec<_>>(),
            ),

            // special case for pipe
            Expr::Binary(binary_op, mut x, mut y) => {
                if let BinaryOp::Pipe = binary_op {
                    x = x.traverse_guarded(state);
                    y = y.traverse_guarded(state);
                } else {
                    x = x.traverse(state);
                    y = y.traverse(state);
                }

                Expr::Binary(binary_op, x, y)
            }

            // postfix
            Expr::Attribute(expr, attr) => Expr::Attribute(expr.traverse(state), attr.clone()),
            Expr::MappedAttribute(expr, attr) => Expr::MappedAttribute(expr.traverse(state), attr),
            Expr::RawAttribute(expr, attr) => Expr::RawAttribute(expr.traverse(state), attr),
            Expr::MappedRawAttribute(expr, spanned) => {
                Expr::MappedRawAttribute(expr.traverse(state), spanned)
            }
            Expr::ScopedAttribute(expr, value) => {
                Expr::ScopedAttribute(expr.traverse(state), value.traverse_guarded(state))
            }
            Expr::MappedScopedAttribute(expr, value) => {
                Expr::MappedScopedAttribute(expr.traverse(state), value.traverse_guarded(state))
            }
            Expr::Call(a, items) => {
                Expr::Call(a.traverse(state), traverse_call_items(state, items))
            }
            Expr::MappedCall(a, call_items) => {
                Expr::MappedCall(a.traverse(state), traverse_call_items(state, call_items))
            }
            Expr::Subscript(x, list_items) => {
                Expr::Subscript(x.traverse(state), traverse_list_items(state, list_items))
            }
            Expr::MappedSubscript(expr, list_items) => {
                Expr::MappedSubscript(expr.traverse(state), traverse_list_items(state, list_items))
            }
            Expr::Tuple(items) => Expr::Tuple(traverse_list_items(state, items)),
            Expr::List(items) => Expr::List(traverse_list_items(state, items)),
            Expr::Mapping(items) => Expr::Mapping(
                items
                    .into_iter()
                    .map(|item| match item {
                        MappingItem::Item(key, value) => {
                            MappingItem::Item(key.traverse(state), value.traverse(state))
                        }
                        MappingItem::Spread(i) => MappingItem::Spread(i.traverse(state)),
                        MappingItem::Ident(i) => MappingItem::Ident(i),
                    })
                    .collect::<Vec<_>>(),
            ),
            Expr::Slice(a, b, c) => Expr::Slice(
                a.map(|e| e.traverse(state)),
                b.map(|e| e.traverse(state)),
                c.map(|e| e.traverse(state)),
            ),
            Expr::Unary(unary_op, expr) => {
                if let UnaryOp::Bind = unary_op {
                    state.set_do(span);
                }

                Expr::Unary(unary_op, expr.traverse(state))
            }
            Expr::Await(x) => {
                state.set_async(span);
                Expr::Await(x.traverse(state))
            }
            Expr::Yield(x) => {
                state.set_generator(span);
                Expr::Yield(x.traverse(state))
            }
            Expr::YieldFrom(x) => {
                state.set_generator(span);
                Expr::YieldFrom(x.traverse(state))
            }
        };

        t.spanned(span).indirect()
    }
}

fn traverse_assign_lhs<'src>(
    state: &mut ResolveState<'src>,
    lhs: Indirect<SExpr<'src>>,
    typ: Option<DeclType>,
) -> Indirect<SExpr<'src>> {
    match lhs.value {
        Expr::Ident(ident) => {
            if let Some(decl) = typ {
                state.declare_in_top_scope(ident.clone(), decl);
            }

            let decl = match state.resolve(&ident, true) {
                Ok(decl) => decl,
                Err(errs) => {
                    state.errors.extend(errs);
                    return error_ident(state, lhs.span);
                }
            };

            let expr = Expr::Ident(ident).spanned(lhs.span).indirect();

            state.resolutions.insert(expr.as_ref().into(), decl.clone());
            expr
        }
        Expr::Tuple(items) => {
            let items = items
                .into_iter()
                .map(|item| match item {
                    ListItem::Item(inner) => {
                        let inner = traverse_assign_lhs(state, inner, typ);
                        ListItem::Item(inner)
                    }
                    ListItem::Spread(inner) => {
                        let inner = traverse_assign_lhs(state, inner, typ);
                        ListItem::Spread(inner)
                    }
                })
                .collect::<Vec<_>>();
            Expr::Tuple(items).spanned(lhs.span).indirect()
        }
        Expr::List(items) => {
            let items = items
                .into_iter()
                .map(|item| match item {
                    ListItem::Item(inner) => {
                        let inner = traverse_assign_lhs(state, inner, typ);
                        ListItem::Item(inner)
                    }
                    ListItem::Spread(inner) => {
                        let inner = traverse_assign_lhs(state, inner, typ);
                        ListItem::Spread(inner)
                    }
                })
                .collect::<Vec<_>>();
            Expr::List(items).spanned(lhs.span).indirect()
        }
        Expr::Mapping(items) => {
            let items = items
                .into_iter()
                .map(|item| match item {
                    MappingItem::Item(key, value) => MappingItem::Item(
                        key.traverse_guarded(state),
                        traverse_assign_lhs(state, value, typ),
                    ),
                    MappingItem::Spread(inner) => {
                        MappingItem::Spread(traverse_assign_lhs(state, inner, typ))
                    }
                    MappingItem::Ident(inner) => {
                        MappingItem::Ident(traverse_assign_lhs(state, inner, typ))
                    }
                })
                .collect::<Vec<_>>();
            Expr::Mapping(items).spanned(lhs.span).indirect()
        }
        _ => lhs.traverse(state),
    }
}

trait SStmtExt<'src> {
    fn traverse(self, state: &mut ResolveState<'src>) -> Indirect<SStmt<'src>>;
}

impl<'src> SStmtExt<'src> for Indirect<SStmt<'src>> {
    fn traverse(self, state: &mut ResolveState<'src>) -> Indirect<SStmt<'src>> {
        let span = self.span;

        let stmt = match self.value {
            Stmt::Decl(idents, typ) => {
                for ident in idents.iter() {
                    state.declare_in_top_scope(ident.clone(), typ);
                }

                Stmt::Decl(idents, typ)
            }
            Stmt::Assign(lhs, rhs, typ) => {
                // LHS will create lifted decls
                let lhs = traverse_assign_lhs(state, lhs, typ);

                let rhs = rhs.traverse_guarded(state);

                // commit lifted
                let scope = state.top_scope();
                scope.locals.extend(scope.lifted_decls.drain(..));

                Stmt::Assign(lhs, rhs, typ)
            }
            Stmt::Expr(expr) => Stmt::Expr(expr.traverse_guarded(state)),
            Stmt::Return(expr) => Stmt::Return(expr.traverse_guarded(state)),
            Stmt::While(cond, body) => {
                Stmt::While(cond.traverse_guarded(state), body.traverse_guarded(state))
            }
            Stmt::For(pattern, iter, body) => {
                let (pattern, scope, _meta) = pattern_scoped(state, pattern);

                let body = state
                    .scoped(scope, |state| body.traverse_guarded(state))
                    .value;

                Stmt::For(pattern, iter.traverse_guarded(state), body)
            }
            Stmt::Try(body, cases, finally) => {
                let body = body.traverse_guarded(state);
                let cases = cases
                    .into_iter()
                    .map(|case| {
                        let (pattern, scope) = if let Some(pattern) = case.pattern {
                            let (pattern, scope, _meta) = pattern_scoped(state, pattern);
                            (Some(pattern), scope)
                        } else {
                            (None, state.scopes.insert(Scope::new()))
                        };

                        let body = state
                            .scoped(scope, |state| case.body.traverse_guarded(state))
                            .value;

                        MatchCase {
                            pattern,
                            guard: case.guard.map(|x| x.traverse_guarded(state)),
                            body,
                        }
                    })
                    .collect::<Vec<_>>();
                let finally = finally.map(|x| x.traverse_guarded(state));

                Stmt::Try(body, cases, finally)
            }
            Stmt::Assert(a, b) => Stmt::Assert(
                a.traverse_guarded(state),
                b.map(|x| x.traverse_guarded(state)),
            ),
            Stmt::Raise(expr) => Stmt::Raise(expr.map(|x| x.traverse_guarded(state))),
            Stmt::Import(import_stmt) => {
                let scope_key = *state.scope_stack.last().unwrap();
                let scope = &mut state.scopes[scope_key];

                if import_stmt.reexport {
                    if !scope.is_global {
                        state.errors.extend(simple_err(
                            "Re-exporting imports is only allowed in the global scope",
                            span,
                        ));
                    }
                }

                match &import_stmt.imports {
                    ImportList::Star => {
                        // TODO
                        let base_module = import_stmt
                            .trunk
                            .iter()
                            .map(|ident| ident.value.0.as_ref())
                            .collect::<Vec<_>>()
                            .join(".");

                        let full_module = ".".repeat(import_stmt.level) + &base_module;

                        if import_stmt.reexport {
                            state
                                .export_stars
                                .push(Ident(full_module.into()).spanned(span));
                        }
                    }
                    ImportList::Leaves(imports) => {
                        for (ident, as_name) in imports {
                            let decl = state.declarations.insert_declaration(
                                as_name.clone().unwrap_or(ident.clone()),
                                scope_key,
                                if import_stmt.reexport {
                                    DeclType::Export
                                } else {
                                    DeclType::Let
                                },
                            );

                            scope.locals.push(decl);
                        }
                    }
                }

                Stmt::Import(import_stmt)
            }
            Stmt::Break => Stmt::Break,
            Stmt::Module => Stmt::Module,
            Stmt::Continue => Stmt::Continue,
        };

        stmt.spanned(span).indirect()
    }
}

pub fn resolve_names<'src>(
    source: &'src str,
    expr: impl IntoIndirect<SExpr<'src>>,
    allow_await: bool,
) -> (ResolveState<'src>, TlErrs, Indirect<SExpr<'src>>) {
    let mut state = ResolveState::new(source);
    state.allow_top_level_await = allow_await;

    let expr = state
        .scoped(state.root_scope.clone(), |state| {
            expr.indirect().traverse_expecting_scope(state)
        })
        .value;

    if !state.fn_stack.is_empty() {
        panic!("Function context stack is not empty after resolving names");
    }

    if !state.scope_stack.is_empty() {
        panic!("Scope context stack is not empty after resolving names");
    }

    if !state.placeholder_stack.is_empty() {
        panic!("Placeholder context stack is not empty after resolving names");
    }

    let errors = state.errors.clone();

    (state, errors, expr)
}
