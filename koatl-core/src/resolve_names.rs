use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::ptr::{self};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::transform::TfErrs;
use crate::{
    transform::{TfErrBuilder, TfResult},
    types::Type,
};
use parser::ast::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RefHash {
    id: usize,
}

impl<T> From<&T> for RefHash {
    fn from(value: &T) -> Self {
        RefHash {
            id: (value as *const T) as usize,
        }
    }
}

#[derive(Debug, Clone)]
pub struct RcKey<T>(pub Rc<T>);

impl<T> Deref for RcKey<T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.0.deref()
    }
}

impl<T> From<Rc<T>> for RcKey<T> {
    fn from(value: Rc<T>) -> Self {
        RcKey(value)
    }
}

impl<T> Hash for RcKey<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.0.as_ref() as *const T).hash(state);
    }
}

impl<T> PartialEq for RcKey<T> {
    fn eq(&self, other: &Self) -> bool {
        // Compare by pointer address.
        ptr::eq(self.0.as_ref(), other.0.as_ref())
    }
}

impl<T> Eq for RcKey<T> {}

struct PlaceholderCtx<'src> {
    activated: bool,
    decl: DeclarationRef<'src>,
    span: Span,
}

impl<'src> PlaceholderCtx<'src> {
    fn new(decl: DeclarationRef<'src>, span: Span) -> Self {
        Self {
            activated: false,
            decl,
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FnInfo<'src> {
    pub is_do: bool,
    pub is_async: bool,
    pub is_generator: bool,
    pub is_placeholder: bool,

    pub arg_names: HashMap<Ident<'src>, DeclarationRef<'src>>,
    pub captures: HashSet<RcKey<RefCell<Declaration<'src>>>>,
}

impl FnInfo<'_> {
    pub fn new() -> Self {
        Self {
            is_do: false,
            is_async: false,
            is_generator: false,
            is_placeholder: false,
            arg_names: HashMap::new(),
            captures: HashSet::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Declaration<'src> {
    pub name: Ident<'src>,
    pub scope: Rc<RefCell<Scope<'src>>>,
    pub loc: Span,
    pub typ: Type,
    pub modifier: DeclType,
    pub is_fn_arg: bool,
}

pub type DeclarationRef<'src> = Rc<RefCell<Declaration<'src>>>;

impl<'src> Declaration<'src> {
    pub fn new(
        name: SIdent<'src>,
        scope: Rc<RefCell<Scope<'src>>>,
        modifier: DeclType,
    ) -> DeclarationRef<'src> {
        Rc::new(RefCell::new(Declaration {
            name: name.value,
            scope,
            loc: name.span,
            typ: Type::Unprocessed,
            modifier,
            is_fn_arg: false,
        }))
    }
}

#[derive(Debug)]
pub struct Scope<'src> {
    pub locals: Vec<DeclarationRef<'src>>,

    pub is_class_scope: bool,
    pub is_global_scope: bool,
    pub is_fn_scope: bool,

    /**
     * This enables special treatment for defining
     * recursive functions; lifted_decls are added to the
     * containing scope when processing the function body.
     */
    lifted_decls: Vec<DeclarationRef<'src>>,
}

type ScopeRef<'src> = Rc<RefCell<Scope<'src>>>;

impl<'src> Scope<'src> {
    fn new() -> ScopeRef<'src> {
        Rc::new(RefCell::new(Self {
            locals: Vec::new(),
            lifted_decls: Vec::new(),
            is_fn_scope: false,
            is_class_scope: false,
            is_global_scope: false,
        }))
    }
}

struct FindResult<'src> {
    decl: DeclarationRef<'src>,
    local: bool,
    fn_local: bool,
}

trait ScopeStackExt<'src> {
    fn find_decl(&self, ident: &SIdent<'src>) -> Option<FindResult<'src>>;
}

impl<'src> ScopeStackExt<'src> for Vec<Rc<RefCell<Scope<'src>>>> {
    fn find_decl(&self, ident: &SIdent<'src>) -> Option<FindResult<'src>> {
        let mut fn_local = true;
        let mut local = true;
        for scope in self.iter().rev().map(|s| s.borrow()) {
            if let Some(decl) = scope
                .locals
                .iter()
                .rev()
                .find(|d| d.borrow().name == ident.value)
            {
                return Some(FindResult {
                    decl: decl.clone(),
                    local,
                    fn_local,
                });
            }

            if scope.is_fn_scope {
                fn_local = false;
            }

            local = false;
        }

        None
    }
}

pub struct PatternInfo<'src> {
    pub default: bool,
    pub decls: Vec<DeclarationRef<'src>>,
}

#[allow(dead_code)]
pub struct ResolveState<'src> {
    pub source: &'src str,
    pub allow_top_level_await: bool,

    pub export_stars: Vec<String>,

    pub root_scope: Rc<RefCell<Scope<'src>>>,
    pub types: HashMap<RefHash, Type>,
    pub resolutions: HashMap<RefHash, DeclarationRef<'src>>,
    pub functions: HashMap<RefHash, FnInfo<'src>>,
    pub patterns: HashMap<RefHash, PatternInfo<'src>>,

    pub errors: TfErrs,

    placeholder_ctx_stack: Vec<PlaceholderCtx<'src>>,
    fn_ctx_stack: Vec<FnInfo<'src>>,
    scope_ctx_stack: Vec<Rc<RefCell<Scope<'src>>>>,

    scope_id_counter: usize,
}

struct ScopedOutput<'src, T> {
    pub value: T,
    pub scope: Rc<RefCell<Scope<'src>>>,
}

impl<'src> ResolveState<'src> {
    fn new(source: &'src str) -> Self {
        ResolveState {
            allow_top_level_await: false,
            source,

            export_stars: Vec::new(),

            root_scope: Scope::new(),
            types: HashMap::new(),
            resolutions: HashMap::new(),
            functions: HashMap::new(),
            patterns: HashMap::new(),

            errors: TfErrs::new(),

            scope_id_counter: 0,

            placeholder_ctx_stack: Vec::new(),
            fn_ctx_stack: Vec::new(),
            scope_ctx_stack: Vec::new(),
        }
    }

    fn resolve(&mut self, ident: &SIdent<'src>, lhs: bool) -> TfResult<DeclarationRef<'src>> {
        if let Some(found) = self.scope_ctx_stack.find_decl(ident) {
            if found.fn_local {
                return Ok(found.decl);
            }

            let Some(fn_ctx) = self.fn_ctx_stack.last_mut() else {
                return Err(TfErrBuilder::default()
                    .message("Internal error: no function context")
                    .span(ident.span)
                    .build_errs());
            };

            fn_ctx.captures.insert(found.decl.clone().into());

            return Ok(found.decl);
        }

        let scope = self.scope_ctx_stack.last().unwrap();

        if lhs {
            if scope.borrow().is_class_scope || scope.borrow().is_global_scope {
                let decl = Declaration::new(ident.clone(), scope.clone(), DeclType::Let);

                scope.borrow_mut().locals.push(decl.clone());

                return Ok(decl);
            }
        }

        if !lhs {
            let decl = Declaration::new(ident.clone(), scope.clone(), DeclType::Let);

            let global = &self.scope_ctx_stack[0];
            global.borrow_mut().locals.push(decl.clone());

            return Ok(decl);
        }

        return Err(TfErrBuilder::default()
            .message("Undeclared identifier; either declare with 'let' or mark as 'global'.")
            .span(ident.span)
            .build_errs());
    }

    fn scoped<F, O>(&mut self, scope: Rc<RefCell<Scope<'src>>>, f: F) -> ScopedOutput<'src, O>
    where
        F: FnOnce(&mut ResolveState<'src>) -> O,
    {
        self.scope_id_counter += 1;
        self.scope_ctx_stack.push(scope);
        let result = f(self);
        let scope = self.scope_ctx_stack.pop().unwrap();

        if !scope.borrow().lifted_decls.is_empty() {
            panic!("Internal error: lifted_decls should be empty at the end of scope processing");
        }

        ScopedOutput {
            value: result,
            scope,
        }
    }

    fn placeholder_guarded<F>(&mut self, span: Span, f: F) -> Indirect<SExpr<'src>>
    where
        F: FnOnce(&mut ResolveState<'src>) -> Indirect<SExpr<'src>>,
    {
        let scope = Scope::new();
        scope.borrow_mut().is_fn_scope = true;

        let ph_decl = Declaration::new(
            Ident("x".into()).spanned(span),
            scope.clone(),
            DeclType::Let,
        );

        self.placeholder_ctx_stack
            .push(PlaceholderCtx::new(ph_decl, span));
        self.fn_ctx_stack.push(FnInfo::new());

        let result = f(self);

        let mut fn_ctx = self.fn_ctx_stack.pop().unwrap();
        let placeholder_ctx = self.placeholder_ctx_stack.pop().unwrap();

        if placeholder_ctx.activated {
            if fn_ctx.is_async || fn_ctx.is_generator || fn_ctx.is_do {
                self.errors.extend(
                    TfErrBuilder::default()
                        .message("Await, bind, and yield are not allowed in placeholder functions")
                        .span(span)
                        .build_errs(),
                );
            }

            fn_ctx.is_placeholder = true;

            fn_ctx
                .arg_names
                .insert(Ident("x".into()), placeholder_ctx.decl.clone());

            scope.borrow_mut().locals.push(placeholder_ctx.decl.clone());

            let fn_node = SExprInner::Fn(
                vec![SArgDefItem::Arg(
                    SPatternInner::Capture(Some(Ident("x".into()).spanned(span)))
                        .spanned(span)
                        .indirect(),
                    None,
                )],
                result,
            )
            .spanned(span)
            .indirect();

            self.functions.insert((&fn_node).into(), fn_ctx);

            return fn_node;
        } else {
            return result;
        }
    }

    fn declare(&mut self, name: SIdent<'src>, modifier: DeclType) -> DeclarationRef<'src> {
        let scope = &mut self.scope_ctx_stack.last().unwrap().borrow_mut();

        match modifier {
            DeclType::Global => {
                if scope.is_global_scope || scope.is_class_scope {
                    self.errors.extend(
                        TfErrBuilder::default()
                            .message(
                                "Global declarations are not allowed at the module or class level",
                            )
                            .span(name.span)
                            .build_errs(),
                    );
                }
            }
            DeclType::Export => {
                if !scope.is_global_scope {
                    self.errors.extend(
                        TfErrBuilder::default()
                            .message("Exports are only allowed at the module level")
                            .span(name.span)
                            .build_errs(),
                    );
                }
            }
            DeclType::Let | DeclType::Const => {
                if scope.is_class_scope || scope.is_global_scope {
                    if let Some(found) = self.scope_ctx_stack.find_decl(&name) {
                        self.errors.extend(
                            TfErrBuilder::default()
                                .message("Cannot shadow declarations in a class or global scope")
                                .span(name.span)
                                .context("Declared here", found.decl.borrow().loc)
                                .build_errs(),
                        );
                    }
                }
            }
        };

        let decl = Declaration::new(
            name.clone(),
            self.scope_ctx_stack.last().unwrap().clone(),
            modifier,
        );

        scope.locals.push(decl.clone());

        decl
    }

    fn set_async(&mut self, span: Span) -> () {
        if let Some(fn_ctx) = self.fn_ctx_stack.last_mut() {
            fn_ctx.is_async = true;
        } else if !self.allow_top_level_await {
            self.errors.extend(
                TfErrBuilder::default()
                    .message("Await is only allowed in a function context")
                    .span(span)
                    .build_errs(),
            );
        }
    }

    fn set_do(&mut self, span: Span) -> () {
        if let Some(fn_ctx) = self.fn_ctx_stack.last_mut() {
            fn_ctx.is_do = true;
        } else {
            self.errors.extend(
                TfErrBuilder::default()
                    .message("Bind operator is only allowed in a function context")
                    .span(span)
                    .build_errs(),
            );
        }
    }

    fn set_generator(&mut self, span: Span) -> () {
        if let Some(fn_ctx) = self.fn_ctx_stack.last_mut() {
            fn_ctx.is_generator = true;
        } else {
            self.errors.extend(
                TfErrBuilder::default()
                    .message("Generator is only allowed in a function context")
                    .span(span)
                    .build_errs(),
            );
        }
    }
}

fn error_expr<'src>(span: Span) -> Indirect<SExpr<'src>> {
    Expr::Literal(Literal::None.spanned(span))
        .spanned(span)
        .indirect()
}

fn traverse_placeholder<'src>(state: &mut ResolveState<'src>, span: Span) -> Indirect<SExpr<'src>> {
    if let Some(placeholder_ctx) = state.placeholder_ctx_stack.last_mut() {
        placeholder_ctx.activated = true;

        let ident = Ident("x".into()).spanned(span);
        let expr = Expr::Ident(ident).spanned(span).indirect();
        state
            .resolutions
            .insert((&expr).into(), placeholder_ctx.decl.clone());

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
                    state.errors.extend(TfErrBuilder::default()
                                .message(
                                    "Capture patterns must start with a lowercase letter; to match a type, add '()'",
                                )
                                .span(self.span)
                                .build_errs());
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
                        state.errors.extend(
                            TfErrBuilder::default()
                                .message("Default pattern makes remaining patterns unreachable")
                                .span(span)
                                .build_errs(),
                        );
                    }

                    default |= meta.default;
                    let (pattern, mut meta) = item.traverse(state);

                    meta.captures.sort_by_key(|x| x.0.clone());

                    if captures != meta.captures {
                        state.errors.extend(
                            TfErrBuilder::default()
                                .message("Or patterns must bind the same names")
                                .span(span)
                                .build_errs(),
                        );
                    }

                    items.push(pattern);
                }

                Pattern::Or(items)
            }
            Pattern::Value(v) => Pattern::Value(v),
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
) -> (Indirect<SPattern<'src>>, ScopeRef<'src>, PatternMeta<'src>) {
    let scope = Scope::new();

    let (pattern, meta) = pattern.traverse(state);

    scope
        .borrow_mut()
        .locals
        .extend(meta.captures.iter().map(|x| {
            Declaration::new(
                x.clone().spanned(pattern.span),
                scope.clone(),
                DeclType::Let,
            )
        }));

    (pattern, scope, meta)
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
                        return error_expr(span);
                    }
                };

                let expr = Expr::Ident(ident).spanned(span).indirect();
                state.resolutions.insert((&expr).into(), decl.clone());

                return expr;
            }
            Expr::Checked(expr, pattern) => {
                let pattern = if let Some(pattern) = pattern {
                    let (pattern, meta) = pattern.traverse(state);

                    if !meta.captures.is_empty() {
                        state.errors.extend(
                            TfErrBuilder::default()
                                .message(concat!(
                                    "Non-'_' captures in a 'matches' are only ",
                                    "allowed in 'if ... matches ...', ",
                                    "or 'if ... matches not ...' constructions."
                                ))
                                .span(pattern.span)
                                .build_errs(),
                        );
                    }

                    if meta.default {
                        None
                    } else {
                        state.patterns.insert(
                            (&pattern).into(),
                            PatternInfo {
                                default: false,
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
                    state.errors.extend(TfErrBuilder::default()
                        .message(
                            "Non-'_' capture patterns are not allowed in bare 'matches'-expressions",
                        )
                        .span(pattern.span)
                        .build_errs());
                }
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
                    state
                        .scoped(Scope::new(), |state| {
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

                        let then = state.scoped(then_scope, |state| then.traverse(state)).value;

                        let else_ = else_.map(|else_| {
                            state
                                .scoped(Scope::new(), |state| else_.traverse(state))
                                .value
                        });

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

                                let then = state
                                    .scoped(Scope::new(), |state| then.traverse(state))
                                    .value;

                                // TODO: then scope must be never

                                state
                                    .scope_ctx_stack
                                    .last_mut()
                                    .unwrap()
                                    .borrow_mut()
                                    .locals
                                    .extend(else_scope.borrow().locals.iter().map(|x| x.clone()));

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
                                    state
                                        .scoped(Scope::new(), |state| {
                                            else_.map(|x| x.traverse(state))
                                        })
                                        .value,
                                );
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                }

                Expr::If(
                    cond.traverse(state),
                    state
                        .scoped(Scope::new(), |state| then.traverse(state))
                        .value,
                    else_.map(|else_| {
                        state
                            .scoped(Scope::new(), |state| else_.traverse(state))
                            .value
                    }),
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
                            (None, Scope::new())
                        };

                        SMatchCase {
                            pattern,
                            guard: case.guard.map(|g| g.traverse(state)),
                            body: state.scoped(scope, |state| case.body.traverse(state)).value,
                        }
                    })
                    .collect::<Vec<_>>();

                Expr::Match(subject, cases)
            }
            Expr::Class(bases, body) => {
                let bases = traverse_call_items(state, bases);

                let scope = Scope::new();
                scope.borrow_mut().is_class_scope = true;

                let body = state.scoped(scope, |state| body.traverse(state)).value;

                Expr::Class(bases, body)
            }
            Expr::Fn(arg_def_items, body) => {
                let mut all_captures = vec![];

                let items = arg_def_items
                    .into_iter()
                    .map(|item| match item {
                        ArgDefItem::Arg(arg, default) => {
                            let (pattern, meta) = arg.traverse(state);

                            for capture in meta.captures {
                                all_captures.push(capture.spanned(pattern.span));
                            }

                            ArgDefItem::Arg(pattern, default.map(|x| x.traverse(state)))
                        }
                        ArgDefItem::ArgSpread(arg) => {
                            all_captures.push(arg.clone());
                            ArgDefItem::ArgSpread(arg)
                        }
                        ArgDefItem::KwargSpread(arg) => {
                            all_captures.push(arg.clone());
                            ArgDefItem::KwargSpread(arg)
                        }
                    })
                    .collect::<Vec<_>>();

                for (i, cap) in all_captures.iter().enumerate() {
                    if let Some(found) = all_captures[..i].iter().find(|x| x.value == cap.value) {
                        state.errors.extend(
                            TfErrBuilder::default()
                                .message("Duplicate capture in function arguments")
                                .context("First captured here", found.span)
                                .span(cap.span)
                                .build_errs(),
                        );
                    }
                }

                state.fn_ctx_stack.push(FnInfo::new());

                let scope = Scope::new();
                scope.borrow_mut().is_fn_scope = true;

                let decls = all_captures
                    .iter()
                    .map(|x| Declaration::new(x.clone(), scope.clone(), DeclType::Let));

                let n_scopes = state.scope_ctx_stack.len();

                let (n_fndef_lifted, n_classdef_lifted) = {
                    // lift decls
                    let fndef_scope = &mut state.scope_ctx_stack.last().unwrap().borrow_mut();
                    let n_fndef_lifted = fndef_scope.lifted_decls.len();

                    let drained = fndef_scope.lifted_decls.drain(..).collect::<Vec<_>>();
                    fndef_scope.locals.extend(drained);

                    let n_classdef_lifted = if n_scopes >= 2 {
                        let classdef_scope = &mut state.scope_ctx_stack[n_scopes - 2].borrow_mut();
                        let n = classdef_scope.lifted_decls.len();

                        let drained = classdef_scope.lifted_decls.drain(..).collect::<Vec<_>>();
                        classdef_scope.locals.extend(drained);

                        n
                    } else {
                        0
                    };

                    (n_fndef_lifted, n_classdef_lifted)
                };

                scope.borrow_mut().locals.extend(decls);

                let body = state.scoped(scope, |state| body.traverse(state)).value;

                {
                    // put back
                    let fndef_scope = &mut state.scope_ctx_stack.last().unwrap().borrow_mut();
                    let nlocals = fndef_scope.locals.len();

                    let drained = fndef_scope
                        .locals
                        .drain(nlocals - n_fndef_lifted..)
                        .collect::<Vec<_>>();
                    fndef_scope.lifted_decls.extend(drained);

                    if n_classdef_lifted > 0 {
                        let classdef_scope = &mut state.scope_ctx_stack[n_scopes - 2].borrow_mut();
                        let nlocals = classdef_scope.locals.len();

                        let drained = classdef_scope
                            .locals
                            .drain(nlocals - n_classdef_lifted..)
                            .collect::<Vec<_>>();
                        classdef_scope.lifted_decls.extend(drained);
                    }
                }

                let fn_ctx = state.fn_ctx_stack.pop().unwrap();
                let expr = Expr::Fn(items, body).spanned(span).indirect();

                state.functions.insert((&expr).into(), fn_ctx);

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
) -> Indirect<SExpr<'src>> {
    match lhs.value {
        Expr::Ident(ident) => {
            let decl = match state.resolve(&ident, true) {
                Ok(decl) => decl,
                Err(errs) => {
                    state.errors.extend(errs);
                    return error_expr(lhs.span);
                }
            };

            let expr = Expr::Ident(ident).spanned(lhs.span).indirect();
            state.resolutions.insert((&expr).into(), decl.clone());
            expr
        }
        Expr::Tuple(items) => {
            let items = items
                .into_iter()
                .map(|item| match item {
                    ListItem::Item(inner) => {
                        let inner = traverse_assign_lhs(state, inner);
                        ListItem::Item(inner)
                    }
                    ListItem::Spread(inner) => {
                        let inner = traverse_assign_lhs(state, inner);
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
                        let inner = traverse_assign_lhs(state, inner);
                        ListItem::Item(inner)
                    }
                    ListItem::Spread(inner) => {
                        let inner = traverse_assign_lhs(state, inner);
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
                        traverse_assign_lhs(state, value),
                    ),
                    MappingItem::Spread(inner) => {
                        MappingItem::Spread(traverse_assign_lhs(state, inner))
                    }
                    MappingItem::Ident(inner) => {
                        MappingItem::Ident(traverse_assign_lhs(state, inner))
                    }
                })
                .collect::<Vec<_>>();
            Expr::Mapping(items).spanned(lhs.span).indirect()
        }
        _ => lhs,
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
                    state.declare(ident.clone(), typ);
                }

                Stmt::Decl(idents, typ)
            }
            Stmt::Assign(lhs, rhs, typ) => {
                // LHS will create lifted decls
                let lhs = traverse_assign_lhs(state, lhs);

                let rhs = rhs.traverse_guarded(state);

                // commit lifted
                let scope =
                    &mut state.scope_ctx_stack.last_mut().unwrap().borrow_mut() as &mut Scope;
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
                            (None, Scope::new())
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
                let scope_ref = state.scope_ctx_stack.last_mut().unwrap();
                let scope = &mut scope_ref.borrow_mut();

                if import_stmt.reexport {
                    if !scope.is_global_scope {
                        state.errors.extend(
                            TfErrBuilder::default()
                                .message("Re-exporting imports is only allowed in the global scope")
                                .span(span)
                                .build_errs(),
                        );
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

                        state.export_stars.push(full_module);
                    }
                    ImportList::Leaves(imports) => {
                        for (ident, as_name) in imports {
                            let decl = Declaration::new(
                                as_name.clone().unwrap_or(ident.clone()),
                                scope_ref.clone(),
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
) -> (ResolveState<'src>, Indirect<SExpr<'src>>) {
    let mut state = ResolveState::new(source);
    state.allow_top_level_await = allow_await;
    state.root_scope = Scope::new();
    state.root_scope.borrow_mut().is_global_scope = true;

    let expr = state
        .scoped(state.root_scope.clone(), |state| {
            expr.indirect().traverse(state)
        })
        .value;

    if !state.fn_ctx_stack.is_empty() {
        panic!("Function context stack is not empty after resolving names");
    }

    if !state.scope_ctx_stack.is_empty() {
        panic!("Scope context stack is not empty after resolving names");
    }

    if !state.placeholder_ctx_stack.is_empty() {
        panic!("Placeholder context stack is not empty after resolving names");
    }

    (state, expr)
}
