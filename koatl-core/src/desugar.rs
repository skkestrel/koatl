use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::ptr;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    transform::{TfErrBuilder, TfResult},
    types::Type,
};
use parser::ast::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct BoxHash(usize);

impl BoxHash {
    fn new<T>(value: &Box<T>) -> Self {
        return BoxHash((value.as_ref() as *const T) as usize);
    }
}

#[derive(Debug, Clone)]
struct RcKey<T>(Rc<T>);

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

trait FnCtxStackExt {
    fn set_async(&mut self, allow_top_level_await: bool, span: &Span) -> TfResult<()>;
    fn set_generator(&mut self, span: &Span) -> TfResult<()>;
    fn set_do(&mut self, span: &Span) -> TfResult<()>;
}

impl<'src> FnCtxStackExt for Vec<FnInfo<'src>> {
    fn set_async(&mut self, allow_top_level_await: bool, span: &Span) -> TfResult<()> {
        if let Some(fn_ctx) = self.last_mut() {
            fn_ctx.is_async = true;
        } else if !allow_top_level_await {
            return Err(TfErrBuilder::default()
                .message("Await is only allowed in a function context")
                .span(*span)
                .build_errs());
        }

        Ok(())
    }

    fn set_do(&mut self, span: &Span) -> TfResult<()> {
        if let Some(fn_ctx) = self.last_mut() {
            fn_ctx.is_do = true;
        } else {
            return Err(TfErrBuilder::default()
                .message("Bind operator is only allowed in a function context")
                .span(*span)
                .build_errs());
        }

        Ok(())
    }

    fn set_generator(&mut self, span: &Span) -> TfResult<()> {
        if let Some(fn_ctx) = self.last_mut() {
            fn_ctx.is_generator = true;
        } else {
            return Err(TfErrBuilder::default()
                .message("Generator is only allowed in a function context")
                .span(*span)
                .build_errs());
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Declaration<'src> {
    pub name: Ident<'src>,
    pub scope: Rc<RefCell<Scope<'src>>>,
    pub loc: Span,
    pub typ: Type,
    pub modifier: DeclType,
}

pub type DeclarationRef<'src> = Rc<RefCell<Declaration<'src>>>;

#[derive(Debug)]
struct Scope<'src> {
    locals: Vec<DeclarationRef<'src>>,

    is_class_scope: bool,
    is_global_scope: bool,
    is_fn_scope: bool,

    /**
     * This enables special treatment for defining
     * recursive functions; lifted_decls are added to the
     * containing scope when processing the function body.
     */
    lifted_decls: Vec<DeclarationRef<'src>>,

    /**
     * this is a really messed up hack to handle Python's
     * weird class scoping rules -
     * method bodies TWO scopes lower
     * are able to see the class definition
     */
    lifted_class_decls: Vec<DeclarationRef<'src>>,
}

impl<'src> Scope<'src> {
    fn new() -> Self {
        Self {
            locals: Vec::new(),
            lifted_decls: Vec::new(),
            lifted_class_decls: Vec::new(),
            is_fn_scope: false,
            is_class_scope: false,
            is_global_scope: false,
        }
    }
}

trait ScopeRefExt<'src> {
    fn declare(
        &self,
        state: &State<'src>,
        name: SIdent<'src>,
        modifier: DeclType,
    ) -> TfResult<DeclarationRef<'src>>;
}

impl<'src> ScopeRefExt<'src> for Rc<RefCell<Scope<'src>>> {
    fn declare(
        &self,
        state: &State<'src>,
        name: SIdent<'src>,
        modifier: DeclType,
    ) -> TfResult<DeclarationRef<'src>> {
        let scope = self.borrow();

        match modifier {
            DeclType::Global => {
                if scope.is_global_scope || scope.is_class_scope {
                    return Err(TfErrBuilder::default()
                        .message("Global declarations are not allowed at the module or class level")
                        .span(name.span)
                        .build_errs());
                }
            }
            DeclType::Export => {
                if !scope.is_global_scope {
                    return Err(TfErrBuilder::default()
                        .message("Exports are only allowed at the module level")
                        .span(name.span)
                        .build_errs());
                }
            }
            DeclType::Let | DeclType::Const => {
                if scope.is_class_scope || scope.is_global_scope {
                    if let Some(found) = state.scope_ctx_stack.find_decl(&name) {
                        return Err(TfErrBuilder::default()
                            .message("Cannot shadow declarations in a class or global scope")
                            .span(name.span)
                            .context("Declared here", found.decl.borrow().loc)
                            .build_errs());
                    }
                }
            }
        };

        let decl = Rc::new(RefCell::new(Declaration {
            name: name.value,
            scope: self.clone(),
            loc: name.span,
            typ: Type::Unknown,
            modifier,
        }));

        self.borrow_mut().locals.push(decl.clone());

        Ok(decl)
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

struct PatternInfo<'src> {
    pub default: bool,
    pub decls: Vec<DeclarationRef<'src>>,
}

#[allow(dead_code)]
struct State<'src> {
    source: &'src str,
    allow_top_level_await: bool,

    root_scope: Rc<RefCell<Scope<'src>>>,
    types: HashMap<BoxHash, Type>,
    resolutions: HashMap<BoxHash, DeclarationRef<'src>>,
    functions: HashMap<BoxHash, FnInfo<'src>>,
    patterns: HashMap<BoxHash, PatternInfo<'src>>,

    placeholder_ctx_stack: Vec<PlaceholderCtx<'src>>,
    fn_ctx_stack: Vec<FnInfo<'src>>,
    scope_ctx_stack: Vec<Rc<RefCell<Scope<'src>>>>,

    scope_id_counter: usize,
}

struct ScopedOutput<'src, T> {
    pub value: T,
    pub scope: Rc<RefCell<Scope<'src>>>,
}

impl<'src> State<'src> {
    fn new(source: &'src str) -> TfResult<Self> {
        Ok(State {
            allow_top_level_await: false,
            source,

            root_scope: Rc::new(RefCell::new(Scope::new())),
            types: HashMap::new(),
            resolutions: HashMap::new(),
            functions: HashMap::new(),
            patterns: HashMap::new(),

            scope_id_counter: 0,

            placeholder_ctx_stack: Vec::new(),
            fn_ctx_stack: Vec::new(),
            scope_ctx_stack: Vec::new(),
        })
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

        if self.scope_ctx_stack.len() >= 2 {
            let decl_scope = self.scope_ctx_stack[self.scope_ctx_stack.len() - 2].clone();
            if let Some(found) = decl_scope
                .borrow()
                .lifted_decls
                .iter()
                .find(|d| d.borrow().name == ident.value)
            {
                return Ok(found.clone());
            }
        }

        if self.scope_ctx_stack.len() >= 3 {
            let decl_scope = self.scope_ctx_stack[self.scope_ctx_stack.len() - 3].clone();
            if let Some(found) = decl_scope
                .borrow()
                .lifted_class_decls
                .iter()
                .find(|d| d.borrow().name == ident.value)
            {
                return Ok(found.clone());
            }
        }

        let scope = self.scope_ctx_stack.last().unwrap();

        if lhs {
            if scope.borrow().is_class_scope || scope.borrow().is_global_scope {
                let decl = scope.declare(self, ident.clone(), DeclType::Let)?;
                return Ok(decl);
            }
        }

        if !lhs {
            let global = &self.scope_ctx_stack[0];
            let decl = global.declare(self, ident.clone(), DeclType::Let)?;
            return Ok(decl);
        }

        Err(TfErrBuilder::default()
            .message("Undeclared identifier; either declare with 'let' or mark as 'global'.")
            .span(ident.span)
            .build_errs())
    }

    fn scoped<F, O>(
        &mut self,
        scope: Rc<RefCell<Scope<'src>>>,
        f: F,
    ) -> TfResult<ScopedOutput<'src, O>>
    where
        F: FnOnce(&mut State<'src>) -> TfResult<O>,
    {
        self.scope_id_counter += 1;
        self.scope_ctx_stack.push(scope);
        let result = f(self);
        let scope = self.scope_ctx_stack.pop().unwrap();
        scope.borrow_mut().lifted_class_decls.clear();
        scope.borrow_mut().lifted_decls.clear();

        result.map(|value| ScopedOutput { value, scope })
    }

    fn placeholder_guarded<F>(&mut self, span: Span, f: F) -> TfResult<Indirect<SExpr<'src>>>
    where
        F: FnOnce(&mut State<'src>) -> TfResult<Indirect<SExpr<'src>>>,
    {
        let mut scope = Scope::new();
        scope.is_fn_scope = true;
        let scope = Rc::new(RefCell::new(scope));

        let ph_decl = Rc::new(RefCell::new(Declaration {
            name: Ident("x".into()),
            scope: scope.clone(),
            loc: span,
            typ: Type::Unknown,
            modifier: DeclType::Let,
        }));

        self.placeholder_ctx_stack
            .push(PlaceholderCtx::new(ph_decl, span));
        self.fn_ctx_stack.push(FnInfo::new());

        let result = match f(self) {
            Err(e) => {
                self.placeholder_ctx_stack.pop();
                self.fn_ctx_stack.pop();
                return Err(e);
            }
            Ok(result) => result,
        };

        let mut fn_ctx = self.fn_ctx_stack.pop().unwrap();
        let placeholder_ctx = self.placeholder_ctx_stack.pop().unwrap();

        if placeholder_ctx.activated {
            if fn_ctx.is_async || fn_ctx.is_generator || fn_ctx.is_do {
                return Err(TfErrBuilder::default()
                    .message("Await, bind, and yield are not allowed in placeholder functions")
                    .span(span)
                    .build_errs());
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

            self.functions.insert(BoxHash::new(&fn_node), fn_ctx);

            return Ok(fn_node);
        } else {
            return Ok(result);
        }
    }
}

fn traverse_placeholder<'src>(
    state: &mut State<'src>,
    span: Span,
) -> TfResult<Indirect<SExpr<'src>>> {
    if let Some(placeholder_ctx) = state.placeholder_ctx_stack.last_mut() {
        placeholder_ctx.activated = true;

        let ident = Ident("x".into()).spanned(span);
        let expr = Expr::Ident(ident).spanned(span).indirect();
        state
            .resolutions
            .insert(BoxHash::new(&expr), placeholder_ctx.decl.clone());

        return Ok(expr);
    }

    return Err(TfErrBuilder::default()
        .message("Internal error: no placeholder context found")
        .span(span)
        .build_errs());
}

struct PatternMeta<'src> {
    pub default: bool,
    pub captures: Vec<Ident<'src>>,
}

trait SPatternExt<'src> {
    fn traverse(
        self,
        state: &mut State<'src>,
    ) -> TfResult<(Indirect<SPattern<'src>>, PatternMeta<'src>)>;
}

impl<'src> SPatternExt<'src> for Indirect<SPattern<'src>> {
    fn traverse(
        self,
        state: &mut State<'src>,
    ) -> TfResult<(Indirect<SPattern<'src>>, PatternMeta<'src>)> {
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
                    return Err(TfErrBuilder::default()
                                .message(
                                    "Capture patterns must start with a lowercase letter; to match a type, add '()'",
                                )
                                .span(self.span)
                                .build_errs());
                }

                if let Some(cap) = cap {
                    captures.push(cap.value.clone());
                }

                Pattern::Capture(cap)
            }
            Pattern::As(pattern, name) => {
                let (pattern, meta) = pattern.traverse(state)?;
                captures.extend(meta.captures);
                captures.push(name.value.clone());
                Pattern::As(pattern, name)
            }
            Pattern::Or(items) => {
                let mut iter = items.iter();
                let initial = iter.next().ok_or_else(|| {
                    TfErrBuilder::default()
                        .message("Or pattern must have at least one item")
                        .span(self.span)
                        .build_errs()
                })?;

                let mut initial = initial.preprocess()?;

                for item in iter {
                    if initial.default {
                        return Err(TfErrBuilder::default()
                            .message("Default pattern makes remaining patterns unreachable")
                            .span(item.span)
                            .build_errs())?;
                    }

                    let meta = item.preprocess()?;
                    initial.default |= meta.default;

                    if meta.captures != initial.captures {
                        return Err(TfErrBuilder::default()
                            .message("Or patterns must bind the same names")
                            .span(item.span)
                            .build_errs());
                    }
                }

                initial
            }
            Pattern::Value(..) | Pattern::Literal(..) => PatternMeta {
                default: false,
                captures: vec![],
            },
            Pattern::Sequence(items) => {
                let mut captures = vec![];
                for item in items {
                    match item {
                        PatternSequenceItem::Item(inner) => {
                            let meta = inner.preprocess()?;
                            captures.extend(meta.captures);
                        }
                        PatternSequenceItem::Spread(Some(inner)) => {
                            captures.push(inner.value.clone());
                        }
                        PatternSequenceItem::Spread(None) => {}
                    }
                }

                PatternMeta {
                    default: false,
                    captures,
                }
            }
            Pattern::Mapping(items) => {
                let mut captures = vec![];
                for item in items {
                    match item {
                        PatternMappingItem::Ident(id) => captures.push(id.value.clone()),
                        PatternMappingItem::Item(_key, value) => {
                            let meta = value.preprocess()?;
                            captures.extend(meta.captures);
                        }
                        PatternMappingItem::Spread(Some(id)) => {
                            captures.push(id.value.clone());
                        }
                        PatternMappingItem::Spread(None) => {}
                    }
                }

                PatternMeta {
                    default: false,
                    captures,
                }
            }
            Pattern::Class(_, items) => {
                let mut captures = vec![];

                for item in items {
                    match item {
                        PatternClassItem::Item(inner) => {
                            let meta = inner.preprocess()?;
                            captures.extend(meta.captures);
                        }
                        PatternClassItem::Kw(_, inner) => {
                            let meta = inner.preprocess()?;
                            captures.extend(meta.captures);
                        }
                    }
                }

                PatternMeta {
                    default: false,
                    captures,
                }
            }
        };

        Ok((
            pattern.spanned(span).indirect(),
            PatternMeta { default, captures },
        ))
    }
}

fn traverse_list_items<'src>(
    state: &mut State<'src>,
    items: Vec<SListItem<'src>>,
) -> TfResult<Vec<SListItem<'src>>> {
    items
        .into_iter()
        .map(|i| -> TfResult<SListItem<'src>> {
            Ok(match i {
                ListItem::Item(i) => ListItem::<STree>::Item(i.traverse(state)?),
                ListItem::Spread(i) => ListItem::<STree>::Spread(i.traverse(state)?),
            })
        })
        .collect::<TfResult<_>>()
}

fn traverse_call_items<'src>(
    state: &mut State<'src>,
    items: Vec<SCallItem<'src>>,
) -> TfResult<Vec<SCallItem<'src>>> {
    items
        .into_iter()
        .map(|item| {
            Ok(match item {
                CallItem::Arg(i) => CallItem::Arg(i.traverse_deep_guarded(state)?),
                CallItem::Kwarg(name, i) => {
                    CallItem::Kwarg(name.clone(), i.traverse_deep_guarded(state)?)
                }
                CallItem::ArgSpread(i) => CallItem::ArgSpread(i.traverse_deep_guarded(state)?),
                CallItem::KwargSpread(i) => CallItem::KwargSpread(i.traverse_deep_guarded(state)?),
            })
        })
        .collect::<TfResult<_>>()
}

trait SExprExt<'src> {
    fn traverse(self, state: &mut State<'src>) -> TfResult<Indirect<SExpr<'src>>>;
    fn traverse_guarded(self, state: &mut State<'src>) -> TfResult<Indirect<SExpr<'src>>>;
    fn traverse_deep_guarded(self, state: &mut State<'src>) -> TfResult<Indirect<SExpr<'src>>>;
}

impl<'src> SExprExt<'src> for Indirect<SExpr<'src>> {
    fn traverse_deep_guarded(self, state: &mut State<'src>) -> TfResult<Indirect<SExpr<'src>>> {
        match self.value {
            Expr::Placeholder => traverse_placeholder(state, self.span),
            _ => self.traverse_guarded(state),
        }
    }

    fn traverse_guarded(self, state: &mut State<'src>) -> TfResult<Indirect<SExpr<'src>>> {
        state.placeholder_guarded(self.span, |state| self.traverse(state))
    }

    fn traverse(self: Self, state: &mut State<'src>) -> TfResult<Indirect<SExpr<'src>>> {
        let span = self.span;

        let t = match self.value {
            Expr::Literal(_) => return Ok(self),
            Expr::Ident(ident) => {
                let decl = state.resolve(&ident, false)?;
                let expr = Expr::Ident(ident).spanned(span).indirect();
                state.resolutions.insert(BoxHash::new(&expr), decl.clone());
                return Ok(expr);
            }
            Expr::Checked(expr, pattern) => {
                let pattern = if let Some(pattern) = pattern {
                    let (pattern, meta) = pattern.traverse(state)?;

                    if !meta.captures.is_empty() {
                        return Err(TfErrBuilder::default()
                            .message(
                                "Non-'_' capture patterns are not allowed in 'try'-expressions",
                            )
                            .span(pattern.span)
                            .build_errs());
                    }

                    if meta.default {
                        None
                    } else {
                        state.patterns.insert(
                            BoxHash::new(&pattern),
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

                Expr::Checked(expr.traverse_guarded(state)?, pattern)
            }
            Expr::Decorated(deco, expr) => Expr::Call(
                deco.traverse(state)?,
                vec![CallItem::Arg(expr.traverse(state)?)],
            ),
            Expr::Matches(x, pattern) => {
                let (pattern, meta) = pattern.traverse(state)?;
                if !meta.captures.is_empty() {
                    return Err(TfErrBuilder::default()
                        .message(
                            "Non-'_' capture patterns are not allowed in bare 'matches'-expressions",
                        )
                        .span(pattern.span)
                        .build_errs());
                }
                Expr::Matches(x.traverse(state)?, pattern)
            }
            Expr::Placeholder => return traverse_placeholder(state, span),
            Expr::Block(stmts) => {
                let mut new_stmts = Vec::new();
                for stmt in stmts {
                    new_stmts.push(stmt.traverse(state)?);
                }
                Expr::Block(new_stmts)
            }
            Expr::If(cond, then, else_) => todo!(), // scoping special rules
            Expr::Match(_, match_cases) => todo!(), // scoping special rules
            Expr::Class(bases, body) => {
                let bases = traverse_call_items(state, bases)?;

                let scope = Rc::new(RefCell::new(Scope {
                    is_class_scope: true,
                    is_fn_scope: false,
                    is_global_scope: false,
                    locals: vec![],
                    lifted_decls: Vec::new(),
                    lifted_class_decls: Vec::new(),
                }));

                let body = body.traverse(state)?;
                Expr::Class(bases, body)
            }
            Expr::Fn(arg_def_items, body) => {
                let mut all_captures = vec![];

                let items = arg_def_items
                    .into_iter()
                    .map(|item| {
                        Ok(match item {
                            ArgDefItem::Arg(arg, default) => {
                                let (pattern, meta) = arg.traverse(state)?;

                                for capture in meta.captures {
                                    all_captures.push(capture.spanned(pattern.span));
                                }

                                ArgDefItem::Arg(
                                    pattern,
                                    default.map(|x| x.traverse(state)).transpose()?,
                                )
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
                    })
                    .collect::<TfResult<_>>()?;

                for (i, cap) in all_captures.iter().enumerate() {
                    if let Some(found) = all_captures[..i].iter().find(|x| x.value == cap.value) {
                        return Err(TfErrBuilder::default()
                            .message("Duplicate capture in function arguments")
                            .context("First captured here", found.span)
                            .span(cap.span)
                            .build_errs());
                    }
                }

                state.fn_ctx_stack.push(FnInfo::new());

                let scope = Rc::new(RefCell::new(Scope {
                    is_fn_scope: true,
                    is_class_scope: false,
                    is_global_scope: false,
                    locals: vec![],
                    lifted_decls: Vec::new(),
                    lifted_class_decls: Vec::new(),
                }));

                let decls = all_captures.iter().map(|x| {
                    Rc::new(RefCell::new(Declaration {
                        name: x.value.clone(),
                        scope: scope.clone(),
                        loc: x.span,
                        typ: Type::Unknown,
                        modifier: DeclType::Let,
                    }))
                });

                scope.borrow_mut().locals.extend(decls);

                let body = state.scoped(scope, |state| body.traverse(state))?.value;

                Expr::Fn(items, body)
            }
            Expr::Fstr(spanned, items) => Expr::Fstr(
                spanned,
                items
                    .into_iter()
                    .map(|(expr, suffix)| {
                        Ok((
                            FmtExpr {
                                expr: expr.expr.traverse(state)?,
                                fmt: expr.fmt,
                            },
                            suffix,
                        ))
                    })
                    .collect::<TfResult<_>>()?,
            ),

            // special case for pipe
            Expr::Binary(binary_op, mut x, mut y) => {
                if let BinaryOp::Pipe = binary_op {
                    x = x.traverse_guarded(state)?;
                    y = y.traverse_guarded(state)?;
                } else {
                    x = x.traverse(state)?;
                    y = y.traverse(state)?;
                }

                Expr::Binary(binary_op, x, y)
            }

            // postfix
            Expr::Attribute(expr, attr) => Expr::Attribute(expr.traverse(state)?, attr.clone()),
            Expr::MappedAttribute(expr, attr) => Expr::MappedAttribute(expr.traverse(state)?, attr),
            Expr::RawAttribute(expr, attr) => Expr::RawAttribute(expr.traverse(state)?, attr),
            Expr::MappedRawAttribute(expr, spanned) => {
                Expr::MappedRawAttribute(expr.traverse(state)?, spanned)
            }
            Expr::ScopedAttribute(expr, value) => {
                Expr::ScopedAttribute(expr.traverse(state)?, value.traverse_guarded(state)?)
            }
            Expr::MappedScopedAttribute(expr, value) => {
                Expr::MappedScopedAttribute(expr.traverse(state)?, value.traverse_guarded(state)?)
            }
            Expr::Call(a, items) => {
                Expr::Call(a.traverse(state)?, traverse_call_items(state, items)?)
            }
            Expr::MappedCall(a, call_items) => {
                Expr::MappedCall(a.traverse(state)?, traverse_call_items(state, call_items)?)
            }
            Expr::Subscript(x, list_items) => {
                Expr::Subscript(x.traverse(state)?, traverse_list_items(state, list_items)?)
            }
            Expr::MappedSubscript(expr, list_items) => Expr::MappedSubscript(
                expr.traverse(state)?,
                traverse_list_items(state, list_items)?,
            ),
            Expr::Tuple(items) => Expr::Tuple(traverse_list_items(state, items)?),
            Expr::List(items) => Expr::List(traverse_list_items(state, items)?),
            Expr::Mapping(items) => Expr::Mapping(
                items
                    .into_iter()
                    .map(|item| {
                        Ok(match item {
                            MappingItem::Item(key, value) => {
                                MappingItem::Item(key.traverse(state)?, value.traverse(state)?)
                            }
                            MappingItem::Spread(i) => MappingItem::Spread(i.traverse(state)?),
                            MappingItem::Ident(i) => MappingItem::Ident(i),
                        })
                    })
                    .collect::<TfResult<_>>()?,
            ),
            Expr::Slice(a, b, c) => Expr::Slice(
                a.map(|e| e.traverse(state)).transpose()?,
                b.map(|e| e.traverse(state)).transpose()?,
                c.map(|e| e.traverse(state)).transpose()?,
            ),
            Expr::Unary(unary_op, expr) => Expr::Unary(unary_op, expr.traverse(state)?),
            Expr::Await(x) => Expr::Await(x.traverse(state)?),
            Expr::Yield(x) => Expr::Yield(x.traverse(state)?),
            Expr::YieldFrom(x) => Expr::YieldFrom(x.traverse(state)?),
        };

        Ok(t.spanned(span).indirect())
    }
}

trait SStmtExt<'src> {
    fn traverse(self, state: &mut State<'src>) -> TfResult<Indirect<SStmt<'src>>>;
}

impl<'src> SStmtExt<'src> for Indirect<SStmt<'src>> {
    fn traverse(self, state: &mut State<'src>) -> TfResult<Indirect<SStmt<'src>>> {
        let span = self.span;

        let stmt = match self.value {
            Stmt::Decl(idents, typ) => {
                for ident in idents.iter() {
                    state
                        .scope_ctx_stack
                        .last()
                        .unwrap()
                        .declare(state, ident.clone(), typ)?;
                }

                Stmt::Decl(idents, typ)
            }
            Stmt::Assign(lhs, rhs, typ) => {
                todo!("find lhs decls");
            }
            Stmt::Expr(expr) => Stmt::Expr(expr.traverse_guarded(state)?),
            Stmt::Return(expr) => Stmt::Return(expr.traverse_guarded(state)?),
            Stmt::While(cond, body) => {
                Stmt::While(cond.traverse_guarded(state)?, body.traverse_guarded(state)?)
            }
            Stmt::For(pattern, iter, body) => {
                let (pattern, meta) = pattern.traverse(state)?;

                todo!("special scoping rules");

                Stmt::For(
                    pattern,
                    iter.traverse_guarded(state)?,
                    body.traverse_guarded(state)?,
                )
            }
            Stmt::Import(import_stmt) => Stmt::Import(import_stmt),
            Stmt::Try(body, cases, finally) => {
                let body = body.traverse_guarded(state)?;
                let cases = cases
                    .into_iter()
                    .map(|case| {
                        let pattern = if let Some(pattern) = case.pattern {
                            let (pattern, meta) = pattern.traverse(state)?;

                            Some(pattern)
                        } else {
                            None
                        };

                        todo!("special scoping rules");

                        Ok(MatchCase {
                            pattern,
                            guard: case.guard.map(|x| x.traverse_guarded(state)).transpose()?,
                            body: case.body.traverse_guarded(state)?,
                        })
                    })
                    .collect::<TfResult<_>>()?;
                let finally = finally.map(|x| x.traverse_guarded(state)).transpose()?;

                Stmt::Try(body, cases, finally)
            }
            Stmt::Assert(a, b) => Stmt::Assert(
                a.traverse_guarded(state)?,
                b.map(|x| x.traverse_guarded(state)).transpose()?,
            ),
            Stmt::Raise(expr) => Stmt::Raise(expr.map(|x| x.traverse_guarded(state)).transpose()?),
            Stmt::Break => Stmt::Break,
            Stmt::Module => Stmt::Module,
            Stmt::Continue => Stmt::Continue,
        };

        Ok(stmt.spanned(span).indirect())
    }
}
