import functools
import inspect


def delegate_args(target, *, args=None, kwargs=None, renames=None, defaults=None):
    """Decorator that delegates argument defaults from a target function's signature.

    Args:
        target: The function whose signature to copy defaults from.
        args: List of argument names to delegate (as strings).
        kwargs: Name of the parameter that receives extra **kwargs for the target (as a string).
            When present, the wrapped function's signature is expanded to include all
            keyword-only and positional-or-keyword params of target not already in args.
            At call time, those extra args are collected into a dict and passed as the
            kwargs parameter. If target itself has **kwargs (VAR_KEYWORD), the delegate's
            kwargs acts as a catch-all, absorbing all unknown keyword arguments.
        renames: Dict mapping local_name -> original_name in target's signature.
        defaults: Dict mapping local_name -> override default value.
    """
    if args is None:
        args = []
    if renames is None:
        renames = {}
    if defaults is None:
        defaults = {}

    target_params = inspect.signature(target).parameters

    # Check if target accepts **kwargs (VAR_KEYWORD)
    target_has_var_keyword = any(
        p.kind == inspect.Parameter.VAR_KEYWORD for p in target_params.values()
    )

    # Build a mapping of local_name -> default value for explicitly delegated args
    resolved_defaults = {}
    for local_name in args:
        if local_name in defaults:
            resolved_defaults[local_name] = defaults[local_name]
        else:
            orig_name = renames.get(local_name, local_name)
            if orig_name in target_params:
                param = target_params[orig_name]
                if param.default is not inspect.Parameter.empty:
                    resolved_defaults[local_name] = param.default

    # Find extra target params to expose when kwargs is used
    extra_params = {}
    if kwargs is not None:
        explicitly_delegated = set()
        for local_name in args:
            explicitly_delegated.add(renames.get(local_name, local_name))

        for pname, param in target_params.items():
            if param.kind in [
                inspect.Parameter.KEYWORD_ONLY,
                inspect.Parameter.POSITIONAL_OR_KEYWORD,
            ]:
                if pname not in explicitly_delegated:
                    extra_params[pname] = param

    needs_wrapper = bool(extra_params) or (
        kwargs is not None and target_has_var_keyword
    )

    def decorator(fn):
        # Find the actual underlying function for __kwdefaults__.
        # If fn is a wrapper from a previous delegate_args, __kwdefaults__
        # on the wrapper has no effect; we must set it on the real function.
        actual_fn = fn
        while hasattr(actual_fn, "__wrapped__"):
            actual_fn = actual_fn.__wrapped__

        # Apply defaults for explicitly delegated args
        if resolved_defaults:
            if actual_fn.__kwdefaults__ is None:
                actual_fn.__kwdefaults__ = {}
            for name, value in resolved_defaults.items():
                actual_fn.__kwdefaults__.setdefault(name, value)

        if not needs_wrapper:
            if kwargs is not None:
                if actual_fn.__kwdefaults__ is None:
                    actual_fn.__kwdefaults__ = {}
                actual_fn.__kwdefaults__.setdefault(kwargs, {})
            # Update __signature__ if fn is a wrapper from a previous delegate
            if resolved_defaults and hasattr(fn, "__signature__"):
                params = []
                for p in fn.__signature__.parameters.values():
                    if (
                        p.name in resolved_defaults
                        and p.default is inspect.Parameter.empty
                    ):
                        params.append(p.replace(default=resolved_defaults[p.name]))
                    else:
                        params.append(p)
                fn.__signature__ = fn.__signature__.replace(parameters=params)
            return fn

        # Build new signature: replace the kwargs param with the extra target params
        sig = inspect.signature(fn)
        existing_param_names = {p.name for p in sig.parameters.values()}
        new_params = []
        for param in sig.parameters.values():
            if param.name == kwargs:
                for ep_name, ep in extra_params.items():
                    # Skip extra params already present (e.g. from a previous delegate)
                    if ep_name not in existing_param_names:
                        new_params.append(
                            ep.replace(kind=inspect.Parameter.KEYWORD_ONLY)
                        )
                # kwargs param is absorbed — don't re-add it
            else:
                # Ensure resolved defaults are reflected in the signature even when
                # fn is a wrapper from a previous delegate_args (where __kwdefaults__
                # has no effect on the wrapper's actual parameter handling).
                if (
                    param.name in resolved_defaults
                    and param.default is inspect.Parameter.empty
                ):
                    new_params.append(
                        param.replace(default=resolved_defaults[param.name])
                    )
                else:
                    new_params.append(param)
        new_sig = sig.replace(parameters=new_params)

        # Precompute defaults for extra params
        extra_defaults = {}
        for pname, param in extra_params.items():
            if (
                pname not in existing_param_names
                and param.default is not inspect.Parameter.empty
            ):
                extra_defaults[pname] = param.default

        # Determine fn's own param names (excluding the kwargs param and **real_kwargs)
        # Used in catch-all mode to avoid stealing args meant for other params
        fn_own_params = {
            name
            for name, p in sig.parameters.items()
            if p.kind != inspect.Parameter.VAR_KEYWORD and name != kwargs
        }

        @functools.wraps(fn)
        def wrapper(*call_args, **call_kwargs):
            # Apply resolved defaults for explicitly delegated args not provided.
            # This is needed because __kwdefaults__ on a wrapper fn from a previous
            # delegate_args layer has no effect (wrappers use *args/**kwargs).
            for name, value in resolved_defaults.items():
                if name not in call_kwargs:
                    call_kwargs[name] = value
            kwargs_dict = dict(extra_defaults)
            remaining_kwargs = {}
            for k, v in call_kwargs.items():
                if k in extra_params and k not in existing_param_names:
                    # Known extra target param (not already handled by another delegate)
                    kwargs_dict[k] = v
                elif target_has_var_keyword and k not in fn_own_params:
                    # Target has **kwargs catch-all and this isn't one of fn's own params
                    kwargs_dict[k] = v
                else:
                    remaining_kwargs[k] = v
            remaining_kwargs[kwargs] = kwargs_dict
            return fn(*call_args, **remaining_kwargs)

        wrapper.__signature__ = new_sig
        return wrapper

    return decorator
