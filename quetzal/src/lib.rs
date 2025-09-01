use ohtli::{Config, Formatter};

wit_bindgen::generate!({
    world: "formatter",
});

struct FormatterImpl;

impl Guest for FormatterImpl {
    #[allow(async_fn_in_trait)]
    fn format(o: _rt::String) -> _rt::String {
        let formatter = Formatter::new(Config::default());
        let result = formatter.format(&o).unwrap_or("".to_string());
        result
    }
}

export!(FormatterImpl);
