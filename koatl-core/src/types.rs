#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Any,
    NoReturn,
    Unprocessed,
    Top,
    Bottom,
}
