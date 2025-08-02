#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Any,
    NoType,
    Unprocessed,
    Top,
    Bottom,
}
