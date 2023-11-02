pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}

impl Object {
    fn inspect(&self) -> String {
        return match self {
            Self::Integer(int) => int.to_string(),
            Self::Boolean(loob) => loob.to_string(),
            Self::Null => "null".into(),
        };
    }
}
