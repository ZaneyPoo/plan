#[allow(dead_code)]
#[derive(Debug, PartialEq, PartialOrd)]
pub enum TokenType {
    // Literals
    IntLit(i64),
    RealLit(f64),
    // TODO: Maybe add this
    // RatLit,
    StrLit(String),
    CharLit(char),
    BoolLit(bool),

    OpenParen,
    CloseParen,
    // TODO: Add these
    // OpenBracket,
    // CloseBracket,
    // OpenCurly,
    // CloseCurly,
    Hash,

    Ident,
    Symbol,

    // Keywords
    Arrow,
    And,
    Begin,
    Case,
    Cond,
    Define,
    Delay,
    Do,
    Else,
    If,
    Lambda,
    Let,
    LetStar,
    LetRec,
    Or,
    Quasiquote,
    Quote,
    Set,
    Unquote,
    UnquoteSplicing,

    Eof,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    begin: (usize, usize),
    end: (usize, usize),
}

#[allow(dead_code)]
pub struct Token {
    kind: TokenType,
    span: Span,
}

#[allow(dead_code)]
impl Token {
    pub fn new(kind: TokenType, span: Span) -> Self {
        Self { kind, span }
    }
}

pub struct Lexer<'src> {
    input: &'src str,
    pos: (usize, usize),
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        Self {  input, pos: (0, 0) }
    }
}
