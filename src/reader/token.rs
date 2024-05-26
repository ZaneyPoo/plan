use std::fmt::Display;

use super::span::Span;

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum TokenType {
    /// #t | #f
    BoolLit(bool),

    // TODO: Split this into a Number subtype then add radix and rational number support
    IntLit(i64),
    RealLit(f64),

    CharLit(char),
    StrLit(String),

    /// (
    OpenParen,
    /// )
    CloseParen,
    /// [
    OpenBracket,
    /// ]
    CloseBracket,
    /// {
    OpenCurly,
    /// }
    CloseCurly,
    /// #(
    OpenVector,
    /// ,
    Comma,
    /// ,@
    CommaAt,
    /// .
    Dot,
    /// '
    SingleQuote,
    // /// `
    // Backtick,

    // We say "fuck it. anything else is an identifier"
    Ident(String),

    /// ;.*\n
    Comment,
    Eof,

    Error(LexError),
    // TODO: Maybe have these be lexed separately
    // Syntactic Keywords
    //Else,
    //Arrow,
    //Define,
    //Unquote,
    //UnquoteSplicing,

    //// Expression Keywords
    //Quote,
    //Lambda,
    //If,
    //Set,
    //Begin,
    //Cond,
    //And,
    //Or,
    //Case,
    //Let,
    //LetStar,
    //LetRec,
    //Do,
    //Delay,
    //Quasiquote,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum LexError {
    UntermStr,
    UnknownChar(char),
    UnexpectedChar { ch: char, expected: &'static str },
    UnexpectedEof,
    InvalidInt(String),
    InvalidReal(String),
    InvalidIdent(String),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Token {
    pub kind: TokenType,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenType, span: Span) -> Self {
        Self { kind, span }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenType as tk;
        use LexError as le;
        match &self.kind {
            tk::BoolLit(boolean) => write!(f, "Token bool: {boolean}, span: {}", self.span),
            tk::IntLit(int) => write!(f, "Token int: {int}, span {}", self.span),
            tk::RealLit(real) => write!(f, "Token real: {real:.2}, span {}", self.span),
            tk::CharLit(ch) => write!(f, "Token char: '{ch}', span {}", self.span),
            tk::StrLit(string) => write!(f, "Token string: \"{string}\", span {}", self.span),
            tk::Ident(ident) => write!(f, "Token identifier: \"{ident}\", span {}", self.span),

            tk::OpenParen => write!(f, "Token ( at {}", self.span),
            tk::CloseParen => write!(f, "Token ) at {}", self.span),
            tk::OpenBracket => write!(f, "Token [ at {}", self.span),
            tk::CloseBracket => write!(f, "Token ] at {}", self.span),
            tk::OpenCurly => write!(f, "Token {{ at {}", self.span),
            tk::CloseCurly => write!(f, "Token }} at {}", self.span),
            tk::OpenVector => write!(f, "Token #( at {}", self.span),
            tk::Comma => write!(f, "Token , at {}", self.span),
            tk::CommaAt => write!(f, "Token , at {}", self.span),
            tk::SingleQuote => write!(f, "Token ' at {}", self.span),
            tk::Comment => write!(f, "Token ; at line {}", self.span.begin.line),
            tk::Dot => write!(f, "Token . at {}", self.span),

            tk::Eof => write!(f, "Token EOF at {}", self.span),
            tk::Error(err) => match err {
                le::UntermStr => write!(f, "Error token: unterminated string at {}", self.span),
                le::UnknownChar(ch) => {
                    write!(f, "Error token: unknown character '{ch}' at {}", self.span)
                }
                le::UnexpectedChar { ch, expected } => match expected.len() {
                    0 => write!(f, "Error token: Illegal character '{ch}' at {}", self.span),

                    1 => write!(
                        f,
                        "Error token: expected '{expected}', got '{ch}' at {}",
                        self.span
                    ),
                    _ => {
                        // TODO: Format the expected string to be a comma separated list of characters
                        write!(
                            f,
                            "Error token: expected one of {expected}, got '{ch}' at {}",
                            self.span
                        )
                    }
                },
                le::UnexpectedEof => write!(f, "Error token: Unexpected EOF at {}", self.span.end),
                le::InvalidInt(bad_int) => write!(
                    f,
                    "Error token: Invalid integer literal \"{bad_int}\", at {}",
                    self.span
                ),
                le::InvalidReal(bad_real) => {
                    write!(f, "Token string: \"{bad_real}\", span {}", self.span)
                }
                le::InvalidIdent(bad_ident) => {
                    write!(f, "Token string: \"{bad_ident}\", span {}", self.span)
                }
            },
        }
    }
}

#[macro_export]
macro_rules! mk_tok {
    {$kind:tt, $span:tt} => {
        Token::new(tok![$kind], span![$span])
    };
}

/// Macro for token shorthand
#[macro_export]
macro_rules! tok {
    [str $arg:literal] => {
        TokenType::StrLit($arg.to_string())
    };
    [str $arg:ident] => {
        TokenType::StrLit($arg.to_owned())
    };
    [ident $sym:ident] => {
        TokenType::Ident($sym.to_string())
    };
    [ident $str:literal] => {
        TokenType::Ident($str.to_string())
    };
    [int $int:expr] => {
        TokenType::IntLit($int)
    };
    [real $real:expr] => {
        TokenType::RealLit($real)
    };
    [#t] => {
        TokenType::BoolLit(true)
    };
    [#f] => {
        TokenType::BoolLit(false)
    };
    [char $ch:expr] => {
        TokenType::CharLit($ch)
    };
    //[char $ch:literal] => {
    //    TokenType::CharLit($ch)
    //};
    [lparen] => {
        TokenType::OpenParen
    };
    [rparen] => {
        TokenType::CloseParen
    };
    [lbrack] => {
        TokenType::OpenBracket
    };
    [rbrack] => {
        TokenType::CloseBracket
    };
    [lcurly] => {
        TokenType::OpenCurly
    };
    [rcurly] => {
        TokenType::CloseCurly
    };
    [vec] => {
        TokenType::OpenVector
    };
    [,] => {
        TokenType::Comma
    };
    [quote] => {
        TokenType::SingleQuote
    };
    [,@] => {
        TokenType::CommaAt
    };
    [.] => {
        TokenType::Dot
    };
    [backtick] => {
        TokenType::Backtick
    };
    [; $( $span:expr )?] => {
        TokenType::Comment
    };
    [eof] => {
        TokenType::Eof
    };
}

/// Macro for shorthand error tokens
#[macro_export]
macro_rules! etok {
    [unterm_str] => {
        TokenType::Error(LexError::UntermStr)
    };
    [unknown_char $char:expr] => {
        TokenType::Error(LexError::UnknownChar($char))
    };
    [unexpected_char $char:expr] => {
        TokenType::Error(LexError::UnexpectedChar{ ch: $char, expected: "" })
    };
    [expected $e:literal, got $g:expr] => {
        TokenType::Error(LexError::UnexpectedChar { ch: $g, expected: $e })
    };
    [expected_digit, got $g:expr] => {
        TokenType::Error(LexError::UnexpectedChar { ch: $g, expected: "1234567890" })
    };
    [unexpected_eof] => {
        TokenType::Error(LexError::UnexpectedEof)
    };
    [invalid_int $e:expr] => {
        TokenType::Error(LexError::InvalidInt($e.to_string()))
    };
    [invalid_real $e:expr] => {
        TokenType::Error(LexError::InvalidReal($e.to_string()))
    };
    [invalid_ident $e:expr] => {
        TokenType::Error(LexError::InvalidIdent($e.to_string()))
    };
}
