// Info on Scheme's lexical structure from here:
// https://groups.csail.mit.edu/mac/ftpdir/scheme-reports/r5rs-html/r5rs_9.html#SEC73
use super::span::{Position, Span};
use super::token::{LexError, Token, TokenType};
use crate::{etok, span, tok};
use std::{iter::Peekable, str::Chars};

pub struct Lexer<'src> {
    input: &'src str,
    stream: Peekable<Chars<'src>>,
    pos: Position,
    current: Option<char>,
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        Self {
            input,
            stream: input.chars().peekable(),
            pos: Position { line: 1, col: 1 },
            current: None,
        }
    }

    pub fn next_token(&mut self) -> Token {
        // TODO: Refactor this to be more multilayered
        // since there's so many intersecting states
        self.skip_whitespace();
        let token_start = self.pos;

        let kind = match self.next_char() {
            Some('(') => tok![lparen],
            Some(')') => tok![rparen],
            Some('[') => tok![lbrack],
            Some(']') => tok![rbrack],
            Some('{') => tok![lcurly],
            Some('}') => tok![rcurly],
            Some(',') => match self.peek_char() {
                Some('@') => tok![,@],
                Some(_) | None => tok![,],
            },
            Some('\'') => tok![quote],
            Some(';') => self.consume_comment(),
            Some('"') => self.consume_string(),
            Some('#') => match self.next_char() {
                Some('t' | 'T') => tok![#t],
                Some('f' | 'F') => tok![#f],
                Some('(') => tok![vec],
                Some('\\') => self.consume_char_lit(),
                Some(ch) => etok![expected "tf(\\", got ch],
                None => tok![eof],
            },
            Some(ch @ '0'..='9') => {
                let mut buf = ch.to_string();
                self.consume_number(&mut buf)
            }
            Some(ch) if is_symbol_start(ch) => self.consume_ident(),
            Some(ch) => etok![unknown_char ch],

            None => tok![eof],
        };

        let token = Token::new(kind, span![(token_start) to (self.pos)]);

        // Psuedo panic mode: clear the chamber please!!
        if let TokenType::Error(_) = token.kind {
            while self.peek_char().is_some_and(|c| !is_delimiter(*c)) {
                self.next_char();
            }
        }

        token
    }

    fn skip_whitespace(&mut self) {
        while self.peek_char().is_some_and(|c| c.is_ascii_whitespace()) {
            self.next_char();
        }
    }

    fn consume_comment(&mut self) -> TokenType {
        while self.expect_current() != '\n' {
            self.next_char();
        }

        tok![;]
    }

    fn consume_string(&mut self) -> TokenType {
        let mut buf = String::new();

        while let Some(ch) = self.next_char() {
            match ch {
                '"' => break,
                '\\' => match self.next_char() {
                    Some('n') => buf.push('\n'),
                    Some('t') => buf.push('\t'),
                    Some('r') => buf.push('\r'),
                    Some('0') => buf.push('\0'),
                    Some('\\') => buf.push('\\'),
                    Some('\'') => buf.push('\''),
                    Some('"') => buf.push('"'),
                    None => return etok![unexpected_eof],
                    _ => return etok![unknown_esc self.expect_current()],
                },
                _ => buf.push(ch),
            }
        }

        match self.current {
            Some('"') => {
                tok![str buf]
            }
            Some(_) | None => {
                etok![unterm_str]
            }
        }
    }

    fn consume_char_lit(&mut self) -> TokenType {
        // In favor of simplicity, we just have
        match self.next_char() {
            Some('\\') => {
                let token_type = match self.peek_char() {
                    Some('n') => tok![char '\n'],
                    Some('r') => tok![char '\r'],
                    Some('t') => tok![char '\t'],
                    Some('0') => tok![char '\0'],
                    Some(_) | None => tok![char '\\'],
                };
                self.next_char();
                token_type
            }
            Some(ch) => tok![char ch],
            None => etok![unexpected_eof],
        }
    }

    fn consume_number(&mut self, num_buf: &mut String) -> TokenType {
        //let last = self.fill_buf_while(|c| c.is_ascii_digit() && c != '.', num_buf);
        while self.peek_char().is_some_and(|c| c.is_ascii_digit() && *c != '.') {
            num_buf.push(self.expect_next());
        }

        match self.peek_char() {
            Some(&'.') => {
                num_buf.push(self.expect_next());
                self.consume_real(num_buf)
            }
            Some(ch) if is_delimiter(*ch) => match num_buf.parse::<i64>() {
                Ok(num) => tok![int num],
                Err(_) => etok![invalid_int num_buf],
            },
            Some(ch) => etok![expected ".", got *ch],
            None => etok![unexpected_eof],
        }
    }

    fn consume_real(&mut self, real_buf: &mut String) -> TokenType {
        while self.peek_char().is_some_and(|c| c.is_ascii_digit()){
            real_buf.push(self.expect_next());
        }

        match self.peek_char() {
            Some(ch) if is_delimiter(*ch) => match real_buf.parse::<f64>() {
                Ok(num) => tok![real num],
                Err(_) => etok![invalid_real real_buf],
            },
            Some(ch) => etok![expected "1234567890", got *ch],
            None => etok![unexpected_eof],
        }
    }

    fn consume_ident(&mut self) -> TokenType {
        let mut buf = self.expect_current().to_string();

        while self.peek_char().is_some_and(|c| !is_delimiter(*c)) {
            buf.push(self.expect_next());
        }

        // FIXME: This is kinda cheating a bit
        if let Ok(integer) = buf.parse::<i64>() {
            println!("parsed int {integer} from string \"{buf}\"");
            tok![int integer]
        } else if let Ok(float) = buf.parse::<f64>() {
            println!("parsed real {float} from string \"{buf}\"");
            tok![real float]
        } else {
            tok![ident buf]
        }
    }

    fn next_char(&mut self) -> Option<char> {
        self.current = self.stream.next();

        if let Some(c) = self.current {
            if c == '\n' {
                self.pos.incr_line();
            } else {
                self.pos.incr_col();
            }
        };

        self.current
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.stream.peek()
    }

    fn expect_next(&mut self) -> char {
        self.next_char()
            .expect("expect_next() called on non-guaranteed char")
    }

    fn expect_current(&mut self) -> char {
        self.current
            .expect("expect_current() called on non-guaranteed char")
    }
}

/// Scheme defines delimiters as:
/// ( ) [ ] { } ; " ' ` |
/// or any whitespace character
pub fn is_delimiter(ch: char) -> bool {
    ch.is_ascii_whitespace()
        || matches!(
            ch,
            '(' | ')' | ';' | '"' | '\'' | '`' | '|' | '[' | ']' | '{' | '}'
        )
}

pub fn is_symbol_start(ch: char) -> bool {
    ch.is_alphabetic()
        || matches!(
            ch,
            '+' | '-'
                | '.'
                | '*'
                | '/'
                | '<'
                | '='
                | '>'
                | '!'
                | '?'
                | ':'
                | '$'
                | '%'
                | '_'
                | '&'
                | '~'
                | '^'
        )
}

pub fn is_escape_seq(ch: char) -> bool {
    matches!(ch, 'n' | 't' | 'r' | '0' | '\\' | '\'' | '"')
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_tokens(sample: &str, answers: &[Token]) {
        let mut lexer = Lexer::new(sample);

        println!("Test string:\n{sample}");
        for answer in answers {
            let token = lexer.next_token();
            // TODO: Actually worry about the spans being correct
            assert_eq!(
                answer.kind, token.kind,
                "\nExpected: {answer}\nGot: {token}"
            )
        }
    }

    // These tests are shamelessly stolen from here: https://github.com/MattX/peroxide/blob/master/src/lex.rs
    #[test]
    fn char_test() {
        let sample = concat! {
            r"#\!", "\n",
            r"#\n", "\n",
            r"#\#", "\n",
            r"#\ ", "\n",
            r"#\\n", "\n",
            r"#\\t", "\n",
            r"#\\r", "\n",
            r"#\\0", "\n",
            r"#\\", "\n",
        };
        let answers = &[
            Token::new(tok![char '!'], span!(1,1 to 1,3)),
            Token::new(tok![char 'n'], span!(2,1 to 2,3)),
            Token::new(tok![char '#'], span!(4,1 to 3,3)),
            Token::new(tok![char ' '], span!(3,1 to 3,3)),
            Token::new(tok![char '\n'], span!(5,1 to 4,3)),
            Token::new(tok![char '\t'], span!(6,1 to 5,3)),
            Token::new(tok![char '\r'], span!(7,1 to 6,3)),
            Token::new(tok![char '\0'], span!(8,1 to 8,3)),
            Token::new(tok![char '\\'], span!(9,1 to 9,3)),
            Token::new(tok![eof], span!(10, 1)),
        ];

        assert_tokens(sample, answers)
    }

    #[test]
    fn int_test() {
        // TODO: Get this working
        let sample = concat! {
            "123\n",
            "0\n",
            "-123\n",
            "+123\n",
            "12x3\n",
            "123x\n"
        };
        let answers = &[
            Token::new(tok![int 123], span![1,1 to 1,3]),
            Token::new(tok![int 0], span![2, 1]),
            Token::new(tok![int - 123], span![3,1 to 3,4]),
            Token::new(tok![int 123], span![4,1 to 4,4]),
            Token::new(etok![expected ".", got 'x'], span![5,1 to 5,4]),
            Token::new(etok![expected ".", got 'x'], span![6,1 to 6,4]),
            Token::new(tok![eof], span![7, 1]),
        ];

        assert_tokens(sample, answers);
    }

    #[test]
    fn real_test() {
        let sample = concat! {
            "123.4567\n",
            ".4567\n",
            "+.0\n",
            "-.0\n",
            "0.060\n",
            "-0a.\n",
            "-0.123d\n",
        };
        let answers = &[
            Token::new(tok![real 123.4567], span![1,1 to 1,8]),
            Token::new(tok![real 0.4567], span![2,1 to 2,5]),
            Token::new(tok![real 0.], span![3,1 to 3,2]),
            Token::new(tok![real - 0.], span![4,1 to 4,3]),
            Token::new(tok![real 0.06], span![5,1 to 5,4]),
            Token::new(tok![ident "-0a."], span![5,1 to 5,3]),
            Token::new(tok![ident "-0.123d"], span![6,1 to 6,7]),
            Token::new(tok![eof], span![6, 8]),
        ];

        assert_tokens(sample, answers);
    }

    #[test]
    fn bool_test() {
        let sample = concat! { 
            "#f\n", 
            "#t\n", 
        };
        let answers = &[
            Token::new(tok![#f], span![1,1 to 1,2]),
            Token::new(tok![#t], span![1,4 to 1,5]),
            Token::new(tok![eof], span![1, 6]),
        ];

        assert_tokens(sample, answers);
    }

    #[test]
    fn parens_test() {
        let sample = concat! {
            "() \n", 
            "(  ) ", 
        };
        let answers = &[
            Token::new(tok![lparen], span![1, 1]),
            Token::new(tok![rparen], span![1, 2]),
            Token::new(tok![lparen], span![2, 1]),
            Token::new(tok![rparen], span![2, 4]),
            Token::new(tok![eof], span![2, 6]),
        ];

        assert_tokens(sample, answers);
    }

    #[test]
    fn ident_test() {
        let sample = concat! {
            "abc\n",
            "<=\n",
            "+\n",
            ".\n",
            "...\n",
        };
        let answers = &[
            Token::new(tok![ident "abc"], span![1,1 to 1,3]),
            Token::new(tok![ident "<="], span![2,1 to 2,2]),
            Token::new(tok![ident "+"], span![3,1 to 3,1]),
            Token::new(tok![ident "."], span![4,1 to 4,1]),
            Token::new(tok![ident "..."], span![5,1 to 5,3]),
            Token::new(tok![eof], span![6, 1]),
        ];

        assert_tokens(sample, answers);
    }

    #[test]
    fn error_test() {
        // TODO: Test more of the error tokens
        let sample = concat! {
            "#\n",
            "\"abc\n",
        };
        let answers = &[
            Token::new(etok![expected "tf(\\", got '\n'], span![1,1 to 1,2]),
            Token::new(etok![unterm_str], span![1,1 to 1,5]),
        ];

        assert_tokens(sample, answers);
    }

    #[test]
    fn string_test() {
        let sample = concat! {
            "\"abcdef\"\n",
            "\"abc\\\"def\"\n",
            "\"abc\\\\def\"\n",
            "\"abc\\ndef\"\n",
        };
        let answers = &[
            Token::new(tok![str "abcdef"], span![1,2 to 1,7]),
            Token::new(tok![str "abc\"def"], span![2,2 to 2,9]),
            Token::new(tok![str "abc\\def"], span![3,2 to 3,9]),
            Token::new(tok![str "abc\ndef"], span![4,2 to 4,9]),
            Token::new(tok![eof], span![5, 1]),
        ];

        assert_tokens(sample, answers);
    }

    #[test]
    fn comment_test() {
        let sample = concat! { 
            "; this is a comment\n", 
            "(+ 2 ; this is a comment inline\n",
            "3)\n"
        };
        let answers = &[
            Token::new(tok![;], span![1,1 to 1,20]),
            Token::new(tok![lparen], span!(2, 1)),
            Token::new(tok![ident "+"], span!(2, 2)),
            Token::new(tok![int 2], span!(2, 4)),
            Token::new(tok![;], span![2,6 to 2,32]),
            Token::new(tok![int 3], span!(3, 1)),
            Token::new(tok![rparen], span!(3, 2)),
            Token::new(tok![eof], span!(4, 1)),
        ];

        assert_tokens(sample, answers);
    }
}
