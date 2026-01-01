use TokenKind::*;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::str::Chars;

#[derive(Clone)]
pub struct TokenStream<'a> {
    pub source: Chars<'a>,
    source_len: usize,
}

/// Semantic &str but only holding the indices
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceRef {
    pub start: usize,
    pub end: usize,
}

impl SourceRef {
    pub fn within<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start..self.end]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Int,
    Str,
    Semi,
    Let,
    Eq,
    Eof,
    Plus,
    Dash,
    Ident,
    True,
    False,
    Return,
    Bang,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Star,
    FSlash,
    Func,
    Arrow,
    Colon,
    Comma,
}

static KEYWORDS: Lazy<HashMap<&'static str, TokenKind>> = Lazy::new(|| {
    let mut kws = HashMap::new();
    kws.insert("true", True);
    kws.insert("false", False);
    kws.insert("let", Let);
    kws.insert("return", Return);
    kws.insert("fn", Func);
    kws
});

fn ident_kind(ident: &str) -> TokenKind {
    KEYWORDS.get(ident).cloned().unwrap_or(Ident)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,

    /// Offset and length in bytes into the original source string.
    pub start: usize,
    pub end: usize,
}

impl Token {
    fn eof() -> Self {
        Self {
            kind: Eof,
            start: usize::MAX,
            end: usize::MAX,
        }
    }

    pub fn sref(&self) -> SourceRef {
        SourceRef {
            start: self.start,
            end: self.end,
        }
    }
}

fn difference<'a>(start: &'a str, end: &'a str) -> &'a str {
    let diff = start.len() - end.len();
    &start[0..diff]
}

impl<'a> TokenStream<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source: source.chars(),
            source_len: source.len(),
        }
    }

    pub fn peek(&self) -> Token {
        self.clone().take()
    }

    pub fn take(&mut self) -> Token {
        let start_loc = self.source.clone().as_str();
        let Some(c) = self.next_char() else {
            return Token::eof();
        };

        let tok = match c {
            '+' => Plus,
            '-' => {
                // Dash may be standalone minus sign or start an Arrow
                if self.peek_char().unwrap_or('_') == '>' {
                    self.next_char(); // Eat the '>'
                    Arrow
                } else {
                    Dash
                }
            }
            ';' => Semi,
            '=' => Eq,
            '!' => Bang,
            '(' => LParen,
            ')' => RParen,
            '{' => LBrace,
            '}' => RBrace,
            '*' => Star,
            '/' => FSlash,
            ':' => Colon,
            ',' => Comma,
            c if Self::is_ident_start(c) => {
                self.ident_or_unknown();
                let id = difference(start_loc, self.current_loc());
                ident_kind(id)
            }
            c if c.is_numeric() => self.number(),
            '"' => {
                self.string();
                Str
            }
            _ => {
                panic!(
                    "The character {} at position {} was not known to start any legal tokens.",
                    c, start_loc
                );
            }
        };

        let end_loc = self.current_loc();
        let token = Token {
            kind: tok,
            start: self.source_len - start_loc.len(),
            end: self.source_len - end_loc.len(),
        };
        self.advance_whitespace();
        token
    }

    fn next_char(&mut self) -> Option<char> {
        self.source.next()
    }

    fn peek_char(&self) -> Option<char> {
        self.source.clone().next()
    }

    fn is_ident_start(c: char) -> bool {
        c.is_ascii_alphabetic()
    }

    pub fn current_loc(&self) -> &str {
        self.source.clone().as_str()
    }

    pub fn current_pos(&self) -> usize {
        self.source_len - self.current_loc().len()
    }

    fn ident_or_unknown(&mut self) {
        self.advance_until_ident_end();
    }

    fn number(&mut self) -> TokenKind {
        self.advance_until(|c| !c.is_numeric());
        // FIXME: Handle turning decimals into floats
        Int
    }

    fn string(&mut self) {
        self.advance_until(|c| c == '"');
        // Consume the close quote, or just Eof if it was left open.
        self.next_char();
    }

    fn advance_until_ident_end(&mut self) {
        self.advance_while(|c| c == '_' || c.is_alphanumeric());
    }

    /// Moves past whitespace, returns number of bytes passed.
    fn advance_whitespace(&mut self) {
        self.advance_while(|c| c.is_whitespace());
    }

    /// Consumes chars until pred returns true or EOF is reached. It does not consume the character for which `pred` returns true.
    fn advance_until<P>(&mut self, mut pred: P)
    where
        P: FnMut(char) -> bool,
    {
        while let Some(nc) = self.peek_char() {
            if pred(nc) {
                break;
            } else {
                self.next_char();
            }
        }
    }

    /// Consumes chars while pred returns true or until EOF is reached.
    /// It does not consume the first character for which pred returns false.
    fn advance_while<P>(&mut self, mut pred: P)
    where
        P: FnMut(char) -> bool,
    {
        self.advance_until(|c| !pred(c));
    }

    fn advancen(&mut self, n: usize) {
        for _ in 0..n {
            self.source.next();
        }
    }

    pub fn collect(mut self) -> Vec<Token> {
        let mut tokens = Vec::<Token>::new();
        loop {
            let token = self.take();
            let kind = token.kind;
            tokens.push(token);
            if kind == TokenKind::Eof {
                break;
            }
        }
        return tokens;
    }
}

pub trait Lexer<'a> {
    /// Consume and return the next token.
    fn take(&mut self) -> Token;

    /// Return the next token without consuming it.
    fn peek(&self) -> Token;

    /// Distance in bytes from start of the text being lex'd. (For keeping track of location spans).
    fn current_pos(&self) -> usize;

    fn remaining_program(&self) -> &'a str;
}

impl<'a> Lexer<'a> for TokenStream<'a> {
    fn take(&mut self) -> Token {
        self.take()
    }

    fn peek(&self) -> Token {
        self.peek()
    }

    fn current_pos(&self) -> usize {
        self.current_pos()
    }

    fn remaining_program(&self) -> &'a str {
        self.source.as_str()
    }
}

#[cfg(test)]
mod tests {
    use std::usize;

    use super::*;
    use std::iter::zip;

    #[test]
    fn lex_simple_int() {
        let stmt = "let x = 587;";
        let expected = [
            Token {
                kind: Let,
                start: 0,
                end: 3,
            },
            Token {
                kind: Ident,
                start: 4,
                end: 5,
            },
            Token {
                kind: Eq,
                start: 6,
                end: 7,
            },
            Token {
                kind: Int,
                start: 8,
                end: 11,
            },
            Token {
                kind: Semi,
                start: 11,
                end: 12,
            },
            Token {
                kind: Eof,
                start: usize::MAX,
                end: usize::MAX,
            },
        ];

        test_full(stmt, &expected);
    }

    #[test]
    fn lex_simple_str() {
        let stmt = "let z = \"hi I'm ben\";";
        let expected = [
            Token {
                kind: Let,
                start: 0,
                end: 3,
            },
            Token {
                kind: Ident,
                start: 4,
                end: 5,
            },
            Token {
                kind: Eq,
                start: 6,
                end: 7,
            },
            Token {
                kind: Str,
                start: 8,
                end: 20,
            },
            Token {
                kind: Semi,
                start: 20,
                end: 21,
            },
            Token {
                kind: Eof,
                start: usize::MAX,
                end: usize::MAX,
            },
        ];

        test_full(stmt, &expected);
    }

    #[test]
    fn lex_not_ident() {
        let program = r#"
        let x = false;
        !x;
        "#
        .trim();

        let expected = [Let, Ident, Eq, False, Semi, Bang, Ident, Semi];
        test_kind(program, &expected);
    }

    fn test_full(source: &str, expected: &[Token]) {
        let tok_stream = TokenStream::new(source);
        let tokens = tok_stream.collect();

        for (observed, expected) in zip(tokens, expected) {
            assert_eq!(observed, *expected)
        }
    }

    fn test_kind(source: &str, expected: &[TokenKind]) {
        let tok_stream = TokenStream::new(source);
        let tokens = tok_stream.collect();

        for (observed, expected) in zip(tokens, expected) {
            assert_eq!(observed.kind, *expected)
        }
    }
}
