use crate::lexer::*;
use TokenKind::*;

#[derive(Debug)]
pub enum ParseErrorKind {
    UnexpectedToken {
        found: TokenKind,
        expected: TokenKind,
    },
    UnexpectedEof,
    InvalidLiteral(String),
    TailExprNotAllowed,
}

use ParseErrorKind::*;

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub loc: SourceRef,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Negate,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Plus,
    Minus,
    Mul,
    Divide,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    IntLit(i64),
    StrLit(SourceRef),
    BoolLit(bool),
    Ident(SourceRef),
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        lhs: Box<Expr>,
        op: BinOp,
        rhs: Box<Expr>,
    },
    Block {
        stmts: Vec<Stmt>,
        tail_expr: Option<Box<Expr>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub name: SourceRef,
    pub ty: SourceRef,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeclKind {
    Top(Vec<Stmt>),
    Let {
        name: SourceRef,
        value: Expr,
    },
    Func {
        name: SourceRef,
        parameters: Vec<Parameter>,
        return_type: SourceRef,
        body: Expr, // Block expr
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StmtKind {
    Expr(Box<Expr>),
    Decl(Box<Decl>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    pub(crate) kind: ExprKind,
    sref: SourceRef,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub sref: SourceRef,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Decl {
    pub(crate) kind: DeclKind,
    pub sref: SourceRef,
}

pub struct Program {
    pub top: Decl,
}

enum StmtOrTailExpr {
    Stmt(Stmt),
    TailExpr(Expr),
}

impl Program {
    fn unwrap_top_stmts(self) -> Vec<Stmt> {
        match self.top.kind {
            DeclKind::Top(stmts) => stmts,
            _ => unreachable!("top decl wasn't DeclKind::Top??"),
        }
    }
}

struct Parser<'a, L>
where
    L: Lexer<'a>,
{
    source: &'a str,
    tokens: L,
}

impl<'a> Parser<'a, TokenStream<'a>> {
    pub fn new(source: &'a str) -> Self {
        Self::new_from_lexer(source, TokenStream::new(source))
    }
}

impl<'a, L> Parser<'a, L>
where
    L: Lexer<'a>,
{
    pub fn new_from_lexer(source: &'a str, lexer: L) -> Self {
        Self {
            source: source,
            tokens: lexer,
        }
    }

    fn is_finished(&self) -> bool {
        self.tokens.peek().kind == Eof
    }

    pub fn parse(mut self) -> Result<Program, ParseError> {
        let mut stmts = Vec::new();

        loop {
            if self.is_finished() {
                break;
            }
            let stmt = self.stmt()?;
            stmts.push(stmt);
        }

        Ok(Program {
            top: Decl {
                kind: DeclKind::Top(stmts),
                sref: SourceRef {
                    start: 0,
                    end: self.source.len(),
                },
            },
        })
    }

    fn stmt(&mut self) -> Result<Stmt, ParseError> {
        match self.stmt_or_tail_expr()? {
            StmtOrTailExpr::Stmt(stmt) => Ok(stmt),
            StmtOrTailExpr::TailExpr(expr) => Err(ParseError {
                kind: TailExprNotAllowed,
                loc: expr.sref,
            }),
        }
    }

    /// Parses the next sequence of tokens as either a statement or tail
    /// expression.
    fn stmt_or_tail_expr(&mut self) -> Result<StmtOrTailExpr, ParseError> {
        let nt = self.tokens.peek();

        match nt.kind {
            Let => {
                let let_decl = self.let_decl()?;
                let sref = let_decl.sref;
                Ok(StmtOrTailExpr::Stmt(Stmt {
                    kind: StmtKind::Decl(Box::new(let_decl)),
                    sref: sref,
                }))
            }
            Eof => {
                panic!("stmt_or_tail should never be called with Eof as next token");
            }
            _ => {
                // If it's not any keyword initiated statements, then evaluate
                // as an expression.
                let expr = self.expr()?;

                // It's either an expression statement or tail expression
                // based on presence of semi colon immediately after the expression.
                if let Some(semi) = self.take_if(Semi) {
                    let stmt_sref = self.curr_sref(semi.end);
                    Ok(StmtOrTailExpr::Stmt(Stmt {
                        kind: StmtKind::Expr(Box::new(expr)),
                        sref: stmt_sref,
                    }))
                } else {
                    Ok(StmtOrTailExpr::TailExpr(expr))
                }
            }
        }
    }

    fn expr(&mut self) -> Result<Expr, ParseError> {
        self.add_sub_expr()
    }

    fn mul_div_expr(&mut self) -> Result<Expr, ParseError> {
        self.binary_expr(&[(Star, BinOp::Mul), (FSlash, BinOp::Divide)], |p| {
            p.unary_expr()
        })
    }

    fn add_sub_expr(&mut self) -> Result<Expr, ParseError> {
        self.binary_expr(&[(Plus, BinOp::Plus), (Dash, BinOp::Minus)], |p| {
            p.mul_div_expr()
        })
    }

    fn binary_expr<F>(
        &mut self,
        matching_prec_tokens: &[(TokenKind, BinOp)],
        mut next_rule: F,
    ) -> Result<Expr, ParseError>
    where
        F: FnMut(&mut Self) -> Result<Expr, ParseError>,
    {
        let start_loc = self.tokens.current_pos();
        let mut lhs = next_rule(self)?;

        loop {
            let maybe_bin_op = self.tokens.peek();
            let Some((_, bin_op)) = matching_prec_tokens
                .iter()
                .find(|(kind, _)| *kind == maybe_bin_op.kind)
            else {
                break;
            };
            // Eat the bin op
            self.tokens.take();

            let rhs = next_rule(self)?;
            let sref = SourceRef {
                start: start_loc,
                end: self.tokens.current_pos(),
            };
            lhs = Expr {
                kind: ExprKind::Binary {
                    lhs: Box::new(lhs),
                    op: *bin_op,
                    rhs: Box::new(rhs),
                },
                sref: sref,
            };
        }

        return Ok(lhs);
    }

    fn unary_expr(&mut self) -> Result<Expr, ParseError> {
        let start_loc = self.tokens.current_pos();
        let maybe_unary_op = self.tokens.peek();
        match maybe_unary_op.kind {
            Bang => {
                self.tokens.take(); // Eat the !
                let expr = self.unary_expr()?;
                Ok(Expr {
                    kind: ExprKind::Unary {
                        op: UnaryOp::Not,
                        expr: Box::new(expr),
                    },
                    sref: SourceRef {
                        start: start_loc,
                        end: self.tokens.current_pos(),
                    },
                })
            }
            Dash => {
                self.tokens.take(); // Eat the -
                let expr = self.unary_expr()?;
                Ok(Expr {
                    kind: ExprKind::Unary {
                        op: UnaryOp::Negate,
                        expr: Box::new(expr),
                    },
                    sref: SourceRef {
                        start: start_loc,
                        end: self.tokens.current_pos(),
                    },
                })
            }
            _ => self.primary_expr(),
        }
    }

    fn primary_expr(&mut self) -> Result<Expr, ParseError> {
        let nt = self.tokens.take();

        match nt.kind {
            Int => {
                let value_str = nt.sref().within(self.source);
                let value: i64 = value_str.parse().unwrap();
                Ok(Expr {
                    kind: ExprKind::IntLit(value),
                    sref: nt.sref(),
                })
            }
            Str => {
                let value_with_quotes = nt.sref();
                // String literals are always stored with their surrounding quotes. For now we'll assume just single pair of quotes
                // and drop them when unwrapping here.
                let value = SourceRef {
                    start: value_with_quotes.start + 1,
                    end: value_with_quotes.end - 1,
                };
                Ok(Expr {
                    kind: ExprKind::StrLit(value),
                    sref: value_with_quotes,
                })
            }
            True => Ok(Expr {
                kind: ExprKind::BoolLit(true),
                sref: nt.sref(),
            }),
            False => Ok(Expr {
                kind: ExprKind::BoolLit(false),
                sref: nt.sref(),
            }),
            LParen => {
                let expr = self.expr()?;
                self.expect(RParen)?;
                // Overwrite the expression to include the parenthases in the sref.
                let expr = Expr {
                    kind: expr.kind,
                    sref: self.curr_sref(nt.start),
                };
                Ok(expr)
            }
            Ident => {
                let name = nt.sref();
                Ok(Expr {
                    kind: ExprKind::Ident(name),
                    sref: name,
                })
            }
            LBrace => self.block(nt),
            _ => panic!("Primary expression."),
        }
    }

    // Note: When this is invoked we've already consumed the '{'
    fn block(&mut self, lbrace: Token) -> Result<Expr, ParseError> {
        let start_loc = lbrace.start;

        let mut stmts = Vec::new();
        let mut tail_expr = None;
        loop {
            let nt = self.tokens.peek();

            if nt.kind == Eof {
                return Err(ParseError {
                    kind: UnexpectedEof,
                    loc: self.curr_sref(start_loc),
                });
            }
            if nt.kind == RBrace {
                break;
            }

            let stmt_or_tail_expr = self.stmt_or_tail_expr()?;
            match stmt_or_tail_expr {
                StmtOrTailExpr::Stmt(stmt) => stmts.push(stmt),
                StmtOrTailExpr::TailExpr(expr) => {
                    tail_expr = Some(Box::new(expr));
                    break;
                }
            };
        }

        self.expect(RBrace)?;
        Ok(Expr {
            kind: ExprKind::Block { stmts, tail_expr },
            sref: self.curr_sref(start_loc),
        })
    }

    fn let_decl(&mut self) -> Result<Decl, ParseError> {
        let start_pos = self.tokens.current_pos();

        self.expect(Let)?;
        let name = self.expect(Ident)?.sref();

        self.expect(Eq)?;

        let expr = self.expr()?;

        self.expect(Semi)?;

        let end_pos = self.tokens.current_pos();
        Ok(Decl {
            kind: DeclKind::Let {
                name: name,
                value: expr,
            },
            sref: SourceRef {
                start: start_pos,
                end: end_pos,
            },
        })
    }

    fn expect(&mut self, expected_kind: TokenKind) -> Result<Token, ParseError> {
        let token = self.tokens.take();
        if token.kind != expected_kind {
            Err(ParseError {
                kind: UnexpectedToken {
                    found: token.kind,
                    expected: expected_kind,
                },
                loc: token.sref(),
            })
        } else {
            Ok(token)
        }
    }

    fn check(&mut self, checked_kind: TokenKind) -> Option<Token> {
        let token = self.tokens.peek();
        if token.kind == checked_kind {
            Some(token)
        } else {
            None
        }
    }

    /// If the next token kind is `kind`, then consumes and returns then next token.
    /// Otherwise returns None and *does not* consume the next token
    fn take_if(&mut self, kind: TokenKind) -> Option<Token> {
        if let Some(token) = self.check(kind) {
            self.tokens.take();
            Some(token)
        } else {
            None
        }
    }

    /// Just a small convenience to make an sref from a given start spanning to
    /// current lexer position.
    fn curr_sref(&mut self, start: usize) -> SourceRef {
        SourceRef {
            start: start,
            end: self.tokens.current_pos(),
        }
    }
}

//fn basic_walk<F, R>(ast: &Program, source: &str, mut pred: F)
//where F: FnMut()

#[cfg(test)]
mod tests {
    use super::*;

    struct MockLexer<'a> {
        tokens: &'a [Token],
        pos: usize,
    }

    impl<'a> Lexer<'a> for MockLexer<'a> {
        fn take(&mut self) -> Token {
            let token = self.tokens[self.pos];
            self.pos += 1;
            token
        }

        fn peek(&self) -> Token {
            self.tokens[self.pos]
        }

        fn current_pos(&self) -> usize {
            self.pos
        }
    }

    /// Convenience for tests to construct AST nodes including valid source locs.
    struct SRefFinder<'a> {
        source: &'a str,
    }

    impl<'a> SRefFinder<'a> {
        fn find_skipping(&self, target: &str, skip: usize) -> SourceRef {
            let (start, _) = self.source.match_indices(target).nth(skip).unwrap();
            let end = start + target.len();
            SourceRef { start, end }
        }

        fn find(&self, target: &str) -> SourceRef {
            self.find_skipping(target, 0)
        }

        fn find_block(&self, block_index: usize) -> SourceRef {
            let mut chars = self.source.chars();
            let mut open_braces_to_pass = block_index;
            let start_loc: usize;
            // Find the start of first block
            loop {
                let Some(char) = chars.next() else {
                    panic!("Can't find the block");
                };

                if char != '{' {
                    continue;
                }

                if open_braces_to_pass == 0 {
                    start_loc = self.source.len() - chars.as_str().len();
                    break;
                } else {
                    open_braces_to_pass -= 1;
                }
            }

            loop {
                let Some(char) = chars.next() else {
                    panic!("No closing brace?")
                };
                if char != '}' {
                    continue;
                }
                let end_loc = self.source.len() - chars.as_str().len();
                return SourceRef {
                    start: start_loc,
                    end: end_loc,
                };
            }
        }
    }

    // === Convenience helpers for building AST nodes ===
    fn ident(sref: SourceRef) -> Expr {
        Expr {
            kind: ExprKind::Ident(sref),
            sref,
        }
    }

    fn int_lit(value: i64, sref: SourceRef) -> Expr {
        Expr {
            kind: ExprKind::IntLit(value),
            sref,
        }
    }

    fn bin(lhs: Expr, op: BinOp, rhs: Expr, sref: SourceRef) -> Expr {
        Expr {
            kind: ExprKind::Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            },
            sref,
        }
    }

    fn let_decl(name: SourceRef, value: Expr, sref: SourceRef) -> Stmt {
        Stmt {
            kind: StmtKind::Decl(Box::new(Decl {
                kind: DeclKind::Let { name, value },
                sref,
            })),
            sref,
        }
    }

    #[test]
    fn test_basic_let_decls() {
        let source = r#"
        let x = 5;
        let y = 10;
        let z = x + y;
        let a = "hi there";
        let b = true;
        let c = a + b;
        let d = !c + -2;
        "#
        .trim();

        let parser = Parser::new(source);
        let program = parser.parse().unwrap();
        program.pretty_print(source);
    }

    #[test]
    fn test_mul_add_precedence() {
        let source = "let myVar = x * 8 + y;";
        let parser = Parser::new(source);
        let sref_finder = SRefFinder { source };

        let stmts = parser.parse().unwrap().unwrap_top_stmts();
        assert_eq!(stmts.len(), 1);

        let x = ident(sref_finder.find("x"));
        let y = ident(sref_finder.find_skipping("y", 1));
        let eight = int_lit(8, sref_finder.find("8"));
        let my_var = sref_finder.find("myVar");

        let mul = bin(x, BinOp::Mul, eight, sref_finder.find("x * 8 "));
        let add = bin(mul, BinOp::Plus, y, sref_finder.find("x * 8 + y"));
        let stmt = let_decl(
            my_var,
            add,
            SourceRef {
                start: 0,
                end: source.len(),
            },
        );

        assert_eq!(stmts[0], stmt);
    }

    #[test]
    fn test_simple_block() {
        let source = r#"
          let y = {
          let z = 9;
          4 + z
        };
        "#
        .trim();

        let parser = Parser::new(source);
        let sref_finder = SRefFinder { source };

        let program = parser.parse().unwrap();
        program.pretty_print(source);

        let stmts = program.unwrap_top_stmts();
        assert_eq!(stmts.len(), 1);

        let nine = int_lit(9, sref_finder.find("9"));
        let let_z = let_decl(sref_finder.find("z"), nine, sref_finder.find("let z = 9;"));
        let four = int_lit(4, sref_finder.find("4"));
        let z = ident(sref_finder.find("z"));
        let add_expr = bin(four, BinOp::Plus, z, sref_finder.find("4 + z"));
        let block = Expr {
            kind: ExprKind::Block {
                stmts: vec![let_z],
                tail_expr: Some(Box::new(add_expr)),
            },
            sref: sref_finder.find_block(0),
        };
        let let_y = let_decl(
            sref_finder.find("y"),
            block,
            SourceRef {
                start: 0,
                end: source.len(),
            },
        );

        stmts[0].assert_eq(&let_y, source);
    }
}
