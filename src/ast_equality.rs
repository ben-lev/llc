use crate::parser::*;
use std::iter::zip;

impl Stmt {
    pub fn assert_eq(&self, other: &Stmt, source: &str) {
        match (&self.kind, &other.kind) {
            (StmtKind::Expr(me_expr), StmtKind::Expr(other_expr)) => {
                me_expr.assert_eq(other_expr, source);
            }
            (StmtKind::Decl(me_decl), StmtKind::Decl(other_decl)) => {
                me_decl.assert_eq(other_decl, source);
            }
            _ => {
                self.pretty_print(0, source);
                other.pretty_print(0, source);
                panic!("Mismatched stmt kinds!")
            }
        }
    }
}

impl Decl {
    pub fn kind_tag(&self) -> &'static str {
        match self.kind {
            DeclKind::Top(_) => "Top",
            DeclKind::Let { .. } => "Let",
            DeclKind::Func { .. } => "Func",
        }
    }

    pub fn assert_eq(&self, other: &Decl, source: &str) {
        match (&self.kind, &other.kind) {
            (
                DeclKind::Let {
                    name: my_name,
                    value: my_value,
                },
                DeclKind::Let {
                    name: other_name,
                    value: other_value,
                },
            ) => {
                assert_eq!(my_name, other_name);
                my_value.assert_eq(&other_value, source);
            }
            (DeclKind::Top(stmts), DeclKind::Top(other_stmts)) => {
                assert_eq!(stmts.len(), other_stmts.len());
                for (my_stmt, other_stmt) in zip(stmts, other_stmts) {
                    my_stmt.assert_eq(&other_stmt, source);
                }
            }
            (
                DeclKind::Func {
                    name,
                    parameters,
                    return_type,
                    body,
                },
                DeclKind::Func {
                    name: other_name,
                    parameters: other_parameters,
                    return_type: other_return_type,
                    body: other_body,
                },
            ) => {
                assert_eq!(name, other_name);
                assert_eq!(parameters, other_parameters);
                assert_eq!(return_type, other_return_type);
                body.assert_eq(other_body, source);
            }
            _ => {
                self.pretty_print(0, source);
                other.pretty_print(0, source);
                panic!(
                    "non-matching decl types! {} vs {}",
                    self.kind_tag(),
                    other.kind_tag()
                );
            }
        }
    }
}

impl Expr {
    pub fn assert_eq(&self, other: &Expr, source: &str) {
        match (&self.kind, &other.kind) {
            (ExprKind::IntLit(a), ExprKind::IntLit(b)) => {
                assert_eq!(a, b, "IntLit mismatch: {} vs {}", a, b);
            }
            (ExprKind::StrLit(a), ExprKind::StrLit(b)) => {
                let sa = a.within(source);
                let sb = b.within(source);
                assert_eq!(sa, sb, "StrLit mismatch: '{}' vs '{}'", sa, sb);
            }
            (ExprKind::BoolLit(a), ExprKind::BoolLit(b)) => {
                assert_eq!(a, b, "BoolLit mismatch: {} vs {}", a, b);
            }
            (ExprKind::Ident(a), ExprKind::Ident(b)) => {
                let sa = a.within(source);
                let sb = b.within(source);
                assert_eq!(sa, sb, "Ident mismatch: '{}' vs '{}'", sa, sb);
            }
            (ExprKind::Unary { op: ao, expr: ae }, ExprKind::Unary { op: bo, expr: be }) => {
                assert_eq!(ao, bo, "UnaryOp mismatch");
                ae.assert_eq(be, source);
            }
            (
                ExprKind::Binary {
                    lhs: al,
                    op: ao,
                    rhs: ar,
                },
                ExprKind::Binary {
                    lhs: bl,
                    op: bo,
                    rhs: br,
                },
            ) => {
                assert_eq!(ao, bo, "BinaryOp mismatch");
                al.assert_eq(bl, source);
                ar.assert_eq(br, source);
            }
            (
                ExprKind::Block {
                    stmts: astmts,
                    tail_expr: atail,
                },
                ExprKind::Block {
                    stmts: bstmts,
                    tail_expr: btail,
                },
            ) => {
                assert_eq!(astmts.len(), bstmts.len(), "Block stmt length mismatch");
                for (a_stmt, b_stmt) in zip(astmts, bstmts) {
                    a_stmt.assert_eq(b_stmt, source);
                }

                match (atail, btail) {
                    (Some(a_expr), Some(b_expr)) => a_expr.assert_eq(b_expr, source),
                    (None, None) => {}
                    _ => panic!("Block tail expression mismatch"),
                }
            }
            _ => {
                // For debugging, print both expressions
                println!("Left expression:");
                self.pretty_print_inline(0, source);
                println!("Right expression:");
                other.pretty_print_inline(0, source);
                panic!("Expression kinds do not match");
            }
        }
    }
}
