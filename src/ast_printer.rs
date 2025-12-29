use crate::parser::*;

/// Almost all code in this file was written by AI.
/// Do not use printing for any functional behavior.

impl Program {
    /// Pretty-print the program AST to stdout, using the original source
    pub fn pretty_print(&self, source: &str) {
        println!("Program");
        self.top.pretty_print(0, source);
    }
}

impl Decl {
    fn pretty_print(&self, indent: usize, source: &str) {
        let ind = indent_str(indent);

        match &self.kind {
            DeclKind::Top(stmts) => {
                println!("{ind}Top {{");
                for stmt in stmts {
                    stmt.pretty_print(indent + 1, source);
                }
                println!("{ind}}}");
            }

            DeclKind::Let { name, value } => {
                let name_str = name.within(source);
                println!(
                    "{ind}Let {{ name: {name_str}, value: {} }}",
                    value.display(source)
                );
            }

            DeclKind::Func {
                name,
                parameters,
                return_type,
                body,
            } => {
                let name_str: &str = name.within(source);

                print!("{ind}fn {}(", name_str);
                for param in parameters {
                    print!(
                        "{}: {},",
                        param.name.within(source),
                        param.ty.within(source)
                    );
                }
                println!(") -> {} {{", return_type.within(source));
                body.pretty_print(indent + 2, source);
                println!("{ind}}}");
            }
        }
    }
}

impl Stmt {
    fn pretty_print(&self, indent: usize, source: &str) {
        let ind = indent_str(indent);

        match &self.kind {
            StmtKind::Expr(expr) => {
                println!("{ind}ExprStmt {}", expr.display(source));
            }

            StmtKind::Decl(decl) => {
                decl.pretty_print(indent, source);
            }
        }
    }
}

impl Expr {
    /// Return a string representation of the expression,
    /// using the source to resolve identifiers.
    fn display(&self, source: &str) -> String {
        match &self.kind {
            ExprKind::IntLit(i) => i.to_string(),

            ExprKind::StrLit(span) => {
                let s = span.within(source);
                format!("\"{s}\"")
            }

            ExprKind::BoolLit(b) => b.to_string(),

            ExprKind::Ident(span) => span.within(source).to_string(),

            ExprKind::Unary { op, expr } => {
                let op_str = match op {
                    UnaryOp::Not => "!",
                    UnaryOp::Negate => "-",
                };
                format!("{op_str}{}", expr.display(source))
            }

            ExprKind::Binary { lhs, op, rhs } => {
                let op_str = match op {
                    BinOp::Plus => "+",
                    BinOp::Minus => "-",
                    BinOp::Mul => "*",
                    BinOp::Divide => "/",
                };
                format!("({} {op_str} {})", lhs.display(source), rhs.display(source))
            }

            ExprKind::Block { stmts, tail_expr } => {
                let mut out = String::from("{ ");

                for stmt in stmts {
                    match &stmt.kind {
                        StmtKind::Expr(expr) => {
                            out.push_str(&expr.display(source));
                            out.push_str("; ");
                        }
                        StmtKind::Decl(_) => {
                            out.push_str("<decl>; ");
                        }
                    }
                }

                if let Some(expr) = tail_expr {
                    out.push_str(&expr.display(source));
                    out.push(' ');
                }

                out.push('}');
                out
            }
        }
    }

    fn pretty_print(&self, indent: usize, source: &str) {
        let ind = indent_str(indent);
        println!("{ind}Expr {}", self.display(source));
    }
}

/* -------------------------------------------------------------------------- */
/* Helpers                                                                    */
/* -------------------------------------------------------------------------- */

fn indent_str(indent: usize) -> String {
    "  ".repeat(indent)
}
