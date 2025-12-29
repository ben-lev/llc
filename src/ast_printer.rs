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
                print!("{ind}Let {{ name: {name_str}, value: ");
                value.pretty_print_inline(indent, source);
                println!(" }};")
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
                println!(") -> {} ", return_type.within(source));
                body.pretty_print_inline(indent + 2, source);
            }
        }
    }
}

impl Stmt {
    fn pretty_print(&self, indent: usize, source: &str) {
        let ind = indent_str(indent);

        match &self.kind {
            StmtKind::Expr(expr) => {
                expr.pretty_print_inline(indent, source);
                println!(";")
            }

            StmtKind::Decl(decl) => {
                decl.pretty_print(indent, source);
            }
        }
    }
}

impl Expr {
    /// Pretty-print the expression **inline** without newlines at the end.
    /// `indent` is used only for blocks / nested expressions that need indentation.
    fn pretty_print_inline(&self, indent: usize, source: &str) {
        match &self.kind {
            ExprKind::IntLit(i) => {
                print!("{i}");
            }

            ExprKind::StrLit(span) => {
                let s = span.within(source);
                print!("\"{s}\"");
            }

            ExprKind::BoolLit(b) => {
                print!("{b}");
            }

            ExprKind::Ident(span) => {
                print!("{}", span.within(source));
            }

            ExprKind::Unary { op, expr } => {
                let op_str = match op {
                    UnaryOp::Not => "!",
                    UnaryOp::Negate => "-",
                };
                print!("{op_str}");
                expr.pretty_print_inline(indent, source);
            }

            ExprKind::Binary { lhs, op, rhs } => {
                let op_str = match op {
                    BinOp::Plus => "+",
                    BinOp::Minus => "-",
                    BinOp::Mul => "*",
                    BinOp::Divide => "/",
                };
                print!("(");
                lhs.pretty_print_inline(indent, source);
                print!(" {op_str} ");
                rhs.pretty_print_inline(indent, source);
                print!(")");
            }

            ExprKind::Block { stmts, tail_expr } => {
                let ind = indent_str(indent);
                print!("{{");

                for stmt in stmts {
                    stmt.pretty_print(indent + 1, source);
                }

                if let Some(expr) = tail_expr {
                    expr.pretty_print_inline(indent + 1, source);
                    println!("");
                }

                print!("{ind}}}");
            }
        }
    }
}

/* -------------------------------------------------------------------------- */
/* Helpers                                                                    */
/* -------------------------------------------------------------------------- */

fn indent_str(indent: usize) -> String {
    "  ".repeat(indent)
}
