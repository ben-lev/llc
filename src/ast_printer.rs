use crate::parser::*;

/// Almost all code in this file was written by AI. Don't use printing for any functional behavior.

impl Program {
    /// Pretty-print the program AST to stdout, using the original source
    pub fn pretty_print(&self, source: &str) {
        println!("Program:");
        self.top.pretty_print(0, source);
    }
}

impl Decl {
    fn pretty_print(&self, indent: usize, source: &str) {
        let ind = "  ".repeat(indent);
        match &self.kind {
            DeclKind::Top(stmts) => {
                println!("{}Top {{", ind);
                for stmt in stmts {
                    stmt.pretty_print(indent + 1, source);
                }
                println!("{}}}", ind);
            }
            DeclKind::Let { name, value } => {
                // Resolve the identifier name from the source string
                let name_str = &source[name.start..name.end];
                println!(
                    "{}Let {{ name: {}, value: {} }}",
                    ind,
                    name_str,
                    value.display(source)
                );
            }
        }
    }
}

impl Stmt {
    fn pretty_print(&self, indent: usize, source: &str) {
        let ind = "  ".repeat(indent);
        match &self.kind {
            StmtKind::Expr(expr) => {
                println!("{}ExprStmt: {}", ind, expr.display(source));
            }
            StmtKind::Decl(decl) => {
                decl.pretty_print(indent, source);
            }
        }
    }
}

impl Expr {
    /// Return a string representation of the expression, using the source to resolve identifiers
    fn display(&self, source: &str) -> String {
        match &self.kind {
            ExprKind::IntLit(i) => i.to_string(),
            ExprKind::StrLit(sref) => {
                let s = &source[sref.start..sref.end];
                format!("\"{}\"", s) // wrap in quotes for readability
            }
            ExprKind::BoolLit(truthy) => truthy.to_string(),
            ExprKind::Ident(name) => {
                let ident = &source[name.start..name.end];
                ident.to_string()
            }
            ExprKind::Unary { op, expr } => {
                let op_str = match op {
                    UnaryOp::Not => "!",
                    UnaryOp::Negate => "-",
                };
                format!("{}{}", op_str, expr.display(source))
            }
            ExprKind::Binary { lhs, op, rhs } => {
                let op_str = match op {
                    BinOp::Plus => "+",
                    BinOp::Minus => "-",
                    BinOp::Mul => "*",
                    BinOp::Divide => "/",
                };
                format!(
                    "({} {} {})",
                    lhs.display(source),
                    op_str,
                    rhs.display(source)
                )
            }
        }
    }
}
