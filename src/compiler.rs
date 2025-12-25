use crate::lexer::*;

pub fn compile(source: &str) {
    println!("Compiling program: \n\n\n{}\n\n", source);
    let mut tok_stream = TokenStream::new(source);

    loop {
        let tok = tok_stream.take();
        if tok.kind == TokenKind::Eof {
            break;
        }

        let ss = &source[tok.start..tok.end];
        println!(
            "Next token: ({:?}, {}) from: ({}, {})",
            tok.kind, ss, tok.start, tok.end
        )
    }
}
