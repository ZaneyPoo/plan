mod reader;
mod evaluator;
mod printer;

use std::io::{prelude::*, stdin, stdout};

use reader::read;
use evaluator::eval;
use printer::print;


fn repl(prompt: &'static str) -> std::io::Result<()> {
    let mut buffer = String::new();
    let stdin = stdin();
    let mut stdout = stdout();

    loop {
        print!("{prompt}");
        stdout.flush()?;

        stdin.read_line(&mut buffer)?;
        print(eval(read(&buffer), ""));

        buffer.clear();
    }
}

fn main() {
    if let Err(e) = repl("user> ") {
        panic!("{e}")
    }
}
