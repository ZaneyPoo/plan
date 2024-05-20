mod evaluator;
mod printer;
mod reader;

use evaluator::eval;
use printer::print;
use reader::read;

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

const DEFAULT_PROMPT: &str = ">>> ";
const HISTORY_FILE: &str = ".risphistory";

fn repl(prompt: &'static str) -> rustyline::Result<()> {
    let mut rl = DefaultEditor::new()?;

    if rl.load_history(HISTORY_FILE).is_err() {
        eprintln!("No previous history. Creating new file");
    }

    loop {
        let line = rl.readline(prompt);
        match line {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());
                println!("{}", print(eval(read(line.as_str()), "")));
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => {
                break;
            }
            Err(e) => return Err(e),
        }
    }

    let _ = rl.save_history(HISTORY_FILE);
    Ok(())
}

fn main() {
    if let Err(e) = repl(DEFAULT_PROMPT) {
        panic!("Error: {e}")
    }
}
