use std::io;
use std::io::Write;

fn main() {
    // REPL.
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut buffer = String::new();
        let stdin = io::stdin();
        stdin.read_line(&mut buffer).unwrap();
        print!("{buffer}");
    }
}
