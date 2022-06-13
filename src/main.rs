use serde_json::Value;
use std::collections::HashMap;
use std::io::Write;
use std::{fmt, io};

enum Atom {
    String(String),
    Number(f64),
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Atom::String(x) => {
                write!(f, "{}\n", x)
            }
            Atom::Number(x) => {
                write!(f, "{}\n", x)
            }
        }
    }
}

fn main() {
    // REPL.
    let mut env = HashMap::<String, Atom>::new();
    loop {
        // Print prompt and flush to ensure it makes it to screen.
        print!("> ");
        io::stdout().flush().unwrap();

        // Read the user input.
        let mut buffer = String::new();
        let stdin = io::stdin();
        stdin.read_line(&mut buffer).unwrap();
        let input: Vec<Value> = match serde_json::from_str(&buffer) {
            Ok(v) => v,
            Err(_) => {
                print!("invalid input\n");
                continue;
            }
        };

        // Handle empty input.
        if input.len() == 0 {
            print!("{:?}\n", input);
            continue;
        }

        // Evaluate input.
        match &input[0] {
            Value::Null => {}
            Value::Bool(_) => {}
            Value::Number(_) => {}
            Value::String(x) => match x.as_str() {
                "define" => {
                    let symbol = match &input[1] {
                        Value::String(y) => y,
                        _ => {
                            print!("invalid input\n");
                            continue;
                        }
                    };
                    match &input[2] {
                        Value::Number(y) => {
                            env.insert(symbol.to_string(), Atom::Number(y.as_f64().unwrap()));
                        }
                        Value::String(y) => {
                            env.insert(symbol.to_string(), Atom::String(y.to_string()));
                        }
                        _ => {
                            print!("invalid input\n");
                            continue;
                        }
                    }
                }
                _ => {
                    // Check environment.
                    if env.get(x).is_some() {
                        // Found symbol in environment.
                        print!("{}", env.get(x).unwrap());
                        continue;
                    }
                    print!("unknown symbol\n");
                    continue;
                }
            },
            Value::Array(_) => {}
            Value::Object(_) => {}
        }

        print!("{:?}\n", input);
    }
}
