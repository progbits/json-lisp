use serde::Deserialize;
use std::collections::HashMap;
use std::io;
use std::io::Write;

#[derive(Clone, Debug, Deserialize)]
#[serde(untagged)]
enum Expression {
    Boolean(bool),
    String(String),
    Number(f64),
    List(Vec<Expression>),
}

impl Expression {
    fn must_string(self) -> String {
        match self {
            Expression::Boolean(_) => {
                panic!("not a string")
            }
            Expression::String(s) => return s,
            Expression::Number(_) => {
                panic!("not a string")
            }
            Expression::List(_) => {
                panic!("not a string")
            }
        }
    }
}

#[derive(Clone, Debug)]
struct Environment {
    env: HashMap<String, Expression>,
}

impl Environment {
    fn new_with(self, k: String, v: Expression) -> Self {
        let mut env = self.env.clone();
        env.insert(k, v);
        return Environment { env };
    }
}

fn evaluate(expr: Expression, env: Environment) -> Result<(Expression, Environment), &'static str> {
    return match expr {
        Expression::Boolean(_) => Ok((expr, env)),
        Expression::String(ref x) => {
            // Actual strings are double quoted.
            if x.starts_with("\"") && x.ends_with("\"") {
                return Ok((expr.clone(), env.clone()));
            }

            // Not a string literal, try and find symbol in environment.
            match env.env.get(x) {
                Some(y) => Ok((y.clone(), env.clone())),
                None => Ok((expr.clone(), env.clone())),
            }
        }
        Expression::Number(_) => Ok((expr, env)),
        Expression::List(x) => {
            let first = x.get(0).unwrap();
            return match first {
                Expression::Boolean(_) => Err("blah"),
                Expression::String(y) => match y.as_str() {
                    "define" => {
                        let id = x.get(1).unwrap().clone().must_string();
                        let expr = evaluate(x.get(2).unwrap().clone(), env.clone()).unwrap();
                        let env = env.new_with(id, expr.0);
                        Ok((Expression::Boolean(true), env))
                    }
                    "+" => {
                        let lhs = x.get(1).unwrap();
                        let rhs = x.get(2).unwrap();
                        let result = match (lhs, rhs) {
                            (Expression::Number(x), Expression::Number(y)) => {
                                Expression::Number(x + y)
                            }
                            _ => {
                                panic!("can only add numbers")
                            }
                        };
                        return Ok((result, env));
                    }
                    _ => return evaluate(first.clone(), env),
                },
                Expression::Number(_) => Err("whoops"),
                Expression::List(_) => Err("whoops"),
            };
        }
    };
}

fn main() {
    let mut env = Environment {
        env: HashMap::<String, Expression>::new(),
    };

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        // Read the user input.
        let mut buffer = String::new();
        let stdin = io::stdin();
        stdin.read_line(&mut buffer).unwrap();
        let input: Expression = match serde_json::from_str(&buffer) {
            Ok(v) => v,
            Err(e) => {
                println!("invalid input: {e}\n");
                continue;
            }
        };

        let (expr, new_env) = evaluate(input, env.clone()).unwrap();
        println!("{:?}", expr);
        env = new_env;
    }
}
