use serde::Deserialize;
use std::collections::HashMap;
use std::io;
use std::io::Write;

#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(untagged)]
enum Expression {
    Boolean(bool),
    String(String),
    Number(f64),
    List(Vec<Expression>),
    Lambda {
        formals: Vec<Expression>,
        body: Box<Expression>,
        env: HashMap<String, Expression>,
    },
}

impl Expression {
    fn must_string(self) -> Result<String, &'static str> {
        match self {
            Expression::String(s) => return Ok(s),
            _ => Err("not a string"),
        }
    }

    fn must_number(self) -> Result<f64, &'static str> {
        return match self {
            Expression::Number(x) => Ok(x),
            _ => Err("not a number"),
        };
    }

    fn must_list(self) -> Result<Vec<Expression>, &'static str> {
        match self {
            Expression::List(x) => return Ok(x),
            _ => Err("not a list"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct Environment {
    env: HashMap<String, Expression>,
}

impl Environment {
    fn new() -> Environment {
        Environment {
            env: HashMap::new(),
        }
    }

    fn new_with(self, k: String, v: Expression) -> Self {
        let mut env = self.env;
        env.insert(k, v);
        return Environment { env };
    }
}

fn get_op(op: String) -> Result<Box<dyn Fn(f64, f64) -> f64>, &'static str> {
    match op.as_str() {
        "+" => Ok(Box::new(|x, y| x + y)),
        "-" => Ok(Box::new(|x, y| x - y)),
        "*" => Ok(Box::new(|x, y| x * y)),
        "/" => Ok(Box::new(|x, y| x / y)),
        _ => Err("unknown operator"),
    }
}

fn evaluate(expr: Expression, env: Environment) -> Result<(Expression, Environment), &'static str> {
    return match expr {
        Expression::Boolean(_) => Ok((expr, env)),
        Expression::String(ref x) => {
            // Actual strings are double quoted.
            if x.starts_with("\"") && x.ends_with("\"") {
                return Ok((expr, env));
            }

            // Not a string literal, try and find symbol in environment.
            match env.env.get(x) {
                Some(y) => Ok((y.clone(), env)),
                None => Ok((expr, env)),
            }
        }
        Expression::Number(_) => Ok((expr, env)),
        Expression::List(ref x) => {
            if x.len() == 0 {
                return Ok((expr, env));
            }

            let first = x.get(0).unwrap();
            return match first {
                Expression::Boolean(_) => Err("blah"),
                Expression::String(y) => match y.as_str() {
                    "define" => {
                        let id = x.get(1).unwrap().clone().must_string()?;
                        let expr = evaluate(x.get(2).unwrap().clone(), env.clone()).unwrap();
                        let env = env.new_with(id, expr.0);
                        Ok((Expression::Boolean(true), env))
                    }
                    "lambda" => {
                        let formals = x.get(1).unwrap().clone().must_list()?;
                        let body = x.get(2).unwrap();
                        let env = env;
                        Ok((
                            Expression::Lambda {
                                formals,
                                body: Box::new(body.clone()),
                                env: env.clone().env,
                            },
                            env,
                        ))
                    }
                    "+" | "-" | "*" | "/" => {
                        let op = get_op(y.to_string())?;
                        let lhs = x.get(1).unwrap();
                        let rhs = x.get(2).unwrap();
                        let result = match (lhs, rhs) {
                            (Expression::Number(x), Expression::Number(y)) => {
                                Expression::Number(op(*x, *y))
                            }
                            (Expression::String(x), Expression::Number(y)) => {
                                let lhs_from_env = match env.env.get(x) {
                                    Some(x) => x,
                                    None => return Err("nothing bound to variable"),
                                };
                                let lhs_from_env = lhs_from_env.clone().must_number()?;
                                Expression::Number(op(lhs_from_env, *y))
                            }
                            (Expression::Number(x), Expression::String(y)) => {
                                let rhs_from_env = match env.env.get(y) {
                                    Some(y) => y,
                                    None => return Err("nothing bound to variable"),
                                };
                                let rhs_from_env = rhs_from_env.clone().must_number()?;
                                Expression::Number(op(*x, rhs_from_env))
                            }
                            (Expression::String(x), Expression::String(y)) => {
                                let lhs_env = env.env.get(x).unwrap().clone().must_number()?;
                                let rhs_env = env.env.get(y).unwrap().clone().must_number()?;
                                Expression::Number(op(lhs_env, rhs_env))
                            }
                            _ => return Err("can only add numbers"),
                        };
                        return Ok((result, env));
                    }
                    v => {
                        // Look up the symbol in the current environment.
                        let v_env = env.env.get(v);
                        match v_env {
                            None => return Err("whoops failed to find in environment"),
                            Some(y) => {
                                match y.clone() {
                                    Expression::Lambda {
                                        formals,
                                        body,
                                        mut env,
                                    } => {
                                        // Populate environment with formal parameters.
                                        for (i, f) in formals.iter().enumerate() {
                                            env.insert(
                                                f.clone().must_string()?,
                                                x.get(1 + i).unwrap().clone(),
                                            );
                                        }
                                        // Evaluate the body of the expression.
                                        return evaluate(*body, Environment { env });
                                    }
                                    _ => Err("cannot call value"),
                                }
                            }
                        }
                    }
                },
                Expression::Number(_) => Err("whoops"),
                Expression::List(_) => Err("whoops"),
                Expression::Lambda { .. } => Err("whoops"),
            };
        }
        Expression::Lambda { formals, body, env } => {
            return Err("whoops");
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

        match evaluate(input, env.clone()) {
            Ok((expr, new_env)) => {
                println!("{:?}", expr);
                env = new_env;
            }
            Err(e) => {
                println!("{e}")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{evaluate, Environment, Expression};
    use std::collections::HashMap;

    struct TestCase {
        expr: Expression,
        expr_env: Environment,
        result: Expression,
        result_env: Environment,
    }

    #[test]
    fn empty_expression() {
        let env = Environment {
            env: HashMap::new(),
        };
        let expr = Expression::List(vec![]);

        let (result, new_env) = evaluate(expr, env).unwrap();

        assert_eq!(result, Expression::List(vec![]));
        assert!(new_env.env.is_empty())
    }

    #[test]
    fn basic_expression() {
        let test_cases: Vec<TestCase> = vec![
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("+".to_string()),
                    Expression::Number(1.0),
                    Expression::Number(2.14),
                ]),
                expr_env: Environment::new(),
                result: Expression::Number(3.14),
                result_env: Environment::new(),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("-".to_string()),
                    Expression::Number(3.14),
                    Expression::Number(1.0),
                ]),
                expr_env: Environment::new(),
                result: Expression::Number(2.14),
                result_env: Environment::new(),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("*".to_string()),
                    Expression::Number(3.14),
                    Expression::Number(2.0),
                ]),
                expr_env: Environment::new(),
                result: Expression::Number(6.28),
                result_env: Environment::new(),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("/".to_string()),
                    Expression::Number(6.28),
                    Expression::Number(2.0),
                ]),
                expr_env: Environment::new(),
                result: Expression::Number(3.14),
                result_env: Environment::new(),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("+".to_string()),
                    Expression::String("x".to_string()),
                    Expression::Number(2.0),
                ]),
                expr_env: Environment {
                    env: HashMap::from([("x".to_string(), Expression::Number(3.0))]),
                },
                result: Expression::Number(5.0),
                result_env: Environment {
                    env: HashMap::from([("x".to_string(), Expression::Number(3.0))]),
                },
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("+".to_string()),
                    Expression::Number(2.0),
                    Expression::String("y".to_string()),
                ]),
                expr_env: Environment {
                    env: HashMap::from([("y".to_string(), Expression::Number(3.0))]),
                },
                result: Expression::Number(5.0),
                result_env: Environment {
                    env: HashMap::from([("y".to_string(), Expression::Number(3.0))]),
                },
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("+".to_string()),
                    Expression::String("x".to_string()),
                    Expression::String("y".to_string()),
                ]),
                expr_env: Environment {
                    env: HashMap::from([
                        ("x".to_string(), Expression::Number(2.0)),
                        ("y".to_string(), Expression::Number(3.0)),
                    ]),
                },
                result: Expression::Number(5.0),
                result_env: Environment {
                    env: HashMap::from([
                        ("x".to_string(), Expression::Number(2.0)),
                        ("y".to_string(), Expression::Number(3.0)),
                    ]),
                },
            },
        ];

        for case in test_cases.iter() {
            let (result, result_env) = evaluate(case.expr.clone(), case.expr_env.clone()).unwrap();
            assert_eq!(result, case.result);
            assert_eq!(result_env, case.result_env);
        }
    }

    #[test]
    fn expression_error() {
        let test_cases: Vec<TestCase> = vec![
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("+".to_string()),
                    Expression::String("1.0".to_string()),
                    Expression::Number(2.14),
                ]),
                expr_env: Environment::new(),
                result: Expression::Number(3.14),
                result_env: Environment::new(),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("*".to_string()),
                    Expression::Number(1.0),
                    Expression::String("2.14".to_string()),
                ]),
                expr_env: Environment::new(),
                result: Expression::Number(3.14),
                result_env: Environment::new(),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("&".to_string()),
                    Expression::Number(1.0),
                    Expression::Number(2.14),
                ]),
                expr_env: Environment::new(),
                result: Expression::Number(3.14),
                result_env: Environment::new(),
            },
            // Can't evaluate non-lambda names.
            TestCase {
                expr: Expression::List(vec![Expression::String("x".to_string())]),
                expr_env: Environment {
                    env: HashMap::from([("x".to_string(), Expression::Number(3.0))]),
                },
                result: Expression::Number(3.14),
                result_env: Environment::new(),
            },
            TestCase {
                expr: Expression::List(vec![Expression::String("x".to_string())]),
                expr_env: Environment {
                    env: HashMap::from([(
                        "x".to_string(),
                        Expression::String("hello".to_string()),
                    )]),
                },
                result: Expression::Number(3.14),
                result_env: Environment::new(),
            },
        ];

        for case in test_cases.iter() {
            match evaluate(case.expr.clone(), case.expr_env.clone()) {
                Ok(_) => panic!("expected error"),
                _ => (),
            }
        }
    }

    #[test]
    fn basic_variable_definition() {
        let test_cases: Vec<TestCase> = vec![TestCase {
            expr: Expression::List(vec![
                Expression::String("define".to_string()),
                Expression::String("pi".to_string()),
                Expression::Number(3.14),
            ]),
            expr_env: Environment::new(),
            result: Expression::Boolean(true),
            result_env: Environment {
                env: HashMap::from([("pi".to_string(), Expression::Number(3.14))]),
            },
        }];

        for case in test_cases.iter() {
            let (result, result_env) = evaluate(case.expr.clone(), case.expr_env.clone()).unwrap();
            assert_eq!(result, case.result);
            assert_eq!(result_env, case.result_env);
        }
    }

    #[test]
    fn basic_procedures() {
        let test_cases: Vec<TestCase> = vec![
            // Lambda definition.
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("define".to_string()),
                    Expression::String("square".to_string()),
                    Expression::List(vec![
                        Expression::String("lambda".to_string()),
                        Expression::List(vec![Expression::String("x".to_string())]),
                        Expression::List(vec![
                            Expression::String("*".to_string()),
                            Expression::String("x".to_string()),
                            Expression::String("x".to_string()),
                        ]),
                    ]),
                ]),
                expr_env: Environment::new(),
                result: Expression::Boolean(true),
                result_env: Environment {
                    env: HashMap::from([(
                        "square".to_string(),
                        Expression::Lambda {
                            formals: vec![Expression::String("x".to_string())],
                            body: Box::new(Expression::List(vec![
                                Expression::String("*".to_string()),
                                Expression::String("x".to_string()),
                                Expression::String("x".to_string()),
                            ])),
                            env: HashMap::new(),
                        },
                    )]),
                },
            },
            // Lambda evaluation, empty environment.
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("square".to_string()),
                    Expression::Number(2.0),
                ]),
                expr_env: Environment {
                    env: HashMap::from([(
                        "square".to_string(),
                        Expression::Lambda {
                            formals: vec![Expression::String("x".to_string())],
                            body: Box::new(Expression::List(vec![
                                Expression::String("*".to_string()),
                                Expression::String("x".to_string()),
                                Expression::String("x".to_string()),
                            ])),
                            env: HashMap::new(),
                        },
                    )]),
                },
                result: Expression::Number(4.0),
                result_env: Environment {
                    env: HashMap::from([("x".to_string(), Expression::Number(2.0))]),
                },
            },
        ];

        for case in test_cases.iter() {
            let (result, result_env) = evaluate(case.expr.clone(), case.expr_env.clone()).unwrap();
            assert_eq!(result, case.result);
            assert_eq!(result_env, case.result_env);
        }
    }
}
