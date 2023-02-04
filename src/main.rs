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
    fn must_bool(self) -> Result<bool, &'static str> {
        return match self {
            Expression::Boolean(x) => Ok(x),
            _ => Err("not a boolean"),
        };
    }

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
struct Environment(HashMap<String, Expression>);

impl Environment {
    fn new() -> Environment {
        Environment(HashMap::new())
    }

    fn new_with(self, k: String, v: Expression) -> Self {
        let mut env = self.0;
        env.insert(k, v);
        return Environment(env);
    }
}

fn get_arithmetic_op(op: String) -> Result<Box<dyn Fn(f64, f64) -> f64>, &'static str> {
    match op.as_str() {
        "+" => Ok(Box::new(|x, y| x + y)),
        "-" => Ok(Box::new(|x, y| x - y)),
        "*" => Ok(Box::new(|x, y| x * y)),
        "/" => Ok(Box::new(|x, y| x / y)),
        _ => Err("unknown operator"),
    }
}

fn get_conditional_op(op: String) -> Result<Box<dyn Fn(f64, f64) -> bool>, &'static str> {
    match op.as_str() {
        "=" => Ok(Box::new(|x, y| x == y)),
        "<" => Ok(Box::new(|x, y| x < y)),
        ">" => Ok(Box::new(|x, y| x > y)),
        _ => Err("unknown operator"),
    }
}

// Note, only returns binary logical operators.
fn get_logical_op(op: String) -> Result<Box<dyn Fn(bool, bool) -> bool>, &'static str> {
    match op.as_str() {
        "and" => Ok(Box::new(|x, y| x && y)),
        "or" => Ok(Box::new(|x, y| x || y)),
        _ => Err("unknown operator"),
    }
}

/// Evaluate an expression in the context of an environment returning a new expression and a new environment.
fn evaluate(
    expr: &Expression,
    env: &Environment,
) -> Result<(Expression, Environment), &'static str> {
    return match expr {
        // Boolean expressions evaluate to themselves.
        Expression::Boolean(_) => Ok((expr.clone(), env.clone())),
        // String expressions evaluate to string literals if they are double
        // quoted, or otherwise to their binding in the environment.
        Expression::String(ref x) => {
            if x.starts_with("\"") && x.ends_with("\"") {
                let mut trim = x.trim_start_matches("\"");
                trim = trim.trim_end_matches("\"");
                return Ok((Expression::String(trim.to_string()), env.clone()));
            }
            match env.0.get(x) {
                Some(y) => Ok((y.clone(), env.clone())),
                None => {
                    println!("{:?}", env);
                    Err("symbol not found in environment")
                }
            }
        }
        // Numeric expressions evaluate to themselves.
        Expression::Number(_) => Ok((expr.clone(), env.clone())),
        Expression::List(ref x) => {
            if x.len() == 0 {
                return Ok((expr.clone(), env.clone()));
            }

            let first = x.get(0).unwrap();
            return match first {
                Expression::String(y) => match y.as_str() {
                    "define" => {
                        let id = x.get(1).unwrap().clone().must_string()?;
                        let expr = evaluate(&x.get(2).unwrap(), &env)?;
                        let env = env.clone().new_with(id, expr.0);
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
                                env: env.clone().0,
                            },
                            env.clone(),
                        ))
                    }
                    // Evaluate an arithmetic expression.
                    "+" | "-" | "*" | "/" => {
                        let op = get_arithmetic_op(y.to_string())?;
                        let (lhs, _) = evaluate(&x.get(1).unwrap(), &env)?;
                        let (rhs, _) = evaluate(&x.get(2).unwrap(), &env)?;
                        let result = match (lhs, rhs) {
                            (Expression::Number(x), Expression::Number(y)) => {
                                Expression::Number(op(x, y))
                            }
                            (Expression::String(x), Expression::Number(y)) => {
                                let lhs_from_env = match env.0.get(&x) {
                                    Some(x) => x,
                                    None => return Err("nothing bound to variable"),
                                };
                                let lhs_from_env = lhs_from_env.clone().must_number()?;
                                Expression::Number(op(lhs_from_env, y))
                            }
                            (Expression::Number(x), Expression::String(y)) => {
                                let rhs_from_env = match env.0.get(&y) {
                                    Some(y) => y,
                                    None => return Err("nothing bound to variable"),
                                };
                                let rhs_from_env = rhs_from_env.clone().must_number()?;
                                Expression::Number(op(x, rhs_from_env))
                            }
                            (Expression::String(x), Expression::String(y)) => {
                                let lhs_env = env.0.get(&x).unwrap().clone().must_number()?;
                                let rhs_env = env.0.get(&y).unwrap().clone().must_number()?;
                                Expression::Number(op(lhs_env, rhs_env))
                            }
                            _ => return Err("can only add numbers"),
                        };
                        return Ok((result, env.clone()));
                    }
                    // Evaluate a conditional expression.
                    "=" | "<" | ">" => {
                        let op = get_conditional_op(y.to_string())?;
                        let (lhs, _) = evaluate(&x.get(1).unwrap(), env)?;
                        let (rhs, _) = evaluate(&x.get(2).unwrap(), &env)?;
                        let result = match (lhs, rhs) {
                            (Expression::Number(x), Expression::Number(y)) => {
                                Expression::Boolean(op(x, y))
                            }
                            (Expression::String(x), Expression::Number(y)) => {
                                let lhs_from_env = match env.0.get(&x) {
                                    Some(x) => x,
                                    None => return Err("nothing bound to variable"),
                                };
                                let lhs_from_env = lhs_from_env.clone().must_number()?;
                                Expression::Boolean(op(lhs_from_env, y))
                            }
                            (Expression::Number(x), Expression::String(y)) => {
                                let rhs_from_env = match env.0.get(&y) {
                                    Some(y) => y,
                                    None => return Err("nothing bound to variable"),
                                };
                                let rhs_from_env = rhs_from_env.clone().must_number()?;
                                Expression::Boolean(op(x, rhs_from_env))
                            }
                            (Expression::String(x), Expression::String(y)) => {
                                let lhs_env = env.0.get(&x).unwrap().clone().must_number()?;
                                let rhs_env = env.0.get(&y).unwrap().clone().must_number()?;
                                Expression::Boolean(op(lhs_env, rhs_env))
                            }
                            _ => return Err("can only add numbers"),
                        };
                        return Ok((result, env.clone()));
                    }
                    // Evaluate a logical operator.
                    "and" | "or" | "not" => {
                        // Handle unary `not` operator.
                        if y == "not" {
                            let (lhs, _) = evaluate(&x.get(1).unwrap(), &env)?;
                            return match lhs {
                                Expression::Boolean(b) => {
                                    Ok((Expression::Boolean(!b), env.clone()))
                                }
                                _ => Err("cannot apply logical operator to non-boolean expression"),
                            };
                        }
                        // Handle binary logical operators.
                        let op = get_logical_op(y.to_string())?;
                        let (lhs, _) = evaluate(&x.get(1).unwrap(), &env)?;
                        let (rhs, _) = evaluate(&x.get(2).unwrap(), &env)?;
                        let result = match (lhs, rhs) {
                            (Expression::Boolean(x), Expression::Boolean(y)) => {
                                Expression::Boolean(op(x, y))
                            }
                            (Expression::String(x), Expression::Boolean(y)) => {
                                let lhs_from_env = match env.0.get(&x) {
                                    Some(x) => x,
                                    None => return Err("nothing bound to variable"),
                                };
                                let lhs_from_env = lhs_from_env.clone().must_bool()?;
                                Expression::Boolean(op(lhs_from_env, y))
                            }
                            (Expression::Boolean(x), Expression::String(y)) => {
                                let rhs_from_env = match env.0.get(&y) {
                                    Some(y) => y,
                                    None => return Err("nothing bound to variable"),
                                };
                                let rhs_from_env = rhs_from_env.clone().must_bool()?;
                                Expression::Boolean(op(x, rhs_from_env))
                            }
                            (Expression::String(x), Expression::String(y)) => {
                                let lhs_env = env.0.get(&x).unwrap().clone().must_bool()?;
                                let rhs_env = env.0.get(&y).unwrap().clone().must_bool()?;
                                Expression::Boolean(op(lhs_env, rhs_env))
                            }
                            _ => return Err("can only add numbers"),
                        };
                        return Ok((result, env.clone()));
                    }
                    // Evaluate a conditional `if` expression.
                    "if" => {
                        let (test, _) = evaluate(&x.get(1).unwrap(), &env)?;
                        match test.must_bool() {
                            Ok(b) => {
                                if b {
                                    evaluate(&x.get(2).unwrap(), &env)
                                } else {
                                    evaluate(&x.get(3).unwrap(), &env)
                                }
                            }
                            Err(_) => Err("test must be a boolean expression"),
                        }
                    }
                    // Evaluate a user defined procedure from the environment.
                    symbol => {
                        // Look up the symbol in the current environment.
                        let v_env = env.0.get(symbol);
                        match v_env {
                            None => return Err("symbol not found"),
                            Some(y) => {
                                match y.clone() {
                                    Expression::Lambda {
                                        formals,
                                        body,
                                        env: mut l_env,
                                    } => {
                                        // Populate environment with formal parameters by
                                        // evaluating each argument.
                                        for (i, f) in formals.iter().enumerate() {
                                            let (eval, _) = evaluate(&x.get(1 + i).unwrap(), &env)?;
                                            l_env.insert(f.clone().must_string()?, eval);
                                        }
                                        // Evaluate the body of the expression.
                                        return evaluate(&body, &Environment(l_env));
                                    }
                                    _ => Err("symbol is not a procedure"),
                                }
                            }
                        }
                    }
                },
                Expression::List(_) => {
                    let (expr, env) = evaluate(&first, &env).unwrap();
                    let mut xp = x.clone();
                    xp[0] = expr;
                    evaluate(&Expression::List(xp), &env)
                }
                Expression::Lambda {
                    formals,
                    body,
                    env: eenv,
                } => {
                    let mut l_env = eenv.clone();
                    // Populate environment with formal parameters by
                    // evaluating each argument.
                    for (i, f) in formals.iter().enumerate() {
                        let arg = match x.get(1 + i) {
                            Some(a) => a.clone(),
                            None => return Err("not enough arguments"),
                        };
                        let (eval, _) = evaluate(&arg, &env)?;
                        l_env.insert(f.clone().must_string()?, eval);
                    }
                    // Evaluate the body of the expression.
                    let bdy = &**body;
                    evaluate(&bdy, &Environment(l_env))
                }
                _ => {
                    println!("this is unimplemented: {:?}", x);
                    return Err("unimplemented");
                }
            };
        }
        Expression::Lambda {
            formals: _,
            body: _,
            env: _,
        } => Ok((expr.clone(), env.clone())),
    };
}

fn main() {
    let mut env = Environment::new();

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

        match evaluate(&input, &env) {
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
        let env = Environment::new();
        let expr = Expression::List(vec![]);

        let (result, new_env) = evaluate(&expr, &env).unwrap();

        assert_eq!(result, Expression::List(vec![]));
        assert!(new_env.0.is_empty())
    }

    #[test]
    fn literal_expression() {
        let env = Environment::new();
        let expr = Expression::List(vec![]);

        let test_cases: Vec<TestCase> = vec![
            TestCase {
                expr: Expression::Boolean(true),
                expr_env: Environment::new(),
                result: Expression::Boolean(true),
                result_env: Environment::new(),
            },
            TestCase {
                expr: Expression::String("\"hello world\"".to_string()),
                expr_env: Environment::new(),
                result: Expression::String("hello world".to_string()),
                result_env: Environment::new(),
            },
            TestCase {
                expr: Expression::Number(3.14),
                expr_env: Environment::new(),
                result: Expression::Number(3.14),
                result_env: Environment::new(),
            },
            TestCase {
                expr: Expression::String("square".to_string()),
                expr_env: Environment(HashMap::from([(
                    "square".to_string(),
                    Expression::Lambda {
                        formals: vec![],
                        body: Box::new(Expression::List(vec![
                            Expression::String("*".to_string()),
                            Expression::String("x".to_string()),
                            Expression::String("x".to_string()),
                        ])),
                        env: HashMap::new(),
                    },
                )])),
                result: Expression::Lambda {
                    formals: vec![],
                    body: Box::new(Expression::List(vec![
                        Expression::String("*".to_string()),
                        Expression::String("x".to_string()),
                        Expression::String("x".to_string()),
                    ])),
                    env: HashMap::new(),
                },
                result_env: Environment(HashMap::from([(
                    "square".to_string(),
                    Expression::Lambda {
                        formals: vec![],
                        body: Box::new(Expression::List(vec![
                            Expression::String("*".to_string()),
                            Expression::String("x".to_string()),
                            Expression::String("x".to_string()),
                        ])),
                        env: HashMap::new(),
                    },
                )])),
            },
        ];

        for case in test_cases.iter() {
            let (result, result_env) = evaluate(&case.expr, &case.expr_env).unwrap();
            assert_eq!(result, case.result);
            assert_eq!(result_env, case.result_env);
        }
    }

    #[test]
    fn basic_expression() {
        let test_cases: Vec<TestCase> = vec![
            // Arithmetic expressions.
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
                expr_env: Environment(HashMap::from([("x".to_string(), Expression::Number(3.0))])),
                result: Expression::Number(5.0),
                result_env: Environment(HashMap::from([(
                    "x".to_string(),
                    Expression::Number(3.0),
                )])),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("+".to_string()),
                    Expression::Number(2.0),
                    Expression::String("y".to_string()),
                ]),
                expr_env: Environment(HashMap::from([("y".to_string(), Expression::Number(3.0))])),
                result: Expression::Number(5.0),
                result_env: Environment(HashMap::from([(
                    "y".to_string(),
                    Expression::Number(3.0),
                )])),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("+".to_string()),
                    Expression::String("x".to_string()),
                    Expression::String("y".to_string()),
                ]),
                expr_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Number(2.0)),
                    ("y".to_string(), Expression::Number(3.0)),
                ])),
                result: Expression::Number(5.0),
                result_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Number(2.0)),
                    ("y".to_string(), Expression::Number(3.0)),
                ])),
            },
            // Conditional expressions.
            // TODO: Add test cases for literal operands.
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("=".to_string()),
                    Expression::String("x".to_string()),
                    Expression::String("y".to_string()),
                ]),
                expr_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Number(3.0)),
                    ("y".to_string(), Expression::Number(3.0)),
                ])),
                result: Expression::Boolean(true),
                result_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Number(3.0)),
                    ("y".to_string(), Expression::Number(3.0)),
                ])),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("=".to_string()),
                    Expression::String("x".to_string()),
                    Expression::String("y".to_string()),
                ]),
                expr_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Number(1.0)),
                    ("y".to_string(), Expression::Number(3.0)),
                ])),
                result: Expression::Boolean(false),
                result_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Number(1.0)),
                    ("y".to_string(), Expression::Number(3.0)),
                ])),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("<".to_string()),
                    Expression::String("x".to_string()),
                    Expression::String("y".to_string()),
                ]),
                expr_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Number(1.0)),
                    ("y".to_string(), Expression::Number(3.0)),
                ])),
                result: Expression::Boolean(true),
                result_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Number(1.0)),
                    ("y".to_string(), Expression::Number(3.0)),
                ])),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("<".to_string()),
                    Expression::String("x".to_string()),
                    Expression::String("y".to_string()),
                ]),
                expr_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Number(3.0)),
                    ("y".to_string(), Expression::Number(1.0)),
                ])),
                result: Expression::Boolean(false),
                result_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Number(3.0)),
                    ("y".to_string(), Expression::Number(1.0)),
                ])),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String(">".to_string()),
                    Expression::String("x".to_string()),
                    Expression::String("y".to_string()),
                ]),
                expr_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Number(1.0)),
                    ("y".to_string(), Expression::Number(3.0)),
                ])),
                result: Expression::Boolean(false),
                result_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Number(1.0)),
                    ("y".to_string(), Expression::Number(3.0)),
                ])),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String(">".to_string()),
                    Expression::String("x".to_string()),
                    Expression::String("y".to_string()),
                ]),
                expr_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Number(3.0)),
                    ("y".to_string(), Expression::Number(1.0)),
                ])),
                result: Expression::Boolean(true),
                result_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Number(3.0)),
                    ("y".to_string(), Expression::Number(1.0)),
                ])),
            },
            // Logical expressions.
            // TODO: Add test cases for literal operands.
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("not".to_string()),
                    Expression::String("x".to_string()),
                ]),
                expr_env: Environment(HashMap::from([(
                    "x".to_string(),
                    Expression::Boolean(true),
                )])),
                result: Expression::Boolean(false),
                result_env: Environment(HashMap::from([(
                    "x".to_string(),
                    Expression::Boolean(true),
                )])),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("not".to_string()),
                    Expression::String("x".to_string()),
                ]),
                expr_env: Environment(HashMap::from([(
                    "x".to_string(),
                    Expression::Boolean(false),
                )])),
                result: Expression::Boolean(true),
                result_env: Environment(HashMap::from([(
                    "x".to_string(),
                    Expression::Boolean(false),
                )])),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("and".to_string()),
                    Expression::String("x".to_string()),
                    Expression::String("y".to_string()),
                ]),
                expr_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Boolean(true)),
                    ("y".to_string(), Expression::Boolean(false)),
                ])),
                result: Expression::Boolean(false),
                result_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Boolean(true)),
                    ("y".to_string(), Expression::Boolean(false)),
                ])),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("and".to_string()),
                    Expression::String("x".to_string()),
                    Expression::String("y".to_string()),
                ]),
                expr_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Boolean(false)),
                    ("y".to_string(), Expression::Boolean(true)),
                ])),
                result: Expression::Boolean(false),
                result_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Boolean(false)),
                    ("y".to_string(), Expression::Boolean(true)),
                ])),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("and".to_string()),
                    Expression::String("x".to_string()),
                    Expression::String("y".to_string()),
                ]),
                expr_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Boolean(true)),
                    ("y".to_string(), Expression::Boolean(true)),
                ])),
                result: Expression::Boolean(true),
                result_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Boolean(true)),
                    ("y".to_string(), Expression::Boolean(true)),
                ])),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("or".to_string()),
                    Expression::String("x".to_string()),
                    Expression::String("y".to_string()),
                ]),
                expr_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Boolean(true)),
                    ("y".to_string(), Expression::Boolean(false)),
                ])),
                result: Expression::Boolean(true),
                result_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Boolean(true)),
                    ("y".to_string(), Expression::Boolean(false)),
                ])),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("or".to_string()),
                    Expression::String("x".to_string()),
                    Expression::String("y".to_string()),
                ]),
                expr_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Boolean(false)),
                    ("y".to_string(), Expression::Boolean(true)),
                ])),
                result: Expression::Boolean(true),
                result_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Boolean(false)),
                    ("y".to_string(), Expression::Boolean(true)),
                ])),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("or".to_string()),
                    Expression::String("x".to_string()),
                    Expression::String("y".to_string()),
                ]),
                expr_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Boolean(true)),
                    ("y".to_string(), Expression::Boolean(true)),
                ])),
                result: Expression::Boolean(true),
                result_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Boolean(true)),
                    ("y".to_string(), Expression::Boolean(true)),
                ])),
            },
            // `if` expressions.
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("if".to_string()),
                    Expression::String("x".to_string()),
                    Expression::String("foo".to_string()),
                    Expression::String("bar".to_string()),
                ]),
                expr_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Boolean(true)),
                    ("foo".to_string(), Expression::Number(1.0)),
                ])),
                result: Expression::Number(1.0),
                result_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Boolean(true)),
                    ("foo".to_string(), Expression::Number(1.0)),
                ])),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("if".to_string()),
                    Expression::String("x".to_string()),
                    Expression::String("foo".to_string()),
                    Expression::List(vec![
                        Expression::String("+".to_string()),
                        Expression::String("y".to_string()),
                        Expression::String("z".to_string()),
                    ]),
                ]),
                expr_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Boolean(false)),
                    ("y".to_string(), Expression::Number(2.0)),
                    ("z".to_string(), Expression::Number(5.0)),
                ])),
                result: Expression::Number(7.0),
                result_env: Environment(HashMap::from([
                    ("x".to_string(), Expression::Boolean(false)),
                    ("y".to_string(), Expression::Number(2.0)),
                    ("z".to_string(), Expression::Number(5.0)),
                ])),
            },
        ];

        for case in test_cases.iter() {
            let (result, result_env) = evaluate(&case.expr, &case.expr_env).unwrap();
            assert_eq!(result, case.result);
            assert_eq!(result_env, case.result_env);
        }
    }

    #[test]
    fn complex_expression() {
        let test_cases: Vec<TestCase> = vec![
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("+".to_string()),
                    Expression::List(vec![
                        Expression::String("+".to_string()),
                        Expression::Number(2.0),
                        Expression::Number(3.0),
                    ]),
                    Expression::Number(3.0),
                ]),
                expr_env: Environment::new(),
                result: Expression::Number(8.0),
                result_env: Environment::new(),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("+".to_string()),
                    Expression::List(vec![
                        Expression::String("-".to_string()),
                        Expression::List(vec![
                            Expression::String("*".to_string()),
                            Expression::Number(2.0),
                            Expression::Number(5.0),
                        ]),
                        Expression::List(vec![
                            Expression::String("+".to_string()),
                            Expression::Number(2.0),
                            Expression::Number(3.0),
                        ]),
                    ]),
                    Expression::Number(3.0),
                ]),
                expr_env: Environment::new(),
                result: Expression::Number(8.0),
                result_env: Environment::new(),
            },
        ];

        for case in test_cases.iter() {
            let (result, result_env) = evaluate(&case.expr, &case.expr_env).unwrap();
            assert_eq!(result, case.result);
            assert_eq!(result_env, case.result_env);
        }
    }

    #[test]
    fn expression_error() {
        // TODO: Result should be optional as nothing is actually returned.
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
            // Symbol not found in environment.
            TestCase {
                expr: Expression::String("x".to_string()),
                expr_env: Environment::new(),
                result: Expression::Number(3.14),
                result_env: Environment::new(),
            },
            // Can't evaluate symbols not of type lambda.
            TestCase {
                expr: Expression::List(vec![Expression::String("x".to_string())]),
                expr_env: Environment(HashMap::from([("x".to_string(), Expression::Number(3.0))])),
                result: Expression::Number(3.14),
                result_env: Environment::new(),
            },
            TestCase {
                expr: Expression::List(vec![Expression::String("x".to_string())]),
                expr_env: Environment(HashMap::from([(
                    "x".to_string(),
                    Expression::String("hello".to_string()),
                )])),
                result: Expression::Number(3.14),
                result_env: Environment::new(),
            },
        ];

        for case in test_cases.iter() {
            match evaluate(&case.expr, &case.expr_env) {
                Ok(_) => panic!("expected error"),
                _ => (),
            }
        }
    }

    #[test]
    fn basic_variable_definition() {
        let test_cases: Vec<TestCase> = vec![
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("define".to_string()),
                    Expression::String("pi".to_string()),
                    Expression::Number(3.14),
                ]),
                expr_env: Environment::new(),
                result: Expression::Boolean(true),
                result_env: Environment(HashMap::from([(
                    "pi".to_string(),
                    Expression::Number(3.14),
                )])),
            },
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("define".to_string()),
                    Expression::String("pi".to_string()),
                    Expression::String("\"3.14\"".to_string()),
                ]),
                expr_env: Environment::new(),
                result: Expression::Boolean(true),
                result_env: Environment(HashMap::from([(
                    "pi".to_string(),
                    Expression::String("3.14".to_string()),
                )])),
            },
        ];

        for case in test_cases.iter() {
            let (result, result_env) = evaluate(&case.expr, &case.expr_env).unwrap();
            assert_eq!(result, case.result);
            assert_eq!(result_env, case.result_env);
        }
    }

    #[test]
    fn basic_procedures() {
        let test_cases: Vec<TestCase> = vec![
            // Anonymous lambda as an expression.
            TestCase {
                expr: Expression::List(vec![
                    Expression::List(vec![
                        Expression::String("lambda".to_string()),
                        Expression::List(vec![Expression::String("x".to_string())]),
                        Expression::List(vec![
                            Expression::String("*".to_string()),
                            Expression::String("x".to_string()),
                            Expression::String("x".to_string()),
                        ]),
                    ]),
                    Expression::Number(25.0),
                ]),
                expr_env: Environment::new(),
                result: Expression::Number(625.0),
                result_env: Environment(HashMap::from([(
                    "x".to_string(),
                    Expression::Number(25.0),
                )])),
            },
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
                result_env: Environment(HashMap::from([(
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
                )])),
            },
            // Lambda evaluation, empty environment.
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("square".to_string()),
                    Expression::Number(2.0),
                ]),
                expr_env: Environment(HashMap::from([(
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
                )])),
                result: Expression::Number(4.0),
                result_env: Environment(HashMap::from([(
                    "x".to_string(),
                    Expression::Number(2.0),
                )])),
            },
            // Lambda evaluation, expression argument.
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("square".to_string()),
                    Expression::List(vec![
                        Expression::String("+".to_string()),
                        Expression::Number(3.0),
                        Expression::Number(2.0),
                    ]),
                ]),
                expr_env: Environment(HashMap::from([(
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
                )])),
                result: Expression::Number(25.0),
                result_env: Environment(HashMap::from([(
                    "x".to_string(),
                    Expression::Number(5.0),
                )])),
            },
            // Lambda evaluation, argument from environment.
            TestCase {
                expr: Expression::List(vec![
                    Expression::String("square".to_string()),
                    Expression::String("x".to_string()),
                ]),
                expr_env: Environment(HashMap::from([
                    (
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
                    ),
                    ("x".to_string(), Expression::Number(7.0)),
                ])),
                result: Expression::Number(49.0),
                result_env: Environment(HashMap::from([(
                    "x".to_string(),
                    Expression::Number(7.0),
                )])),
            },
            // Lambda evaluation, no formals.
            TestCase {
                expr: Expression::List(vec![Expression::String("square".to_string())]),
                expr_env: Environment(HashMap::from([(
                    "square".to_string(),
                    Expression::Lambda {
                        formals: vec![],
                        body: Box::new(Expression::List(vec![
                            Expression::String("*".to_string()),
                            Expression::String("x".to_string()),
                            Expression::String("x".to_string()),
                        ])),
                        env: HashMap::from([("x".to_string(), Expression::Number(7.0))]),
                    },
                )])),
                result: Expression::Number(49.0),
                result_env: Environment(HashMap::from([(
                    "x".to_string(),
                    Expression::Number(7.0),
                )])),
            },
        ];

        for case in test_cases.iter() {
            let (result, result_env) = evaluate(&case.expr, &case.expr_env).unwrap();
            assert_eq!(result, case.result);
            assert_eq!(result_env, case.result_env);
        }
    }
}
