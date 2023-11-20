use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{Expr, Node, Program, Stmt},
    environment::Environment,
    object::Object,
    token::Str,
};

use self::builtins::Builtins;

pub mod builtins;
#[cfg(test)]
mod test_evaluator;

const NULL: Object = Object::Null;
const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);

#[inline]
pub fn eval(node: &Node, env: &Rc<RefCell<Environment>>, builtins: &Builtins) -> Object {
    match *node {
        Node::Program(ref prog) => eval_program(&prog, env, builtins),
        Node::Expression(ref expr) => match **expr {
            Expr::IntegerLiteral { ref value } => return Object::Integer(value.1),
            Expr::Boolean { value } => return native_bool_to_boolean_object(value),
            Expr::Identifier { ref name } => eval_identifier(name, env, builtins),
            Expr::PrefixExpression {
                ref operator,
                ref right,
            } => {
                let val = eval(right, env, builtins);
                if is_error(&val) {
                    return val;
                }
                eval_prefix_expression(&operator, val)
            }
            Expr::InfixExpression {
                ref left,
                ref operator,
                ref right,
            } => {
                let left = eval(&left, env, builtins);
                if is_error(&left) {
                    return left;
                }
                let right = eval(&right, env, builtins);
                if is_error(&right) {
                    return right;
                }
                eval_infix_expression(&(**operator), left, right)
            }
            Expr::IfExpression {
                ref condition,
                ref consequence,
                ref alternative,
            } => {
                let cond = eval(condition, env, builtins);
                if is_error(&cond) {
                    return cond;
                }
                eval_if_expression(condition, consequence, alternative, env, builtins)
            }
            Expr::FunctionLiteral {
                ref parameters,
                ref body,
            } => Object::Function(parameters.clone(), body.clone(), env.clone()),
            Expr::CallExpression {
                ref function,
                ref arguments,
            } => {
                let function = eval(function, env, builtins);
                if is_error(&function) {
                    return function;
                }

                let args = eval_expressions(arguments, env, builtins);
                if args.len() == 1 && is_error(&args[0]) {
                    return args[0].clone();
                }

                apply_function(function, args, builtins)
            }
            Expr::StringLiteral { ref value } => Object::String(value.clone()),
            Expr::ArrayLiteral { ref elements } => {
                let elems = eval_expressions(elements, env, builtins);
                if elems.len() == 1 && is_error(&elems[0]) {
                    return elems[0].clone();
                }
                Object::Array(elems)
            }
            Expr::IndexExpression {
                ref left,
                ref index,
            } => {
                let left = eval(left, env, builtins);
                if is_error(&left) {
                    return left;
                }

                let index = eval(index, env, builtins);
                if is_error(&index) {
                    return index;
                }

                eval_index_expression(left, index)
            }
            _ => NULL,
        },
        Node::Statement(ref stmt) => match **stmt {
            Stmt::ExpressionStatement { ref expr } => eval(&expr, env, builtins),
            Stmt::BlockStatement { ref stmts } => eval_block_statements(&stmts, env, builtins),
            Stmt::ReturnStatement { ref value } => {
                let val = eval(&value, env, builtins);
                if is_error(&val) {
                    return val;
                }
                Object::Return(Box::new(val))
            }
            Stmt::LetStatement {
                ref ident,
                ref value,
            } => {
                let val = eval(&value, env, builtins);
                if is_error(&val) {
                    return val;
                }
                let Node::Expression(expr) = ident else {
                    return new_error(format!("ident is not Expression. got={:?}", ident));
                };
                let Expr::Identifier { ref name } = **expr else {
                    return new_error(format!("expr is not Identifier. got={:?}", expr));
                };
                let mut env = (*env).borrow_mut();
                if let Some(val) = env.set(name.clone(), val) {
                    return val;
                }
                NULL
            }
            _ => NULL,
        },
        _ => NULL,
    }
}

#[inline]
fn eval_block_statements(
    stmts: &Vec<Node>,
    env: &Rc<RefCell<Environment>>,
    builtins: &Builtins,
) -> Object {
    let mut result: Object = Object::Null;

    for stmt in stmts {
        result = eval(&stmt, env, builtins);

        match result {
            Object::Return(_) => return result,
            Object::Error(_) => return result,
            _ => {}
        }
    }

    result
}

#[inline]
fn native_bool_to_boolean_object(value: bool) -> Object {
    if value {
        return TRUE;
    }
    FALSE
}

#[inline]
fn eval_prefix_expression(op: &Str, right: Object) -> Object {
    match &(**op) {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => new_error(format!("unknown operator: {}{:?}", op, right)),
    }
}

#[inline]
fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        TRUE => FALSE,
        FALSE => TRUE,
        NULL => TRUE,
        _ => FALSE,
    }
}

#[inline]
fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    if let Object::Integer(int) = right {
        return Object::Integer(-1 * int);
    }
    new_error(format!("unknown operator: -{:?}", right))
}

#[inline]
fn eval_infix_expression(op: &str, left: Object, right: Object) -> Object {
    match (&left, &right) {
        (Object::Integer(left), Object::Integer(right)) => {
            eval_integer_infix_expression(op, *left, *right)
        }
        (Object::Boolean(left), Object::Boolean(right)) => match op {
            "==" => native_bool_to_boolean_object(left == right),
            "!=" => native_bool_to_boolean_object(left != right),
            _ => new_error(format!(
                "unknown operator: {:?} {} {:?}",
                Object::Boolean(*left),
                op,
                Object::Boolean(*right)
            )),
        },
        (Object::String(left), Object::String(right)) => match op {
            "+" => eval_string_infix_expression(left, right),
            _ => new_error(format!(
                "unknown operator: {:?} {} {:?}",
                Object::String(left.clone()),
                op,
                Object::String(right.clone())
            )),
        },
        _ => new_error(format!("type mismatch: {:?} {} {:?}", left, op, right)),
    }
}

#[inline]
fn eval_integer_infix_expression(op: &str, left: i64, right: i64) -> Object {
    match op {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => native_bool_to_boolean_object(left < right),
        ">" => native_bool_to_boolean_object(left > right),
        "==" => native_bool_to_boolean_object(left == right),
        "!=" => native_bool_to_boolean_object(left != right),
        _ => new_error(format!(
            "unknown operator: {:?} {} {:?}",
            Object::Integer(left),
            op,
            Object::Integer(right)
        )),
    }
}

#[inline]
fn eval_string_infix_expression(left: &Str, right: &Str) -> Object {
    Object::String((left.to_string() + right).into())
}

#[inline]
fn eval_if_expression(
    condition: &Node,
    consequence: &Node,
    alternative: &Node,
    env: &Rc<RefCell<Environment>>,
    builtins: &Builtins,
) -> Object {
    if is_truthy(eval(condition, env, builtins)) {
        return eval(consequence, env, builtins);
    }
    eval(alternative, env, builtins)
}

#[inline]
fn is_truthy(obj: Object) -> bool {
    match obj {
        NULL => false,
        TRUE => true,
        FALSE => false,
        _ => true,
    }
}

#[inline]
fn eval_program(prog: &Program, env: &Rc<RefCell<Environment>>, builtins: &Builtins) -> Object {
    let mut result = NULL;

    for stmt in prog.stmts.iter() {
        result = eval(&stmt, env, builtins);

        match result {
            Object::Error(_) => return result,
            Object::Return(val) => return *val,
            _ => {}
        }
    }

    result
}

#[inline]
fn new_error(msg: String) -> Object {
    Object::Error(msg.into())
}

#[inline]
fn is_error(obj: &Object) -> bool {
    if let Object::Error(_) = obj {
        return true;
    }

    false
}

#[inline]
fn eval_identifier(node: &Str, env: &Rc<RefCell<Environment>>, builtins: &Builtins) -> Object {
    let env = (*env).borrow();
    if let Some(obj) = env.get(node.clone()) {
        return obj.clone();
    }

    if let Some(obj) = builtins.get(node) {
        return obj.clone();
    }

    new_error(format!("identifier not found: {}", node))
}

#[inline]
fn eval_expressions(
    args: &Vec<Node>,
    env: &Rc<RefCell<Environment>>,
    builtins: &Builtins,
) -> Vec<Object> {
    let mut objs = vec![];

    for arg in args {
        let evaluated = eval(arg, env, builtins);
        if is_error(&evaluated) {
            return vec![evaluated];
        }
        objs.push(evaluated);
    }

    objs
}

#[inline]
fn eval_index_expression(left: Object, right: Object) -> Object {
    if let (Object::Array(ref elems), Object::Integer(int)) = (&left, &right) {
        return eval_array_index_expression(elems, *int);
    }
    new_error(format!("index operator not supported: {:?}", right))
}

#[inline]
fn eval_array_index_expression(array: &Vec<Object>, index: i64) -> Object {
    let max = array.len() as i64 - 1;

    if index < 0 || index > max {
        return NULL;
    }

    array[index as usize].clone()
}

#[inline]
fn apply_function(func: Object, args: Vec<Object>, builtins: &Builtins) -> Object {
    if let Object::Function(ref params, ref body, ref env) = func {
        let extend_env = extend_function_env(params, args, env);
        let evaluated = eval(body, &Rc::new(RefCell::new(extend_env)), builtins);
        return unwrap_return_value(evaluated);
    } else if let Object::Builtin(ref func) = func {
        func(args)
    } else {
        return new_error(format!("not a function: {:?}", func));
    }
}

#[inline]
fn extend_function_env(
    params: &Vec<Node>,
    args: Vec<Object>,
    env: &Rc<RefCell<Environment>>,
) -> Environment {
    let mut env = Environment::new_enclosed(env.clone());

    for (index, param) in params.iter().enumerate() {
        if let Node::Expression(ref expr) = *param {
            if let Expr::Identifier { ref name } = **expr {
                env.set(name.clone(), args[index].clone());
            }
        }
    }

    return env;
}

#[inline]
fn unwrap_return_value(obj: Object) -> Object {
    let Object::Return(value) = obj else {
        return obj;
    };
    return *value;
}
