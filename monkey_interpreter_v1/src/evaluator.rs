use crate::{
    ast::*,
    environment::Environment,
    object::{self, *},
};
use std::{cell::RefCell, rc::Rc};

pub mod builtins;
#[cfg(test)]
mod evaluator_test;

const NULL: object::Null = object::Null {};
const TRUE: object::Boolean = object::Boolean { value: true };
const FALSE: object::Boolean = object::Boolean { value: false };

pub fn eval(
    node: Rc<&dyn Node>,
    env: Rc<RefCell<Environment>>,
    builtins: builtins::Builtins,
) -> Option<Rc<dyn Object>> {
    if let Ok(int) = node.into_integer_literal() {
        return Some(Rc::new(object::Integer { value: int.value }));
    } else if let Ok(boolean) = node.into_bool() {
        return Some(Rc::new(native_bool_to_boolean_object(boolean.value)));
    } else if let Ok(string) = node.into_string() {
        return Some(Rc::new(OString {
            value: string.value.clone(),
        }));
    } else if let Ok(ident) = node.into_identifier() {
        return eval_identifier(ident, env, builtins.clone());
    } else if let Ok(exp_stmt) = node.into_expression() {
        return eval(
            exp_stmt
                .expression
                .as_ref()
                .expect("There is no expression")
                .into_node(),
            env,
            builtins,
        );
    } else if let Ok(func) = node.into_func() {
        let mut params = vec![];
        for p in &func.parameters {
            params.push(
                p.into_identifier()
                    .unwrap_or_else(|e| panic!("{}", e))
                    .clone(),
            );
        }
        return Some(Rc::new(Function {
            parameters: params,
            body: func.body.clone().unwrap(),
            env,
        }));
    } else if let Ok(call) = node.into_call() {
        let function = eval(
            call.function
                .as_ref()
                .expect("There is no function in call")
                .into_node(),
            env.clone(),
            builtins.clone(),
        );
        if is_error(&function) {
            return function;
        }
        let args = eval_expressions(call.arguments.as_ref(), env, builtins.clone());
        if args.len() == 1 && is_error(&Some(args[0].clone())) {
            return Some(args[0].clone());
        }
        return apply_function(function.expect("There is no function"), args, builtins);
    } else if let Ok(let_stmt) = node.into_let() {
        let val = eval(
            let_stmt
                .value
                .as_ref()
                .expect("There is no value in let statement")
                .into_node(),
            env.clone(),
            builtins,
        );
        if is_error(&val) {
            return val;
        }
        env.borrow_mut().set(
            let_stmt.name.value.clone(),
            val.expect("There is no value evaluated"),
        );
    } else if let Ok(array) = node.into_array() {
        let elems = eval_expressions(&array.elements, env, builtins);
        if elems.len() == 1 && is_error(&Some(elems[0].clone())) {
            return Some(elems[0].clone());
        }
        return Some(Rc::new(Array { elements: elems }));
    } else if let Ok(block_stmt) = node.into_block() {
        return eval_block_statements(&block_stmt.statements, env, builtins);
    } else if let Ok(index) = node.into_ind() {
        let left = eval(
            index.left.as_ref().expect("left is empty").into_node(),
            env.clone(),
            builtins.clone(),
        );
        if is_error(&left) {
            return left;
        }
        let i = eval(
            index.index.as_ref().expect("index is empty").into_node(),
            env,
            builtins,
        );
        if is_error(&i) {
            return i;
        }
        return eval_index_expression(
            left.expect("There is no left"),
            i.expect("There is no index"),
        );
    } else if let Ok(ret) = node.into_return() {
        let val = eval(
            ret.return_value
                .as_ref()
                .expect("There is no return value")
                .into_node(),
            env,
            builtins,
        );
        if is_error(&val) {
            return val;
        }
        return Some(Rc::new(ReturnValue {
            value: val.expect("There is nothing evaluated"),
        }));
    } else if let Ok(if_exp) = node.into_if() {
        return Some(eval_if_expression(if_exp.into_node(), env, builtins));
    } else if let Ok(prefix) = node.into_prefix() {
        let right = eval(
            prefix
                .right
                .as_ref()
                .expect("There is no right")
                .into_node(),
            env,
            builtins,
        );
        if is_error(&right) {
            return right;
        }
        return Some(eval_prefix_expression(
            &prefix.operator,
            right.expect("There is no node"),
        ));
    } else if let Ok(infix) = node.into_infix() {
        let left = eval(
            infix
                .left
                .as_ref()
                .expect("There is no left to the infix")
                .into_node(),
            env.clone(),
            builtins.clone(),
        );
        if is_error(&left) {
            return left;
        }
        let right = eval(
            infix
                .right
                .as_ref()
                .expect("There is no right to the infix")
                .into_node(),
            env,
            builtins,
        );
        if is_error(&right) {
            return right;
        }
        return Some(eval_infix_expression(
            &infix.operator,
            left.expect("left did not evaluate proper"),
            right.expect("right did not evaluate proper"),
        ));
    } else if let Ok(prog) = node.into_program() {
        return eval_program(&prog.statements, env, builtins);
    }

    None
}

fn eval_program(
    stmts: &Vec<Box<dyn Statement>>,
    env: Rc<RefCell<Environment>>,
    builtins: builtins::Builtins,
) -> Option<Rc<dyn Object>> {
    let mut result = None;

    for stmt in stmts {
        result = eval(stmt.into_node(), env.clone(), builtins.clone());

        if let Some(res) = result.as_ref() {
            if let Ok(ret) = res.into_return() {
                return Some(ret.value.clone());
            } else if let Ok(_) = res.into_error() {
                return result;
            }
        }
    }

    result
}

fn eval_block_statements(
    stmts: &Vec<Box<dyn Statement>>,
    env: Rc<RefCell<Environment>>,
    builtins: builtins::Builtins,
) -> Option<Rc<dyn Object>> {
    let mut result = None;

    for stmt in stmts {
        result = eval(stmt.into_node(), env.clone(), builtins.clone());

        if result.is_some() {
            let rt = result.as_ref().unwrap().object_type();

            if rt == RETURN_VALUE_OBJ || rt == ERROR_OBJ {
                return result;
            }
        }
    }

    result
}

fn eval_identifier(
    node: &Identifier,
    env: Rc<RefCell<Environment>>,
    builtins: builtins::Builtins,
) -> Option<Rc<dyn Object>> {
    if let Some(val) = env.borrow().get(&node.value.clone()) {
        return Some(val.clone());
    }

    if let Some(builtin) = builtins.get(&node.value.clone()) {
        return Some(builtin.clone());
    }

    Some(new_error(format!("identifier not found: {}", node.value)))
}

fn eval_prefix_expression(operator: &String, right: Rc<dyn Object>) -> Rc<dyn Object> {
    return match &operator[..] {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => new_error(format!("unknown error: {}", right.object_type())),
    };
}

fn eval_infix_expression(
    operator: &String,
    left: Rc<dyn Object>,
    right: Rc<dyn Object>,
) -> Rc<dyn Object> {
    if left.object_type() == INTEGER_OBJ && right.object_type() == INTEGER_OBJ {
        return eval_integer_infix_expression(operator, left, right);
    }
    if left.object_type() == STRING_OBJ && right.object_type() == STRING_OBJ {
        return eval_string_infix_expression(operator, left, right);
    }
    if operator == "==" {
        let left_value = left.into_bool().unwrap_or_else(|e| panic!("{}", e)).value;
        let right_value = right.into_bool().unwrap_or_else(|e| panic!("{}", e)).value;
        return Rc::new(native_bool_to_boolean_object(left_value == right_value));
    }
    if operator == "!=" {
        let left_value = left.into_bool().unwrap_or_else(|e| panic!("{}", e)).value;
        let right_value = right.into_bool().unwrap_or_else(|e| panic!("{}", e)).value;
        return Rc::new(native_bool_to_boolean_object(left_value != right_value));
    }
    if left.object_type() != right.object_type() {
        return new_error(format!(
            "type mismatch: {} {} {}",
            left.object_type(),
            operator,
            right.object_type()
        ));
    }

    new_error(format!(
        "unknown operator: {} {} {}",
        left.object_type(),
        operator,
        right.object_type()
    ))
}

fn eval_expressions(
    exps: &Vec<Box<dyn Expression>>,
    env: Rc<RefCell<Environment>>,
    builtins: builtins::Builtins,
) -> Vec<Rc<dyn Object>> {
    let mut result = vec![];

    for exp in exps.iter() {
        let evaluated = eval(exp.into_node(), env.clone(), builtins.clone());
        if is_error(&evaluated) {
            return vec![evaluated.expect("Nothing in evaluated")];
        }
        result.push(evaluated.expect("Nothing in evaluated"));
    }

    result
}

fn eval_bang_operator_expression(right: Rc<dyn Object>) -> Rc<dyn Object> {
    if let Ok(boolean) = right.into_bool() {
        return Rc::new(match boolean.value {
            true => FALSE,
            false => TRUE,
        });
    }

    if let Ok(_) = right.into_null() {
        return Rc::new(TRUE);
    }

    Rc::new(FALSE)
}

fn eval_minus_prefix_operator_expression(right: Rc<dyn Object>) -> Rc<dyn Object> {
    if !right.object_type().eq(INTEGER_OBJ) {
        return new_error(format!("unknown operator: -{}", right.object_type()));
    }

    let value = right.into_int().unwrap_or_else(|e| panic!("{}", e)).value;
    return Rc::new(Integer { value: -value });
}

fn eval_integer_infix_expression(
    operator: &String,
    left: Rc<dyn Object>,
    right: Rc<dyn Object>,
) -> Rc<dyn Object> {
    let left_value = left.into_int().unwrap_or_else(|e| panic!("{}", e)).value;
    let right_value = right.into_int().unwrap_or_else(|e| panic!("{}", e)).value;

    match &operator[..] {
        "+" => Rc::new(Integer {
            value: left_value + right_value,
        }),
        "-" => Rc::new(Integer {
            value: left_value - right_value,
        }),
        "*" => Rc::new(Integer {
            value: left_value * right_value,
        }),
        "/" => Rc::new(Integer {
            value: left_value / right_value,
        }),
        "<" => Rc::new(native_bool_to_boolean_object(left_value < right_value)),
        ">" => Rc::new(native_bool_to_boolean_object(left_value > right_value)),
        "==" => Rc::new(native_bool_to_boolean_object(left_value == right_value)),
        "!=" => Rc::new(native_bool_to_boolean_object(left_value != right_value)),
        _ => new_error(format!(
            "unknown operator: {} {} {}",
            left.object_type(),
            operator,
            right.object_type()
        )),
    }
}

fn eval_string_infix_expression(
    operator: &String,
    left: Rc<dyn Object>,
    right: Rc<dyn Object>,
) -> Rc<dyn Object> {
    if operator != "+" {
        return new_error(format!(
            "unknown operator: {} {} {}",
            left.object_type(),
            operator,
            right.object_type()
        ));
    }
    let mut left_val = left
        .into_string()
        .expect("There was no left string")
        .value
        .clone();
    left_val.push_str(
        &right
            .into_string()
            .expect("There was no right string")
            .value,
    );
    return Rc::new(OString { value: left_val });
}

fn eval_if_expression(
    if_exp: Rc<&dyn Node>,
    env: Rc<RefCell<Environment>>,
    builtins: builtins::Builtins,
) -> Rc<dyn Object> {
    let if_exp = if_exp.into_if().unwrap_or_else(|e| panic!("{}", e));
    let condition = eval(
        if_exp
            .condition
            .as_ref()
            .expect("There is no condition")
            .into_node(),
        env.clone(),
        builtins.clone(),
    )
    .expect("eval got nothing");

    if is_error(&Some(condition.clone())) {
        return condition;
    }

    if is_truthy(condition) {
        return eval(
            if_exp
                .consequence
                .as_ref()
                .expect("There is no consequence")
                .into_node(),
            env,
            builtins,
        )
        .expect("eval got nothing");
    } else if if_exp.alternative.is_some() {
        return eval(
            if_exp
                .alternative
                .as_ref()
                .expect("There is no alternative")
                .into_node(),
            env,
            builtins,
        )
        .expect("eval got nothing");
    }

    return Rc::new(NULL);
}

fn eval_index_expression(left: Rc<dyn Object>, right: Rc<dyn Object>) -> Option<Rc<dyn Object>> {
    if left.object_type() == ARRAY_OBJ && right.object_type() == INTEGER_OBJ {
        return eval_array_index_expression(left, right);
    }

    Some(new_error(format!(
        "index operator not supported: {}",
        left.object_type()
    )))
}

fn eval_array_index_expression(
    array: Rc<dyn Object>,
    index: Rc<dyn Object>,
) -> Option<Rc<dyn Object>> {
    let array_obj = array.into_array().unwrap_or_else(|e| panic!("{}", e));
    let idx = index.into_int().unwrap_or_else(|e| panic!("{}", e)).value;
    let max = array_obj.elements.len() as i64 - 1;
    if idx < 0 || idx > max {
        return Some(Rc::new(NULL));
    }

    return Some(array_obj.elements[idx as usize].clone());
}

fn apply_function(
    func: Rc<dyn Object>,
    args: Vec<Rc<dyn Object>>,
    builtins: builtins::Builtins,
) -> Option<Rc<dyn Object>> {
    if let Ok(function) = func.into_fn() {
        let extended = extend_function_env(function, &args);
        let eval = eval(function.body.as_ref().into_node(), extended, builtins);
        return Some(unwrap_return_value(
            eval.expect("There is not evaluated function body"),
        ));
    } else if let Ok(function) = func.into_built() {
        return Some(function.func.clone()(args));
    } else {
        return Some(new_error(format!("not a functon: {}", func.object_type())));
    }
}

fn extend_function_env(func: &Function, args: &Vec<Rc<dyn Object>>) -> Rc<RefCell<Environment>> {
    let env = Rc::new(RefCell::new(Environment::new_enc(func.env.clone())));

    for (index, param) in func.parameters.iter().enumerate() {
        env.borrow_mut()
            .set(param.value.clone(), args[index].clone().into());
    }

    env
}

fn unwrap_return_value(obj: Rc<dyn Object>) -> Rc<dyn Object> {
    if let Ok(ret) = obj.into_return() {
        return ret.value.clone();
    }

    obj
}

fn is_truthy(obj: Rc<dyn Object>) -> bool {
    if let Ok(_) = obj.into_null() {
        return false;
    } else if let Ok(bool) = obj.into_bool() {
        return bool.value;
    }
    true
}

fn native_bool_to_boolean_object(input: bool) -> object::Boolean {
    if input {
        return TRUE;
    }

    FALSE
}

fn new_error(message: String) -> Rc<dyn Object> {
    Rc::new(Error { message })
}

fn is_error(obj: &Option<Rc<dyn Object>>) -> bool {
    if let Some(obj) = obj {
        return obj.object_type() == ERROR_OBJ;
    }

    false
}
