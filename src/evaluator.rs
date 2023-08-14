use crate::{
    object::{
        self,
        *
    },
    ast::*, 
    environment::Environment
};
use std::rc::Rc;

const NULL: object::Null = object::Null{};
const TRUE: object::Boolean = object::Boolean{ value: true };
const FALSE: object::Boolean = object::Boolean{ value: false };

pub fn eval(node: Rc<&dyn Node>, env: &mut Environment) -> Option<Rc<dyn Object>> {
    if let Ok(int) = node.into_integer_literal() {
        return Some(Rc::new(object::Integer { value: int.value }));
    } else if let Ok(boolean) = node.into_bool() {
        return Some(Rc::new(native_bool_to_boolean_object(boolean.value)));
    } else if let Ok(ident) = node.into_identifier() {
        return eval_identifier(ident, env);
    } else if let Ok(exp_stmt) = node.into_expression() {
        return eval(exp_stmt.expression.as_ref().expect("There is no expression").into_node(), env);
    } else if let Ok(let_stmt) = node.into_let() {
        let val = eval(let_stmt.value.as_ref().expect("There is no value in let statement").into_node(), env);
        if is_error(&val) {
            return val;
        }
        env.set(let_stmt.name.value.clone(), val.expect("There is no value evaluated"));
    } else if let Ok(block_stmt) = node.into_block() {
        return eval_block_statements(&block_stmt.statements, env);
    } else if let Ok(ret) = node.into_return() {
        let val = eval(ret.return_value.as_ref().expect("There is no return value").into_node(), env);
        if is_error(&val) {
            return val;
        }
        return Some(Rc::new(ReturnValue { value: val.expect("There is nothing evaluated") }));
    } else if let Ok(if_exp) = node.into_if() {
        return Some(eval_if_expression(if_exp.into_node(), env));
    } else if let Ok(prefix) = node.into_prefix() {
        let right = eval(prefix.right.as_ref().expect("There is no right").into_node(), env);
        if is_error(&right) {
            return right;
        }
        return Some(eval_prefix_expression(&prefix.operator, right.expect("There is no node")));
    } else if let Ok(infix) = node.into_infix() {
        let left = eval(infix.left.as_ref().expect("There is no left to the infix").into_node(), env);
        if is_error(&left) {
            return left;
        }
        let right = eval(infix.right.as_ref().expect("There is no right to the infix").into_node(), env);
        if is_error(&right) {
            return right;
        }
        return Some(eval_infix_expression(&infix.operator, left.expect("left did not evaluate proper"), right.expect("right did not evaluate proper")));
    } else if let Ok(prog) = node.into_program() {
        println!("{} : {}", node.to_string(), prog.statements.len());
        return eval_program(&prog.statements, env);
    }

    None
}

fn eval_program(stmts: &Vec<Box<dyn Statement>>, env: &mut Environment) -> Option<Rc<dyn Object>> {
    let mut result = None;

    for stmt in stmts {
        result = eval(stmt.into_node(), env);
        
        if let Some(res) = result.as_ref() {
            if let Ok(ret) = res.into_return() {
                return Some(ret.value.clone());
            } else if let Ok(_) = res.into_error() {
                return result
            }
        }
    }

    result
}

fn eval_block_statements(stmts: &Vec<Box<dyn Statement>>, env: &mut Environment) -> Option<Rc<dyn Object>> {
    let mut result = None;

    for stmt in stmts {
        result = eval(stmt.into_node(), env);

        if result.is_some() {
            let rt = result.as_ref().unwrap().object_type();
            
            if rt == RETURN_VALUE_OBJ || rt == ERROR_OBJ {
                return result;
            }
        }
    }

    result
}

fn eval_identifier(node: &Identifier, env: &mut Environment) -> Option<Rc<dyn Object>> {
    let val = env.get(&node.value.clone());
    
    if val.is_none() {
        return Some(new_error(format!("identifier not found: {}", node.value)));
    }

    val
}   

fn eval_prefix_expression(operator: &String, right: Rc<dyn Object>) -> Rc<dyn Object> {
    return match &operator[..] {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => new_error(format!("unknown error: {}", right.object_type())),
    };
}

fn eval_infix_expression(operator: &String, left: Rc<dyn Object>, right: Rc<dyn Object>) -> Rc<dyn Object> {
    if left.object_type() == INTEGER_OBJ && right.object_type() == INTEGER_OBJ {
        return eval_integer_infix_expression(operator, left, right);
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
        return new_error(format!("type mismatch: {} {} {}", left.object_type(), operator, right.object_type()));
    }
    
    new_error(format!("unknown operator: {} {} {}", left.object_type(), operator, right.object_type()))
}

fn eval_bang_operator_expression(right: Rc<dyn Object>) -> Rc<dyn Object> {
    if let Ok(boolean) = right.into_bool() {
        return Rc::new( match boolean.value {
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

fn eval_integer_infix_expression(operator: &String, left: Rc<dyn Object>, right: Rc<dyn Object>) -> Rc<dyn Object> {
    let left_value = left.into_int().unwrap_or_else(|e| panic!("{}", e)).value;
    let right_value = right.into_int().unwrap_or_else(|e| panic!("{}", e)).value;

    match &operator[..] {
        "+" => Rc::new(Integer{ value: left_value + right_value }),
        "-" => Rc::new(Integer { value: left_value - right_value }),
        "*" => Rc::new(Integer { value: left_value * right_value }),
        "/" => Rc::new(Integer { value: left_value / right_value }),
        "<" => Rc::new(native_bool_to_boolean_object(left_value < right_value)),
        ">" => Rc::new(native_bool_to_boolean_object(left_value > right_value)),
        "==" => Rc::new(native_bool_to_boolean_object(left_value == right_value)),
        "!=" => Rc::new(native_bool_to_boolean_object(left_value != right_value)),
        _ => new_error(format!("unknown operator: {} {} {}", left.object_type(), operator, right.object_type()))
    }
}

fn eval_if_expression(if_exp: Rc<&dyn Node>, env: &mut Environment) -> Rc<dyn Object> {
    let if_exp = if_exp.into_if().unwrap_or_else(|e| panic!("{}", e));
    let condition = eval(if_exp.condition.as_ref().expect("There is no condition").into_node(), env).expect("eval got nothing");

    if is_error(&Some(condition.clone())) {
        return condition;
    }

    if is_truthy(condition) {
        return eval(if_exp.consequence.as_ref().expect("There is no consequence").into_node(), env).expect("eval got nothing");
    } else if if_exp.alternative.is_some() {
        return eval(if_exp.alternative.as_ref().expect("There is no alternative").into_node(), env).expect("eval got nothing");
    }

    return Rc::new(NULL);
}


fn is_truthy(obj: Rc<dyn Object>) -> bool {
    if let Ok(_) = obj.into_null() {
        return false;
    } else if let Ok(bool) = obj.into_bool() {
        return bool.value
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
