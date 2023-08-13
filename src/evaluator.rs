use crate::{
    object::{
        self,
        *
    },
    ast::*
};

const NULL: object::Null = object::Null{};
const TRUE: object::Boolean = object::Boolean{ value: true };
const FALSE: object::Boolean = object::Boolean{ value: false };

pub fn eval(node: Box<&dyn Node>) -> Option<Box<dyn Object>> {
    if let Ok(int) = node.into_integer_literal() {
        return Some(Box::new(object::Integer { value: int.value }));
    } else if let Ok(boolean) = node.into_bool() {
        return Some(Box::new(native_bool_to_boolean_object(boolean.value)));
    } else if let Ok(exp_stmt) = node.into_expression() {
        return eval(exp_stmt.expression.as_ref().expect("There is no expression").into_node());
    } else if let Ok(prefix) = node.into_prefix() {
        let right = eval(prefix.right.as_ref().expect("There is no right").into_node());
        return Some(eval_prefix_expression(&prefix.operator, right.expect("There is no node")));
    } else if let Ok(infix) = node.into_infix() {
        let left = eval(infix.left.as_ref().expect("There is no left to the infix").into_node()).expect("There is no left evaluation");
        let right = eval(infix.right.as_ref().expect("There is no right to the infix").into_node()).expect("There is not right evaluation");
        return Some(eval_infix_expression(&infix.operator, left, right));
    } else if let Ok(prog) = node.into_program() {
        return eval_statements(&prog.statements);
    }

    None
}

fn eval_statements(stmts: &Vec<Box<dyn Statement>>) -> Option<Box<dyn Object>> {
    let mut result = None;

    for stmt in stmts {
        result = eval(stmt.into_node());
    }

    result
}

fn eval_prefix_expression(operator: &String, right: Box<dyn Object>) -> Box<dyn Object> {
    return match &operator[..] {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Box::new(NULL),
    };
}

fn eval_infix_expression(operator: &String, left: Box<dyn Object>, right: Box<dyn Object>) -> Box<dyn Object> {
    match (&left.object_type()[..], &right.object_type()[..]) {
        (INTEGER_OBJ, INTEGER_OBJ) => eval_integer_infix_expression(operator, left, right),
        (_, _) => Box::new(NULL),
    }
}

fn eval_bang_operator_expression(right: Box<dyn Object>) -> Box<dyn Object> {
    if let Ok(boolean) = right.into_bool() {
        return Box::new( match boolean.value {
            true => FALSE,
            false => TRUE,
        });
    }

    if let Ok(_) = right.into_null() {
        return Box::new(TRUE);
    }

    Box::new(FALSE)
}

fn eval_minus_prefix_operator_expression(right: Box<dyn Object>) -> Box<dyn Object> {
    if !right.object_type().eq(INTEGER_OBJ) {
        return Box::new(NULL)
    }

    let value = right.into_int().unwrap_or_else(|e| panic!("{}", e)).value;
    return Box::new(Integer { value: -value });
}

fn eval_integer_infix_expression(operator: &String, left: Box<dyn Object>, right: Box<dyn Object>) -> Box<dyn Object> {
    let left_value = left.into_int().unwrap_or_else(|e| panic!("{}", e)).value;
    let right_value = right.into_int().unwrap_or_else(|e| panic!("{}", e)).value;

    match &operator[..] {
        "+" => Box::new(Integer{ value: left_value + right_value }),
        "-" => Box::new(Integer { value: left_value - right_value }),
        "*" => Box::new(Integer { value: left_value * right_value }),
        "/" => Box::new(Integer { value: left_value / right_value }),
        _ => Box::new(NULL)
    }
}

fn native_bool_to_boolean_object(input: bool) -> object::Boolean {
    if input {
        return TRUE;
    }

    FALSE
}
