use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{Node, TNode},
    environment::Environment,
    evaluator,
    lexer::Lexer,
    object::Object,
    parser::Parser,
};

use super::builtins::new_builtins;

enum Expected<'a> {
    Int(i64),
    String(&'a str),
    Null,
}

#[test]
fn test_eval_integer_expression() {
    let tests = vec![
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("50 - 100 + 50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];

    for tt in tests {
        let eval = test_eval(tt.0);
        test_integer_object(eval, tt.1);
    }
}

#[test]
fn test_eval_boolean_expression() {
    let tests = vec![
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 < 1", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true),
    ];

    for tt in tests {
        let eval = test_eval(tt.0);
        test_boolean_object(eval, tt.1);
    }
}

#[test]
fn test_bang_operator() {
    let tests = vec![
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
    ];

    for tt in tests {
        let eval = test_eval(tt.0);
        test_boolean_object(eval, tt.1);
    }
}

#[test]
fn test_if_else_expression() {
    let tests = vec![
        ("if (true) { 10 }", 10),
        ("if (false) { 10 }", 0),
        ("if (1) { 10 }", 10),
        ("if (1 < 2) { 10 }", 10),
        ("if (1 > 2) { 10 }", 0),
        ("if (1 > 2) { 10 } else { 20 }", 20),
        ("if (1 < 2) { 10 } else { 20 }", 10),
    ];

    for tt in tests {
        let eval = test_eval(tt.0);
        if tt.1 != 0 {
            test_integer_object(eval, tt.1);
        } else {
            test_null_object(eval);
        }
    }
}

#[test]
fn test_return_statements() {
    let tests = vec![
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2 * 5; 9;", 10),
        (
            "
        if (10 > 1) {
            if (10 > 1) {
                return 10;
            }

            return 1;
        }
        ",
            10,
        ),
    ];

    for tt in tests {
        test_integer_object(test_eval(tt.0), tt.1);
    }
}

#[test]
fn test_error_handling() {
    let tests = vec![
        ("5 + true;", "type mismatch: Integer(5) + Boolean(true)"),
        ("5 + true; 5;", "type mismatch: Integer(5) + Boolean(true)"),
        ("-true;", "unknown operator: -Boolean(true)"),
        (
            "true + false;",
            "unknown operator: Boolean(true) + Boolean(false)",
        ),
        (
            "5; true + false; 5;",
            "unknown operator: Boolean(true) + Boolean(false)",
        ),
        (
            "if (10 > 1) { true + false; }",
            "unknown operator: Boolean(true) + Boolean(false)",
        ),
        (
            "if (10 > 1) {
                if (10 > 1) {
                    return true + false;
                }
                
                return 1;
            }
        ",
            "unknown operator: Boolean(true) + Boolean(false)",
        ),
        ("foobar", "identifier not found: foobar"),
        (
            "\"Hello\" - \"World\"",
            "unknown operator: String(\"Hello\") - String(\"World\")",
        ),
    ];

    for (i, tt) in tests.iter().enumerate() {
        let eval = test_eval(tt.0);

        let Object::Error(msg) = eval else {
            panic!("Object is not an Error. got={:?}", eval);
        };

        if &(*msg) != tt.1 {
            panic!(
                "wrong error message: {}. got='{}', expected='{}'",
                i, msg, tt.1
            );
        }
    }
}

#[test]
fn test_let_statements() {
    let tests = vec![
        ("let a = 5; a;", 5),
        ("let a = 5 * 5; a;", 25),
        ("let a = 5; let b = a; b;", 5),
        ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
    ];

    for tt in tests {
        test_integer_object(test_eval(tt.0), tt.1);
    }
}

#[test]
fn test_function_object() {
    let input = "fn(x) { x + 2; };";

    let eval = test_eval(input);
    let Object::Function(params, body, ..) = eval else {
        panic!("object is not a function, got={:?}", eval);
    };

    if params.len() != 1 {
        panic!(
            "function has wrong parameters, want={}, got={}",
            1,
            params.len()
        );
    }

    if params[0].string() != "x" {
        panic!("parameter is not 'x'. got={:?}", params[0]);
    }

    if body.string() != "(x + 2)" {
        panic!("body is not '(x + 2)', got='{}'", body.string());
    }
}

#[test]
fn test_function_application() {
    let tests = vec![
        ("let identity = fn(x) { x; }; identity(5);", 5),
        ("let identity = fn(x) { return x; }; identity(5);", 5),
        ("let double = fn(x) { x * 2; }; double(5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
        ("fn(x) { x; }(5)", 5),
    ];

    for tt in tests {
        test_integer_object(test_eval(tt.0), tt.1);
    }
}

#[test]
fn test_closure() {
    let input = "
let newAddr = fn(x) {
    fn(y) { x + y };
};

let addTwo = newAddr(2);
addTwo(2);
";

    test_integer_object(test_eval(input), 4);
}

#[test]
fn test_string_literal() {
    let input = "\"Hello World!\"";

    let evaluated = test_eval(input);
    let Object::String(value) = evaluated else {
        panic!("Object is not a string, got={:?}", evaluated);
    };

    if &(*value) != "Hello World!" {
        panic!("String has wrong value, got={}", value);
    }
}

#[test]
fn test_sting_concatenation() {
    let input = "\"Hello\" + \" \" + \"World!\"";

    let evaluated = test_eval(input);
    let Object::String(value) = evaluated else {
        panic!("Object is not a string, got={:?}", evaluated);
    };

    if &(*value) != "Hello World!" {
        panic!("String has wrong value, got={}", value);
    }
}

#[test]
fn test_builtin_functions() {
    let tests = vec![
        ("len(\"\")", Expected::Int(0)),
        ("len(\"four\")", Expected::Int(4)),
        ("len(\"hello world\")", Expected::Int(11)),
        (
            "len(1)",
            Expected::String("argument to `len` not supported, got Integer(1)"),
        ),
        (
            "len(\"one\", \"two\")",
            Expected::String("wrong number of arguments. got=2, want=1"),
        ),
    ];

    for tt in tests {
        let evaluated = test_eval(tt.0);

        match tt.1 {
            Expected::Int(int) => test_integer_object(evaluated, int),
            Expected::String(string) => {
                let Object::Error(msg) = evaluated else {
                    panic!("evaluated is not an error object, got={:?}", evaluated);
                };
                if &(*msg) != string {
                    panic!("wrong error message. expected='{}', got='{}'", string, msg);
                }
            }
            _ => {}
        }
    }
}

#[test]
fn test_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";

    let eval = test_eval(input);

    let Object::Array(elems) = eval else {
        panic!("evaluated is not an Array object, got={:?}", eval);
    };

    if elems.len() != 3 {
        panic!("array has wrong num of elements, got={}", elems.len());
    }

    test_integer_object(elems[0].clone(), 1);
    test_integer_object(elems[1].clone(), 4);
    test_integer_object(elems[2].clone(), 6);
}

#[test]
fn test_array_index_expression() {
    let tests = vec![
        ("[1, 2, 3][0]", Expected::Int(1)),
        ("[1, 2, 3][1]", Expected::Int(2)),
        ("[1, 2, 3][2]", Expected::Int(3)),
        ("let i = 0;[1][i]", Expected::Int(1)),
        ("[1, 2, 3][1 + 1]", Expected::Int(3)),
        ("let myArray = [1, 2, 3]; myArray[2]", Expected::Int(3)),
        (
            "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2]",
            Expected::Int(6),
        ),
        (
            "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
            Expected::Int(2),
        ),
        ("[1, 2, 3][3]", Expected::Null),
        ("[1, 2, 3][-1]", Expected::Null),
    ];

    for tt in tests {
        let eval = test_eval(tt.0);
        match tt.1 {
            Expected::Int(int) => test_integer_object(eval, int),
            Expected::Null => test_null_object(eval),
            _ => {}
        }
    }
}

fn test_eval(input: &str) -> Object {
    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let env = Rc::new(RefCell::new(Environment::new()));

    let prog = p.parse_program();
    let builtins = new_builtins();
    evaluator::eval(&Node::Program(Box::new(prog)), &env, &builtins)
}

fn test_integer_object(obj: Object, expected: i64) {
    let Object::Integer(int) = obj else {
        panic!("Object is not an Integer. got={:?}", obj);
    };
    if int != expected {
        panic!("object has wrong value. got={}, wanted={}", int, expected)
    }
}

fn test_boolean_object(obj: Object, expected: bool) {
    let Object::Boolean(loob) = obj else {
        panic!("Object is not a Boolean. got={:?}", obj);
    };
    if loob != expected {
        panic!("object has wrong value. got={}, wanted={}", loob, expected);
    }
}

fn test_null_object(obj: Object) {
    let Object::Null = obj else {
        panic!("Object is not Null. got={:?}", obj);
    };
}
