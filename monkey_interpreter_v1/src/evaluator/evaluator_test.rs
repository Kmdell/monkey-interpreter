use crate::{
    ast::Node,
    environment::Environment,
    evaluator::{builtins, *},
    lexer::Lexer,
    parser::Parser,
};
use std::rc::Rc;

fn test_eval(input: String) -> Rc<dyn Object> {
    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let prog = p.parse_program();
    let env = Environment::new();
    let builtins = builtins::new_builtins();
    return eval(
        prog.expect("There is no program parsed").into_node(),
        Rc::new(RefCell::new(env)),
        builtins,
    )
    .unwrap();
}

fn test_integer_object(obj: Rc<dyn Object>, expected: i64) {
    let result = obj.into_int().unwrap_or_else(|e| panic!("{}", e));

    if result.value != expected {
        panic!(
            "object has wrong value, got={}, expected={}",
            result.value, expected
        );
    }
}

fn test_boolean_object(obj: Rc<dyn Object>, expected: bool) {
    let result = obj.into_bool().unwrap_or_else(|e| panic!("{}", e));

    if result.value != expected {
        panic!(
            "object had wrong value, got={}, expected={}",
            result.value, expected
        );
    }
}

fn test_null_object(obj: Rc<dyn Object>) {
    obj.into_null().unwrap_or_else(|e| panic!("{}", e));
}

#[test]
fn test_eval_integer_expression() {
    struct Test {
        input: String,
        expected: i64,
    }

    let tests = vec![
        Test {
            input: "5".into(),
            expected: 5,
        },
        Test {
            input: "10".into(),
            expected: 10,
        },
        Test {
            input: "-5".into(),
            expected: -5,
        },
        Test {
            input: "-10".into(),
            expected: -10,
        },
        Test {
            input: "5 + 5 + 5 + 5 - 10".into(),
            expected: 10,
        },
        Test {
            input: "2 * 2 * 2 * 2 * 2".into(),
            expected: 32,
        },
        Test {
            input: "-50 + 100 + -50".into(),
            expected: 0,
        },
        Test {
            input: "5 * 2 + 10".into(),
            expected: 20,
        },
        Test {
            input: "5 + 2 * 10".into(),
            expected: 25,
        },
        Test {
            input: "20 + 2 * -10".into(),
            expected: 0,
        },
        Test {
            input: "50 / 2 * 2 + 10".into(),
            expected: 60,
        },
        Test {
            input: "2 * (5 + 10)".into(),
            expected: 30,
        },
        Test {
            input: "3 * 3 * 3 + 10".into(),
            expected: 37,
        },
        Test {
            input: "3 * (3 * 3) + 10".into(),
            expected: 37,
        },
        Test {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".into(),
            expected: 50,
        },
    ];

    for tt in tests {
        let evaluated = test_eval(tt.input);
        test_integer_object(evaluated, tt.expected);
    }
}

#[test]
fn test_eval_boolean_expression() {
    struct Test {
        input: String,
        expected: bool,
    }

    let tests = vec![
        Test {
            input: "true".into(),
            expected: true,
        },
        Test {
            input: "false".into(),
            expected: false,
        },
        Test {
            input: "1 < 2".into(),
            expected: true,
        },
        Test {
            input: "1 > 2".into(),
            expected: false,
        },
        Test {
            input: "1 < 1".into(),
            expected: false,
        },
        Test {
            input: "1 > 1".into(),
            expected: false,
        },
        Test {
            input: "1 == 1".into(),
            expected: true,
        },
        Test {
            input: "1 != 1".into(),
            expected: false,
        },
        Test {
            input: "1 != 1".into(),
            expected: false,
        },
        Test {
            input: "1 != 2".into(),
            expected: true,
        },
        Test {
            input: "true == true".into(),
            expected: true,
        },
        Test {
            input: "false == false".into(),
            expected: true,
        },
        Test {
            input: "true == false".into(),
            expected: false,
        },
        Test {
            input: "true != false".into(),
            expected: true,
        },
        Test {
            input: "false != true".into(),
            expected: true,
        },
        Test {
            input: "(1 < 2) == true".into(),
            expected: true,
        },
        Test {
            input: "(1 < 2) == false".into(),
            expected: false,
        },
        Test {
            input: "(1 > 2) == true".into(),
            expected: false,
        },
        Test {
            input: "(1 > 2) == false".into(),
            expected: true,
        },
    ];

    for tt in tests {
        let evaluated = test_eval(tt.input);
        test_boolean_object(evaluated, tt.expected);
    }
}

#[test]
fn test_bang_operator() {
    struct Test {
        input: String,
        expected: bool,
    }

    let tests = vec![
        Test {
            input: "!true".into(),
            expected: false,
        },
        Test {
            input: "!false".into(),
            expected: true,
        },
        Test {
            input: "!5".into(),
            expected: false,
        },
        Test {
            input: "!!true".into(),
            expected: true,
        },
        Test {
            input: "!!false".into(),
            expected: false,
        },
        Test {
            input: "!!5".into(),
            expected: true,
        },
    ];

    for tt in tests {
        let evaluated = test_eval(tt.input);
        test_boolean_object(evaluated, tt.expected);
    }
}

#[test]
fn test_if_else_expression() {
    struct Test {
        input: String,
        expected: String,
    }

    let tests = vec![
        Test {
            input: "if (true) { 10 }".into(),
            expected: "10".into(),
        },
        Test {
            input: "if (false) { 10 }".into(),
            expected: "null".into(),
        },
        Test {
            input: "if (1) { 10 }".into(),
            expected: "10".into(),
        },
        Test {
            input: "if (1 < 2) { 10 }".into(),
            expected: "10".into(),
        },
        Test {
            input: "if (1 > 2) { 10 }".into(),
            expected: "null".into(),
        },
        Test {
            input: "if (1 > 2) { 10 } else { 20 }".into(),
            expected: "20".into(),
        },
        Test {
            input: "if (1 < 2) { 10 } else { 20 }".into(),
            expected: "10".into(),
        },
    ];

    for tt in tests {
        let evaluated = test_eval(tt.input);
        if let Ok(int) = tt.expected.parse::<i64>() {
            test_integer_object(evaluated, int);
        } else {
            test_null_object(evaluated);
        }
    }
}

#[test]
fn test_return_statment() {
    struct Test {
        input: String,
        expected: String,
    }

    let tests = vec![
        Test {
            input: "return 10;".into(),
            expected: "10".into(),
        },
        Test {
            input: "return 10; 9;".into(),
            expected: "10".into(),
        },
        Test {
            input: "return 2 * 5; 9;".into(),
            expected: "10".into(),
        },
        Test {
            input: "9; return 2 * 5; 9;".into(),
            expected: "10".into(),
        },
        Test {
            input: "
if (10 > 1) {
    if (10 > 1) {
        return 10;
    }

    return 1;
}"
            .into(),
            expected: "10".into(),
        },
    ];

    for tt in tests {
        let evaluated = test_eval(tt.input);
        if let Ok(int) = tt.expected.parse::<i64>() {
            test_integer_object(evaluated, int);
        } else {
            panic!("The test is not implemented for {}", tt.expected);
        }
    }
}

#[test]
fn test_error_handling() {
    struct Test {
        input: String,
        expected_message: String,
    }

    let tests = vec![
        Test {
            input: "5 + true;".into(),
            expected_message: "type mismatch: INTEGER + BOOLEAN".into(),
        },
        Test {
            input: "5 + true; 5;".into(),
            expected_message: "type mismatch: INTEGER + BOOLEAN".into(),
        },
        Test {
            input: "-true".into(),
            expected_message: "unknown operator: -BOOLEAN".into(),
        },
        Test {
            input: "true + false".into(),
            expected_message: "unknown operator: BOOLEAN + BOOLEAN".into(),
        },
        Test {
            input: "5; true + false; 5;".into(),
            expected_message: "unknown operator: BOOLEAN + BOOLEAN".into(),
        },
        Test {
            input: "if (10 > 1) { true + false; }".into(),
            expected_message: "unknown operator: BOOLEAN + BOOLEAN".into(),
        },
        Test {
            input: "
if (10 > 1) {
    if (10 > 1) {
        return true + false;
    }

    return 1;
}"
            .into(),
            expected_message: "unknown operator: BOOLEAN + BOOLEAN".into(),
        },
        Test {
            input: "foobar".into(),
            expected_message: "identifier not found: foobar".into(),
        },
        Test {
            input: "\"Hello\" - \"World\"".into(),
            expected_message: "unknown operator: STRING - STRING".into(),
        },
    ];

    for tt in tests {
        let evaluated = test_eval(tt.input);

        let error = evaluated.into_error().unwrap_or_else(|e| panic!("{}", e));

        if error.message != tt.expected_message {
            panic!(
                "wrong error message. expected='{}', got='{}'",
                tt.expected_message, error.message
            );
        }
    }
}

#[test]
fn test_let_statements() {
    struct Test {
        input: String,
        expected: String,
    }

    let tests = vec![
        Test {
            input: "let a = 5; a;".into(),
            expected: "5".into(),
        },
        Test {
            input: "let a = 5 * 5; a;".into(),
            expected: "25".into(),
        },
        Test {
            input: "let a = 5; let b = a; b;".into(),
            expected: "5".into(),
        },
        Test {
            input: "let a = 5; let b = a; let c = a + b + 5; c;".into(),
            expected: "15".into(),
        },
    ];

    for tt in tests {
        println!("input: {}", tt.input);
        if let Ok(int) = tt.expected.parse::<i64>() {
            test_integer_object(test_eval(tt.input), int);
        } else {
            panic!("Not found");
        }
    }
}

#[test]
fn test_function_object() {
    let input = "fn(x) { x + 2; }".into();

    let evaluated = test_eval(input);
    let func = evaluated.into_fn().unwrap_or_else(|e| panic!("{}", e));

    if func.parameters.len() != 1 {
        panic!(
            "function has wrong parameters. Parameters={}",
            func.parameters.len()
        );
    }

    if func.parameters[0].to_string() != "x" {
        panic!(
            "parameter is not 'x'. got={}",
            func.parameters[0].to_string()
        );
    }

    let expected_body = "(x + 2)".to_string();

    if func.body.to_string() != expected_body {
        panic!(
            "body is not {}. got={}",
            expected_body,
            func.body.to_string()
        );
    }
}

#[test]
fn test_function_application() {
    struct Test {
        input: String,
        expected: String,
    }

    let tests = vec![
        Test {
            input: "let identity = fn(x) { x; }; identity(5);".into(),
            expected: "5".into(),
        },
        Test {
            input: "let identity = fn(x) { return x; }; identity(5);".into(),
            expected: "5".into(),
        },
        Test {
            input: "let double = fn(x) { x * 2; }; double(5);".into(),
            expected: "10".into(),
        },
        Test {
            input: "let add = fn(x, y) { x + y; }; add(5, 5);".into(),
            expected: "10".into(),
        },
        Test {
            input: "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));".into(),
            expected: "20".into(),
        },
        Test {
            input: "fn(x) { return x; }(5)".into(),
            expected: "5".into(),
        },
    ];

    for tt in tests {
        if let Ok(int) = tt.expected.parse::<i64>() {
            println!("Running Test");
            let obj = test_eval(tt.input);
            println!("{}", obj.object_type());
            test_integer_object(obj, int);
            println!("Test passed");
        } else {
            panic!("Test not implemented for anything other than i64");
        }
    }
}

#[test]
fn test_closure() {
    let input = "
let new_adder = fn(x) { fn(y) { x + y } };

let add_two = new_adder(2);
add_two(2);
"
    .into();

    test_integer_object(test_eval(input), 4);
}

#[test]
fn test_string_literal() {
    let input = "\"Hello World!\"".into();

    let eval = test_eval(input);
    let string = eval
        .as_ref()
        .into_string()
        .unwrap_or_else(|e| panic!("{}", e));

    if string.value != "Hello World!" {
        panic!("String has wrong value, got='{}'", string.value);
    }
}

#[test]
fn test_string_concatenation() {
    let input = "\"Hello\" + \" \" + \"World!\"".into();

    let eval = test_eval(input);
    let string = eval
        .as_ref()
        .into_string()
        .unwrap_or_else(|e| panic!("{}", e));

    if string.value != "Hello World!" {
        panic!("String has wrong value, got={}", string.value);
    }
}

#[test]
fn test_builtin_functions() {
    struct Test {
        input: String,
        expected: String,
    }

    let tests = vec![
        Test {
            input: "len(\"\")".into(),
            expected: "0".into(),
        },
        Test {
            input: "len(\"four\")".into(),
            expected: "4".into(),
        },
        Test {
            input: "len(\"hello world\")".into(),
            expected: "11".into(),
        },
        Test {
            input: "len(1)".into(),
            expected: "argument to `len` not supported, got=INTEGER".into(),
        },
        Test {
            input: "len(\"one\", \"two\")".into(),
            expected: "wrong number of arguments, got=2, want=1".into(),
        },
    ];

    for tt in tests {
        let eval = test_eval(tt.input);

        if let Ok(int) = tt.expected.parse::<i64>() {
            test_integer_object(eval, int);
        } else {
            let err_obj = eval.into_error().unwrap_or_else(|e| panic!("{}", e));
            if err_obj.message != tt.expected {
                panic!(
                    "wrong error message. expected='{}', got='{}'",
                    tt.expected, err_obj.message
                );
            }
        }
    }
}

#[test]
fn test_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]".into();

    let eval = test_eval(input);
    let res = eval.into_array().unwrap_or_else(|e| panic!("{}", e));

    if res.elements.len() != 3 {
        panic!(
            "array has wrong num of elements, got={}",
            res.elements.len()
        );
    }

    test_integer_object(res.elements[0].clone(), 1);
    test_integer_object(res.elements[1].clone(), 4);
    test_integer_object(res.elements[2].clone(), 6);
}

#[test]
fn test_array_index_expressions() {
    struct Test {
        input: String,
        expected: String,
    }

    let tests = vec![
        Test {
            input: "[1, 2, 3][0]".into(),
            expected: "1".into(),
        },
        Test {
            input: "[1, 2, 3][1]".into(),
            expected: "2".into(),
        },
        Test {
            input: "[1, 2, 3][2]".into(),
            expected: "3".into(),
        },
        Test {
            input: "let i = 0; [1][i];".into(),
            expected: "1".into(),
        },
        Test {
            input: "[1, 2, 3][1 + 1];".into(),
            expected: "3".into(),
        },
        Test {
            input: "let myArray = [1, 2, 3]; myArray[2];".into(),
            expected: "3".into(),
        },
        Test {
            input: "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];".into(),
            expected: "6".into(),
        },
        Test {
            input: "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];".into(),
            expected: "2".into(),
        },
        Test {
            input: "[1, 2, 3][3]".into(),
            expected: "Null".into(),
        },
        Test {
            input: "[1, 2, 3][-1]".into(),
            expected: "Null".into(),
        },
    ];

    for tt in tests {
        let eval = test_eval(tt.input);
        if let Ok(int) = tt.expected.parse::<i64>() {
            test_integer_object(eval, int);
        } else {
            test_null_object(eval);
        }
    }
}
