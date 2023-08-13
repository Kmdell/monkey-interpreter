use crate::{
    lexer::Lexer,
    parser::Parser,
    object::*, 
    evaluator::*, ast::Node
};

fn test_eval(input: String) -> Box<dyn Object> {
    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let prog = p.parse_program();

    return eval(prog.expect("There is no program parsed").into_node()).unwrap();
}

fn test_integer_object(obj: Box<dyn Object>, expected: i64) {
    let result = obj.into_int().unwrap_or_else(|e| panic!("{}", e));

    if result.value != expected {
        panic!("object has wrong value, got={}, expected={}", result.value, expected);
    }
}

fn test_boolean_object(obj: Box<dyn Object>, expected: bool) {
    let result = obj.into_bool().unwrap_or_else(|e| panic!("{}", e));

    if result.value != expected {
        panic!("object had wrong value, got={}, expected={}", result.value, expected);
    }
}

#[test]
fn test_eval_integer_expression() {
    struct Test {
        input: String,
        expected: i64
    }

    let tests = vec![
        Test {
            input: "5".into(),
            expected: 5
        },
        Test {
            input: "10".into(),
            expected: 10
        },
        Test {
            input: "-5".into(),
            expected: -5
        },
        Test {
            input: "-10".into(),
            expected: -10
        },
        Test {
            input: "5 + 5 + 5 + 5 - 10".into(),
            expected: 10
        },
        Test {
            input: "2 * 2 * 2 * 2 * 2".into(),
            expected: 32
        },
        Test {
            input: "-50 + 100 + -50".into(),
            expected: 0
        },
        Test {
            input: "5 * 2 + 10".into(),
            expected: 20
        },
        Test {
            input: "5 + 2 * 10".into(),
            expected: 25
        },
        Test {
            input: "20 + 2 * -10".into(),
            expected: 0
        },
        Test {
            input: "50 / 2 * 2 + 10".into(),
            expected: 60
        },
        Test {
            input: "2 * (5 + 10)".into(),
            expected: 30
        },
        Test {
            input: "3 * 3 * 3 + 10".into(),
            expected: 37
        },
        Test {
            input: "3 * (3 * 3) + 10".into(),
            expected: 37
        },
        Test {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".into(),
            expected: 50
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
        expected: bool
    }

    let tests = vec![
        Test {
            input: "true".into(),
            expected: true
        },
        Test {
            input: "false".into(),
            expected: false
        }
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
        expected: bool
    }

    let tests = vec![
        Test {
            input: "!true".into(),
            expected: false
        },
        Test {
            input: "!false".into(),
            expected: true
        },
        Test {
            input: "!5".into(),
            expected: false
        },
        Test {
            input: "!!true".into(),
            expected: true
        },
        Test {
            input: "!!false".into(),
            expected: false
        },
        Test {
            input: "!!5".into(),
            expected: true
        }
    ];

    for tt in tests {
        let evaluated = test_eval(tt.input);
        test_boolean_object(evaluated, tt.expected);
    }
}
