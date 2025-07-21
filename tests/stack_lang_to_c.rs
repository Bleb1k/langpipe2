use langpipe::{
    AnyIrType, CodeGenerator, CodegenStage, Diagnostic, IrTransformStage, IrTransformer, IrType,
    Lexer, LexerStage, Parser, ParserStage, Pipeline, Result, Severity, SourceLocation, Token,
    TokenIr,
};

#[derive(Clone)]
struct StackFunctionAst {
    return_type: String,
    name: String,
    parameters: Vec<Parameter>,
    body: Vec<StackInstruction>,
}

impl IrType for StackFunctionAst {
    fn validate(&self) -> Result<()> {
        Ok(())
    }
}

#[derive(Clone)]
struct Parameter {
    type_name: String,
    name: String,
}

#[derive(Clone)]
enum StackInstruction {
    Push(String), // variable or literal
    BinaryOp(String),
    Return,
}

#[derive(Clone)]
struct CFunctionAst {
    return_type: String,
    name: String,
    parameters: Vec<Parameter>,
    body: Vec<Statement>,
}

impl IrType for CFunctionAst {
    fn validate(&self) -> Result<()> {
        Ok(())
    }
}

#[derive(Clone)]
enum Statement {
    Return(Expression),
}

#[derive(Clone)]
enum Expression {
    BinaryOp(String, Box<Expression>, Box<Expression>),
    Variable(String),
    Literal(String),
}

struct MyLexer;

impl Lexer for MyLexer {
    fn lex(&self, source: &str) -> Result<AnyIrType> {
        let mut tokens = vec![];
        let chars: Vec<char> = source.chars().collect();
        let len = chars.len();
        let mut i = 0;
        let mut line = 1;
        let mut column = 1;

        while i < len {
            let c = chars[i];

            if c.is_whitespace() {
                if c == '\n' {
                    line += 1;
                    column = 1;
                } else {
                    column += 1;
                }
                i += 1;
                continue;
            }

            let start_line = line;
            let start_column = column;

            if c.is_alphabetic() {
                let start_i = i;
                while i < len && (chars[i].is_alphanumeric() || chars[i] == '_') {
                    column += 1;
                    i += 1;
                }
                let ident: String = chars[start_i..i].iter().collect();
                let kind = match ident.as_str() {
                    "int" | "return" => "keyword",
                    _ => "identifier",
                }
                .to_string();
                tokens.push(Token {
                    kind,
                    value: ident,
                    location: SourceLocation {
                        file: "input".to_string(),
                        line: start_line,
                        column: start_column,
                    },
                })
            } else if c.is_ascii_digit() {
                let start_i = i;
                while i < len && chars[i].is_ascii_digit() {
                    column += 1;
                    i += 1;
                }
                let num: String = chars[start_i..i].iter().collect();
                tokens.push(Token {
                    kind: "number".to_string(),
                    value: num,
                    location: SourceLocation {
                        file: "input".to_string(),
                        line: start_line,
                        column: start_column,
                    },
                });
            } else {
                match c {
                    '+' | '-' | '*' | '/' => {
                        tokens.push(Token {
                            kind: "operator".to_string(),
                            value: c.to_string(),
                            location: SourceLocation {
                                file: "input".to_string(),
                                line: start_line,
                                column: start_column,
                            },
                        });
                        column += 1;
                        i += 1;
                    }
                    '(' | ')' | '{' | '}' | ',' => {
                        tokens.push(Token {
                            kind: "punctuation".to_string(),
                            value: c.to_string(),
                            location: SourceLocation {
                                file: "input".to_string(),
                                line: start_line,
                                column: start_column,
                            },
                        });
                        column += 1;
                        i += 1;
                    }
                    _ => {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            message: format!("Unexpected character: '{}'", c),
                            location: Some(SourceLocation {
                                file: "input".to_string(),
                                line: start_line,
                                column: start_column,
                            }),
                            hints: vec!["Expected valid token".to_string()],
                        });
                    }
                }
            }
        }

        Ok(Box::new(TokenIr { tokens }))
    }
}

struct StackParser;

impl Parser for StackParser {
    fn parse(&self, tokens: &dyn IrType) -> Result<AnyIrType> {
        let token_ir: &TokenIr = tokens
            .as_any()
            .downcast_ref::<TokenIr>()
            .ok_or(Diagnostic {
                severity: Severity::Error,
                message: "Expected TokenIr".to_string(),
                location: None,
                hints: vec![],
            })?;

        let tokens = &token_ir.tokens;
        let mut pos = 0;

        let return_type = tokens
            .get(pos)
            .ok_or(Diagnostic {
                severity: Severity::Error,
                message: "Expected return type".to_string(),
                location: None,
                hints: vec![],
            })?
            .value
            .clone();
        pos += 1;

        let name = tokens
            .get(pos)
            .ok_or(Diagnostic {
                severity: Severity::Error,
                message: "Expected function name".to_string(),
                location: Some({
                    let prev_tok = tokens.get(pos - 1).unwrap();
                    let mut location = prev_tok.location.clone();
                    location.column += prev_tok.value.len() as u32;
                    location
                }),
                hints: vec![],
            })?
            .value
            .clone();
        pos += 1;

        if pos >= tokens.len() || tokens[pos].value != "(" {
            return Err(Diagnostic {
                severity: Severity::Error,
                message: "Expected '('".to_string(),
                location: Some({
                    let prev_tok = tokens.get(pos - 1).unwrap();
                    let mut location = prev_tok.location.clone();
                    location.column += prev_tok.value.len() as u32;
                    location
                }),
                hints: vec![],
            });
        }
        pos += 1;

        let mut parameters = Vec::new();
        while pos < tokens.len() && tokens[pos].value != ")" {
            let type_name = tokens
                .get(pos)
                .ok_or(Diagnostic {
                    severity: Severity::Error,
                    message: "Expected type name".to_string(),
                    location: Some({
                        let prev_tok = tokens.get(pos - 1).unwrap();
                        let mut location = prev_tok.location.clone();
                        location.column += prev_tok.value.len() as u32;
                        location
                    }),
                    hints: vec![],
                })?
                .value
                .clone();
            pos += 1;

            while pos < tokens.len() && tokens[pos].kind == "identifier" {
                let name = tokens[pos].value.clone();
                parameters.push(Parameter {
                    type_name: type_name.clone(),
                    name,
                });
                pos += 1;
            }

            if pos < tokens.len() && tokens[pos].value == "," {
                pos += 1;
            }
        }

        pos += 1; // Skip ')'
        pos += 1; // Skip '{'

        let mut body = Vec::new();
        while pos < tokens.len() && tokens[pos].value != "}" {
            let token = &tokens[pos];
            if token.kind == "identifier" || token.kind == "number" {
                body.push(StackInstruction::Push(token.value.clone()));
            } else if token.kind == "operator" {
                body.push(StackInstruction::BinaryOp(token.value.clone()));
            } else if token.value == "return" {
                body.push(StackInstruction::Return);
                // pos += 1;
                break;
            } else {
                return Err(Diagnostic {
                    severity: Severity::Error,
                    message: format!("Unexpected token in body: {}", token.value),
                    location: Some(token.location.clone()),
                    hints: vec![],
                });
            }
            pos += 1;
        }

        Ok(Box::new(StackFunctionAst {
            return_type,
            name,
            parameters,
            body,
        }))
    }
}

struct StackToCAstTransformer;

impl IrTransformer for StackToCAstTransformer {
    fn transform(&self, from: &dyn IrType) -> Result<AnyIrType> {
        let stack_ast = from
            .as_any()
            .downcast_ref::<StackFunctionAst>()
            .ok_or(Diagnostic {
                severity: Severity::Error,
                message: "Expected StackFunctionAst".to_string(),
                location: None,
                hints: vec![],
            })?;

        let mut expr_stack: Vec<Expression> = Vec::new();

        for instr in &stack_ast.body {
            match instr {
                StackInstruction::Push(var) => {
                    expr_stack.push(if let Ok(_) = var.parse::<i64>() {
                        Expression::Literal(var.clone())
                    } else {
                        Expression::Variable(var.clone())
                    });
                }
                StackInstruction::BinaryOp(op) => {
                    if expr_stack.len() < 2 {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            message: "Not enough operands for binary operator".to_string(),
                            location: None,
                            hints: vec![],
                        });
                    }
                    let rhs = expr_stack.pop().unwrap();
                    let lhs = expr_stack.pop().unwrap();
                    expr_stack.push(Expression::BinaryOp(
                        op.clone(),
                        Box::new(lhs),
                        Box::new(rhs),
                    ));
                }
                StackInstruction::Return => {}
            }
        }

        let return_expr = expr_stack.pop().expect("No return expression");

        Ok(Box::new(CFunctionAst {
            return_type: stack_ast.return_type.clone(),
            name: stack_ast.name.clone(),
            parameters: stack_ast.parameters.clone(),
            body: vec![Statement::Return(return_expr)],
        }))
    }
}

struct CCodeGenerator;

impl CodeGenerator for CCodeGenerator {
    fn generate(&self, ir: &dyn IrType) -> Result<AnyIrType> {
        let c_function = ir
            .as_any()
            .downcast_ref::<CFunctionAst>()
            .ok_or(Diagnostic {
                severity: Severity::Error,
                message: "Expected CFunctionAst".to_string(),
                location: None,
                hints: vec![],
            })?;

        let mut code = String::new();

        code.push_str(&format!("{} {}(", c_function.return_type, c_function.name));

        for (i, param) in c_function.parameters.iter().enumerate() {
            if i > 0 {
                code.push_str(", ");
            }
            code.push_str(&format!("{} {}", param.type_name, param.name));
        }
        code.push_str(") {\n");

        for stmt in &c_function.body {
            match stmt {
                Statement::Return(expr) => {
                    code.push_str("  return ");
                    code.push_str(&expr_to_str(expr));
                    code.push_str(";\n");
                }
            }
        }

        code.push_str("}\n");

        Ok(Box::new(code))
    }
}

fn expr_to_str(expr: &Expression) -> String {
    match expr {
        Expression::BinaryOp(op, lhs, rhs) => {
            format!("({} {} {})", expr_to_str(lhs), op, expr_to_str(rhs))
        }
        Expression::Variable(name) => name.clone(),
        Expression::Literal(val) => val.clone(),
    }
}

#[test]
fn test() -> Result<()> {
    let pipeline = Pipeline {
        stages: vec![
            Box::new(LexerStage {
                lexer: Box::new(MyLexer),
            }),
            Box::new(ParserStage {
                parser: Box::new(StackParser),
            }),
            Box::new(IrTransformStage {
                transformer: Box::new(StackToCAstTransformer),
                input_key: "ast".to_string(),
                output_key: "c_ast".to_string(),
            }),
            Box::new(CodegenStage {
                codegen: Box::new(CCodeGenerator),
                input_key: "c_ast".to_string(),
                output_key: "c_code".to_string(),
            }),
        ],
    };

    let source = r#"
        int add(int a b) {
            a a b + + b + 1 2 + + return
        }
    "#;

    let ctx = match pipeline.compile(source.to_string()) {
        Ok(ctx) => ctx,
        Err(e) => return Err(e),
    };

    if !ctx.diagnostics.is_empty() {
        for diag in &ctx.diagnostics {
            println!("{diag:?}");
        }
    }

    let Some(ir) = ctx.irs.get("c_code") else {
        println!("c_code is not compiled");
        return Ok(());
    };
    let Some(code) = ir.as_ref().as_any().downcast_ref::<String>() else {
        println!("Can't cast c_code ir to string, got {:?}", ir.type_id());
        return Ok(());
    };
    println!("{code}");

    Ok(())
}
