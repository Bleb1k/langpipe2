use std::{
    any::{Any, TypeId},
    collections::HashMap,
};

use downcast_rs::Downcast;

pub type Result<T> = std::result::Result<T, Diagnostic>;

#[derive(Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub location: Option<SourceLocation>,
    pub hints: Vec<String>,
}

#[derive(Debug)]
pub enum Severity {
    Error,
    Warning,
    Info,
}

#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub file: String,
    pub line: u32,
    pub column: u32,
}

pub type AnyIrType = Box<dyn IrType>;

pub trait IrType: Downcast {
    fn validate(&self) -> Result<()>;
}

impl IrType for String {
    fn validate(&self) -> Result<()> {
        Ok(())
    }
}

pub struct CompilationContext {
    pub source: String,
    pub irs: HashMap<String, AnyIrType>,
    pub diagnostics: Vec<Diagnostic>,
}

pub trait CompilationStage {
    fn input_ir(&self) -> Option<String>;
    fn output_ir(&self) -> String;
    fn execute(&self, ctx: &mut CompilationContext) -> Result<()>;
}

#[derive(Debug)]
pub struct Token {
    pub kind: String,
    pub value: String,
    pub location: SourceLocation,
}

pub struct TokenIr {
    pub tokens: Vec<Token>,
}
impl IrType for TokenIr {
    fn validate(&self) -> Result<()> {
        if self.tokens.is_empty() {
            return Err(Diagnostic {
                severity: Severity::Error,
                message: "Empty token list".to_string(),
                location: None,
                hints: vec![],
            });
        }
        Ok(())
        // todo!("More sophisticated validation?")
    }
}

pub trait Lexer {
    fn lex(&self, source: &str) -> Result<AnyIrType>;
}

pub struct LexerStage {
    pub lexer: Box<dyn Lexer>,
}

impl CompilationStage for LexerStage {
    fn input_ir(&self) -> Option<String> {
        None
    }
    fn output_ir(&self) -> String {
        "tokens".to_string()
    }
    fn execute(&self, ctx: &mut CompilationContext) -> Result<()> {
        let tokens = self.lexer.lex(&ctx.source)?;
        tokens.validate()?;
        ctx.irs.insert("tokens".to_string(), tokens);
        Ok(())
    }
}

pub trait Parser {
    fn parse(&self, tokens: &dyn IrType) -> Result<AnyIrType>;
}

pub struct ParserStage {
    pub parser: Box<dyn Parser>,
}

impl CompilationStage for ParserStage {
    fn input_ir(&self) -> Option<String> {
        Some("tokens".to_string())
    }
    fn output_ir(&self) -> String {
        "ast".to_string()
    }
    fn execute(&self, ctx: &mut CompilationContext) -> Result<()> {
        let token_ir = ctx.irs.get("tokens").ok_or(Diagnostic {
            severity: Severity::Error,
            message: "Missing tokens IR".to_string(),
            location: None,
            hints: vec![],
        })?;
        let ast_ir = self.parser.parse(token_ir.as_ref())?;
        ast_ir.validate()?;
        ctx.irs.insert("ast".to_string(), ast_ir);
        Ok(())
    }
}

pub trait IrTransformer {
    fn transform(&self, from: &dyn IrType) -> Result<AnyIrType>;
}

pub struct IrTransformStage {
    pub transformer: Box<dyn IrTransformer>,
    pub input_key: String,
    pub output_key: String,
}

impl CompilationStage for IrTransformStage {
    fn input_ir(&self) -> Option<String> {
        Some(self.input_key.clone())
    }
    fn output_ir(&self) -> String {
        self.output_key.clone()
    }
    fn execute(&self, ctx: &mut CompilationContext) -> Result<()> {
        let from_ir = ctx.irs.get(&self.input_key).ok_or(Diagnostic {
            severity: Severity::Error,
            message: format!("Missing IR: {}", self.input_key),
            location: None,
            hints: vec![],
        })?;
        let to_ir = self.transformer.transform(from_ir.as_ref())?;
        to_ir.validate()?;
        ctx.irs.insert(self.output_key.clone(), to_ir);
        Ok(())
    }
}

pub trait OptimizationPass {
    fn run(&self, ir: &mut dyn IrType) -> Result<()>;
}

pub struct OptimizationPassStage {
    pass: Box<dyn OptimizationPass>,
    ir_key: String,
}

impl CompilationStage for OptimizationPassStage {
    fn input_ir(&self) -> Option<String> {
        Some(self.ir_key.clone())
    }
    fn output_ir(&self) -> String {
        self.ir_key.clone()
    }
    fn execute(&self, ctx: &mut CompilationContext) -> Result<()> {
        let ir_box = ctx.irs.get_mut(&self.ir_key).ok_or(Diagnostic {
            severity: Severity::Error,
            message: format!("Missing IR: {}", self.ir_key),
            location: None,
            hints: vec![],
        })?;
        self.pass.run(ir_box.as_mut())?;
        Ok(())
    }
}

pub trait CodeGenerator {
    fn generate(&self, ir: &dyn IrType) -> Result<AnyIrType>;
}

pub struct CodegenStage {
    pub codegen: Box<dyn CodeGenerator>,
    pub input_key: String,
    pub output_key: String,
}

impl CompilationStage for CodegenStage {
    fn input_ir(&self) -> Option<String> {
        Some(self.input_key.clone())
    }
    fn output_ir(&self) -> String {
        self.output_key.clone()
    }
    fn execute(&self, ctx: &mut CompilationContext) -> Result<()> {
        let ir = ctx.irs.get(&self.input_key).ok_or(Diagnostic {
            severity: Severity::Error,
            message: format!("Missing IR: {}", self.input_key),
            location: None,
            hints: vec![],
        })?;
        // println!("{:?}", ir);
        let machine_code_ir = self.codegen.generate(ir.as_ref())?;
        machine_code_ir.validate()?;
        ctx.irs.insert(self.output_key.clone(), machine_code_ir);
        Ok(())
    }
}

pub struct Pipeline {
    pub stages: Vec<Box<dyn CompilationStage>>,
}

impl Pipeline {
    pub fn compile(&self, source: String) -> Result<CompilationContext> {
        let mut ctx = CompilationContext {
            source,
            irs: HashMap::new(),
            diagnostics: vec![],
        };
        for stage in &self.stages {
            if let Err(diag) = stage.execute(&mut ctx) {
                println!("{:?}", diag);
                ctx.diagnostics.push(diag);
                // continue or abort based on severity
            }
        }
        Ok(ctx)
    }
}
