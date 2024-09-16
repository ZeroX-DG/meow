extern crate meowscript;

use std::{fs, path::Path};

use clap::Parser;
use codespan_reporting::term::{
    self,
    termcolor::{ColorChoice, StandardStream},
};
use meowscript::{
    compiler::{CompileErrorType, JavaScriptCompiler, ProgramChecker},
    parser::ParsingErrorType,
};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    input: String,
}

fn main() {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();
    let args = Args::parse();
    let file_path = args.input.clone();
    let file_content = fs::read_to_string(args.input).expect("Unable to read file");
    let tokens = meowscript::lexer::Lexer::tokenize(&file_content).expect("Error while tokenizing");
    let ast = match meowscript::parser::Parser::parse(tokens) {
        Ok(program) => program,
        Err(error) => {
            let error = match error.error_type().clone() {
                ParsingErrorType::UnexpectedToken(token) => error.add_file(
                    Path::new(&file_path)
                        .file_name()
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .to_string(),
                    file_content,
                    token.span,
                ),
                ParsingErrorType::MissingSemicolon(span) => error.add_file(
                    Path::new(&file_path)
                        .file_name()
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .to_string(),
                    file_content,
                    span,
                ),
            };

            term::emit(
                &mut writer.lock(),
                &config,
                error.files(),
                error.diagnostic(),
            )
            .unwrap();
            return;
        }
    };

    let mut program_checker = ProgramChecker::new();
    if let Err(error) = program_checker.check(&ast) {
        let error = match error.error_type().clone() {
            CompileErrorType::Reassignment(span) => error.add_file(
                Path::new(&file_path)
                    .file_name()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_string(),
                file_content,
                span,
            ),
        };

        term::emit(
            &mut writer.lock(),
            &config,
            error.files(),
            error.diagnostic(),
        )
        .unwrap();
        return;
    }

    // println!("{}", serde_json::to_string_pretty(&ast).unwrap());

    let output = meowscript::compiler::Compiler::<JavaScriptCompiler>::compile(ast);

    println!("Compiled:");
    println!("{}", output);
    println!("--------");
    println!("Execution:");

    let executed = std::process::Command::new("node")
        .arg("-e")
        .arg(output)
        .output()
        .unwrap();

    println!("{}", unsafe {
        String::from_utf8_unchecked(executed.stdout)
    });
}
