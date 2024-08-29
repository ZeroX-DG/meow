extern crate meowscript;

use std::fs;

use clap::Parser;
use meowscript::compiler::JavaScriptCompiler;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    input: String,
}

fn main() {
    let args = Args::parse();
    let file_content = fs::read_to_string(args.input).expect("Unable to read file");
    let tokens = meowscript::lexer::Lexer::tokenize(&file_content).expect("Error while tokenizing");
    let ast = meowscript::parser::Parser::parse(tokens).expect("Error while parsing tokens");

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
