extern crate meowscript;

use std::fs;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    input: String,
}

fn main() {
    let args = Args::parse();
    let file_content = fs::read_to_string(args.input).expect("Unable to read file");
    let compiled = meowscript::compile(&file_content);
    print!("Original: \n\n{}\n-----------------------\n", file_content);
    print!("Compiled: \n\n{}", compiled);
}
