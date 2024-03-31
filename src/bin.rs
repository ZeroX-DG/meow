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
    meowscript::compile(&file_content);
}
