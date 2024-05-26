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
    let ast = meowscript::compile(&file_content);

    // Print out JSON for now so we can debug with a json viewer
    println!("{}", serde_json::to_string_pretty(&ast).unwrap());
}
