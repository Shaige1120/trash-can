extern crate colored;

use std::path::Path;
// use std::io::prelude::*;
use std::fs::File;
use std::env;
use std::io::{self, Write};
use self::colored::*;


// use lexer::*;
// use parser::*;
// use var::*;

mod lexer;
mod parser;
mod var;

fn basic(src: &str, vars: &mut Vec<var::Var>) {

    let mut tokens = lexer::lexer(src.clone());
    if tokens.is_empty() { return; }
    let fff = parser::parser(&mut tokens, vars);


    match fff {
        Ok(x) => println!("Out: {}", x),
        Err(r) => println!("{}: {}", "Err".red(), r),
    };
}

fn repl(do_init: bool){
    let mut inp = String::new();
    let mut vars: Vec<var::Var> = vec![];
    
    if do_init { var::init_var(&mut vars) };

    loop {
        print!("In: ");
        io::stdout().flush().unwrap();

        match io::stdin().read_line(&mut inp) {
            Ok(_) =>
            {
                basic(&inp, &mut vars);
                // let mut tokens = lexer::lexer(&inp);
                // let res = parser::parser(&mut tokens, &mut vars);
                //
                // match res {
                //     Ok(x) => {
                //         print!("Out:");
                //         println!(" {}", x);
                //     },
                //     Err(r) => {
                //         print!("{}", "Err".red());
                //         println!(": {}", r)
                //     },
                // };
            }
            Err(er) => println!("Error occured:\n {}", er),
        }

        inp = String::new();
    }
}

fn file_read(fname: &str) {
    let path = Path::new(fname);
    let disp = path.display();
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut do_init = true;

    for i in args {
        if i == "--no-init" { do_init = false; }
    }

    repl(do_init);
}
