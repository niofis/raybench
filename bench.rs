extern crate clap;
extern crate time;
use clap::{App, AppSettings, Arg, SubCommand};
use std::fs::File;
use std::io::{BufWriter, Write};
use std::process::Command;

fn baseline() {
    println!("Compiling...");
    Command::new("gcc")
        .args(&["crb.c", "-o", "crb", "-std=c11", "-O3", "-lm"])
        .spawn()
        .expect("compilation did not succeed")
        .wait();

    println!("Running...");
    let start = time::precise_time_s();
    let output = Command::new("./crb").output().expect("crb failed to run");
    let end = time::precise_time_s();
    println!("Baseline C running time: {:.}s", end - start);

    let ppm_string = String::from_utf8_lossy(&output.stdout);
    let mut ppm = BufWriter::new(File::create("baseline.ppm").expect("Error creating file"));
    write!(ppm, "{}", ppm_string);
    println!("Saved baseline.ppm file");
}

fn main() {
    let matches = App::new("raybench runner")
        .version("0.1")
        .author("Enrique <niofis@gmail.com>")
        .about("Compiles runs and compares different raybench tests.\nAvailable implementations: c, rust, js, cs, python")
        .subcommand(
            SubCommand::with_name("baseline")
            .about("builds and runs the baseline C implementation")
        )
        .subcommand(
            SubCommand::with_name("run")
                .about("runs and compares the implementations specified")
                .arg(
                    Arg::with_name("implementations")
                        .required(true)
                        .help("one or multiple benchmarks separated by commas")
                        .takes_value(true),
                ),
        )
        .setting(AppSettings::ArgRequiredElseHelp)
        .get_matches();

    if let Some(_matches) = matches.subcommand_matches("baseline") {
        baseline();
    } else if let Some(matches) = matches.subcommand_matches("run") {
        if let Some(langs) = matches.value_of("implementations") {
            println!("{:?}", langs);
        }
    }
}
