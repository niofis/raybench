extern crate clap;
extern crate time;
use clap::{App, AppSettings, Arg, SubCommand};
use std::fs::File;
use std::io::{BufWriter, Write};
use std::process::Command;

fn compile_run(
    name: &str,
    compiler: &str,
    args: Vec<&str>,
    run: &str,
    ppm: &str,
) -> (String, f64, String) {
    println!("{}\nCompiling...", &name);
    Command::new(compiler)
        .args(&args)
        .spawn()
        .expect("compilation did not succeed")
        .wait();

    println!("Running...");
    let start = time::precise_time_s();
    let output = Command::new(run).output().expect("failed to run");
    let end = time::precise_time_s();
    let elapsed = end - start;
    println!("Running time: {:.4}s", elapsed);

    let ppm_string = String::from_utf8_lossy(&output.stdout);
    let mut ppm_file = BufWriter::new(File::create(ppm).expect("error creating file"));
    write!(ppm_file, "{}", &ppm_string);

    (name.to_string(), elapsed, ppm_string.to_string())
}

fn baseline() -> (String, f64, String) {
    compile_run(
        "Baseline (C lang)",
        "gcc",
        vec!["crb.c", "-o", "baseline", "-std=c11", "-O3", "-lm"],
        "./baseline",
        "baseline.ppm",
    )
}

fn c_lang() -> (String, f64, String) {
    compile_run(
        "C",
        "gcc",
        vec!["crb.c", "-o", "crb", "-std=c11", "-O3", "-lm"],
        "./crb",
        "crb.ppm",
    )
}

fn rust_lang() -> (String, f64, String) {
    compile_run(
        "Rust Alt",
        "rustc",
        vec!["rsrb_alt.rs", "-o", "rsrb_alt", "-O"],
        "./rsrb_alt",
        "rsrb_alt.ppm",
    )
}

fn go_lang() -> (String, f64, String) {
    compile_run("Go", "go", vec!["build", "gorb.go"], "./gorb", "gorb.ppm")
}

fn main() {
    let matches = App::new("raybench runner")
        .version("0.1")
        .author("Enrique <niofis@gmail.com>")
        .about("Compiles runs and compares different raybench tests.\nAvailable implementations: c, rust, js, go, cs")
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
            let results: Vec<(String, f64, String)> = langs
                .split(",")
                .filter_map(|lang| {
                    if lang == "c" {
                        return Some(c_lang());
                    } else if lang == "rust" {
                        return Some(rust_lang());
                    } else if lang == "go" {
                        return Some(go_lang());
                    } else {
                        return None;
                    }
                })
                .collect();
            results
                .iter()
                .for_each(|(name, elapsed, _)| println!("{:7} \t {:.4}s", name, elapsed));
        }
    }
}
