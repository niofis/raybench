#!/usr/bin/env run-cargo-script
//! Install cargo-script first:
//! cargo install cargo-script
//!
//! ```cargo
//! [dependencies]
//! time = "0.1"
//! clap = "2"
//! ```

extern crate clap;
extern crate time;
use clap::{App, AppSettings, Arg, SubCommand};
use std::fs::File;
use std::io::{BufWriter, Write};
use std::process::Command;

fn compile_run(name: &str, compile: &str, run: &str, ppm: &str) -> (String, f64, String) {
    println!("{}\nCompiling...", &name);
    let args: Vec<&str> = compile.split(" ").collect();
    Command::new(args[0])
        .args(&args[1..])
        .spawn()
        .expect("compilation did not succeed")
        .wait()
        .expect("wait");

    println!("Running...");
    let start = time::precise_time_s();
    let args: Vec<&str> = run.split(" ").collect();
    let output = Command::new(args[0])
        .args(&args[1..])
        .output()
        .expect("failed to run");
    let end = time::precise_time_s();
    let elapsed = end - start;
    println!("Running time: {:.4}s", elapsed);

    let ppm_string = String::from_utf8_lossy(&output.stdout);
    let mut ppm_file = BufWriter::new(File::create(ppm).expect("error creating file"));
    write!(ppm_file, "{}", &ppm_string).expect("write ppm");

    (name.to_string(), elapsed, ppm_string.to_string())
}

fn simply_run(name: &str, run: &str, ppm: &str) -> (String, f64, String) {
    println!("{}\nRunning...", &name);
    let start = time::precise_time_s();
    let args: Vec<&str> = run.split(" ").collect();
    let output = Command::new(args[0])
        .args(&args[1..])
        .output()
        .expect("failed to run");
    let end = time::precise_time_s();
    let elapsed = end - start;
    println!("Running time: {:.4}s", elapsed);

    let ppm_string = String::from_utf8_lossy(&output.stdout);
    let mut ppm_file = BufWriter::new(File::create(ppm).expect("error creating file"));
    write!(ppm_file, "{}", &ppm_string).expect("write ppm");

    (name.to_string(), elapsed, ppm_string.to_string())
}

fn baseline() -> (String, f64, String) {
    compile_run(
        "Baseline (C lang)",
        "gcc crb.c -o baseline -std=c11 -O3 -lm",
        "./baseline",
        "baseline.ppm",
    )
}

fn c_lang() -> (String, f64, String) {
    compile_run("C", "gcc crb.c -o crb -std=c11 -O3 -lm", "./crb", "crb.ppm")
}

fn rust_lang() -> (String, f64, String) {
    compile_run(
        "Rust Alt",
        "rustc rsrb_alt.rs -o rsrb_alt -O",
        "./rsrb_alt",
        "rsrb_alt.ppm",
    )
}

fn rust_lang_2() -> (String, f64, String) {
    compile_run(
        "Rust Alt 2",
        "rustc rsrb_alt_2.rs -o rsrb_alt_2 -O",
        "./rsrb_alt_2",
        "rsrb_alt_2.ppm",
    )
}

fn go_lang() -> (String, f64, String) {
    compile_run("Go", "go build gorb.go", "./gorb", "gorb.ppm")
}

fn js_lang() -> (String, f64, String) {
    simply_run("Javascript", "node jsrb.js", "jsrb.ppm")
}

fn cs_lang() -> (String, f64, String) {
    compile_run("C#", "sh ./csrb/compile.sh", "sh ./csrb/run.sh", "csrb.ppm")
}

fn nim_lang() -> (String, f64, String) {
    compile_run("Nim", "nim c -d:release nimrb.nim", "./nimrb", "nimrb.ppm")
}

fn wren_lang() -> (String, f64, String) {
    simply_run("Wren", "wren_cli wrenrb.wren", "wrenrb.ppm")
}

fn lua_lang() -> (String, f64, String) {
    simply_run("Lua", "lua luarb.lua", "luarb.ppm")
}

fn swift_lang() -> (String, f64, String) {
    compile_run(
        "Swift",
        "swiftc swrb.swift -o swrb -Ounchecked -lm",
        "./swrb",
        "swrb.ppm",
    )
}

fn main() {
    let matches = App::new("raybench runner")
        .version("0.1")
        .author("Enrique <niofis@gmail.com>")
        .about("Compiles runs and compares different raybench tests.\nAvailable implementations: c, rust, js, go, cs, nim, wren, lua, swift")
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
            let mut results: Vec<(String, f64, String)> = langs
                .split(",")
                .filter_map(|lang_str| {
                    let lang = lang_str.trim();
                    if lang == "c" {
                        return Some(c_lang());
                    } else if lang == "rust" {
                        return Some(rust_lang());
                    } else if lang == "rust2" {
                        return Some(rust_lang_2());
                    } else if lang == "go" {
                        return Some(go_lang());
                    } else if lang == "js" {
                        return Some(js_lang());
                    } else if lang == "cs" {
                        return Some(cs_lang());
                    } else if lang == "nim" {
                        return Some(nim_lang());
                    } else if lang == "wren" {
                        return Some(wren_lang());
                    } else if lang == "lua" {
                        return Some(lua_lang());
                    } else if lang == "swift" {
                        return Some(swift_lang());
                    } else {
                        return None;
                    }
                })
                .collect();
            results.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());
            results
                .iter()
                .for_each(|(name, elapsed, _)| println!("{:7} \t {:.4}s", name, elapsed));
        }
    }
}
