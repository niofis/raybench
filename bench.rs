#!/usr/bin/env run-cargo-script
//! Install cargo-script first:
//! cargo install cargo-script
//! cargo script bench.rs
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

fn c_lang() -> (String, f64, String) {
    compile_run("C", "sh ./c/compile.sh", "sh ./c/run.sh", "./tmp/crb.ppm")
}

fn rust_lang() -> (String, f64, String) {
    compile_run(
        "Rust Alt",
        "sh ./rust/compile.sh",
        "sh ./rust/run.sh",
        "./tmp/rsrb_alt.ppm",
    )
}

fn go_lang() -> (String, f64, String) {
    compile_run(
        "Go",
        "sh ./go/compile.sh",
        "sh ./go/run.sh",
        "./tmp/gorb.ppm",
    )
}

fn java_lang() -> (String, f64, String) {
    compile_run(
        "Java",
        "sh ./java/compile.sh",
        "sh ./java/run.sh",
        "./tmp/javarb.ppm",
    )
}

fn scala_lang() -> (String, f64, String) {
    compile_run(
        "Scala",
        "sh ./scala/compile.sh",
        "sh ./scala/run.sh",
        "./tmp/scalarb.ppm",
    )
}

fn haxe_lang() -> (String, f64, String) {
    simply_run("Haxe", "sh ./haxe/run.sh", "./tmp/haxerb.ppm")
}

fn js_lang() -> (String, f64, String) {
    simply_run("Javascript", "sh ./javascript/run.sh", "./tmp/jsrb.ppm")
}

fn cs_lang() -> (String, f64, String) {
    compile_run(
        "C#",
        "sh ./csharp/compile.sh",
        "sh ./csharp/run.sh",
        "./tmp/csrb.ppm",
    )
}

fn nim_lang() -> (String, f64, String) {
    compile_run(
        "Nim",
        "sh ./nim/compile.sh",
        "sh ./nim/run.sh",
        "./tmp/nimrb.ppm",
    )
}

fn wren_lang() -> (String, f64, String) {
    simply_run("Wren", "sh ./wren/run.sh", "./tmp/wrenrb.ppm")
}

fn lua_lang() -> (String, f64, String) {
    simply_run("Lua", "sh ./lua/run.sh", "./tmp/luarb.ppm")
}

fn luajit_lang() -> (String, f64, String) {
    simply_run("LuaJIT", "sh ./lua/run_jit.sh", "./tmp/luarbjit.ppm")
}

fn swift_lang() -> (String, f64, String) {
    compile_run(
        "Swift",
        "sh ./swift/compile.sh",
        "sh ./swift/run.sh",
        "./tmp/swrb.ppm",
    )
}

fn zig_lang() -> (String, f64, String) {
    compile_run(
        "Zig",
        "sh ./zig/compile.sh",
        "sh ./zig/run.sh",
        "./tmp/zigrb.ppm",
    )
}

fn main() {
    let matches = App::new("raybench runner")
        .version("0.1")
        .author("Enrique <niofis@gmail.com>")
        .about("Compiles runs and compares different raybench tests.\nAvailable implementations: c, rust, js, go, cs, nim, wren, lua, luajit, swift, haxe, java, scala")
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

    if let Some(matches) = matches.subcommand_matches("run") {
        if let Some(langs) = matches.value_of("implementations") {
            let mut results: Vec<(String, f64, String)> = langs
                .split(",")
                .filter_map(|lang_str| {
                    let lang = lang_str.trim();
                    if lang == "c" {
                        return Some(c_lang());
                    } else if lang == "rust" {
                        return Some(rust_lang());
                    } else if lang == "go" {
                        return Some(go_lang());
                    } else if lang == "haxe" {
                        return Some(haxe_lang());
                    } else if lang == "js" {
                        return Some(js_lang());
                    } else if lang == "java" {
                        return Some(java_lang());
                    } else if lang == "cs" {
                        return Some(cs_lang());
                    } else if lang == "nim" {
                        return Some(nim_lang());
                    } else if lang == "wren" {
                        return Some(wren_lang());
                    } else if lang == "lua" {
                        return Some(lua_lang());
                    } else if lang == "luajit" {
                        return Some(luajit_lang());
                    } else if lang == "swift" {
                        return Some(swift_lang());
                    } else if lang == "scala" {
                        return Some(scala_lang());
                    } else if lang == "zig" {
                        return Some(zig_lang());
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
