#!/usr/bin/env run-cargo-script
//! Install cargo-script first:
//! cargo install cargo-script
//! cargo script bench.rs -- run c -m
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

fn platform_details() -> (String, String) {
    let output = Command::new("sh")
    .arg("./os.sh")
    .output()
    .expect("failed to run");
    let os = String::from_utf8_lossy(&output.stdout).to_string().replace("\n", "");

    let output = Command::new("sh")
    .arg("./cpu.sh")
    .output()
    .expect("failed to run");
    let cpu = String::from_utf8_lossy(&output.stdout).to_string().replace("\n", "");

    (os, cpu)
}

fn compile_run(name: &str, path: &str, ppm: &str) -> (String, String, f64, String) {
    let output = Command::new("sh")
    .arg(format!("{}/version.sh", path))
    .output()
    .expect("failed to run");
    let version = String::from_utf8_lossy(&output.stdout).to_string().replace("\n", "");

    println!("{} ({})\nCompiling...", &name, &version);

    Command::new("sh")
        .arg(format!("{}/compile.sh", path))
        .spawn()
        .expect("compilation did not succeed")
        .wait()
        .expect("wait");

    println!("Running...");
    let start = time::precise_time_s();
    let output = Command::new("sh")
        .arg(format!("{}/run.sh", path))
        .output()
        .expect("failed to run");
    let end = time::precise_time_s();
    let elapsed = end - start;
    println!("Running time: {:.4}s", elapsed);

    let ppm_string = String::from_utf8_lossy(&output.stdout);
    let mut ppm_file = BufWriter::new(File::create(ppm).expect("error creating file"));
    write!(ppm_file, "{}", &ppm_string).expect("write ppm");

    (name.to_string(), version, elapsed, ppm_string.to_string())
}

fn simply_run(name: &str, path: &str, ppm: &str) -> (String, String, f64, String) {
    let output = Command::new("sh")
    .arg(format!("{}/version.sh", path))
    .output()
    .expect("failed to run");
    let version = String::from_utf8_lossy(&output.stdout).to_string().replace("\n", "");

    println!("{} ({})\nRunning...", &name, &version);

    let start = time::precise_time_s();
    let output = Command::new("sh")
        .arg(format!("{}/run.sh", path))
        .output()
        .expect("failed to run");
    let end = time::precise_time_s();
    let elapsed = end - start;
    println!("Running time: {:.4}s", elapsed);

    let ppm_string = String::from_utf8_lossy(&output.stdout);
    let mut ppm_file = BufWriter::new(File::create(ppm).expect("error creating file"));
    write!(ppm_file, "{}", &ppm_string).expect("write ppm");

    (name.to_string(), version, elapsed, ppm_string.to_string())
}

fn c_lang() -> (String, String, f64, String) {
    compile_run("C", "./c", "./tmp/crb.ppm")
}

fn rust_lang() -> (String, String, f64, String) {
    compile_run(
        "Rust Alt",
        "./rust",
        "./tmp/rsrb_alt.ppm",
    )
}

fn go_lang() -> (String, String, f64, String) {
    compile_run(
        "Go",
        "./go",
        "./tmp/gorb.ppm",
    )
}

fn java_lang() -> (String, String, f64, String) {
    compile_run(
        "Java",
        "./java",
        "./tmp/javarb.ppm",
    )
}

fn scala_lang() -> (String, String, f64, String) {
    compile_run(
        "Scala",
        "./scala",
        "./tmp/scalarb.ppm",
    )
}

fn haxe_lang() -> (String, String, f64, String) {
    simply_run("Haxe", "./haxe", "./tmp/haxerb.ppm")
}

fn factor_lang() -> (String, String, f64, String) {
    simply_run("Factor", "./factor", "./tmp/factor.ppm")
}

fn js_lang() -> (String, String, f64, String) {
    simply_run("Javascript", "./javascript", "./tmp/jsrb.ppm")
}

fn cs_lang() -> (String, String, f64, String) {
    compile_run(
        "C#",
        "./csharp",
        "./tmp/csrb.ppm",
    )
}

fn nim_lang() -> (String, String, f64, String) {
    compile_run(
        "Nim",
        "./nim",
        "./tmp/nimrb.ppm",
    )
}

fn wren_lang() -> (String, String, f64, String) {
    simply_run("Wren", "./wren", "./tmp/wrenrb.ppm")
}

fn lisp_lang() -> (String, String, f64, String) {
    compile_run(
        "Lisp",
        "./lisp",
        "./tmp/lisprb.ppm",
    )
}

fn lua_lang() -> (String, String, f64, String) {
    simply_run("Lua", "./lua", "./tmp/luarb.ppm")
}

fn luajit_lang() -> (String, String, f64, String) {
    simply_run("LuaJIT", "./luajit", "./tmp/luarbjit.ppm")
}

fn swift_lang() -> (String, String, f64, String) {
    compile_run(
        "Swift",
        "./swift",
        "./tmp/swrb.ppm",
    )
}

fn zig_lang() -> (String, String, f64, String) {
    compile_run(
        "Zig",
        "./zig",
        "./tmp/zigrb.ppm",
    )
}

fn wat_lang() -> (String, String, f64, String) {
    compile_run(
        "Webassembly",
        "./webassembly",
        "./tmp/wasmrt.ppm",
    )
}

fn odin_lang() -> (String, String, f64, String) {
    compile_run(
        "Odin",
        "./odin",
        "./tmp/odinrb.ppm",
    )
}

fn plain_results(results: Vec<(String, String, f64, String)>) {
    let (os, cpu) = platform_details();
            
    println!("{} ({})", os, cpu);
    results
        .iter()
        .for_each(|(name, version, elapsed, _)| println!("{:7} \t {:.4}s ({})", name, elapsed, version));
}

fn markdown_results(results: Vec<(String, String, f64, String)>) {
    let (os, cpu) = platform_details();
    
    println!("#### {} ({})", os, cpu);
    println!("|Language|Running Time|Version|");
    println!("|--------|----------------|-------|");
    results
        .iter()
        .for_each(|(name, version, elapsed, _)| println!("|{:7}|{:.4} (s)|{}|", name, elapsed, version));
}

fn main() {
    let matches = App::new("raybench runner")
        .version("0.1")
        .author("Enrique <niofis@gmail.com>")
        .about("Compiles runs and compares different raybench tests.\nAvailable implementations: c,rust,js,go,cs,nim,wren,lua,luajit,swift,haxe,java,scala,lisp,wasm,factor")
        .subcommand(
            SubCommand::with_name("run")
                .about("runs and compares the implementations specified")
                .arg(
                    Arg::with_name("implementations")
                        .required(true)
                        .help("one or multiple benchmarks separated by commas")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("markdown")
                        .long("markdown")
                        .short("m")
                        .required(false)
                        .help("prints results in markdown format")
                        .takes_value(false),
                ),
        )
        .setting(AppSettings::ArgRequiredElseHelp)
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("run") {
        if let Some(langs) = matches.value_of("implementations") {
            let mut results: Vec<(String, String, f64, String)> = langs
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
                    } else if lang == "lisp"{
                        return Some(lisp_lang());
                    } else if lang == "lua" {
                        return Some(lua_lang());
                    } else if lang == "luajit" {
                        return Some(luajit_lang());
                    } else if lang == "swift" {
                        return Some(swift_lang());
                    } else if lang == "scala" {
                        return Some(scala_lang());
                    } else if lang == "wasm" {
                        return Some(wat_lang());
                    } else if lang == "zig" {
                        return Some(zig_lang());
                    } else if lang == "odin" {
                        return Some(odin_lang());
                    } else if lang == "factor" {
                        return Some(factor_lang());
                    } else {
                        return None;
                    }
                })
                .collect();

            results.sort_by(|a, b| a.2.partial_cmp(&b.2).unwrap());
            
            if matches.is_present("markdown") {
                markdown_results(results);
            } else {
                plain_results(results);
            }
        }
    }
}
