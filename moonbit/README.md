# moonbit raybench

Thoughts while building:

- The compiler complains if mutation is indicated but not used
- For pretty print use `derive(Show)`
- What is with the `///|` injected but the formatter all over the place? Seems to be doc strings
- No global mutability, cool
- You don't specify the name of the struct when creating a new instance of it
- All unused stuff will emit a warning from the compiler
- VSCode integration is great, includes formatter and intellisense
- It has a great type system, the VSCode integration will show you the type of a variable as soon as it is known
- No semicolon
- Trying to print something with no Show trait will show you an error in VSCode, no nede to compile
- Compilation is very fast
- Runtime speed is OK, almost 300% of rust
- Types are inferred most of the time, but you can also hint the compiler using `Type::{}`
