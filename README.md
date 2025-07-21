# langpipe

`langpipe` is a flexible and extensible Rust library designed for building custom compilers and interpreters. It
provides a structured framework for creating a compilation pipeline that includes lexing, parsing, transforming
intermediate representations (IR), optimizing, and generating target code. With `langpipe`, you can easily define your
own programming languages and compile them into various targets, such as WASM, MLIR, LLVM, Beam, JVM Bytecode, C, and
more.

## Features

- **Modular Architecture**: The library is built around a modular design, allowing you to implement custom lexers,
  parsers, transformers, optimizers, code generators, and even your own custom compilation steps.
- **Intermediate Representation (IR)**: Supports defining and validating multiple IR types, enabling complex
  transformations and optimizations.
- **Diagnostic System**: Provides a robust diagnostic system for error reporting, including severity levels (Error,
  Warning, Info) and detailed messages with source locations.
- **Extensible**: Easily extend the library by implementing the provided traits for your specific language features and
  compilation needs.
- **Example Included**: The crate includes a complete example of a simple stack-based language that compiles to C,
  demonstrating the capabilities of the library. Check the `tests` folder for usage examples.

## Usage

To use `langpipe`, add the following to your `Cargo.toml`:

```bash
[dependencies]
langpipe = "0.1.0"  # Replace with the latest version
```

## Contributing

Contributions are welcome! If you have suggestions for improvements or new features, feel free to open an issue or
submit a pull request.
