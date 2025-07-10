# Hatch Compiler

A complete compiler implementation for the **Falcon** functional programming language, written in OCaml. This project demonstrates advanced compiler design principles by translating high-level functional code into optimized x86-64 assembly.

## 🚀 Features

### Language Features

- **Functional Programming**: First-class functions, closures, and recursion
- **Type System**: Static type checking with runtime validation
- **Data Structures**: Tuples with indexing and type checking
- **Concurrency**: Parallel execution with channel-based communication
- **Control Flow**: Conditional expressions and let bindings
- **Arithmetic**: Integer operations with overflow protection

### Compiler Features

- **Multi-phase Compilation**: Lexical analysis → Parsing → Semantic analysis → Code generation
- **Assembly Generation**: Direct x86-64 assembly output with NASM compatibility
- **Memory Management**: Automatic heap allocation and garbage collection
- **Error Handling**: Comprehensive compile-time and runtime error reporting
- **Optimization**: Stack frame optimization and register allocation

## 📋 Prerequisites

- **OCaml** (4.14.0 or later)
- **Dune** (3.0 or later)
- **Menhir** (3.0 or later)
- **NASM** (2.15 or later)
- **Clang** (12.0 or later)
- **Batteries** library for OCaml

## 🛠️ Installation

1. **Clone the repository**:

   ```bash
   git clone <repository-url>
   cd compiler
   ```

2. **Install OCaml dependencies**:

   ```bash
   opam install dune menhir batteries ounit2
   ```

3. **Build the project**:
   ```bash
   make build
   ```

## 🎯 Usage

### Compiling a Program

```bash
# Compile a .bird file to executable
./hatch test_code/example.bird

# Run the compiled program
./output/example.run
```

### Running Tests

```bash
# Run all tests
make test

# Run specific test file
./tests
```

### Project Structure

```
compiler/
├── src/
│   ├── language/          # Lexer, parser, and AST definitions
│   ├── compiler/          # Core compilation logic
│   ├── main/             # Entry point
│   └── tests/            # Test suite
├── resources/            # Runtime C code
├── test_code/           # Example programs
└── output/              # Generated assembly and executables
```

## 📝 Language Syntax

### Basic Types

```ocaml
42                    # Integer
true                  # Boolean
false                 # Boolean
(1, 2, 3)            # Tuple
```

### Arithmetic Operations

```ocaml
1 + 2                # Addition
3 - 1                # Subtraction
4 * 5                # Multiplication
after(3)             # Increment
before(5)            # Decrement
```

### Control Flow

```ocaml
if x = 0 then 1 else 2    # Conditional
let x = 5 in x + 1        # Let binding
```

### Functions

```ocaml
def factorial n =
  if n = 0 then 1 else
  n * factorial (n - 1)
end

factorial 5
```

### Tuples

```ocaml
let t = (1, true, 3) in
t[0] + t[2]              # Tuple indexing
```

### Concurrency

```ocaml
parallel(print(1))        # Parallel execution
send(channel, value)      # Send to channel
receive(channel)          # Receive from channel
sleep(1000)              # Sleep in milliseconds
```

### Type Checking

```ocaml
isint(42)                # Type checking
isbool(true)             # Type checking
istuple((1, 2))          # Type checking
```

## 🏗️ Architecture

### Compilation Pipeline

1. **Lexical Analysis** (`lexer.mll`)

   - Tokenizes source code using OCamllex
   - Handles comments, identifiers, and literals

2. **Parsing** (`parser.mly`)

   - Generates AST using Menhir parser generator
   - Implements operator precedence and associativity

3. **Semantic Analysis** (`wellformedness.ml`)

   - Type checking and scope validation
   - Error reporting for ill-formed programs

4. **Code Generation** (`compiler.ml`)
   - AST to x86-64 assembly translation
   - Register allocation and stack management
   - Runtime system integration

### Memory Layout

- **Tagged Values**: Integers (LSB=0), booleans (LSB=11), tuples (LSB=1), closures (LSB=1)
- **Heap Management**: Dynamic allocation for tuples and closures
- **Stack Frames**: Automatic stack frame management with alignment

### Runtime System

- **C Runtime**: Memory allocation, I/O, and system calls
- **Error Handling**: Structured error codes and messages
- **Concurrency**: Thread management and synchronization

## 🧪 Testing

The project includes 80+ comprehensive test cases covering:

- **Arithmetic Operations**: Addition, subtraction, multiplication
- **Control Flow**: Conditionals, let bindings, recursion
- **Functions**: Parameter passing, closures, mutual recursion
- **Tuples**: Creation, indexing, type checking
- **Concurrency**: Parallel execution, channels, synchronization
- **Error Handling**: Type errors, bounds checking, runtime failures

Run tests with:

```bash
make test
```

## 🔧 Development

### Adding New Features

1. **Language Extension**:

   - Add tokens to `lexer.mll`
   - Update grammar in `parser.mly`
   - Extend AST in `asts.ml`
   - Implement compilation in `compiler.ml`

2. **Optimization**:
   - Modify `compiler.ml` for code generation improvements
   - Update `assembly.ml` for new instruction patterns

### Debugging

- **Assembly Output**: Generated in `output/` directory
- **Runtime Errors**: Use `stopWithError` for structured error reporting
- **Memory Issues**: Check heap allocation and stack frame management

## 📊 Performance

The compiler generates optimized x86-64 assembly with:

- **Register Allocation**: Efficient use of x86-64 registers
- **Stack Optimization**: Minimal stack frame overhead
- **Memory Access**: Optimized memory access patterns
- **Function Calls**: Efficient calling conventions

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## 📄 License

This project is part of an academic compiler design course. Please refer to your institution's academic integrity policies.

## 🙏 Acknowledgments

- **OCaml Community**: For the excellent functional programming language
- **Menhir**: For the powerful parser generator
- **NASM**: For the x86-64 assembler
- **Academic Instructors**: For guidance in compiler design principles

---

**Note**: This compiler demonstrates advanced concepts in programming language design and implementation. It serves as both an educational tool and a foundation for more sophisticated language features.
