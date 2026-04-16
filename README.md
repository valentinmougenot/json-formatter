

# json-formatter

A JSON formatter written in Haskell

## Build

```bash
cabal build
```

## Usage

```bash
cabal run json-formatter -- <file> [--sort]
```

## Features

- Parses and pretty-prints JSON files
- Supports null, booleans, integers, floats, strings, arrays, and objects
- Handles string escape sequences
- Handles alphabetical field sort
