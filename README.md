# Haskell Brainfuck Interpreter
A Brainfuck interpreter written in Haskell

## Getting Started
1. ghc, cabal, and the Haskell language server:

    ```curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh```

2. Install Parsec:

    ```cabal install parsec```

3. Build the interpreter by running `make`
4. Run the interpreter:

    ```./main FILENAME```

where `FILENAME` is the name of the .b file you would like to execute