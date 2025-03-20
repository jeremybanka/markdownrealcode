# Run tests in watch mode using ghcid
test:
    ghcid --command="cabal repl test:mdrc-tests" --test="main"

# Build the project
build:
    cabal build

# Run the test suite once
test-run:
    cabal test --test-show-details=always --test-option=--color

# Format Haskell source files using ormolu
fmt:
    find src test -name '*.hs' | xargs ormolu -i

# Lint Haskell source files with hlint
lint:
    hlint src test

# Clean up generated files
clean:
    cabal clean

try:
    cabal run mdrc-cli
