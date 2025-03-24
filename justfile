build:
    cabal build

test:
    ghcid markdownrealcode.cabal -c "cabal repl test:mdrc-tests" --test="Main.main"

test-run:
    cabal test --test-show-details=always --test-option=--color

fmt:
    find src test -name '*.hs' | xargs ormolu -i

lint:
    hlint src test

clean:
    cabal clean

try:
    cabal run mdrc-cli

watch:
    cabal run mdrc-cli -- -w

vsix:
    cd vsix && bun install
    ./vsix/build.bun.ts

install:
    just build
    cp $(cabal list-bin mdrc-cli) ~/.local/bin/markdownrealcode

uninstall:
    rm ~/.local/bin/markdownrealcode
