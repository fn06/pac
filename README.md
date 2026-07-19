# Pac

The Package Calculus: cross-ecosystem dependency resolution by reduction to a
common core, solved with [PubGrub](https://github.com/RyanGibb/ocaml-pubgrub).

## Build

```sh
opam pin add pubgrub ~/projects/ocaml-pubgrub  # or a git pin
dune build && dune runtest
```

## Usage

`.pac` instances (core and concurrent calculi):

```sh
pac parse -f tests/example.pac
pac solve -f tests/example.pac -q 'A 1'
pac check -f tests/example.pac -q 'A 1' -r 'A 1,B 1,C 1,D 2' -c core
pac reduce -f tests/concurrent.pac -g major --from concurrent --to core
```

## Debian frontend

Reduces a `Packages` file to the core calculus lazily -- version constraints,
alternatives, Provides, and Conflicts/Breaks become synthetic core packages,
computed per lookup as the solver asks for them -- and solves with unmodified
PubGrub. Resolutions are checked against the original Debian semantics.

```sh
curl -sO http://deb.debian.org/debian/dists/bookworm/main/binary-amd64/Packages.xz
xz -d Packages.xz
pac debian -f Packages -q 'libreoffice, postfix (>= 3.7)'
pac debian -f Packages -q 'postfix, exim4'   # unsatisfiable, with explanation
pac deb-compare 1.0~rc1 1.0                  # dpkg version ordering
```
