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

## opam frontend

Reduces an opam-repository checkout to the core calculus lazily. Nested
`&`/`|` dependency formulas become synthetic disjunct packages; filters go
through the variable-formula reduction as synthetic `<x>` packages, pinned to
an assignment (`--var os=macos`) or left for the solver to choose
(`--free os`); conflicts and conflict-classes become guard packages. Depopts
are correctly ignored (opam 2 semantics). `--with-test`/`--with-doc` enable
the variable for the queried packages only, as opam does — a root-only shim
substitutes it into their formulas before encoding, so the global `<x>`
package stays false and dependencies' test deps stay out (`--var
with-test=true` remains available as the resolution-wide assignment).
Resolutions are checked against the elaborated opam semantics.

```sh
pac opam -r ~/projects/opam-repository -q 'lwt (>= 5.5), dune'
pac opam -r ~/projects/opam-repository -q 'eio' --with-test
pac opam -r tests/opam-repo -q 'gui' --free os   # solver picks the os
```

## Cargo frontend

Reduces the crates.io index to the core calculus — the Concurrent Feature
composition: one synthetic package per semver-compatibility class (majors, and
0.x minors, coexist), feature packages whose versions form the support
relation (unification per selected instance), split intermediates choosing a
class with agreement selectors for `dep/feat` specs, cfg conditions through
the variable machinery, and the `links` key as a conflict class — a synthetic
package per native library whose versions are the claimant crates, discovered
lazily. Fully lazy: only the crates the solver visits are ever parsed. Weak
feature dependencies (`x?/f`) are omitted from the solve, as in cargo itself,
which resolves versions as if all features were enabled and applies weak
features in a post-resolution feature pass. `--dev` includes the
dev-dependencies of the queried crates — never transitive, so they hang off a
root-only shim agreeing with the crate's class choice.

```sh
git clone https://github.com/rust-lang/crates.io-index ~/projects/crates.io-index
pac cargo -r ~/projects/crates.io-index -q 'serde +derive'
pac cargo -r ~/projects/crates.io-index -q 'tokio =1.35.0 +full, axum'
pac cargo -r tests/cargo-index -q 'condy' --free target_family
pac semver-match 1.0.0-rc.1 '^1'
```
