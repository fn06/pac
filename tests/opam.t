opam frontend: filtered package formulas encoded to the core calculus, with
variables as synthetic packages (pinned to an assignment, or solver-chosen).

Version constraints, availability (libjson 2.1 is available: false), the first
alternative of a disjunction, and an os-conditional dependency under the
default os=linux:
  $ pac opam -r opam-repo -q 'app'
  app.1.0
  db-sqlite.1.0
  libjson.2.0
  unixlib.1.0
  check: ok

A global with-test assignment is resolution-wide, pulling test dependencies
of dependencies too (libjson's bench):
  $ pac opam -r opam-repo -q 'app' --var with-test=true
  app.1.0
  bench.1.0
  db-sqlite.1.0
  libjson.2.0
  testfw.1.0
  unixlib.1.0
  check: ok

--with-test scopes it to the queried packages, as opam does (app's testfw,
but not libjson's bench):
  $ pac opam -r opam-repo -q 'app' --with-test
  app.1.0
  db-sqlite.1.0
  libjson.2.0
  testfw.1.0
  unixlib.1.0
  check: ok
  $ pac opam -r opam-repo -q 'libjson' --with-test
  bench.1.0
  libjson.2.0
  check: ok

A different assignment takes the other conditional branch:
  $ pac opam -r opam-repo -q 'app' --var os=win32
  app.1.0
  db-sqlite.1.0
  libjson.2.0
  winlib.1.0
  check: ok

Conflicts: oldlib conflicts libjson >= 2.0, so alone it downgrades:
  $ pac opam -r opam-repo -q 'oldlib, libjson'
  libjson.1.5
  oldlib.1.0
  check: ok

but together with app (which needs >= 2.0) it is unsatisfiable:
  $ pac opam -r opam-repo -q 'oldlib, app'
  Because app 1.0 -> libjson 2.0 and libjson 2.0 -> <oldlib.1.0 !!0> 0, app * requires <oldlib.1.0 !!0> 0.
  And because oldlib 1.0 -> <oldlib.1.0 !!0> 1, oldlib * or app * is forbidden.
  And because root -> oldlib 1.0 and root -> app 1.0, version solving failed.
  [1]

conflict-class: two cli-tools cannot coexist:
  $ pac opam -r opam-repo -q 'tool-a, tool-b'
  Because tool-a 1.0 -> <class cli-tool tool-a> 1 and tool-b 1.0 -> <class cli-tool tool-a> 0, tool-b * or tool-a * is forbidden..
  And because root -> tool-a 1.0 and root -> tool-b 1.0, version solving failed.
  [1]

Free variables: gui wants gtk (linux-only) or cocoa (macos-only). Under
os=freebsd both branches are filtered out and the disjunction is vacuously
satisfied, as in opam itself (cf. the win32-only groups in the ocaml
package); left free, the solver picks an os and takes a real branch:
  $ pac opam -r opam-repo -q 'gui' --var os=freebsd
  gui.1.0
  check: ok
  $ pac opam -r opam-repo -q 'gui' --free os
  os = linux
  gtk.1.0
  gui.1.0
  check: ok

The version pseudo-variable pins siblings to the depender's own version:
  $ pac opam -r opam-repo -q 'sibling-a'
  sibling-a.2.0
  sibling-b.2.0
  check: ok
