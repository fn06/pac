Cargo frontend: the Concurrent Feature composition against a miniature index.

Semver requirement matching:
  $ pac semver-match 1.2.3 '^1'
  match
  $ pac semver-match 2.0.0 '^1'
  no-match
  $ pac semver-match 0.2.3 '^0.2'
  match
  $ pac semver-match 0.3.0 '^0.2'
  no-match
  $ pac semver-match 1.5.0 '>=1.2, <1.6'
  match
  $ pac semver-match 1.0.0-rc.1 '^1'
  no-match

Concurrent versions: the diamond installs both majors of common:
  $ pac cargo -r cargo-index -q 'diamond'
  common 1.0.0
  common 2.0.0
  diamond 1.0.0
  left 1.0.0
  right 1.0.0
  crates parsed: 4
  check: ok

Feature unification: two dependers' features meet on one instance:
  $ pac cargo -r cargo-index -q 'user1, user2'
  shared 1.0.0 (a b)
  user1 1.0.0
  user2 1.0.0
  crates parsed: 3
  check: ok

Optional deps appear only when a feature enables them (dep:extra):
  $ pac cargo -r cargo-index -q 'app'
  app 1.0.0
  crates parsed: 1
  check: ok
  $ pac cargo -r cargo-index -q 'app +plus'
  app 1.0.0 (plus)
  extra 1.0.0
  crates parsed: 2
  check: ok

dep/feat specs enable the dependency's feature:
  $ pac cargo -r cargo-index -q 'deepfeat +boost'
  deepfeat 1.0.0 (boost)
  shared 1.0.0 (a)
  crates parsed: 2
  check: ok

A split dependency's feature spec agrees with the chosen class (newest wins):
  $ pac cargo -r cargo-index -q 'spec +go'
  multi 2.0.0 (f)
  spec 1.0.0 (go)
  crates parsed: 2
  check: ok

cfg-conditional deps under the default target, another target, and free:
  $ pac cargo -r cargo-index -q 'condy'
  condy 1.0.0
  nixonly 1.0.0
  crates parsed: 3
  check: ok
  $ pac cargo -r cargo-index -q 'condy' --var target_family=windows
  condy 1.0.0
  winonly 1.0.0
  crates parsed: 3
  check: ok
  $ pac cargo -r cargo-index -q 'condy' --free target_family
  target_family = windows
  condy 1.0.0
  winonly 1.0.0
  crates parsed: 3
  check: ok

The links key is a conflict class: one crate per native library:
  $ pac cargo -r cargo-index -q 'needz'
  needz 1.0.0
  zlib-a 1.0.0
  crates parsed: 2
  check: ok
  $ pac cargo -r cargo-index -q 'zlib-a, zlib-b'
  Because zlib-a@1 1.0.0 -> <links:z> zlib-a@1 and zlib-b@1 1.0.0 -> <links:z> zlib-b@1, zlib-b@1 * or zlib-a@1 * is forbidden..
  And because root -> zlib-a@1 1.0.0 and root -> zlib-b@1 1.0.0, version solving failed.
  [1]

Even two classes of one crate cannot both link it:
  $ pac cargo -r cargo-index -q 'zsys ^0.9, zsys ^1'
  Because zsys@0.9 0.9.0 -> <links:z> zsys@0.9 and zsys@1 1.0.0 -> <links:z> zsys@1, zsys@1 * or zsys@0.9 * is forbidden..
  And because root -> zsys@0.9 0.9.0 and root -> zsys@1 1.0.0, version solving failed.
  [1]

Dev-dependencies apply only to the queried crates, behind --dev:
  $ pac cargo -r cargo-index -q 'tested'
  tested 1.0.0
  crates parsed: 1
  check: ok
  $ pac cargo -r cargo-index -q 'tested' --dev
  mockery 1.0.0
  tested 1.0.0
  crates parsed: 2
  check: ok
  $ pac cargo -r cargo-index -q 'usetest' --dev
  tested 1.0.0
  usetest 1.0.0
  crates parsed: 2
  check: ok
