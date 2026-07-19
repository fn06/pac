Debian frontend: reduce a Packages file to the core calculus lazily and solve
with unmodified pubgrub.

Versioned dependency, virtual selection, and mutual exclusion via
provide-and-conflict on a shared virtual name (postfix vs exim4):
  $ pac debian -f Packages.test -q 'web-app'
  libhttp 1.3-2
  openssl-shim 3.0-4
  postfix 3.7-1
  web-app 2.0-1
  check: ok

Newest satisfying version is preferred (libhttp 1.3-2, web-app 2.0-1):
  $ pac debian -f Packages.test -q 'web-app (>= 2.0)'
  libhttp 1.3-2
  openssl-shim 3.0-4
  postfix 3.7-1
  web-app 2.0-1
  check: ok

First alternative wins (nano, not vim or ed):
  $ pac debian -f Packages.test -q 'editor-user'
  editor-user 1.0
  nano 7.2-1
  check: ok

Later alternatives are reachable when the first is excluded (nano-hater
conflicts with nano, so editor-user falls back to vim):
  $ pac debian -f Packages.test -q 'editor-user, nano-hater'
  editor-user 1.0
  nano-hater 1.0
  vim 9.0-1
  check: ok

Versioned Provides: only openssl-shim provides tls-provider >= 2.0:
  $ pac debian -f Packages.test -q 'needs-new-tls'
  needs-new-tls 1.0
  openssl-shim 3.0-4
  check: ok

Breaks forces the older web-app alongside legacy-app:
  $ pac debian -f Packages.test -q 'legacy-app, web-app'
  legacy-app 1.0
  libhttp 1.0-1
  web-app 1.0-1
  check: ok

Both mail transport agents at once is unsatisfiable:
  $ pac debian -f Packages.test -q 'postfix, exim4'
  Because exim4 4.96-1 -> <exim4 4.96-1 !! mail-transport-agent> 1 and postfix 3.7-1 -> <exim4 4.96-1 !! mail-transport-agent> 0, exim4 * or postfix * is forbidden..
  And because root -> postfix 3.7-1 and root -> exim4 4.96-1, version solving failed.
  [1]

Unsatisfiable version range:
  $ pac debian -f Packages.test -q 'libhttp (>> 9)'
  root -> libhttp ∅
  [1]
