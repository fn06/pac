Alpine frontend: shared-object and command provides, priorities, !-conflicts.

so: provides and a virtual with two providers; k: priority picks engine-b:
  $ pac apk -f APKINDEX.test -q 'websrv'
  engine-b 1.0-r0
  libtls 2.0-r1
  websrv 2.1-r0
  check: ok

A !-conflict steers the virtual choice to the other provider:
  $ pac apk -f APKINDEX.test -q 'websrv, grumpy'
  engine-a 1.0-r0
  grumpy 1.0-r0
  libtls 2.0-r1
  websrv 2.1-r0
  check: ok

Versioned and fuzzy constraints select within real versions:
  $ pac apk -f APKINDEX.test -q 'oldapp'
  libfoo 1.5-r0
  oldapp 1.0-r0
  check: ok
  $ pac apk -f APKINDEX.test -q 'fuzzapp, libfoo'
  fuzzapp 1.0-r0
  libfoo 1.5-r0
  check: ok

An unversioned provides does not satisfy a versioned constraint:
  $ pac apk -f APKINDEX.test -q 'newapp'
  Because newapp 1.0-r0 -> libbar ∅ and root -> newapp 1.0-r0, version solving failed..
  [1]

Mutual exclusion via provide-and-conflict:
  $ pac apk -f APKINDEX.test -q 'alt1, alt2'
  Because alt1 1.0-r0 -> <alt1.1.0-r0 !! alt2> 1 and alt2 1.0-r0 -> <alt1.1.0-r0 !! alt2> 0, alt2 * or alt1 * is forbidden..
  And because root -> alt1 1.0-r0 and root -> alt2 1.0-r0, version solving failed.
  [1]

install_if: parent-doc appears only when both condition atoms are installed:
  $ pac apk -f APKINDEX.test -q 'parent'
  parent 2.0-r0
  check: ok
  $ pac apk -f APKINDEX.test -q 'parent, docs'
  docs 1.0-r0
  parent 2.0-r0
  parent-doc 2.0-r0
  check: ok
