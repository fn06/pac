dpkg version comparison vectors (deb-version(7)).

Basic ordering and equality:
  $ pac deb-compare 1.0 1.1
  lt
  $ pac deb-compare 1.0 1.0
  eq
  $ pac deb-compare 1.10 1.9
  gt

Tilde sorts before everything, including the empty string:
  $ pac deb-compare 1.0~rc1 1.0
  lt
  $ pac deb-compare 1.0~beta1~svn1245 1.0~beta1
  lt
  $ pac deb-compare 1.0~ 1.0
  lt

Letters compare after end-of-string but before punctuation:
  $ pac deb-compare 1.0a 1.0
  gt
  $ pac deb-compare 1.2a 1.2.3
  lt

Epochs dominate:
  $ pac deb-compare 1:0.1 9.9
  gt
  $ pac deb-compare 2:1.0 1:9.9
  gt

Revisions: absent revision equals zero, splits at the last hyphen:
  $ pac deb-compare 1.2.3 1.2.3-0
  eq
  $ pac deb-compare 1.2.3 1.2.3-1
  lt
  $ pac deb-compare 1.0-1-1 1.0-1
  gt
