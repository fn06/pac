apk version ordering vectors.

  $ pac apk-compare 1.0_alpha1 1.0
  lt
  $ pac apk-compare 1.0 1.0_p1
  lt
  $ pac apk-compare 1.0_rc1 1.0_beta1
  gt
  $ pac apk-compare 1.0-r1 1.0
  gt
  $ pac apk-compare 1.2.3 1.2
  gt
  $ pac apk-compare 1.0a 1.0b
  lt
  $ pac apk-compare 2.10 2.9
  gt
  $ pac apk-compare 1.02 1.1
  lt
