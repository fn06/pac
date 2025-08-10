  $ pac parse -f branching-error.pac
  foo 1.0.0 -> a (1.0.0), -> b (1.0.0);
  foo 1.1.0 -> x (1.0.0), -> y (1.0.0);
  a 1.0.0 -> b (2.0.0);
  b 1.0.0;
  b 2.0.0;
  x 1.0.0 -> y (2.0.0);
  y 1.0.0;
  y 2.0.0;
  $ pac parse -f concurrent.pac
  A 1.0.0 -> B (1.0.0), -> C (1.0.0);
  B 1.0.0 -> D (1.0.0 2.0.0 2.0.1);
  C 1.0.0 -> D (2.0.0 2.0.1 3.0.0);
  D 1.0.0;
  D 2.0.0;
  D 2.0.1;
  D 3.0.0;
  $ pac parse -f conflict-avoidance.pac
  foo 1.1.0 -> bar (2);
  foo 1.0.0;
  bar 2.0.0;
  bar 1.1.0;
  bar 1.0.0;
  $ pac parse -f conflict.pac
  foo 2.0.0 -> bar (1.0.0);
  foo 1.0.0;
  bar 1.0.0 -> foo (1.0.0);
  $ pac parse -f conflict-partial-satisfier.pac
  foo 1.1.0 -> left (1.0.0), -> right (1.0.0);
  foo 1.0.0;
  left 1.0.0 -> shared (1.0.0 2.0.0);
  right 1.0.0 -> shared (1.0.0);
  shared 2.0.0;
  shared 1.0.0 -> target (1.0.0);
  target 2.0.0;
  target 1.0.0;
  $ pac parse -f example.pac
  A 1 -> B (1), -> C (1);
  B 1 -> D (1 2);
  C 1 -> D (2 3);
  D 1;
  D 2;
  D 3;
  $ pac parse -f linear-error.pac
  foo 1.0.0 -> bar (2.0.0);
  bar 2.0.0 -> baz (3.0.0);
  baz 1.0.0;
  baz 3.0.0;
  $ pac parse -f simple.pac
  foo 1.0.0 -> bar (1.0.0 2.0.0);
  bar 1.0.0;
  bar 2.0.0;
  $ pac parse -f test_minimal.pac
  C 1;
