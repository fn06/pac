  $ pac solve -f example.pac -q 'A ( 1 )' -d
  initial incompatibilities
  	(terms: {not Root (Root)}, cause: root)
  	(terms: {Root (Root), not A (1)}, cause: dependency Root Root -> A (1))
  unit propagation on: Root
  new assignment on level 0: Derivation Root (Root) due to incompatibility (terms: {not Root (Root)}, cause: root)
  new assignment on level 0: Derivation A (1) due to incompatibility (terms: {Root (Root), not A (1)}, cause: dependency Root Root -> A (1))
  unit propagation on: A
  unit propagation on: Root
  deciding on A: (1)
  trying version 1
  dependency incompatibilities
  	(terms: {A (1), not C (1)}, cause: dependency A 1 -> C (1))
  	(terms: {A (1), not B (1)}, cause: dependency A 1 -> B (1))
  assignment on level 1: Decision A 1
  unit propagation on: A
  new assignment on level 1: Derivation C (1) due to incompatibility (terms: {A (1), not C (1)}, cause: dependency A 1 -> C (1))
  new assignment on level 1: Derivation B (1) due to incompatibility (terms: {A (1), not B (1)}, cause: dependency A 1 -> B (1))
  unit propagation on: B
  unit propagation on: C
  deciding on B: (1)
  trying version 1
  dependency incompatibilities
  	(terms: {B (1), not D (1, 2)}, cause: dependency B 1 -> D (1, 2))
  assignment on level 2: Decision B 1
  unit propagation on: B
  new assignment on level 2: Derivation D (1, 2) due to incompatibility (terms: {B (1), not D (1, 2)}, cause: dependency B 1 -> D (1, 2))
  unit propagation on: D
  deciding on D: (1, 2)
  trying version 2
  assignment on level 3: Decision D 2
  unit propagation on: D
  deciding on C: (1)
  trying version 1
  dependency incompatibilities
  	(terms: {C (1), not D (2, 3)}, cause: dependency C 1 -> D (2, 3))
  assignment on level 4: Decision C 1
  unit propagation on: C
  C 1, D 2, B 1, A 1
  $ pac solve -f simple.pac -q 'foo ( 1.0.0 )' -d
  initial incompatibilities
  	(terms: {not Root (Root)}, cause: root)
  	(terms: {Root (Root), not foo (1.0.0)}, cause: dependency Root Root -> foo (1.0.0))
  unit propagation on: Root
  new assignment on level 0: Derivation Root (Root) due to incompatibility (terms: {not Root (Root)}, cause: root)
  new assignment on level 0: Derivation foo (1.0.0) due to incompatibility (terms: {Root (Root), not foo (1.0.0)}, cause: dependency Root Root -> foo (1.0.0))
  unit propagation on: foo
  unit propagation on: Root
  deciding on foo: (1.0.0)
  trying version 1.0.0
  dependency incompatibilities
  	(terms: {foo (1.0.0), not bar (1.0.0, 2.0.0)}, cause: dependency foo 1.0.0 -> bar (1.0.0, 2.0.0))
  assignment on level 1: Decision foo 1.0.0
  unit propagation on: foo
  new assignment on level 1: Derivation bar (1.0.0, 2.0.0) due to incompatibility (terms: {foo (1.0.0), not bar (1.0.0, 2.0.0)}, cause: dependency foo 1.0.0 -> bar (1.0.0, 2.0.0))
  unit propagation on: bar
  deciding on bar: (1.0.0, 2.0.0)
  trying version 2.0.0
  assignment on level 2: Decision bar 2.0.0
  unit propagation on: bar
  bar 2.0.0, foo 1.0.0
  $ pac solve -f conflict-avoidance.pac -q 'foo ( 1.0.0 1.1.0 ) bar ( 1.0.0 1.1.0 )' -d
  initial incompatibilities
  	(terms: {not Root (Root)}, cause: root)
  	(terms: {Root (Root), not bar (1.0.0, 1.1.0)}, cause: dependency Root Root -> bar (1.0.0, 1.1.0))
  	(terms: {Root (Root), not foo (1.0.0, 1.1.0)}, cause: dependency Root Root -> foo (1.0.0, 1.1.0))
  unit propagation on: Root
  new assignment on level 0: Derivation Root (Root) due to incompatibility (terms: {not Root (Root)}, cause: root)
  new assignment on level 0: Derivation bar (1.0.0, 1.1.0) due to incompatibility (terms: {Root (Root), not bar (1.0.0, 1.1.0)}, cause: dependency Root Root -> bar (1.0.0, 1.1.0))
  new assignment on level 0: Derivation foo (1.0.0, 1.1.0) due to incompatibility (terms: {Root (Root), not foo (1.0.0, 1.1.0)}, cause: dependency Root Root -> foo (1.0.0, 1.1.0))
  unit propagation on: foo
  unit propagation on: bar
  unit propagation on: Root
  deciding on foo: (1.0.0, 1.1.0)
  trying version 1.1.0
  dependency incompatibilities
  	(terms: {foo (1.1.0), not bar (2)}, cause: dependency foo 1.1.0 -> bar (2))
  not adding due to incompatibility (terms: {foo (1.1.0), not bar (2)}, cause: dependency foo 1.1.0 -> bar (2))
  trying version 1.0.0
  assignment on level 1: Decision foo 1.0.0
  unit propagation on: foo
  deciding on bar: (1.0.0, 1.1.0)
  trying version 1.1.0
  assignment on level 2: Decision bar 1.1.0
  unit propagation on: bar
  bar 1.1.0, foo 1.0.0
  $ pac solve -f conflict.pac -q 'foo ( 1.0.0 2.0.0 )' -d
  initial incompatibilities
  	(terms: {not Root (Root)}, cause: root)
  	(terms: {Root (Root), not foo (1.0.0, 2.0.0)}, cause: dependency Root Root -> foo (1.0.0, 2.0.0))
  unit propagation on: Root
  new assignment on level 0: Derivation Root (Root) due to incompatibility (terms: {not Root (Root)}, cause: root)
  new assignment on level 0: Derivation foo (1.0.0, 2.0.0) due to incompatibility (terms: {Root (Root), not foo (1.0.0, 2.0.0)}, cause: dependency Root Root -> foo (1.0.0, 2.0.0))
  unit propagation on: foo
  unit propagation on: Root
  deciding on foo: (1.0.0, 2.0.0)
  trying version 2.0.0
  dependency incompatibilities
  	(terms: {foo (2.0.0), not bar (1.0.0)}, cause: dependency foo 2.0.0 -> bar (1.0.0))
  assignment on level 1: Decision foo 2.0.0
  unit propagation on: foo
  new assignment on level 1: Derivation bar (1.0.0) due to incompatibility (terms: {foo (2.0.0), not bar (1.0.0)}, cause: dependency foo 2.0.0 -> bar (1.0.0))
  unit propagation on: bar
  deciding on bar: (1.0.0)
  trying version 1.0.0
  dependency incompatibilities
  	(terms: {bar (1.0.0), not foo (1.0.0)}, cause: dependency bar 1.0.0 -> foo (1.0.0))
  not adding due to incompatibility (terms: {bar (1.0.0), not foo (1.0.0)}, cause: dependency bar 1.0.0 -> foo (1.0.0))
  unit propagation on: bar
  conflict resolution on: (terms: {bar (1.0.0), not foo (1.0.0)}, cause: dependency bar 1.0.0 -> foo (1.0.0))
  satisfiying assignment on level 1: Derivation bar (1.0.0) due to incompatibility (terms: {foo (2.0.0), not bar (1.0.0)}, cause: dependency foo 2.0.0 -> bar (1.0.0))
  prior cause (terms: {foo (2.0.0)}, cause: ((terms: {bar (1.0.0), not foo (1.0.0)}, cause: dependency bar 1.0.0 -> foo (1.0.0)) and (terms: {foo (2.0.0), not bar (1.0.0)}, cause: dependency foo 2.0.0 -> bar (1.0.0))))
  conflict resolution on: (terms: {foo (2.0.0)}, cause: ((terms: {bar (1.0.0), not foo (1.0.0)}, cause: dependency bar 1.0.0 -> foo (1.0.0)) and (terms: {foo (2.0.0), not bar (1.0.0)}, cause: dependency foo 2.0.0 -> bar (1.0.0))))
  satisfiying assignment on level 1: Decision foo 2.0.0
  backtracking to level 0
  new incompatibility (terms: {foo (2.0.0)}, cause: ((terms: {bar (1.0.0), not foo (1.0.0)}, cause: dependency bar 1.0.0 -> foo (1.0.0)) and (terms: {foo (2.0.0), not bar (1.0.0)}, cause: dependency foo 2.0.0 -> bar (1.0.0))))
  new assignment on level 0: Derivation not foo (2.0.0) due to incompatibility (terms: {foo (2.0.0)}, cause: ((terms: {bar (1.0.0), not foo (1.0.0)}, cause: dependency bar 1.0.0 -> foo (1.0.0)) and (terms: {foo (2.0.0), not bar (1.0.0)}, cause: dependency foo 2.0.0 -> bar (1.0.0))))
  unit propagation on: foo
  new assignment on level 0: Derivation not bar (1.0.0) due to incompatibility (terms: {bar (1.0.0), not foo (1.0.0)}, cause: dependency bar 1.0.0 -> foo (1.0.0))
  unit propagation on: bar
  deciding on foo: (1.0.0, 2.0.0)
  trying version 2.0.0
  dependency incompatibilities
  	(terms: {foo (2.0.0), not bar (1.0.0)}, cause: dependency foo 2.0.0 -> bar (1.0.0))
  not adding due to incompatibility (terms: {foo (2.0.0), not bar (1.0.0)}, cause: dependency foo 2.0.0 -> bar (1.0.0))
  trying version 1.0.0
  assignment on level 1: Decision foo 1.0.0
  unit propagation on: foo
  foo 1.0.0
  $ pac solve -f conflict-partial-satisfier.pac -q 'foo ( 1.0.0 1.1.0 ) target ( 2.0.0 )' -d
  initial incompatibilities
  	(terms: {not Root (Root)}, cause: root)
  	(terms: {Root (Root), not target (2.0.0)}, cause: dependency Root Root -> target (2.0.0))
  	(terms: {Root (Root), not foo (1.0.0, 1.1.0)}, cause: dependency Root Root -> foo (1.0.0, 1.1.0))
  unit propagation on: Root
  new assignment on level 0: Derivation Root (Root) due to incompatibility (terms: {not Root (Root)}, cause: root)
  new assignment on level 0: Derivation target (2.0.0) due to incompatibility (terms: {Root (Root), not target (2.0.0)}, cause: dependency Root Root -> target (2.0.0))
  new assignment on level 0: Derivation foo (1.0.0, 1.1.0) due to incompatibility (terms: {Root (Root), not foo (1.0.0, 1.1.0)}, cause: dependency Root Root -> foo (1.0.0, 1.1.0))
  unit propagation on: foo
  unit propagation on: target
  unit propagation on: Root
  deciding on foo: (1.0.0, 1.1.0)
  trying version 1.1.0
  dependency incompatibilities
  	(terms: {foo (1.1.0), not right (1.0.0)}, cause: dependency foo 1.1.0 -> right (1.0.0))
  	(terms: {foo (1.1.0), not left (1.0.0)}, cause: dependency foo 1.1.0 -> left (1.0.0))
  assignment on level 1: Decision foo 1.1.0
  unit propagation on: foo
  new assignment on level 1: Derivation right (1.0.0) due to incompatibility (terms: {foo (1.1.0), not right (1.0.0)}, cause: dependency foo 1.1.0 -> right (1.0.0))
  new assignment on level 1: Derivation left (1.0.0) due to incompatibility (terms: {foo (1.1.0), not left (1.0.0)}, cause: dependency foo 1.1.0 -> left (1.0.0))
  unit propagation on: left
  unit propagation on: right
  deciding on left: (1.0.0)
  trying version 1.0.0
  dependency incompatibilities
  	(terms: {left (1.0.0), not shared (1.0.0, 2.0.0)}, cause: dependency left 1.0.0 -> shared (1.0.0, 2.0.0))
  assignment on level 2: Decision left 1.0.0
  unit propagation on: left
  new assignment on level 2: Derivation shared (1.0.0, 2.0.0) due to incompatibility (terms: {left (1.0.0), not shared (1.0.0, 2.0.0)}, cause: dependency left 1.0.0 -> shared (1.0.0, 2.0.0))
  unit propagation on: shared
  deciding on shared: (1.0.0, 2.0.0)
  trying version 2.0.0
  assignment on level 3: Decision shared 2.0.0
  unit propagation on: shared
  deciding on right: (1.0.0)
  trying version 1.0.0
  dependency incompatibilities
  	(terms: {right (1.0.0), not shared (1.0.0)}, cause: dependency right 1.0.0 -> shared (1.0.0))
  not adding due to incompatibility (terms: {right (1.0.0), not shared (1.0.0)}, cause: dependency right 1.0.0 -> shared (1.0.0))
  unit propagation on: right
  conflict resolution on: (terms: {right (1.0.0), not shared (1.0.0)}, cause: dependency right 1.0.0 -> shared (1.0.0))
  satisfiying assignment on level 3: Decision shared 2.0.0
  backtracking to level 1
  new assignment on level 1: Derivation shared (1.0.0) due to incompatibility (terms: {right (1.0.0), not shared (1.0.0)}, cause: dependency right 1.0.0 -> shared (1.0.0))
  unit propagation on: shared
  deciding on shared: (1.0.0)
  trying version 1.0.0
  dependency incompatibilities
  	(terms: {shared (1.0.0), not target (1.0.0)}, cause: dependency shared 1.0.0 -> target (1.0.0))
  not adding due to incompatibility (terms: {shared (1.0.0), not target (1.0.0)}, cause: dependency shared 1.0.0 -> target (1.0.0))
  unit propagation on: shared
  conflict resolution on: (terms: {shared (1.0.0), not target (1.0.0)}, cause: dependency shared 1.0.0 -> target (1.0.0))
  satisfiying assignment on level 1: Derivation shared (1.0.0) due to incompatibility (terms: {right (1.0.0), not shared (1.0.0)}, cause: dependency right 1.0.0 -> shared (1.0.0))
  backtracking to level 0
  new assignment on level 0: Derivation not shared (1.0.0) due to incompatibility (terms: {shared (1.0.0), not target (1.0.0)}, cause: dependency shared 1.0.0 -> target (1.0.0))
  unit propagation on: shared
  new assignment on level 0: Derivation not right (1.0.0) due to incompatibility (terms: {right (1.0.0), not shared (1.0.0)}, cause: dependency right 1.0.0 -> shared (1.0.0))
  new assignment on level 0: Derivation not left (1.0.0) due to incompatibility (terms: {left (1.0.0), not shared (1.0.0, 2.0.0)}, cause: dependency left 1.0.0 -> shared (1.0.0, 2.0.0))
  unit propagation on: left
  new assignment on level 0: Derivation not foo (1.1.0) due to incompatibility (terms: {foo (1.1.0), not left (1.0.0)}, cause: dependency foo 1.1.0 -> left (1.0.0))
  unit propagation on: foo
  unit propagation on: right
  deciding on foo: (1.0.0, 1.1.0)
  trying version 1.1.0
  dependency incompatibilities
  	(terms: {foo (1.1.0), not right (1.0.0)}, cause: dependency foo 1.1.0 -> right (1.0.0))
  	(terms: {foo (1.1.0), not left (1.0.0)}, cause: dependency foo 1.1.0 -> left (1.0.0))
  not adding due to incompatibility (terms: {foo (1.1.0), not right (1.0.0)}, cause: dependency foo 1.1.0 -> right (1.0.0))
  trying version 1.0.0
  assignment on level 1: Decision foo 1.0.0
  unit propagation on: foo
  deciding on target: (2.0.0)
  trying version 2.0.0
  assignment on level 2: Decision target 2.0.0
  unit propagation on: target
  target 2.0.0, foo 1.0.0
  $ pac solve -f linear-error.pac -q 'foo ( 1.0.0 ) baz ( 1.0.0 )' -d
  initial incompatibilities
  	(terms: {not Root (Root)}, cause: root)
  	(terms: {Root (Root), not baz (1.0.0)}, cause: dependency Root Root -> baz (1.0.0))
  	(terms: {Root (Root), not foo (1.0.0)}, cause: dependency Root Root -> foo (1.0.0))
  unit propagation on: Root
  new assignment on level 0: Derivation Root (Root) due to incompatibility (terms: {not Root (Root)}, cause: root)
  new assignment on level 0: Derivation baz (1.0.0) due to incompatibility (terms: {Root (Root), not baz (1.0.0)}, cause: dependency Root Root -> baz (1.0.0))
  new assignment on level 0: Derivation foo (1.0.0) due to incompatibility (terms: {Root (Root), not foo (1.0.0)}, cause: dependency Root Root -> foo (1.0.0))
  unit propagation on: foo
  unit propagation on: baz
  unit propagation on: Root
  deciding on foo: (1.0.0)
  trying version 1.0.0
  dependency incompatibilities
  	(terms: {foo (1.0.0), not bar (2.0.0)}, cause: dependency foo 1.0.0 -> bar (2.0.0))
  assignment on level 1: Decision foo 1.0.0
  unit propagation on: foo
  new assignment on level 1: Derivation bar (2.0.0) due to incompatibility (terms: {foo (1.0.0), not bar (2.0.0)}, cause: dependency foo 1.0.0 -> bar (2.0.0))
  unit propagation on: bar
  deciding on bar: (2.0.0)
  trying version 2.0.0
  dependency incompatibilities
  	(terms: {bar (2.0.0), not baz (3.0.0)}, cause: dependency bar 2.0.0 -> baz (3.0.0))
  not adding due to incompatibility (terms: {bar (2.0.0), not baz (3.0.0)}, cause: dependency bar 2.0.0 -> baz (3.0.0))
  unit propagation on: bar
  conflict resolution on: (terms: {bar (2.0.0), not baz (3.0.0)}, cause: dependency bar 2.0.0 -> baz (3.0.0))
  satisfiying assignment on level 1: Derivation bar (2.0.0) due to incompatibility (terms: {foo (1.0.0), not bar (2.0.0)}, cause: dependency foo 1.0.0 -> bar (2.0.0))
  backtracking to level 0
  new assignment on level 0: Derivation not bar (2.0.0) due to incompatibility (terms: {bar (2.0.0), not baz (3.0.0)}, cause: dependency bar 2.0.0 -> baz (3.0.0))
  unit propagation on: bar
  conflict resolution on: (terms: {foo (1.0.0), not bar (2.0.0)}, cause: dependency foo 1.0.0 -> bar (2.0.0))
  satisfiying assignment on level 0: Derivation not bar (2.0.0) due to incompatibility (terms: {bar (2.0.0), not baz (3.0.0)}, cause: dependency bar 2.0.0 -> baz (3.0.0))
  prior cause (terms: {not baz (3.0.0), foo (1.0.0)}, cause: ((terms: {foo (1.0.0), not bar (2.0.0)}, cause: dependency foo 1.0.0 -> bar (2.0.0)) and (terms: {bar (2.0.0), not baz (3.0.0)}, cause: dependency bar 2.0.0 -> baz (3.0.0))))
  conflict resolution on: (terms: {not baz (3.0.0), foo (1.0.0)}, cause: ((terms: {foo (1.0.0), not bar (2.0.0)}, cause: dependency foo 1.0.0 -> bar (2.0.0)) and (terms: {bar (2.0.0), not baz (3.0.0)}, cause: dependency bar 2.0.0 -> baz (3.0.0))))
  satisfiying assignment on level 0: Derivation foo (1.0.0) due to incompatibility (terms: {Root (Root), not foo (1.0.0)}, cause: dependency Root Root -> foo (1.0.0))
  prior cause (terms: {not baz (3.0.0), Root (Root)}, cause: ((terms: {not baz (3.0.0), foo (1.0.0)}, cause: ((terms: {foo (1.0.0), not bar (2.0.0)}, cause: dependency foo 1.0.0 -> bar (2.0.0)) and (terms: {bar (2.0.0), not baz (3.0.0)}, cause: dependency bar 2.0.0 -> baz (3.0.0)))) and (terms: {Root (Root), not foo (1.0.0)}, cause: dependency Root Root -> foo (1.0.0))))
  conflict resolution on: (terms: {not baz (3.0.0), Root (Root)}, cause: ((terms: {not baz (3.0.0), foo (1.0.0)}, cause: ((terms: {foo (1.0.0), not bar (2.0.0)}, cause: dependency foo 1.0.0 -> bar (2.0.0)) and (terms: {bar (2.0.0), not baz (3.0.0)}, cause: dependency bar 2.0.0 -> baz (3.0.0)))) and (terms: {Root (Root), not foo (1.0.0)}, cause: dependency Root Root -> foo (1.0.0))))
  satisfiying assignment on level 0: Derivation baz (1.0.0) due to incompatibility (terms: {Root (Root), not baz (1.0.0)}, cause: dependency Root Root -> baz (1.0.0))
  prior cause (terms: {Root (Root)}, cause: ((terms: {not baz (3.0.0), Root (Root)}, cause: ((terms: {not baz (3.0.0), foo (1.0.0)}, cause: ((terms: {foo (1.0.0), not bar (2.0.0)}, cause: dependency foo 1.0.0 -> bar (2.0.0)) and (terms: {bar (2.0.0), not baz (3.0.0)}, cause: dependency bar 2.0.0 -> baz (3.0.0)))) and (terms: {Root (Root), not foo (1.0.0)}, cause: dependency Root Root -> foo (1.0.0)))) and (terms: {Root (Root), not baz (1.0.0)}, cause: dependency Root Root -> baz (1.0.0))))
  conflict resolution on: (terms: {Root (Root)}, cause: ((terms: {not baz (3.0.0), Root (Root)}, cause: ((terms: {not baz (3.0.0), foo (1.0.0)}, cause: ((terms: {foo (1.0.0), not bar (2.0.0)}, cause: dependency foo 1.0.0 -> bar (2.0.0)) and (terms: {bar (2.0.0), not baz (3.0.0)}, cause: dependency bar 2.0.0 -> baz (3.0.0)))) and (terms: {Root (Root), not foo (1.0.0)}, cause: dependency Root Root -> foo (1.0.0)))) and (terms: {Root (Root), not baz (1.0.0)}, cause: dependency Root Root -> baz (1.0.0))))
  (((Dep foo 1.0.0 -> bar (2.0.0) && Dep bar 2.0.0 -> baz (3.0.0)) && Dep Root Root -> foo (1.0.0)) && Dep Root Root -> baz (1.0.0))
  $ pac solve -f branching-error.pac -q 'foo ( 1.0.0 )' -d
  initial incompatibilities
  	(terms: {not Root (Root)}, cause: root)
  	(terms: {Root (Root), not foo (1.0.0)}, cause: dependency Root Root -> foo (1.0.0))
  unit propagation on: Root
  new assignment on level 0: Derivation Root (Root) due to incompatibility (terms: {not Root (Root)}, cause: root)
  new assignment on level 0: Derivation foo (1.0.0) due to incompatibility (terms: {Root (Root), not foo (1.0.0)}, cause: dependency Root Root -> foo (1.0.0))
  unit propagation on: foo
  unit propagation on: Root
  deciding on foo: (1.0.0)
  trying version 1.0.0
  dependency incompatibilities
  	(terms: {foo (1.0.0), not b (1.0.0)}, cause: dependency foo 1.0.0 -> b (1.0.0))
  	(terms: {foo (1.0.0), not a (1.0.0)}, cause: dependency foo 1.0.0 -> a (1.0.0))
  assignment on level 1: Decision foo 1.0.0
  unit propagation on: foo
  new assignment on level 1: Derivation b (1.0.0) due to incompatibility (terms: {foo (1.0.0), not b (1.0.0)}, cause: dependency foo 1.0.0 -> b (1.0.0))
  new assignment on level 1: Derivation a (1.0.0) due to incompatibility (terms: {foo (1.0.0), not a (1.0.0)}, cause: dependency foo 1.0.0 -> a (1.0.0))
  unit propagation on: a
  unit propagation on: b
  deciding on a: (1.0.0)
  trying version 1.0.0
  dependency incompatibilities
  	(terms: {a (1.0.0), not b (2.0.0)}, cause: dependency a 1.0.0 -> b (2.0.0))
  not adding due to incompatibility (terms: {a (1.0.0), not b (2.0.0)}, cause: dependency a 1.0.0 -> b (2.0.0))
  unit propagation on: a
  conflict resolution on: (terms: {a (1.0.0), not b (2.0.0)}, cause: dependency a 1.0.0 -> b (2.0.0))
  satisfiying assignment on level 1: Derivation a (1.0.0) due to incompatibility (terms: {foo (1.0.0), not a (1.0.0)}, cause: dependency foo 1.0.0 -> a (1.0.0))
  prior cause (terms: {not b (2.0.0), foo (1.0.0)}, cause: ((terms: {a (1.0.0), not b (2.0.0)}, cause: dependency a 1.0.0 -> b (2.0.0)) and (terms: {foo (1.0.0), not a (1.0.0)}, cause: dependency foo 1.0.0 -> a (1.0.0))))
  conflict resolution on: (terms: {not b (2.0.0), foo (1.0.0)}, cause: ((terms: {a (1.0.0), not b (2.0.0)}, cause: dependency a 1.0.0 -> b (2.0.0)) and (terms: {foo (1.0.0), not a (1.0.0)}, cause: dependency foo 1.0.0 -> a (1.0.0))))
  satisfiying assignment on level 1: Derivation b (1.0.0) due to incompatibility (terms: {foo (1.0.0), not b (1.0.0)}, cause: dependency foo 1.0.0 -> b (1.0.0))
  backtracking to level 0
  new incompatibility (terms: {not b (2.0.0), foo (1.0.0)}, cause: ((terms: {a (1.0.0), not b (2.0.0)}, cause: dependency a 1.0.0 -> b (2.0.0)) and (terms: {foo (1.0.0), not a (1.0.0)}, cause: dependency foo 1.0.0 -> a (1.0.0))))
  new assignment on level 0: Derivation b (2.0.0) due to incompatibility (terms: {not b (2.0.0), foo (1.0.0)}, cause: ((terms: {a (1.0.0), not b (2.0.0)}, cause: dependency a 1.0.0 -> b (2.0.0)) and (terms: {foo (1.0.0), not a (1.0.0)}, cause: dependency foo 1.0.0 -> a (1.0.0))))
  unit propagation on: b
  new assignment on level 0: Derivation not a (1.0.0) due to incompatibility (terms: {a (1.0.0), not b (2.0.0)}, cause: dependency a 1.0.0 -> b (2.0.0))
  conflict resolution on: (terms: {foo (1.0.0), not b (1.0.0)}, cause: dependency foo 1.0.0 -> b (1.0.0))
  satisfiying assignment on level 0: Derivation b (2.0.0) due to incompatibility (terms: {not b (2.0.0), foo (1.0.0)}, cause: ((terms: {a (1.0.0), not b (2.0.0)}, cause: dependency a 1.0.0 -> b (2.0.0)) and (terms: {foo (1.0.0), not a (1.0.0)}, cause: dependency foo 1.0.0 -> a (1.0.0))))
  prior cause (terms: {foo (1.0.0)}, cause: ((terms: {foo (1.0.0), not b (1.0.0)}, cause: dependency foo 1.0.0 -> b (1.0.0)) and (terms: {not b (2.0.0), foo (1.0.0)}, cause: ((terms: {a (1.0.0), not b (2.0.0)}, cause: dependency a 1.0.0 -> b (2.0.0)) and (terms: {foo (1.0.0), not a (1.0.0)}, cause: dependency foo 1.0.0 -> a (1.0.0))))))
  conflict resolution on: (terms: {foo (1.0.0)}, cause: ((terms: {foo (1.0.0), not b (1.0.0)}, cause: dependency foo 1.0.0 -> b (1.0.0)) and (terms: {not b (2.0.0), foo (1.0.0)}, cause: ((terms: {a (1.0.0), not b (2.0.0)}, cause: dependency a 1.0.0 -> b (2.0.0)) and (terms: {foo (1.0.0), not a (1.0.0)}, cause: dependency foo 1.0.0 -> a (1.0.0))))))
  satisfiying assignment on level 0: Derivation foo (1.0.0) due to incompatibility (terms: {Root (Root), not foo (1.0.0)}, cause: dependency Root Root -> foo (1.0.0))
  prior cause (terms: {Root (Root)}, cause: ((terms: {foo (1.0.0)}, cause: ((terms: {foo (1.0.0), not b (1.0.0)}, cause: dependency foo 1.0.0 -> b (1.0.0)) and (terms: {not b (2.0.0), foo (1.0.0)}, cause: ((terms: {a (1.0.0), not b (2.0.0)}, cause: dependency a 1.0.0 -> b (2.0.0)) and (terms: {foo (1.0.0), not a (1.0.0)}, cause: dependency foo 1.0.0 -> a (1.0.0)))))) and (terms: {Root (Root), not foo (1.0.0)}, cause: dependency Root Root -> foo (1.0.0))))
  conflict resolution on: (terms: {Root (Root)}, cause: ((terms: {foo (1.0.0)}, cause: ((terms: {foo (1.0.0), not b (1.0.0)}, cause: dependency foo 1.0.0 -> b (1.0.0)) and (terms: {not b (2.0.0), foo (1.0.0)}, cause: ((terms: {a (1.0.0), not b (2.0.0)}, cause: dependency a 1.0.0 -> b (2.0.0)) and (terms: {foo (1.0.0), not a (1.0.0)}, cause: dependency foo 1.0.0 -> a (1.0.0)))))) and (terms: {Root (Root), not foo (1.0.0)}, cause: dependency Root Root -> foo (1.0.0))))
  ((Dep foo 1.0.0 -> b (1.0.0) && (Dep a 1.0.0 -> b (2.0.0) && Dep foo 1.0.0 -> a (1.0.0))) && Dep Root Root -> foo (1.0.0))
