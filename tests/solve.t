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
  prior cause (terms: {foo (2.0.0)}, cause: ({"terms":[["Pos",{"Name":"bar"},[{"Version":"1.0.0"}]],["Neg",{"Name":"foo"},[{"Version":"1.0.0"}]]],"cause":{"Dependency":[[{"Name":"bar"},{"Version":"1.0.0"}],[{"Name":"foo"},[{"Version":"1.0.0"}]]]}} and {"terms":[["Pos",{"Name":"foo"},[{"Version":"2.0.0"}]],["Neg",{"Name":"bar"},[{"Version":"1.0.0"}]]],"cause":{"Dependency":[[{"Name":"foo"},{"Version":"2.0.0"}],[{"Name":"bar"},[{"Version":"1.0.0"}]]]}}))
  conflict resolution on: (terms: {foo (2.0.0)}, cause: ({"terms":[["Pos",{"Name":"bar"},[{"Version":"1.0.0"}]],["Neg",{"Name":"foo"},[{"Version":"1.0.0"}]]],"cause":{"Dependency":[[{"Name":"bar"},{"Version":"1.0.0"}],[{"Name":"foo"},[{"Version":"1.0.0"}]]]}} and {"terms":[["Pos",{"Name":"foo"},[{"Version":"2.0.0"}]],["Neg",{"Name":"bar"},[{"Version":"1.0.0"}]]],"cause":{"Dependency":[[{"Name":"foo"},{"Version":"2.0.0"}],[{"Name":"bar"},[{"Version":"1.0.0"}]]]}}))
  satisfiying assignment on level 1: Decision foo 2.0.0
  backtracking to level 0
  new incompatibility (terms: {foo (2.0.0)}, cause: ({"terms":[["Pos",{"Name":"bar"},[{"Version":"1.0.0"}]],["Neg",{"Name":"foo"},[{"Version":"1.0.0"}]]],"cause":{"Dependency":[[{"Name":"bar"},{"Version":"1.0.0"}],[{"Name":"foo"},[{"Version":"1.0.0"}]]]}} and {"terms":[["Pos",{"Name":"foo"},[{"Version":"2.0.0"}]],["Neg",{"Name":"bar"},[{"Version":"1.0.0"}]]],"cause":{"Dependency":[[{"Name":"foo"},{"Version":"2.0.0"}],[{"Name":"bar"},[{"Version":"1.0.0"}]]]}}))
  new assignment on level 0: Derivation not foo (2.0.0) due to incompatibility (terms: {foo (2.0.0)}, cause: ({"terms":[["Pos",{"Name":"bar"},[{"Version":"1.0.0"}]],["Neg",{"Name":"foo"},[{"Version":"1.0.0"}]]],"cause":{"Dependency":[[{"Name":"bar"},{"Version":"1.0.0"}],[{"Name":"foo"},[{"Version":"1.0.0"}]]]}} and {"terms":[["Pos",{"Name":"foo"},[{"Version":"2.0.0"}]],["Neg",{"Name":"bar"},[{"Version":"1.0.0"}]]],"cause":{"Dependency":[[{"Name":"foo"},{"Version":"2.0.0"}],[{"Name":"bar"},[{"Version":"1.0.0"}]]]}}))
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
