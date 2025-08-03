  $ pac solve -f simple.pac -q 'foo ( 1.0.0 )'
  $ pac solve -f conflict-avoidance.pac -q 'foo ( 1.0.0 1.1.0 ) bar ( 1.0.0 1.1.0 )'
  $ pac solve -f example.pac -q 'A ( 1 )'
  B 1, D 2, C 1, A 1
  $ pac solve -f circular.pac -q 'A ( 1 )'
  C 1, B 1, A 1
  $ pac solve -f impossible.pac -q 'A ( 1 )'
  (((B 1 depends on D 1 and C 1 depends on D 2) and A 1 depends on C 1) and A 1 depends on B 1)
  $ pac solve -f missing.pac -q 'A ( 1 )'
  (C 1 depends on D 1 and D 1 is not available)
  $ pac solve -f impossible2.pac -q 'A ( 1 )'
  A 1
  $ pac solve -f backtrack.pac -q 'A ( 1 )'
  B 1, D 2, C 2, A 1
  $ pac solve -f long_chain_conflict.pac -q 'A ( 1 )'
  (((D 1 depends on F 1 and F 1 depends on H 1) and B 1 depends on D 1) and A 1 depends on B 1)
  $ pac solve -f long_chain_missing.pac -q 'A ( 1 )'
  ((B 1 depends on C 1 and ((E 1 depends on G 1 and G 1 depends on H 2) and C 1 depends on E 1)) and A 1 depends on B 1)
