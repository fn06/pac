  $ pac check -f example.pac -q 'A (1)' -r ''
  Core resolution: false
  	Root inclusion: true
  	Dependency closure: false
  	Version uniqueness: true
  $ pac check -f example.pac -q 'A (1)' -r 'A 1'
  Core resolution: false
  	Root inclusion: true
  	Dependency closure: false
  	Version uniqueness: true
  $ pac check -f example.pac -q 'A (1)' -r 'A 1, B 1, C 1, D 2, D 3'
  Core resolution: false
  	Root inclusion: true
  	Dependency closure: true
  	Version uniqueness: false
  $ pac check -f example.pac -q 'A (1)' -r 'A 1, B 1, C 1, D 2'
  Core resolution: true
  	Root inclusion: true
  	Dependency closure: true
  	Version uniqueness: true
  $ pac check -f example.pac -q 'A (1)' -r 'A 1, B 1, C 1, D 2' -c concurrent
  Concurrent resolution: true
  	Root inclusion: true
  	Dependency closure: true
  	Version granularity: true
  $ pac check -f example.pac -q 'A (1)' -r 'A 1, B 1, C 1, D 2, D 3' -c concurrent
  Concurrent resolution: true
  	Root inclusion: true
  	Dependency closure: true
  	Version granularity: true
