
buildtests:
	time cabal configure --enable-tests --disable-library-profiling --disable-executable-profiling -O2
	cabal build 

runtests:
	time dist/build/spec/spec -s

tests:
	cabal configure --enable-tests && cabal build && time dist/build/spec/spec -s

install:
	cabal install
	chmod +x scripts/*
	cp scripts/* ~/bin