.PHONY: default all run clean

default: all

all: khartes-idris khartes-haste

node_modules/aws-sdk/node_modules:
	@npm install aws-sdk

node_modules/bucker/node_modules:
	@npm install bucker

.cabal-sandbox:
	@cabal sandbox init

.cabal-sandbox/bin/idris: | .cabal-sandbox
	@cabal install idris

.cabal-sandbox/bin/hastec: | .cabal-sandbox
	@cabal install haste-compiler

khartes-idris: | .cabal-sandbox/bin/idris node_modules/aws-sdk/node_modules node_modules/bucker/node_modules
	@.cabal-sandbox/bin/idris --codegen node -o khartes-idris Main.idr

khartes-haste: | .cabal-sandbox/bin/hastec node_modules/aws-sdk/node_modules node_modules/bucker/node_modules
	@.cabal-sandbox/bin/hastec --start=asap --out=khartes-haste Main.hs

clean:
	@rm -rf *.hi *.ibc *.js *.o khartes*

distclean: | clean
	@rm -rf node_modules

# Local Variables:
# indent-tabs-mode: t
# End:
