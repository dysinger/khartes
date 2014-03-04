.PHONY: default all run clean

default: all

all: khartes-idris khartes-haste

node_modules/aws-sdk:
	@npm install aws-sdk

node_modules/bucker:
	@npm install bucker

.cabal-sandbox:
	@cabal sandbox init

.cabal-sandbox/bin/idris: | .cabal-sandbox
	@cabal install idris

khartes-idris: | .cabal-sandbox/bin/idris node_modules/aws-sdk node_modules/bucker
	@.cabal-sandbox/bin/idris --codegen node -o khartes-idris Main.idr

.cabal-sandbox/bin/hastec: | .cabal-sandbox
	@cabal install haste-complier

khartes-haste: | .cabal-sandbox/bin/hastec node_modules/aws-sdk node_modules/bucker
	@hastec --start=asap --out=khartes-haste Main.hs

clean:
	@rm -rf *.hi *.ibc *.js *.o khartes*

distclean: | clean
	@rm -rf node_modules

# Local Variables:
# indent-tabs-mode: t
# End:
