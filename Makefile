.PHONY: default all run clean

default: all

all: khartes

node_modules/aws-sdk:
	@npm install aws-sdk

node_modules/bucker:
	@npm install bucker

khartes: | node_modules/aws-sdk node_modules/bucker
	@hastec --start=asap --out=khartes Main.hs

run: | khartes
	@node ./khartes

clean:
	@rm -rf *.hi *.ibc *.js *.o khartes

distclean: | clean
	@rm -rf node_modules

# Local Variables:
# indent-tabs-mode: t
# End:
