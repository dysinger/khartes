.PHONY: default all clean test

default: all

all: khartes/target/khartes.jar

khartes/pom.xml:
	@idris --mvn --codegen Java -o khartes Main.idr

khartes/target/khartes.jar: khartes/pom.xml
	@cd khartes && mvn -DmainClass=khartes compile package shade:shade

test:
	@true

run:
	@java -jar khartes/target/khartes.jar

clean:
	@find . -name '*.ibc' -delete
	@rm -rf khartes

# Local Variables:
# indent-tabs-mode: t
# End:
