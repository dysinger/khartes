.PHONY: default all clean test

default: all

all: aws/target/aws.jar

aws/pom.xml:
	@idris --mvn --codegen Java -o aws Main.idr

aws/target/aws.jar: aws/pom.xml
	@cd aws && mvn -DmainClass=aws compile package shade:shade

test:
	@true

run:
	@java -jar aws/target/aws.jar

clean:
	@find . -name '*.ibc' -delete
	@rm -rf aws

# Local Variables:
# indent-tabs-mode: t
# End:
