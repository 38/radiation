#SBT Sucks, do not consider it until it support macro
CLASSPATH=lib/rhino-1.7R5.jar
OUTPUT=bin/
all: build

build: 
	scalac -cp$(CLASSPATH):$(OUTPUT) -d$(OUTPUT) `find -name '*.scala'`

clean: 
	rm -rf bin/

.PHONY:clean

