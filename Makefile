
C=fsc
CFLAGS=-unchecked -deprecation

all: main

main:
	mkdir -p classes;
	$(C) -d classes/ src/*/*.scala src/*/*/*.scala $(CFLAGS);
	cp src/terminus .;
	chmod a+x terminus

clean:
	fsc -shutdown;
	rm -rf classes/;
	rm -f terminus
