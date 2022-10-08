CFLAGS = -std=gnu99 -g -DYYDEBUG

EXE = aparse22

all: $(EXE)

cex:
	bison -d -Wcounterexamples -Wother grammar2x.y >& Wcounterexamples.log
	$(MAKE) all

OBJECTS := grammar2x.tab.o lex.yy.o

$(EXE) : $(OBJECTS)
	gcc $(OBJECTS) -o $@

lex.yy.c : lexer2x.l grammar2x.y
	flex $<

grammar2x.tab.c : grammar2x.y
	bison -d -v -t $<

clean:
	$(RM) *.o

realclean: clean
	$(RM) $(EXE) lex.yy.c grammar2x.tab.c grammar2x.tab.h grammar2x.output
