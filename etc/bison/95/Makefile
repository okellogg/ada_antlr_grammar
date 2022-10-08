CFLAGS = -g -DYYDEBUG

all: aparse95

OBJECTS := y.tab.o lex.yy.o

EXE := aparse95

$(EXE) : $(OBJECTS)
	gcc $(OBJECTS) -o $@

lex.yy.c : lexer9x.l grammar9x.y
	flex $<

y.tab.c : grammar9x.y
	bison -d -v -t -y $<

clean:
	$(RM) *.o

realclean: clean
	$(RM) $(EXE) lex.yy.c y.tab.c y.tab.h y.output

