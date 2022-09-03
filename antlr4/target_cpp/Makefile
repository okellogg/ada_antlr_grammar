# target_cpp/Makefile
#
# Crude Makefile for Ada.g4 C++ target build
# To use this,
# - define a variable ANTLR4 pointing to your ANTLR4 checkout or distro dir;
# - at the `java` invocation in rule `gen` adjust the path starting with
#   ${MVN_HOME} to your antlr4 jarfile.

OBJECTS := AdaParser.o AdaLexer.o

ifeq ($(visit),1)
  OBJECTS := AdaListener.o AdaVisitor.o AdaBaseListener.o AdaBaseVisitor.o $(OBJECTS)
  ANTLR_OPTIONS := -listener -visitor
endif

CXXFLAGS += -g -O0 -Wall -Wno-overloaded-virtual

# We don't need this on our GCC, it is new enough (C++17 is the default)
# CXXFLAGS += -std=c++17

all: gen compile

gen:
	if [ ! -e generated ]; then mkdir generated; fi
	java -jar ${MVN_HOME}/org/antlr/antlr4/4.10.1/antlr-4.10.1-complete.jar \
	       -Dlanguage=Cpp $(ANTLR_OPTIONS) -o generated/ -package adalr Ada.g4

compile:
	c++ -I${ANTLR4}/runtime/Cpp/runtime/src \
	    -I${ANTLR4}/runtime/Cpp/runtime/src/misc \
	    -I${ANTLR4}/runtime/Cpp/runtime/src/atn \
	    -I${ANTLR4}/runtime/Cpp/runtime/src/dfa \
	    -I${ANTLR4}/runtime/Cpp/runtime/src/tree \
	    -I${ANTLR4}/runtime/Cpp/runtime/src/support \
	    -I./generated -c $(CXXFLAGS) generated/*.cpp main.cpp
	c++ -g main.o AdaParser.o AdaLexer.o -o adalr ${ANTLR4}/runtime/Cpp/dist/libantlr4-runtime.a -luuid

clean:
	$(RM) generated/*

