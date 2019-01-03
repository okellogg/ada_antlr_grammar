# Makefile on basis of ANTLR version 3
# ------------------------------------

# Point this to the root directory of your ANTLR3 git clone
# (https://github.com/antlr/antlr3)
ANTLR3_REPO_ROOT ?= ${HOME}/tools/antlr3

# Adapt this to the C include directory of your ANTLR installation:
ANTLR_C_INC_DIR ?= ${ANTLR3_REPO_ROOT}/runtime/C/include

# Adapt this to the C++ include directory of your ANTLR installation:
ANTLR_CPP_INC_DIR ?= ${ANTLR3_REPO_ROOT}/runtime/Cpp/include

# Adapt this to the directory of the libantlr3c.a of your ANTLR installation:
ANTLR_C_LIB_DIR ?= ${ANTLR3_REPO_ROOT}/runtime/C/.libs

# Adapt this to the jarfile of your ANTLR installation:
ANTLR_JAR ?= ${HOME}/Downloads/antlr-3.5.2-complete-no-st3.jar

#####

CPPFLAGS += -g -I${ANTLR_C_INC_DIR} -I${ANTLR_CPP_INC_DIR}

LFLAGS += -L${ANTLR_C_LIB_DIR} -lantlr3c

GENERATED_CPP_FILES := AdaLexer.cpp AdaParser.cpp

GEN_OBJ_FILES := $(GENERATED_CPP_FILES:%.cpp=%.o)

all : Main

Main : Main.cpp $(GEN_OBJ_FILES)
	g++ -o $@ $< $(GEN_OBJ_FILES) $(LFLAGS)

$(GENERATED_CPP_FILES) : ada.g
	java -cp ${ANTLR_JAR} org.antlr.Tool $<

ada.g : AdaAST.hpp

clean :
	$(RM) *.o

realclean : clean
	$(RM) $(GENERATED_CPP_FILES) $(GENERATED_CPP_FILES:%.cpp=%.hpp) AdaTokenTypes.hpp

