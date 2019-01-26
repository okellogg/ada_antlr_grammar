# Adapt this to the C++ include directory of your ANTLR installation:
ANTLR_CPP_INC_DIR ?= /usr/include

# Adapt this to the directory of the C++ libantlr.a of your ANTLR installation:
ANTLR_CPP_LIB_DIR ?= /usr/lib64

# Adapt this to the jarfile of your ANTLR installation:
ANTLR_JAR ?= /usr/share/java/antlr-2.7.7.jar

#####

CPPFLAGS += -g -I${ANTLR_CPP_INC_DIR}

LFLAGS += -L${ANTLR_CPP_LIB_DIR} -lantlr

GENERATED_CPP_FILES := AdaLexer.cpp AdaParser.cpp

GEN_OBJ_FILES := $(GENERATED_CPP_FILES:%.cpp=%.o)

all : Main

Main : Main.cpp AdaUtil.o $(GEN_OBJ_FILES)
	g++ -o $@ $< AdaUtil.o $(GEN_OBJ_FILES) $(LFLAGS)

$(GENERATED_CPP_FILES) : ada.g
	java -cp ${ANTLR_JAR} antlr.Tool $<

ada.g : AdaAST.hpp AdaUtil.hpp

clean :
	$(RM) *.o

realclean : clean
	$(RM) $(GENERATED_CPP_FILES) $(GENERATED_CPP_FILES:%.cpp=%.hpp) AdaTokenTypes.hpp

