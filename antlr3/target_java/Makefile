# Makefile on basis of ANTLR version 3
# ------------------------------------

# Adapt this to the jarfile of your ANTLR installation:
ANTLR_JAR ?= ${HOME}/Downloads/antlr-3.5.2-complete-no-st3.jar

#####

GENERATED_JAVA_FILES := AdaLexer.java AdaParser.java

GEN_CLASS_FILES := $(GENERATED_JAVA_FILES:%.java=%.class)

all : $(GEN_CLASS_FILES)

$(GENERATED_JAVA_FILES) : Ada.g
	java -cp ${ANTLR_JAR} org.antlr.Tool $<

%.class : %.java
	javac -cp .:${ANTLR_JAR} $<

clean :
	$(RM) *.class

realclean : clean
	$(RM) $(GENERATED_JAVA_FILES)

