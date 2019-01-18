/*
 * Ada2005 Recognizer for ANTLR V3
 *
 * Copyright (C) 2019 O. Kellogg <okellogg@users.sourceforge.net>
 *
 * Copying permitted if accompanied by this statement.
 * Derivative works are permitted if accompanied by this statement.
 *
 * Not all rules from the Ada2005 Reference Manual (RM) Annex P,
 * Syntax Summary, are mirrored as rules here.
 * The tree nodes follow the RM grammar as closely as sensible.
 * This applies in particular to the terminals. OTOH, trivially
 * reconstructable non-terminal rules are not reflected in the tree.
 * The rules here deviate from the RM rules when ANTLR reports
 * ambiguities which are resolved manually (without using syntactic
 * predicates).
 *
 */


grammar Ada;

options {
  language = Java;
  output = AST;
  k = 2;
  // ASTLabelType = AdaAST;
}

tokens {
  // RM tokens (synthetic)
  ABORTABLE_PART;
  ABORT_STATEMENT;
  ABSTRACT_SUBPROGRAM_DECLARATION;  /* =>
			     ABSTRACT_{FUNCTION|PROCEDURE}_DECLARATION  */
  ACCEPT_ALTERNATIVE;
  ACCEPT_STATEMENT;
  /* ACCESS_TO_FUNCTION_DEFINITION => ACCESS_TO_FUNCTION_DECLARATION */
  /* ACCESS_TO_OBJECT_DEFINITION => ACCESS_TO_OBJECT_DECLARATION */
  /* ACCESS_TO_PROCEDURE_DEFINITION => ACCESS_TO_PROCEDURE_DECLARATION */
  /* ACCESS_TYPE_DEFINITION => ACCESS_TYPE_DECLARATION */
  ALLOCATOR;
  /* ARRAY_TYPE_DEFINITION => ARRAY_TYPE_DECLARATION */
  ASSIGNMENT_STATEMENT;
  ASYNCHRONOUS_SELECT;
  ATTRIBUTE_DEFINITION_CLAUSE;
  AT_CLAUSE;
  BLOCK_STATEMENT;
  CASE_STATEMENT;
  CASE_STATEMENT_ALTERNATIVE;
  CODE_STATEMENT;
  COMPONENT_DECLARATION;
  COMPONENT_DEFINITION;
  COMPONENT_LIST;    // not currently used as an explicit node
  CONDITION;
  CONDITIONAL_ENTRY_CALL;
  CONTEXT_CLAUSE;
  /* DECIMAL_FIXED_POINT_DEFINITION => DECIMAL_FIXED_POINT_DECLARATION */
  DECLARATIVE_ITEM;  // not currently used
  DECLARATIVE_PART;
  DEFINING_IDENTIFIER_LIST;
  DELAY_ALTERNATIVE;
  DELAY_STATEMENT;
  DELTA_CONSTRAINT;
  /* DERIVED_TYPE_DEFINITION;  =>
     DERIVED_RECORD_EXTENSION, ORDINARY_DERIVED_TYPE_DECLARATION */
  DIGITS_CONSTRAINT;
  DISCRETE_RANGE;   // Not used; instead, directly use its RHS alternatives.
  DISCRIMINANT_ASSOCIATION;
  DISCRIMINANT_CONSTRAINT;
  DISCRIMINANT_SPECIFICATION;
  ENTRY_BODY;
  ENTRY_CALL_ALTERNATIVE;
  ENTRY_CALL_STATEMENT;
  ENTRY_DECLARATION;
  /* ENTRY_INDEX_SPECIFICATION => ENTRY_INDEX_SPECIFICATION_OPT */
  ENUMERATION_REPESENTATION_CLAUSE;
  /* ENUMERATION_TYPE_DEFINITION => ENUMERATION_TYPE_DECLARATION */
  EXCEPTION_DECLARATION;
  EXCEPTION_HANDLER;
  EXCEPTION_RENAMING_DECLARATION;
  EXIT_STATEMENT;
  EXTENDED_RETURN_STATEMENT;
  /* FLOATING_POINT_DEFINITION => FLOATING_POINT_DECLARATION */
  /* FORMAL_ACCESS_TYPE_DEFINITION => FORMAL_ACCESS_TYPE_DECLARATION */
  /* FORMAL_ARRAY_TYPE_DEFINITION => FORMAL_ARRAY_TYPE_DECLARATION */
  /* FORMAL_DECIMAL_FIXED_POINT_DEFINITION =>
     FORMAL_DECIMAL_FIXED_POINT_DECLARATION */
  /* FORMAL_DERIVED_TYPE_DEFINITION =>
     FORMAL_{ORDINARY_DERIVED_TYPE|PRIVATE_EXTENSION}_DECLARATION */
  /* FORMAL_DISCRETE_TYPE_DEFINITION => FORMAL_DISCRETE_TYPE_DECLARATION */
  /* FORMAL_FLOATING_POINT_DEFINITION =>
     FORMAL_FLOATING_POINT_DECLARATION */
  FORMAL_INCOMPLETE_TYPE_DECLARATION;
  /* FORMAL_INTERFACE_TYPE_DEFINITION => INTERFACE_TYPE_DEFINITION */
  /* FORMAL_MODULAR_TYPE_DEFINITION => FORMAL_MODULAR_TYPE_DECLARATION */
  FORMAL_OBJECT_DECLARATION;
  /* FORMAL_ORDINARY_FIXED_POINT_DEFINITION =>
     FORMAL_ORDINARY_FIXED_POINT_DECLARATION */
  FORMAL_PACKAGE_ASSOCIATION;
  FORMAL_PACKAGE_DECLARATION;
  /* FORMAL_PRIVATE_TYPE_DEFINITION => FORMAL_PRIVATE_TYPE_DECLARATION */
  /* FORMAL_SIGNED_INTEGER_TYPE_DEFINITION =>
     FORMAL_SIGNED_INTEGER_TYPE_DECLARATION */
  /* FORMAL_SUBPROGRAM_DECLARATION;  =>
     FORMAL_{FUNCTION|PROCEDURE}_DECLARATION  */
  FORMAL_TYPE_DECLARATION; /* not used, replaced by the corresponding
			      finer grained declarations  */
  /* FORMAL_TYPE_DEFINITION; not used at all; we use declarations
			     not definitions */
  FULL_TYPE_DECLARATION;   /* not used, replaced by the corresponding
			      finer grained declarations  */
  /* FUNCTION_CALL must be established by semantic analysis of PARENTHESIZED_EXPR */
  GENERIC_FORMAL_PART;
  GENERIC_INSTANTIATION;  /* =>
     GENERIC_{FUNCTION|PACKAGE|PROCEDURE}_INSTANTIATION  */
  GENERIC_PACKAGE_DECLARATION;
  GENERIC_RENAMING_DECLARATION;  /* =>
     GENERIC_{FUNCTION|PACKAGE|PROCEDURE}_RENAMING  */
  GENERIC_SUBPROGRAM_DECLARATION; /* =>
     GENERIC_{FUNCTION|PROCEDURE}_DECLARATION  */
  GOTO_STATEMENT;
  HANDLED_SEQUENCE_OF_STATEMENTS;
  IF_STATEMENT;
  INCOMPLETE_TYPE_DECLARATION;
  /* INDEXED_COMPONENT must be established by semantic analysis of PARENTHESIZED_EXPR */
  INDEX_CONSTRAINT;
  INTERFACE_TYPE_DEFINITION;
  KNOWN_DISCRIMINANT_PART;
  LIBRARY_ITEM;
  LOOP_STATEMENT;
  MODE;
  /* MODULAR_TYPE_DEFINITION => MODULAR_TYPE_DECLARATION  */
  NAME;
  /* NULL_EXCLUSION => NULL_EXCLUSION_OPT */
  NULL_PROCEDURE_DECLARATION;
  NULL_STATEMENT;
  NUMBER_DECLARATION;
  OBJECT_DECLARATION;
  OBJECT_RENAMING_DECLARATION;
  OPERATOR_SYMBOL;
  /* ORDINARY_FIXED_POINT_DEFINITION => ORDINARY_FIXED_POINT_DECLARATION  */
  PACKAGE_BODY;
  PACKAGE_BODY_STUB;
  PACKAGE_RENAMING_DECLARATION;
  PACKAGE_SPECIFICATION;
  PARAMETER_SPECIFICATION;
  PREFIX;
  PRIMARY;
  PRIVATE_EXTENSION_DECLARATION;
  PRIVATE_TYPE_DECLARATION;
  PROCEDURE_CALL_STATEMENT;  // NYI, using CALL_STATEMENT for now.
  PROTECTED_BODY;
  PROTECTED_BODY_STUB;
  PROTECTED_TYPE_DECLARATION;
  RAISE_STATEMENT;
  RANGE_ATTRIBUTE_REFERENCE;
  RECORD_REPRESENTATION_CLAUSE;
  /* RECORD_TYPE_DEFINITION => RECORD_TYPE_DECLARATION */
  REQUEUE_STATEMENT;
  RETURN_SUBTYPE_INDICATION;
  SELECTIVE_ACCEPT;
  SELECT_ALTERNATIVE;  /* Not used - instead, we use the finer grained rules
                          ACCEPT_ALTERNATIVE | DELAY_ALTERNATIVE
                          | TERMINATE_ALTERNATIVE  */
  SELECT_STATEMENT;    /* Not used - instead, we use the finer grained rules
                        SELECTIVE_ACCEPT | TIMED_ENTRY_CALL
                        | CONDITIONAL_ENTRY_CALL | ASYNCHRONOUS_SELECT  */
  SEQUENCE_OF_STATEMENTS;
  /* SIGNED_INTEGER_TYPE_DEFINITION => SIGNED_INTEGER_TYPE_DECLARATION */
  SIMPLE_RETURN_STATEMENT;
  SINGLE_PROTECTED_DECLARATION;
  SINGLE_TASK_DECLARATION;
  STATEMENT;
  STATEMENT_IDENTIFIER;
  SUBPROGRAM_BODY;  /* => {FUNCTION|PROCEDURE}_BODY  */
  SUBPROGRAM_BODY_STUB;  /* => {FUNCTION|PROCEDURE}_BODY_STUB  */
  SUBPROGRAM_DECLARATION;  /* => {FUNCTION|PROCEDURE}_DECLARATION  */
  SUBPROGRAM_RENAMING_DECLARATION;  /* =>
			     {FUNCTION|PROCEDURE}_RENAMING_DECLARATION  */
  SUBTYPE_DECLARATION;
  SUBTYPE_INDICATION;
  SUBTYPE_MARK;
  SUBUNIT;
  TASK_BODY;
  TASK_BODY_STUB;
  TASK_TYPE_DECLARATION;
  TERMINATE_ALTERNATIVE;
  TIMED_ENTRY_CALL;
  TRIGGERING_ALTERNATIVE;
  /* TYPE_CONVERSION must be established by semantic analysis of PARENTHESIZED_EXPR */
  TYPE_DECLARATION;   /* not used, replaced by the corresponding
			 finer grained declarations  */
  USE_CLAUSE;
  USE_PACKAGE_CLAUSE;
  USE_TYPE_CLAUSE;
  VARIANT;
  VARIANT_PART;
  WITH_CLAUSE;

  // Non RM synthetic tokens:
  // They exist mainly to normalize the node structure with respect to
  // optional items. (Without them, the presence or absence of an optional
  // item would change the node layout but we want a fixed layout.)
  ABSTRACT_FUNCTION_DECLARATION;
  ABSTRACT_PROCEDURE_DECLARATION;
  ACCESS_TO_FUNCTION_DECLARATION;
  ACCESS_TO_OBJECT_DECLARATION;
  ACCESS_TO_PROCEDURE_DECLARATION;
  ACCESS_TYPE_DECLARATION;  /* not used, replaced by
                             ACCESS_TO_{FUNCTION|OBJECT|PROCEDURE}_DECLARATION
			     */
  AND_INTERFACE_LIST_OPT;
  ARRAY_AGGREG_ELEM_S;
  ARRAY_OBJECT_DECLARATION;
  ARRAY_TYPE_DECLARATION;
  AND_THEN;
  BASIC_DECLARATIVE_ITEMS_OPT;
  BLOCK_BODY;
  BLOCK_BODY_OPT;
  CALL_STATEMENT;       // See {PROCEDURE|ENTRY}_CALL_STATEMENT
  COMPONENT_CLAUSES_OPT;
  COMPONENT_ITEMS;
  COND_CLAUSE;
  DECIMAL_FIXED_POINT_DECLARATION;
  DECLARE_OPT;
  DERIVED_RECORD_EXTENSION;
  DERIVED_TYPE_DECLARATION;
  DISCRETE_SUBTYPE_DEF_OPT;
  DISCRIM_PART_OPT;
  ELSE_OPT;
  ELSIFS_OPT;
  ENTRY_INDEX_OPT;
  ENTRY_INDEX_SPECIFICATION_OPT;
  ENUMERATION_TYPE_DECLARATION;
  EXCEPT_HANDLER_PART_OPT;
  EXTENSION_OPT;
  FLOATING_POINT_DECLARATION;
  /* FORMAL_ACCESS_TYPE_DECLARATION => ACCESS_TYPE_DECLARATION */
  /* FORMAL_ARRAY_TYPE_DECLARATION => ARRAY_TYPE_DECLARATION */
  FORMAL_DECIMAL_FIXED_POINT_DECLARATION;
  FORMAL_DISCRETE_TYPE_DECLARATION;
  FORMAL_FLOATING_POINT_DECLARATION;
  FORMAL_FUNCTION_DECLARATION;
  FORMAL_MODULAR_TYPE_DECLARATION;
  FORMAL_ORDINARY_DERIVED_TYPE_DECLARATION;
  FORMAL_ORDINARY_FIXED_POINT_DECLARATION;
  FORMAL_PACKAGE_ACTUAL_PART_OPT;
  FORMAL_PACKAGE_ASSOCIATION_S;
  FORMAL_PART_OPT;
  FORMAL_PRIVATE_EXTENSION_DECLARATION;
  FORMAL_PRIVATE_TYPE_DECLARATION;
  FORMAL_PROCEDURE_DECLARATION;
  FORMAL_SIGNED_INTEGER_TYPE_DECLARATION;
  FUNCTION_BODY;
  FUNCTION_BODY_STUB;
  FUNCTION_DECLARATION;
  FUNCTION_RENAMING_DECLARATION;
  GENERIC_FUNCTION_DECLARATION;
  GENERIC_FUNCTION_INSTANTIATION;
  GENERIC_FUNCTION_RENAMING;
  GENERIC_PACKAGE_INSTANTIATION;
  GENERIC_PACKAGE_RENAMING;
  GENERIC_PROCEDURE_DECLARATION;
  GENERIC_PROCEDURE_INSTANTIATION;
  GENERIC_PROCEDURE_RENAMING;
  GUARD_OPT;
  IDENTIFIER_COLON_OPT;
  INIT_OPT;
  ITERATION_SCHEME_OPT;
  LABELS_OPT;
  MARK_WITH_CONSTRAINT;
  MODIFIERS;  /* Possible values: abstract access aliased all constant in "in out"
                 limited out private protected reverse synchronized tagged task */
  MODULAR_TYPE_DECLARATION;
  MOD_CLAUSE_OPT;
  // NAME_OR_QUALIFIED;
  NEW_INTERFACELIST_WITH_OPT;
  NOT_IN;
  NULL_EXCLUSION_OPT;
  ORDINARY_DERIVED_TYPE_DECLARATION;
  ORDINARY_FIXED_POINT_DECLARATION;
  OR_ELSE;
  OR_SELECT_OPT;
  OVERRIDING_OPT;
  PARENTHESIZED_EXPR;
  PARENTHESIZED_PRIMARY;
  PRIVATE_TASK_ITEMS_OPT;
  PROCEDURE_BODY;
  PROCEDURE_BODY_STUB;
  PROCEDURE_DECLARATION;
  PROCEDURE_RENAMING_DECLARATION;
  PROT_MEMBER_DECLARATIONS;
  PROT_OP_BODIES_OPT;
  PROT_OP_DECLARATIONS;
  RANGED_EXPRS;  // ugh, what an ugly name
  RECORD_TYPE_DECLARATION;
  SELECTOR_NAMES_OPT;
  SIGNED_INTEGER_TYPE_DECLARATION;
  TASK_ITEMS_OPT;
  /* We cannot currently distinguish between
     INDEXED_COMPONENT, TYPE_CONVERSION, FUNCTION_CALL :
     See NAME with PARENTHESIZED_EXPR */
  UNARY_MINUS;
  UNARY_PLUS;
  VALUE;
  VALUES;
  VARIANTS;
}

@header {
import java.io.*;
import org.antlr.runtime.*;
/*
import org.antlr.runtime.CharStream;
import org.antlr.runtime.Parser;
import org.antlr.runtime.BitSet;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.TokenStream;
import org.antlr.runtime.NoViableAltException;
 */
}

@members {

  // Ada support stuff
  private java.util.Stack<String> m_def_id =
      new java.util.Stack<String>();
  void push_def_id (String defid) {
    m_def_id.push(defid);
  }
  String pop_def_id () {
    String defid = m_def_id.pop();
    return defid;
  }
  boolean end_id_matches_def_id (String endStr) {
    if (m_def_id.size() < 1)
      return false;
    String defStr = pop_def_id();
    return defStr.compareToIgnoreCase(endStr) == 0;
  }
  boolean definable_operator (String string) { // operator_symbol sans "/="
    final String[] ops = {
                          "and", "or", "xor",           // logical
                          "=", "<", "<=", ">", ">=",    // relational (omitting "/=")
                          "+", "\055", "&",             // binary/unary adding - somehow ANTLR does not like "-", had to write as "\055"
                          "*", "/", "mod", "rem",       // multiplying
                          "**", "abs", "not"            // highest precedence
                        };
    for (int i = 0; i < ops.length; i++)
    {
      if (string.compareToIgnoreCase(ops[i]) == 0)
        return true;
    }
    return false;
  }
  boolean is_operator_symbol (String string) {
    return definable_operator(string) || string == "/=";
  }
  void set(Token t, int tokenType, String tokenText) {
    t.setType(tokenType);
    t.setText(tokenText);
  }

    /* Modified from: grammars-v3/Pascal/pascal.g  */

    // This method decides what action to take based on the type of
    //   file we are looking at
    public  static void doFile(File f) throws Exception {
      // If this is a directory, walk each file/dir in that directory
      translateFilePath = f.getParent();
      if (f.isDirectory()) {
        String files[] = f.list();
        for(int i=0; i < files.length; i++)
        {
          doFile(new File(f, files[i]));
        }
      }
      // Otherwise, parse it.
      else
      {
        final String fName = f.getName();
        System.out.println("   " + f.getAbsolutePath());

        if (translateFileName == null) {
          translateFileName = fName; //set this file as the one to translate
          currentFileName = fName;
        }

        // parseFile(fName, new ANTLRNoCaseFileStream(f.getAbsolutePath()));
        parseFile(fName, new ANTLRFileStream(f.getAbsolutePath()));
      }
    }

    // Here's where we do the real work...
    public static void parseFile(String f, ANTLRFileStream /*ANTLRNoCaseFileStream*/ s)
                                throws Exception {
      try {
        currentFileName = f; // set this File as the currentFileName

        // Create a scanner that reads from the input stream passed to us
         AdaLexer lexer = new AdaLexer(s);

         TokenStream tokenStream = new CommonTokenStream(lexer);
         // Create a parser that reads from the scanner
         AdaParser parser = new AdaParser(tokenStream);

         // set AST type to AdaAST (has symbol)
         //  parser.setASTNodeClass("AdaAST");
         //parser.setTreeAdaptor(adaAdaptor);

         // start parsing at the program rule
         compilation_unit_return res = parser.compilation_unit(); 

         // CommonAST t = (CommonAST)res.getTree();

         // do something with the tree
         parser.doTreeAction(f, (CommonTree)res.getTree(), parser.getTokenNames());
         // System.out.println(parser.getAST().toStringList());
      }
      catch (Exception e) {
        System.err.println("parser exception: "+e);
        e.printStackTrace();   // so we can get stack trace
      }
    }

    public void printTree(CommonTree t, int indent) {
      if ( t != null ) {
        StringBuffer sb = new StringBuffer(indent);
        for ( int i = 0; i < indent; i++ )
          sb = sb.append("   ");
        for ( int i = 0; i < t.getChildCount(); i++ ) {
          System.out.println(sb.toString() + t.getChild(i).toString());
          printTree((CommonTree)t.getChild(i), indent+1);
        }
      }
    }

    public void doTreeAction(String f, CommonTree t, String[] tokenNames) {
      if ( t==null ) return;
      if ( showTree ) {
        printTree(t,0);
       /*  ((CommonAST)t).setVerboseStringConversion(true, tokenNames);
         ASTFactory factory = new ASTFactory();
         AST r = factory.create(0,"AST ROOT");
         r.setFirstChild(t);
         ASTFrame frame = new ASTFrame("Ada AST", r);
         frame.setVisible(true);*/
         //System.out.println(t.toStringList());
      }
    }

  static boolean showTree = true;
  public static String translateFilePath;
  public static String translateFileName;
  public static String currentFileName; // not static, recursive USES ... other FileName in currentFileName
  public static String oldtranslateFileName;

  // main program
  public static void main(String[] args) {
    // Use a try/catch block for parser exceptions
    try {
      // if we have at least one command-line argument
      if (args.length > 0 ) {

        // for each directory/file specified on the command line
        for (int i = 0; i < args.length; i++)
        {
          if ( args[i].equals("-showtree") ) {
             showTree = true;
          }
          else {
            System.err.println("Parsing...");
            doFile(new File(args[i])); // parse it
          }
        }
      }
      else
        System.err.println("Usage: java AdaParser <file/directory name>");
    }
    catch(Exception e) {
      System.err.println("exception: "+e);
      e.printStackTrace(System.err);   // so we can get stack trace
    }
  }

}

/* Fragments for case insensitivity */
fragment A : ('a'|'A');
fragment B : ('b'|'B');
fragment C : ('c'|'C');
fragment D : ('d'|'D');
fragment E : ('e'|'E');
fragment F : ('f'|'F');
fragment G : ('g'|'G');
fragment H : ('h'|'H');
fragment I : ('i'|'I');
fragment J : ('j'|'J');
fragment K : ('k'|'K');
fragment L : ('l'|'L');
fragment M : ('m'|'M');
fragment N : ('n'|'N');
fragment O : ('o'|'O');
fragment P : ('p'|'P');
fragment Q : ('q'|'Q');
fragment R : ('r'|'R');
fragment S : ('s'|'S');
fragment T : ('t'|'T');
fragment U : ('u'|'U');
fragment V : ('v'|'V');
fragment W : ('w'|'W');
fragment X : ('x'|'X');
fragment Y : ('y'|'Y');
fragment Z : ('z'|'Z');

/* Keywords */
ABORT            : A B O R T ;
ABS              : A B S ;
ABSTRACT         : A B S T R A C T ;
ACCEPT           : A C C E P T ;
ACCESS           : A C C E S S ;
ALIASED          : A L I A S E D ;
ALL              : A L L ;
AND              : A N D ;
ARRAY            : A R R A Y ;
AT               : A T ;
BEGIN            : B E G I N ;
BODY             : B O D Y ;
CASE             : C A S E ;
CONSTANT         : C O N S T A N T ;
DECLARE          : D E C L A R E ;
DELAY            : D E L A Y ;
DELTA            : D E L T A ;
DIGITS           : D I G I T S ;
DO               : D O ;
ELSE             : E L S E ;
ELSIF            : E L S I F ;
END              : E N D ;
ENTRY            : E N T R Y ;
EXCEPTION        : E X C E P T I O N ;
EXIT             : E X I T ;
FOR              : F O R ;
FUNCTION         : F U N C T I O N ;
GENERIC          : G E N E R I C ;
GOTO             : G O T O ;
IF               : I F ;
IN               : I N ;
INTERFACE        : I N T E R F A C E ;
IS               : I S ;
LIMITED          : L I M I T E D ;
LOOP             : L O O P ;
MOD              : M O D ;
NEW              : N E W ;
NOT              : N O T ;
NuLL             : N U L L ;    // avoid NULL (conflict with ANTLR symbol)
OF               : O F ;
OR               : O R ;
OTHERS           : O T H E R S ;
OUT              : O U T ;
OVERRIDING       : O V E R R I D I N G ;
PACKAGE          : P A C K A G E ;
PARALLEL         : P A R A L L E L ;
PRAGMA           : P R A G M A ;
PRIVATE          : P R I V A T E ;
PROCEDURE        : P R O C E D U R E ;
PROTECTED        : P R O T E C T E D ;
RAISE            : R A I S E ;
RANGE            : R A N G E ;
RECORD           : R E C O R D ;
REM              : R E M ;
RENAMES          : R E N A M E S ;
REQUEUE          : R E Q U E U E ;
RETURN           : R E T U R N ;
REVERSE          : R E V E R S E ;
SELECT           : S E L E C T ;
SEPARATE         : S E P A R A T E ;
SOME             : S O M E ;
SUBTYPE          : S U B T Y P E ;
SYNCHRONIZED     : S Y N C H R O N I Z E D ;
TAGGED           : T A G G E D ;
TASK             : T A S K ;
TERMINATE        : T E R M I N A T E ;
THEN             : T H E N ;
TYPE             : T Y P E ;
UNTIL            : U N T I L ;
USE              : U S E ;
WHEN             : W H E N ;
WHILE            : W H I L E ;
WITH             : W I T H ;
XOR              : X O R ;

/* Quasi keywords for 9.5 synchronization_kind */
BY_ENTRY         : B Y '_' E N T R Y ;
BY_PROTECTED_PROCEDURE : B Y '_' P R O T E C T E D '_' P R O C E D U R E ;
OPTIONAL         : O P T I O N A L ;

/* Quasi keyword for 13.1.1 aspect_mark */
CLASS            : C L A S S ;

/* Quasi keyword for 13.11.3 storage_pool_indicator */
STANDARD         : S T A N D A R D ;

/*----------------------------------------------------------------------------
   OPERATORS
  ----------------------------------------------------------------------------*/
DOT_DOT            :       '..'    ;
LT_LT              :       '<<'    ;
BOX                :       '<>'    ;
GT_GT              :       '>>'    ;
ASSIGN             :       ':='    ;
RIGHT_SHAFT        :       '=>'    ;
NE                 :       '/='    ;
LE                 :       '<='    ;
GE                 :       '>='    ;
EXPON              :       '**'    ;
PIPE               :       '|'     ;
CONCAT             :       '&'     ;
DOT                :       '.'     ;
EQ                 :       '='     ;
LESSTHAN           :       '<'     ;  // avoid LT (conflict with ANTLR symbol)
GREATERTHAN        :       '>'     ;  // for symmetry with LESSTHAN
PLUS               :       '+'     ;
MINUS              :       '-'     ;
MUL                :       '*'     ;
DIV                :       '/'     ;
LPAREN             :       '('     ;
RPAREN             :       ')'     ;
COLON              :       ':'     ;
COMMA              :       ','     ;
SEMI               :       ';'     ;

fragment CHARACTER_LITERAL  :      ;  // will come to life at TIC

/* Literals */

IDENTIFIER
	// options {testLiterals=true;}
            : ( 'A'..'Z' | 'a'..'z' ) ( ('_')? ( 'A'..'Z' | 'a'..'z' | '0'..'9' ) )*
	;


/* ANTLR-2 ada.g had a more refined disambiguation of TIC from CHARACTER_LITERAL
   but required k=4.  Setting k above 2 with ANTLR-3 gives "method too large"
   or "JVM out of memory" errors, at least on this-here grammar.  */
TIC     : '\'' 
	( { input.LA(2) == '\'' }? => . '\'' { $type = CHARACTER_LITERAL; }
	| /* empty */
	)
	;

CHAR_STRING : '"' ('""' | ~('"'))* '"'
	// " <- for braindead editor syntax highlighter
	;

NUMERIC_LIT : ( DIGIT )+
		( '#' BASED_INTEGER ( '.' BASED_INTEGER )? '#'
		| ( '_' ( DIGIT )+ )+  // INTEGER
		)?
		( { input.LA(2) != '.' }? =>
			( '.' ( DIGIT )+ ( '_' ( DIGIT )+ )* ( EXPONENT )?
			| EXPONENT
			)?
		| /* empty */
		)
	;

// a couple protected methods to assist in matching the various numbers

fragment
DIGIT   :  ( '0'..'9' ) ;

fragment
EXPONENT           :  E ('+'|'-')? ( DIGIT )+ ;

fragment
EXTENDED_DIGIT     :  ( DIGIT | 'A'..'F' | 'a'..'f' ) ;

fragment
BASED_INTEGER      :  ( EXTENDED_DIGIT ) ( ('_')? EXTENDED_DIGIT )* ;


// Whitespace -- ignored
WS	:	(	' '
		|	'\t'
		|	'\f'
		|	'\r\n'
		|	'\r'
		|	'\n'
		)
		{ $channel=HIDDEN; }
	;

// Single-line comments
COMMENT :	( '--' (~('\n'|'\r'))* ('\n'|'\r'('\n')?) )
		{ $channel=HIDDEN; }
	;

/* Compilation Unit:  This is the start rule for this parser.
   The rules in this grammar are listed in the order in which
   compilation_unit introduces them, depth first, with the
   exception of the expression related rules which are listed
   towards the end.
   The rule library_unit_declaration is not materialized because
   it acts as a passthrough.
   10.1.1  */
compilation_unit :
	context_clause
	( library_item | subunit )
	( pragma )*
	;

// The pragma related rules are pulled up here to get them out of the way.
// 2.8
pragma  : PRAGMA^ IDENTIFIER pragma_args_opt SEMI!
	;

pragma_args_opt : ( LPAREN! pragma_argument_association ( COMMA! pragma_argument_association )* RPAREN! )?
	;

pragma_argument_association : ( IDENTIFIER RIGHT_SHAFT^ )? expression
	;

// 10.1.2
context_item :
	  pragma  // RM Annex P neglects pragmas; we include them.
	| with_clause
	| use_clause
	;

context_clause :
	( options { greedy=true; } : context_item )*
	;

limited_private_opt : ( LIMITED )? ( PRIVATE )?
	;

with_clause : limited_private_opt w=WITH^ compound_name_list SEMI!
	{ set($w, WITH_CLAUSE, "WITH_CLAUSE"); }
	;

// synthetic (non RM)
compound_name_list : compound_name ( COMMA! compound_name )*
	;

compound_name : IDENTIFIER ( DOT^ IDENTIFIER )*
	// Strangely, the RM never defines this rule, which however is
	// required for tightening up the syntax of certain names
	// (library unit names etc.)
	;

// 8.4
use_clause : u=USE^
		( TYPE! subtype_mark ( COMMA! subtype_mark )*
			{ set($u, USE_TYPE_CLAUSE, "USE_TYPE_CLAUSE"); }
		| compound_name_list
			{ set($u, USE_PACKAGE_CLAUSE, "USE_PACKAGE_CLAUSE"); }
		)
	SEMI!
	;

// The RM defines `subtype_mark' as `name'.
// However, this looks overly permissive.
// AARM 3.2.2 4.a says:
// " Note that name includes attribute_reference; thus, S'Base can be used
//   as a subtype_mark. "
// Thus narrowing down the rule, albeit not to the particular Base attribute:
subtype_mark : compound_name ( TIC IDENTIFIER )?
	// -> ^(SUBTYPE_MARK $subtype_mark)
	;

// non RM
attribute_id : RANGE
	| DIGITS
	| DELTA
	| ACCESS
	| IDENTIFIER
	;

// 10.1.1
library_item : private_opt
		/* Slightly loose; PRIVATE can only precede
		  {generic|package|subprog}_decl.
		  Semantic check required to ensure it.*/
	lib_pkg_spec_or_body
	| lib_subprog_decl_or_rename_or_inst_or_body
	| generic_decl[true]
	// -> ^(LIBRARY_ITEM $library_item)
	;

private_opt : ( PRIVATE )?
	;

lib_pkg_spec_or_body
	: pkg=PACKAGE^
		( BODY! defining_identifier[true, true] IS! pkg_body_part end_id_opt! SEMI!
			{ set($pkg, PACKAGE_BODY, "PACKAGE_BODY"); }
		| defining_identifier[true, true] spec_decl_part[$pkg]
		)
	;

// 8.3.1 overriding_indicator is dissolved into overriding_opt because
//       overriding_indicator is only ever used as an optional item.
overriding_opt :
	( OVERRIDING )?
	;

// 6.1
// This is NOT at the library level.
subprogram_declaration :
	overriding_opt
	( p=PROCEDURE^ defining_identifier[false, false]
		( generic_subp_inst
			{ set($p, GENERIC_PROCEDURE_INSTANTIATION,
				 "GENERIC_PROCEDURE_INSTANTIATION"); }
		| formal_part_opt
			( renames { set($p, PROCEDURE_RENAMING_DECLARATION,
					   "PROCEDURE_RENAMING_DECLARATION"); }
			| is_separate_or_abstract_or_decl[$p]
			)
			SEMI!
		)
	| f=FUNCTION^ defining_designator[false, false]
		( generic_subp_inst
			{ set($f, GENERIC_FUNCTION_INSTANTIATION,
				 "GENERIC_FUNCTION_INSTANTIATION"); }
 		| parameter_and_result_profile
			( renames { set($f, FUNCTION_RENAMING_DECLARATION,
					   "FUNCTION_RENAMING_DECLARATION"); }
			| is_separate_or_abstract_or_decl[$f]
			)
			SEMI!
		)
	)
	;

// 3.1
defining_identifier [boolean lib_level, boolean push_id]
	: { lib_level }? cn=compound_name
		{ if (push_id) push_def_id($cn.text); }
	| { !lib_level }? n=IDENTIFIER
		{ if (push_id) push_def_id($n.getText()); }
	;

// Non RM rule factoring repeated pattern in subprogram_declaration and
// subprog_decl_or_rename_or_inst_or_body
generic_subp_inst : IS! generic_inst SEMI!
	;

// Tail of generic_instantiation, i.e. without "(package|procedure|function) ... is".
// generic_instantiation per se does not exist (optimized away to avoid syn preds).
// 12.3
generic_inst : NEW! compound_name ( LPAREN! generic_actual_part RPAREN! )?
	;

generic_actual_part : generic_association ( COMMA! generic_association )*
	;

// RM rule generic_association has explicit_generic_actual_parameter which would be:
//  expression | variable_name | subprogram_name | entry_name | subtype_mark | package_instance_name
// but since `expression' contains them all, we just use `expression':
generic_association : ( generic_formal_parameter_selector_name RIGHT_SHAFT^ )? expression
	;

// 12.3
generic_formal_parameter_selector_name : ( IDENTIFIER | operator_string )
	;

// 12.3
// explicit_generic_actual_parameter ::= expression | variable_name
//    | subprogram_name | entry_name | subtype_mark
//    | package_instance_name

// 4.3.3
array_aggregate :
	LPAREN!
	( array_aggreg_elem_s ( COMMA! others )?
	| others
	)
	RPAREN!
	;

array_aggreg_elem_s :
	array_aggreg_elem ( COMMA! array_aggreg_elem )*
	;

array_aggreg_elem : ranged_expr_s ( RIGHT_SHAFT^ expression )?
	;

others  : OTHERS^ RIGHT_SHAFT! expression
	;

/* "others" in value_s can appear anywhere.
   A semantic check is necessary to ensure that it appears last.  */
value : ( ranged_expr_s ( RIGHT_SHAFT^ expression )?
	| others
	)
	// -> ^(VALUE $value)
	;

ranged_expr_s : ranged_expr ( PIPE^ ranged_expr )*
	// -> ^(RANGED_EXPRS $ranged_expr_s)
	;

ranged_expr : expression
		( DOT_DOT^ simple_expression
		| RANGE^ range
		)?
	;

range_constraint : RANGE! range
	;

range : ( (range_dots) => range_dots
	| range_attribute_reference
	)
	// Current assumption is we don't need an extra node for range,
	// otherwise uncomment the following line:
	// -> ^(RANGE_EXPR $range)
	;

range_dots : simple_expression DOT_DOT^ simple_expression
	;

// 4.1.4
range_attribute_reference :
	prefix TIC! r=RANGE^ ( LPAREN! expression RPAREN! )?
	  { set($r, RANGE_ATTRIBUTE_REFERENCE, "RANGE_ATTRIBUTE_REFERENCE"); }
	;

// Non RM auxiliary rule for `prefix'
suffix :
	DOT^ ( ALL | IDENTIFIER )
	| p=LPAREN^ expression_s RPAREN!
	    { set($p, PARENTHESIZED_EXPR, "PARENTHESIZED_EXPR"); }
	;

// Here, the definition of `prefix' deviates from the RM.
// This gives us some more strictness than `name' (which the RM uses to
// define `prefix'.)   See also: name
// 4.1
prefix : IDENTIFIER ( suffix )*
	//  -> ^(PREFIX $prefix)
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
formal_parameters :
	LPAREN! parameter_specification ( SEMI! parameter_specification )* RPAREN!
	;

// non RM : 6.1 formal_part, optional
formal_part_opt : ( formal_parameters )?
	;

// 6.1
parameter_specification : def_ids_colon
	( ( null_exclusion_opt ACCESS ) => access_definition
	| aliased_opt mode null_exclusion_opt subtype_mark
	)
	init_opt
	// -> ^(PARAMETER_SPECIFICATION $parameter_specification)
	;

// non RM rule factoring repeated occurrence of defining_identifier_list followed by colon
def_ids_colon : defining_identifier_list COLON!
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
def_id_list_aux : IDENTIFIER ( COMMA! IDENTIFIER )*
	;

// 3.3.1
defining_identifier_list : def_id_list_aux
	// -> ^(DEFINING_IDENTIFIER_LIST $defining_identifier_list)
	;

// 6.1
mode : ( IN )? ( OUT )?
	;

// Non RM auxiliary rule for 8.5
// Embedding in a root node is not strictly necessary
// (from the calling context it is clear we are dealing with renaming)
// but makes tree dumps easier to follow.
renames : RENAMES^ name
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
name_suffix_aux :
	LPAREN!
		( (expression_s RPAREN) => expression_s RPAREN!
		| nullrec_or_values RPAREN!
		)
	;

// Non RM auxiliary rule for `name'
name_suffix :
	DOT^	( ALL
		| IDENTIFIER
		| CHARACTER_LITERAL
		| operator_string
		)
	| name_suffix_aux
	//   -> ^(PARENTHESIZED_EXPR $name_suffix)
	| TIC^ ( parenthesized_primary | attribute_id )
	;

/* TODO :
   `name' is not complete yet due to massive ambiguities which require
   semantic analysis using a symbol table and related lookup functions.
name ::=
     direct_name | explicit_dereference
   | indexed_component | slice
   | selected_component | attribute_reference
   | type_conversion | function_call
   | character_literal | qualified_expression
   // Ada2012: | generalized_reference | generalized_indexing | target_name
 */
name : ( IDENTIFIER | (operator_string) => operator_string )
		( name_suffix )*
	// -> ^(NAME $name)
	;

operator_string
	: { is_operator_symbol(input.LT(1).getText()) }?
		op=CHAR_STRING { $op.setType(OPERATOR_SYMBOL); }
	;

definable_operator_symbol returns [String d]
	@init { $d = null; }
	: { definable_operator(input.LT(1).getText()) }?
		op=CHAR_STRING { $op.setType(OPERATOR_SYMBOL);
				 $d = $op.getText(); }
	;

// Non RM auxiliary rule for `name' and parenthesized_primary
nullrec_or_values :
 	( NuLL RECORD!
 	| value_s extension_opt
 	)
	;

parenthesized_primary
	: pp=LPAREN^ nullrec_or_values RPAREN!
	  { set($pp, PARENTHESIZED_PRIMARY, "PARENTHESIZED_PRIMARY"); }
	;

extension_opt : ( WITH! ( NuLL RECORD! | value_s ) )?
	;

is_separate_or_abstract_or_decl [Token t]
	: IS! separate_or_abstract[t]
	| /* empty */
	  { // based on 6.1 subprogram_declaration
	    if (t.getType() == PROCEDURE)
	      set(t, PROCEDURE_DECLARATION, "PROCEDURE_DECLARATION");
	    else
	      set(t, FUNCTION_DECLARATION, "FUNCTION_DECLARATION");
	  }
	;

separate_or_abstract [Token t]
	: SEPARATE!
		{ // based on 10.1.3 subprogram_body_stub
		  if (t.getType() == PROCEDURE)
		    set(t, PROCEDURE_BODY_STUB, "PROCEDURE_BODY_STUB");
		  else
		    set(t, FUNCTION_BODY_STUB, "FUNCTION_BODY_STUB");
		}
	| ABSTRACT!
		{ // based on 3.9.3 abstract_subprogram_declaration
		  if (t.getType() == PROCEDURE)
		    set(t, ABSTRACT_PROCEDURE_DECLARATION,
			  "ABSTRACT_PROCEDURE_DECLARATION");
		  else
		    set(t, ABSTRACT_FUNCTION_DECLARATION,
			  "ABSTRACT_FUNCTION_DECLARATION");
		}
	| /* empty */
	  { t.getType() == PROCEDURE }? NuLL!
		{ // 6.7
		  set(t, NULL_PROCEDURE_DECLARATION,
			"NULL_PROCEDURE_DECLARATION");
		}
	;

// 6.1
defining_designator [boolean lib_level, boolean push_id]
	: { lib_level }? defining_identifier[true, push_id]
	| { !lib_level }? d=designator { if (push_id) push_def_id($d.text); }
	;

// 6.1
designator returns [String d]
	// @init { String op; }
	: op=definable_operator_symbol { $d = $op.text; }
	| n=IDENTIFIER { $d = $n.getText(); }
	;

parameter_profile : formal_part_opt
	;

parameter_and_result_profile :
	func_formal_part_opt RETURN! null_exclusion_opt ( subtype_mark | access_def_no_nullex )
	;

// Auxiliary rule for func_formal_part_opt
function_formal_part :
	LPAREN! func_param ( SEMI! func_param )* RPAREN!
	;

// formal_part_opt is not strict enough for functions, i.e. it permits
// "in out" and "out" as modes, thus we make an extra rule.
// We are currently on Ada2005; when we go to Ada2012 this can be replaced by formal_part_opt.
func_formal_part_opt : ( function_formal_part )?
	;

func_param : def_ids_colon
	( ( null_exclusion_opt ACCESS ) => access_definition
	| aliased_opt in_opt null_exclusion_opt subtype_mark
	)
	init_opt
	// -> ^(PARAMETER_SPECIFICATION $func_param)
	;

in_opt : ( IN )?
	;

spec_decl_part [Token pkg]
	: ( IS! ( generic_inst
		  { set(pkg, GENERIC_PACKAGE_INSTANTIATION, "GENERIC_PACKAGE_INSTANTIATION");
		    pop_def_id(); }
		| pkg_spec_part
		  { set(pkg, PACKAGE_SPECIFICATION, "PACKAGE_SPECIFICATION"); }
		)
	| renames { set(pkg, PACKAGE_RENAMING_DECLARATION, "PACKAGE_RENAMING_DECLARATION");
		    pop_def_id(); }
	)
	SEMI!
	;

pkg_spec_part : basic_declarative_items_opt
		( PRIVATE basic_declarative_items_opt )?
		end_id_opt!
	;

basic_declarative_items_opt : ( basic_declarative_item | pragma )*
	;

basic_declarative_item
	: pkg=PACKAGE^ defining_identifier[false, true] spec_decl_part[$pkg]
	| tsk=TASK^ task_type_or_single_decl[$tsk]
	| pro=PROTECTED^ prot_type_or_single_decl[$pro] SEMI!
	| subprogram_declaration
	| decl_common
	;

task_type_or_single_decl [Token tsk]
	: TYPE! defining_identifier[false, true] discrim_part_opt task_definition_opt
		{ set(tsk, TASK_TYPE_DECLARATION, "TASK_TYPE_DECLARATION"); }
	| defining_identifier[false, true] task_definition_opt
		{ set(tsk, SINGLE_TASK_DECLARATION, "SINGLE_TASK_DECLARATION"); }
	;

task_definition_opt
	: IS! new_interfacelist_with_opt task_items_opt private_task_items_opt end_id_opt! SEMI!
	| SEMI! { pop_def_id(); }
	;

discrim_part_opt :
	( discrim_part )?
	;

discrim_part :
	LPAREN! (BOX | known_discriminant_part) RPAREN!
	;

// 3.7
known_discriminant_part :
	discriminant_specification ( SEMI! discriminant_specification )*
	;

// 3.7
discriminant_specification :
	 def_ids_colon null_exclusion_opt ( subtype_mark | access_def_no_nullex ) init_opt
	// -> ^(DISCRIMINANT_SPECIFICATION $discriminant_specification)
	;

init_opt : ( ASSIGN expression )?
	;  // `expression' is of course much too loose;
	   // semantic checks are required in the usage contexts.

new_interfacelist_with_opt : ( NEW! interface_list WITH! )?
	;

task_items_opt : ( pragma )* entrydecls_repspecs_opt
	;

entrydecls_repspecs_opt : ( entry_declaration ( pragma | rep_spec )* )*
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
entry_decl :
	overriding_opt ENTRY! IDENTIFIER discrete_subtype_def_opt formal_part_opt SEMI!
	;

// 9.5.2
entry_declaration : entry_decl
	// -> ^(ENTRY_DECLARATION $entry_declaration)
	;

discrete_subtype_def_opt :
	( (LPAREN discrete_subtype_definition RPAREN) =>
	  LPAREN! discrete_subtype_definition RPAREN!
	| /* empty */
	)
	;

discrete_subtype_definition : ( (range) => range
	| subtype_indication
	)
	// Looks alot like discrete_range but it's not
	// (as soon as we start doing semantics.)
	;

rep_spec : r=FOR^ subtype_mark USE! rep_spec_part[$r] SEMI!
	;

rep_spec_part [Token t]
	: RECORD! mod_clause_opt comp_loc_s END! RECORD! // record_type_spec
		{ set(t, RECORD_REPRESENTATION_CLAUSE,
			"RECORD_REPRESENTATION_CLAUSE"); }
	| AT! expression                        // address_spec (Ada83)
		{ set(t, AT_CLAUSE, "AT_CLAUSE"); }
	| expression  // attrib_def. Semantic check must ensure that the
		// respective subtype_mark contains an attribute reference.
		{ set(t, ATTRIBUTE_DEFINITION_CLAUSE, "ATTRIBUTE_DEFINITION_CLAUSE"); }
	;

// J.8
mod_clause :
	AT! MOD! expression SEMI!
	;

mod_clause_opt : ( mod_clause )?
	;

// Variation of 13.5.1 component_clause to include PRAGMA
comp_loc :
	pragma | subtype_mark AT! expression RANGE! range SEMI!
	;

comp_loc_s : ( comp_loc )*
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
private_task_item :
	PRIVATE! ( pragma )* entrydecls_repspecs_opt
	;

// Auxiliary rule for AST normalization of 9.1 task_definition
private_task_items_opt : ( private_task_item )?
	;

prot_type_or_single_decl [Token pro]
	: TYPE! defining_identifier[false, true] discrim_part_opt protected_definition
		{ set(pro, PROTECTED_TYPE_DECLARATION, "PROTECTED_TYPE_DECLARATION"); }
	| defining_identifier[false, true] protected_definition
		{ set(pro, SINGLE_PROTECTED_DECLARATION, "SINGLE_PROTECTED_DECLARATION"); }
	;

protected_definition
	: IS! new_interfacelist_with_opt prot_op_decl_s ( PRIVATE! prot_member_decl_s )? end_id_opt!
	;

prot_op_decl_s : ( protected_operation_declaration )*
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
protected_procedure_declaration :
	PROCEDURE! defining_identifier[false, false] formal_part_opt SEMI!
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
protected_function_declaration :
	FUNCTION! defining_designator[false, false] parameter_and_result_profile SEMI!
	;

// 9.4
protected_operation_declaration : entry_declaration
	| protected_procedure_declaration
	// 	-> ^(PROCEDURE_DECLARATION $protected_operation_declaration)
	| protected_function_declaration
	// 	-> ^(FUNCTION_DECLARATION $protected_operation_declaration)
	| rep_spec
	| pragma
	;

protected_element_declaration : ( protected_operation_declaration | component_declaration )
	;

prot_member_decl_s : ( protected_element_declaration )*
	;

// 3.8
component_declaration : def_ids_colon component_definition init_opt c=SEMI^
	{ set($c, COMPONENT_DECLARATION, "COMPONENT_DECLARATION"); }
	;

// decl_common is shared between declarative_item and basic_declarative_item.
// Following declarations are not defined here: package, task, protected,
// procedure/function (i.e. subprogram).
// The reason is that there is ambiguity between declarative_item and
// basic_declarative_item on seeing the resp. keyword:
// - `package', `task', `protected' may be followed by `body' (ambiguity between spec and body)
// - `procedure', `function' may be followed by `is' (ambiguity between spec and body)
// decl_common only contains specifications.
decl_common :
	  t=TYPE^ IDENTIFIER
		( IS! type_def[$t]  // type_definition is resolved to its
				    // finer grained rules.
		|	( discrim_part
				( IS! derived_or_private_or_record[$t, true]
				| /* empty */
					{ set($t, INCOMPLETE_TYPE_DECLARATION,
						 "INCOMPLETE_TYPE_DECLARATION"); }
				)
			| /* empty */
			  { set($t, INCOMPLETE_TYPE_DECLARATION, "INCOMPLETE_TYPE_DECLARATION"); }
			)
		  /* The artificial derived_or_private_or_record rule
		     gives us some syntax-level control over where a
		     discrim_part may appear.
		     However, a semantic check is still necessary to make
		     sure the discrim_part is not given for a derived type
		     of an elementary type, or for the full view of a
		     private type that turns out to be such.  */
		)
		SEMI!
	| s=SUBTYPE^ IDENTIFIER IS! subtype_indication SEMI!  // subtype_declaration
		{ set($s, SUBTYPE_DECLARATION, "SUBTYPE_DECLARATION"); }
	| generic_decl[false]
	| use_clause
	| r=FOR^ ( (local_enum_name USE LPAREN) => local_enum_name USE!
			enumeration_aggregate
			{ set($r, ENUMERATION_REPESENTATION_CLAUSE,
				 "ENUMERATION_REPESENTATION_CLAUSE"); }
		| subtype_mark USE! rep_spec_part[$r]
		)
		SEMI!
	| (IDENTIFIER COLON EXCEPTION RENAMES) =>
		IDENTIFIER erd=COLON^ EXCEPTION! RENAMES! compound_name SEMI!
			{ set($erd, EXCEPTION_RENAMING_DECLARATION,
				   "EXCEPTION_RENAMING_DECLARATION"); }
	// TBC: The next 3 patterns all lead to OBJECT_RENAMING_DECLARATION,
	//      probably we need separate finer grained tokens?
	| (IDENTIFIER RENAMES) =>
		IDENTIFIER ord1=RENAMES^ name SEMI!
			{ set($ord1, OBJECT_RENAMING_DECLARATION, "OBJECT_RENAMING_DECLARATION"); }
	| (IDENTIFIER COLON null_exclusion_opt subtype_mark RENAMES) =>
		IDENTIFIER ord2=COLON^ null_exclusion_opt subtype_mark RENAMES! name SEMI!
			{ set($ord2, OBJECT_RENAMING_DECLARATION, "OBJECT_RENAMING_DECLARATION"); }
	| (IDENTIFIER COLON access_definition RENAMES) =>
		IDENTIFIER ord3=COLON^ access_definition RENAMES! name SEMI!
			{ set($ord3, OBJECT_RENAMING_DECLARATION, "OBJECT_RENAMING_DECLARATION"); }
	| defining_identifier_list od=COLON^  // object_declaration
		( EXCEPTION!
			{ set($od, EXCEPTION_DECLARATION, "EXCEPTION_DECLARATION"); }
		| (CONSTANT ASSIGN) => CONSTANT! ASSIGN! expression
			{ set($od, NUMBER_DECLARATION, "NUMBER_DECLARATION"); }
		| aliased_constant_opt
			( array_type_definition[$od] init_opt
				{ set($od, ARRAY_OBJECT_DECLARATION,
					  "ARRAY_OBJECT_DECLARATION"); }
				// Not an RM rule but simplifies distinction
				// from the non array object_declaration.
			| subtype_indication init_opt
				{ set($od, OBJECT_DECLARATION, "OBJECT_DECLARATION"); }
			)
		)
		SEMI!
	;

// Tail part of type_definition
// Full type_definition is assembled in decl_common.
// 3.2.1
type_def [Token t]
	: LPAREN! enum_id_s RPAREN!
		{ set(t, ENUMERATION_TYPE_DECLARATION, "ENUMERATION_TYPE_DECLARATION"); }
	| RANGE! range
		{ set(t, SIGNED_INTEGER_TYPE_DECLARATION, "SIGNED_INTEGER_TYPE_DECLARATION"); }
	| MOD! expression
		{ set(t, MODULAR_TYPE_DECLARATION, "MODULAR_TYPE_DECLARATION"); }
	| DIGITS! expression range_constraint_opt
		{ set(t, FLOATING_POINT_DECLARATION, "FLOATING_POINT_DECLARATION"); }
	| DELTA! expression
		( RANGE! range
			{ set(t, ORDINARY_FIXED_POINT_DECLARATION,
				"ORDINARY_FIXED_POINT_DECLARATION"); }
		| DIGITS! expression range_constraint_opt
			{ set(t, DECIMAL_FIXED_POINT_DECLARATION,
				"DECIMAL_FIXED_POINT_DECLARATION"); }
		)
	| array_type_definition[t]
	| access_type_definition[t]
	| ( ( LIMITED | TASK | PROTECTED | SYNCHRONIZED )? INTERFACE ) =>
	  interface_type_definition[t]
	| /* empty */ derived_or_private_or_record[t, false]
	;

enum_id_s : enumeration_literal_specification
		( COMMA! enumeration_literal_specification )*
	;

// 3.5.1
enumeration_literal_specification : IDENTIFIER | CHARACTER_LITERAL
	;

range_constraint_opt : ( range_constraint )?
	;

// 3.6
array_type_definition [Token t]
	: ARRAY! LPAREN! index_or_discrete_range_s RPAREN!
		OF! component_definition
		{ set(t, ARRAY_TYPE_DECLARATION, "ARRAY_TYPE_DECLARATION"); }
	;

index_or_discrete_range_s
	: index_or_discrete_range ( COMMA^ index_or_discrete_range )*
	;

index_or_discrete_range
	: simple_expression
		( DOT_DOT^ simple_expression  // constrained
		| RANGE^ ( BOX                // unconstrained
			| range              // constrained
			)
		)?
	;

// Not using ( subtype_indication | access_definition ) due to
// ambiguity triggered by null_exclusion_opt.
// 3.6
component_definition : aliased_opt null_exclusion_opt
	( subtype_ind_no_nullex | access_def_no_nullex )
	// -> ^(COMPONENT_DEFINITION $component_definition)
	;

aliased_opt : ( ALIASED )?
	;

// This is subtype_indication without the leading null_exclusion_opt.
// We use this instead of subtype_indication in places where null_exclusion_opt
// would introduce ambiguity.
subtype_ind_no_nullex : subtype_mark constraint_opt
	;

// 3.2.2
subtype_indication : null_exclusion_opt subtype_ind_no_nullex
	// -> ^(SUBTYPE_INDICATION $subtype_indication)
	;

constraint_opt : ( range_constraint
	| digits_constraint
	| delta_constraint
	| (index_constraint) => index_constraint
	| discriminant_constraint
	)?
	;

// 3.5.9
digits_constraint : d=DIGITS^ expression range_constraint_opt
	{ set($d, DIGITS_CONSTRAINT, "DIGITS_CONSTRAINT"); }
	;

// J.3
delta_constraint : d=DELTA^ expression range_constraint_opt
	{ set($d, DELTA_CONSTRAINT, "DELTA_CONSTRAINT"); }
	;

// 3.6.1
index_constraint : p=LPAREN^ discrete_range ( COMMA! discrete_range )* RPAREN!
	{ set($p, INDEX_CONSTRAINT, "INDEX_CONSTRAINT"); }
	;

// 3.6.1
discrete_range
	: (range) => range
	| subtype_indication
	;

// 3.7.1
discriminant_constraint : p=LPAREN^ discriminant_association 
		( COMMA! discriminant_association )* RPAREN!
	{ set($p, DISCRIMINANT_CONSTRAINT, "DISCRIMINANT_CONSTRAINT"); }
	;

// 3.7.1
discriminant_association : selector_names_opt expression
	// -> ^(DISCRIMINANT_ASSOCIATION $discriminant_association)
	;

selector_names_opt : ( (association_head) => association_head
	| /* empty */
	)
	;

association_head : selector_name ( PIPE! selector_name )* RIGHT_SHAFT!
	;

// 4.1.3
selector_name : IDENTIFIER  // TBD: sem pred
	;

// null_exclusion is dissolved into null_exclusion_opt because
// null_exclusion is only ever used as an optional item.
// 3.10
null_exclusion_opt : ( NOT NuLL )?
	;

constant_opt : ( CONSTANT )?
	;

/* access_definition creates ambiguities due to the initial `null_exclusion_opt'.
   We opt to manually resolve the ambiguities to avoid syn preds which are speed-expensive.
   This is access_definition without the initial `null_exclusion_opt'.
 */
access_def_no_nullex :
	/* null_exclusion_opt is OMITTED */
	ACCESS^
	( constant_opt subtype_mark
	| protected_opt
		( PROCEDURE parameter_profile
		| FUNCTION parameter_and_result_profile
		)
	)
	;

// access_definition is only used in contexts where the initial
// null_exclusion_opt does not create ambiguity.
access_definition :
	null_exclusion_opt access_def_no_nullex
	;

// access_to_object_definition and access_to_subprogram_definition are
// dissolved into access_type_definition due to little perceived added value
// and to avoid syn pred due to ambiguity. (Syn preds are generally avoided
// as much as possible due to significant speed penalty.)
// 3.10
access_type_definition [Token t]
	: null_exclusion_opt ACCESS!
		( protected_opt
			( PROCEDURE! formal_part_opt
				{ set(t, ACCESS_TO_PROCEDURE_DECLARATION,
					"ACCESS_TO_PROCEDURE_DECLARATION"); }
 			| FUNCTION! func_formal_part_opt RETURN! subtype_mark
				{ set(t, ACCESS_TO_FUNCTION_DECLARATION,
					"ACCESS_TO_FUNCTION_DECLARATION"); }
			)
		| general_access_modifier_opt subtype_indication
			{ set(t, ACCESS_TO_OBJECT_DECLARATION,
				"ACCESS_TO_OBJECT_DECLARATION"); }
		)
	;

limited_task_protected_synchronized_opt
	: ( LIMITED | TASK | PROTECTED | SYNCHRONIZED )?
	;

// 3.9.4
interface_list
	: subtype_mark ( AND subtype_mark )*
	;

and_interface_list_opt
	: ( AND interface_list )?
	;

interface_type_definition [Token t]
	: limited_task_protected_synchronized_opt INTERFACE! and_interface_list_opt
		{ set(t, INTERFACE_TYPE_DEFINITION, "INTERFACE_TYPE_DEFINITION"); }
	;

protected_opt : ( PROTECTED )?
	;

// Modification of general_access_modifier supporting optionality
// 3.10
general_access_modifier_opt : ( CONSTANT | ALL )?
	;

derived_or_private_or_record [Token t, boolean has_discrim]
	: abstract_tagged_limited_synchronized_opt
		( PRIVATE! { set(t, PRIVATE_TYPE_DECLARATION, "PRIVATE_TYPE_DECLARATION"); }
		| record_definition[has_discrim]
			{ set(t, RECORD_TYPE_DECLARATION, "RECORD_TYPE_DECLARATION"); }
		| NEW! subtype_indication
			( ( and_interface_list_opt WITH ) =>
			  and_interface_list_opt WITH!
				( PRIVATE!  { set(t, PRIVATE_EXTENSION_DECLARATION,
						    "PRIVATE_EXTENSION_DECLARATION"); }
				| record_definition[has_discrim]
					{ set(t, DERIVED_RECORD_EXTENSION,
						"DERIVED_RECORD_EXTENSION"); }
				)
			| /* empty */
				{ set(t, ORDINARY_DERIVED_TYPE_DECLARATION,
					"ORDINARY_DERIVED_TYPE_DECLARATION"); }
			)
		)
	;

// 3.8
record_definition [boolean has_discrim]
	: RECORD! component_list[has_discrim] END! RECORD!
	| NuLL! RECORD!  // Thus the component_list is optional in the tree.
			 // If it is absent then we have `null record'.
	;

// 3.8
component_list [boolean has_discrim]
	: NuLL! SEMI!  // Thus the component_list is optional in the tree.
	| component_items ( variant_part { has_discrim }? )?
	| variant_part { has_discrim }?
	;

component_items : ( pragma | component_declaration )+
	// -> ^(COMPONENT_ITEMS $component_items)
	;

// 3.8.1
variant_part : c=CASE^ discriminant_direct_name IS! variant_s END! CASE! SEMI!
	{ set($c, VARIANT_PART, "VARIANT_PART"); }
	;

discriminant_direct_name : IDENTIFIER  // TBD: symtab lookup.
	;

variant_s : ( variant )+
	// -> ^(VARIANTS $variant_s)
	;

// 3.8.1
variant : w=WHEN^ choice_s RIGHT_SHAFT! component_list[true]
	{ set($w, VARIANT, "VARIANT"); }
	;

choice_s : choice ( PIPE^ choice )*
	;

choice : OTHERS
	| (discrete_with_range) => discrete_with_range
	| expression   //  ( DOT_DOT^ simple_expression )?
	;              // No, that's already in discrete_with_range

discrete_with_range : (mark_with_constraint) => mark_with_constraint
	| range
	;

mark_with_constraint : subtype_mark range_constraint
	// -> ^(MARK_WITH_CONSTRAINT $mark_with_constraint)
	;

// Slightly loose , "tagged" shall not appear on derived_type_definition.
abstract_tagged_limited_synchronized_opt
	: ( ABSTRACT )? ( TAGGED )?
	  ( LIMITED | SYNCHRONIZED )?
	;

local_enum_name : IDENTIFIER  // to be refined: do a symbol table lookup
	;

enumeration_aggregate : array_aggregate
	;

aliased_constant_opt : ( ALIASED )? ( CONSTANT )?
	;

generic_decl [boolean lib_level] :
	  g=GENERIC^ generic_formal_part_opt
	( PACKAGE! defining_identifier[lib_level, false]
		( renames
			{ set($g, GENERIC_PACKAGE_RENAMING, "GENERIC_PACKAGE_RENAMING"); }
		| IS! pkg_spec_part
			{ set($g, GENERIC_PACKAGE_DECLARATION, "GENERIC_PACKAGE_DECLARATION"); }
		)
	| PROCEDURE! defining_identifier[lib_level, false] formal_part_opt
		( renames
			{ set($g, GENERIC_PROCEDURE_RENAMING, "GENERIC_PROCEDURE_RENAMING"); }
		  // ^^^ Semantic check must ensure that the (generic_formal)*
		  //     after GENERIC is not given here.
		| /* empty */
			{ set($g, GENERIC_PROCEDURE_DECLARATION, "GENERIC_PROCEDURE_DECLARATION"); }
		)
	| FUNCTION! defining_designator[lib_level, false] parameter_and_result_profile
		( renames
			{ set($g, GENERIC_FUNCTION_RENAMING, "GENERIC_FUNCTION_RENAMING"); }
		  // ^^^ Semantic check must ensure that the (generic_formal)*
		  //     after GENERIC is not given here.
		| /* empty */
			{ set($g, GENERIC_FUNCTION_DECLARATION, "GENERIC_FUNCTION_DECLARATION"); }
		)
	)
	SEMI!
	;

// This is generic_formal_part without the leading keyword `generic'
// (which is handled in generic_decl).
// 12.1
generic_formal_part_opt : ( use_clause | pragma | generic_formal_parameter )*
	;

generic_formal_parameter :
	( t=TYPE^ defining_identifier[false, false]
		( IS!
			( LPAREN! BOX! RPAREN!
				{ set($t, FORMAL_DISCRETE_TYPE_DECLARATION,
					 "FORMAL_DISCRETE_TYPE_DECLARATION"); }
			| RANGE! BOX!
				{ set($t, FORMAL_SIGNED_INTEGER_TYPE_DECLARATION,
					 "FORMAL_SIGNED_INTEGER_TYPE_DECLARATION"); }
			| MOD! BOX!
				{ set($t, FORMAL_MODULAR_TYPE_DECLARATION,
					 "FORMAL_MODULAR_TYPE_DECLARATION"); }
			| DELTA! BOX!
				( DIGITS! BOX!
					{ set($t, FORMAL_DECIMAL_FIXED_POINT_DECLARATION,
						 "FORMAL_DECIMAL_FIXED_POINT_DECLARATION"); }
				| /* empty */
					{ set($t, FORMAL_ORDINARY_FIXED_POINT_DECLARATION,
						 "FORMAL_ORDINARY_FIXED_POINT_DECLARATION"); }
				)
			| DIGITS! BOX!
				{ set($t, FORMAL_FLOATING_POINT_DECLARATION,
					 "FORMAL_FLOATING_POINT_DECLARATION"); }
			| array_type_definition[$t]
			| access_type_definition[$t]
			| /* empty */ discriminable_type_definition[$t]
			)
		| discrim_part IS! discriminable_type_definition[$t]
		)
	| w=WITH^ ( PROCEDURE! defining_identifier[false, false] formal_part_opt subprogram_default_opt
			{ set($w, FORMAL_PROCEDURE_DECLARATION, "FORMAL_PROCEDURE_DECLARATION"); }
		| FUNCTION! defining_designator[false, false] parameter_and_result_profile subprogram_default_opt
			{ set($w, FORMAL_FUNCTION_DECLARATION, "FORMAL_FUNCTION_DECLARATION"); }
		| PACKAGE! defining_identifier[false, false] IS! NEW! compound_name formal_package_actual_part_opt
			{ set($w, FORMAL_PACKAGE_DECLARATION, "FORMAL_PACKAGE_DECLARATION"); }
		)
	| defining_identifier_list fod=COLON^ mode null_exclusion_opt
		( subtype_mark
		| access_def_no_nullex
		)
		init_opt
			{ set($fod, FORMAL_OBJECT_DECLARATION, "FORMAL_OBJECT_DECLARATION"); }
	)
	SEMI!
	;

discriminable_type_definition [Token t]
	: abstract_tagged_limited_synchronized_opt
	  ( PRIVATE! { set(t, FORMAL_PRIVATE_TYPE_DECLARATION, "FORMAL_PRIVATE_TYPE_DECLARATION"); }
	  | NEW! subtype_indication
		( ( and_interface_list_opt WITH ) =>
		  and_interface_list_opt WITH! PRIVATE!
			{ set(t, FORMAL_PRIVATE_EXTENSION_DECLARATION,
				 "FORMAL_PRIVATE_EXTENSION_DECLARATION"); }
		| /* empty */
			{ set(t, FORMAL_ORDINARY_DERIVED_TYPE_DECLARATION,
				 "FORMAL_ORDINARY_DERIVED_TYPE_DECLARATION"); }
		)
	  | /* empty, i.e. abstract_tagged_limited_synchronized_opt is probably TAGGED */
		{ set(t, FORMAL_INCOMPLETE_TYPE_DECLARATION,
			 "FORMAL_INCOMPLETE_TYPE_DECLARATION"); }
	  )
	;

subprogram_default_opt : ( IS! ( BOX | name ) )?
	;

// 12.7
formal_package_actual_part :
	LPAREN!
	      /* Annex P says:
		 ( ( OTHERS RIGHT_SHAFT^ )? BOX
		 | ( generic_actual_part )?
		 | formal_package_association_s
		 )
	       but the 1st and 2nd alternative cause ANTLR nondeterminism because they are
	       contained in formal_package_association_s, thus they are redundant.  */
	       formal_package_association_s
	RPAREN!
	;

formal_package_actual_part_opt :  ( formal_package_actual_part )?
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
formal_pkg_assocs :
	formal_package_association ( COMMA! formal_package_association )*
	( OTHERS! RIGHT_SHAFT! BOX )?
	;

// Auxiliary rule for 12.7 formal_package_actual_part
formal_package_association_s :
	formal_pkg_assocs
	//   -> ^(FORMAL_PACKAGE_ASSOCIATION_S $formal_package_association_s)
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
formal_pkg_assoc :
	( generic_formal_parameter_selector_name RIGHT_SHAFT^ )? ( expression | BOX )
	;

// 12.7
formal_package_association :
	formal_pkg_assoc
	//   -> ^(FORMAL_PACKAGE_ASSOCIATION $formal_package_association)
	;

// Auxiliary to (lib_)subprog_decl_or_rename_or_inst_or_body
proc_decl_or_renaming_or_inst_or_body [Token p] :
	  generic_subp_inst
		{ set(p, GENERIC_PROCEDURE_INSTANTIATION, "GENERIC_PROCEDURE_INSTANTIATION");
		  pop_def_id(); }
	| formal_part_opt
		( renames { set(p, PROCEDURE_RENAMING_DECLARATION,
				  "PROCEDURE_RENAMING_DECLARATION");
		            pop_def_id(); }
		| IS!	( separate_or_abstract[p] { pop_def_id(); }
			| body_part { set(p, PROCEDURE_BODY, "PROCEDURE_BODY"); }
			)
		| /* empty */
		  { pop_def_id();
		    set(p, PROCEDURE_DECLARATION, "PROCEDURE_DECLARATION"); }
		)
		SEMI!
	;

// Auxiliary to (lib_)subprog_decl_or_rename_or_inst_or_body
func_decl_or_renaming_or_inst_or_body [Token f] :
	  generic_subp_inst
		{ set(f, GENERIC_FUNCTION_INSTANTIATION, "GENERIC_FUNCTION_INSTANTIATION");
		  pop_def_id(); }
	| parameter_and_result_profile
		( renames { set(f, FUNCTION_RENAMING_DECLARATION, "FUNCTION_RENAMING_DECLARATION");
		            pop_def_id(); }
		| IS!	( separate_or_abstract[f] { pop_def_id(); }
			| body_part { set(f, FUNCTION_BODY, "FUNCTION_BODY"); }
			)
		| /* empty */
		  { pop_def_id();
		    set(f, FUNCTION_DECLARATION, "FUNCTION_DECLARATION"); }
		)
		SEMI!
	;

lib_subprog_decl_or_rename_or_inst_or_body :
	  p=PROCEDURE^ defining_identifier[true, true] proc_decl_or_renaming_or_inst_or_body[$p]
	| f=FUNCTION^  defining_designator[true, true] func_decl_or_renaming_or_inst_or_body[$f]
	// Passing true to defining_{identifier,designator} argument push_id means
	// all non body alternatives must pop_def_id().
	;

subprog_decl_or_rename_or_inst_or_body :
	overriding_opt
	( p=PROCEDURE^ defining_identifier[false, true] proc_decl_or_renaming_or_inst_or_body[$p]
	| f=FUNCTION^  defining_designator[false, true] func_decl_or_renaming_or_inst_or_body[$f]
	)
	// Passing true to defining_{identifier,designator} argument push_id means
	// all non body alternatives must pop_def_id().
	;

body_part : declarative_part block_body end_id_opt!
	;

// 3.11
declarative_part : ( pragma | declarative_item )*
	;

// A declarative_item may appear in the declarative part of any body.
// 3.11
declarative_item :
	( pkg=PACKAGE^ ( body_is
			( separate { set($pkg, PACKAGE_BODY_STUB, "PACKAGE_BODY_STUB"); }
			| pkg_body_part end_id_opt!
				{ set($pkg, PACKAGE_BODY, "PACKAGE_BODY"); }
			)
			SEMI!
		| defining_identifier[false, true] spec_decl_part[$pkg]
		)
	| tsk=TASK^ ( body_is
			( separate { set($tsk, TASK_BODY_STUB, "TASK_BODY_STUB"); }
			| body_part { set($tsk, TASK_BODY, "TASK_BODY"); }
			)
			SEMI!
		| task_type_or_single_decl[$tsk]
		)
	| pro=PROTECTED^
		( body_is
			( separate { set($pro, PROTECTED_BODY_STUB, "PROTECTED_BODY_STUB"); }
			| prot_op_bodies_opt end_id_opt!
				{ set($pro, PROTECTED_BODY, "PROTECTED_BODY"); }
			)
		| prot_type_or_single_decl[$pro]
		)
		SEMI!
	| subprog_decl_or_rename_or_inst_or_body
	| decl_common
	)
	/* DECLARATIVE_ITEM is just a pass-thru node so we omit it.
	 -> ^(DECLARATIVE_ITEM $declarative_item)
	 */
	;

body_is : BODY! defining_identifier[false, true] IS!
	;

separate : SEPARATE! { pop_def_id(); }
	;

pkg_body_part : declarative_part block_body_opt
	;

block_body_opt : ( BEGIN! handled_sequence_of_statements )?
	;

prot_op_bodies_opt :
	( entry_body
	| subprog_decl_or_body
	| pragma
	)*
	;

subprog_decl_or_body
	: p=PROCEDURE^ defining_identifier[false, true] formal_part_opt
		( IS! body_part
		  { set($p, PROCEDURE_BODY, "PROCEDURE_BODY"); }
		| /* empty */
		  { pop_def_id();
		    set($p, PROCEDURE_DECLARATION, "PROCEDURE_DECLARATION"); }
		)
		SEMI!
	| f=FUNCTION^ defining_designator[false, true] parameter_and_result_profile
		( IS! body_part
		  { set($f, FUNCTION_BODY, "FUNCTION_BODY"); }
		| /* empty */ 
		  { pop_def_id();
		    set($f, FUNCTION_DECLARATION, "FUNCTION_DECLARATION"); }
		)
		SEMI!
	;

block_body : b=BEGIN^ handled_sequence_of_statements
	{ set($b, BLOCK_BODY, "BLOCK_BODY"); }
	;

// 11.2
handled_sequence_of_statements : sequence_of_statements except_handler_part_opt
	// -> ^(HANDLED_SEQUENCE_OF_STATEMENTS $handled_sequence_of_statements)
	;

// 5.1
sequence_of_statements : ( pragma | statement )+
	// -> ^(SEQUENCE_OF_STATEMENTS $sequence_of_statements)
	;

statement : def_labels_opt
	( null_statement
	| exit_statement
	| ( RETURN IDENTIFIER COLON ) => extended_return_statement
	| simple_return_statement
	| goto_statement
	| delay_statement
	| abort_statement
	| raise_statement
	| requeue_statement
	| accept_statement
	| select_statement
	| if_statement
	| case_statement
	| loop_without_stmt_id
	| block_without_stmt_id
	| block_or_loop_with_stmt_id 
	| call_or_assignment
	// | code_stmt  // TBD: resolve ambiguity
	)
	// -> ^(STATEMENT $statement)
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
def_label :
	LT_LT! IDENTIFIER GT_GT!
	;

def_labels_opt : ( def_label )*
	;

// 5.1
null_statement : s=NuLL SEMI!
	{ set($s, NULL_STATEMENT, "NULL_STATEMENT"); }
	;

// 5.3
if_statement : s=IF^ cond_clause elsifs_opt
	  else_opt
	  END! IF! SEMI!
	{ set($s, IF_STATEMENT, "IF_STATEMENT"); }
	;

cond_clause : condition c=THEN^ sequence_of_statements
	{ set($c, COND_CLAUSE, "COND_CLAUSE"); }
	;

condition : expression
	// -> ^(CONDITION $condition)
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
elsif_clause :  ELSIF! cond_clause
	;

elsifs_opt : ( elsif_clause )*
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
else_clause :  ELSE! sequence_of_statements
	;

else_opt : ( else_clause )?
	;

// 5.4
case_statement : s=CASE^ expression IS! alternative_s END! CASE! SEMI!
	{ set($s, CASE_STATEMENT, "CASE_STATEMENT"); }
	;

alternative_s : ( case_statement_alternative )+
	;

// 5.4
case_statement_alternative : s=WHEN^ choice_s RIGHT_SHAFT! sequence_of_statements
	{ set($s, CASE_STATEMENT_ALTERNATIVE, "CASE_STATEMENT_ALTERNATIVE"); }
	;

block_or_loop_with_stmt_id :
	statement_identifier
	( loop_stmt id_opt! s=SEMI^
	  { set($s, LOOP_STATEMENT, "LOOP_STATEMENT"); }
	| block end_id_opt! b=SEMI^
	  { set($b, BLOCK_STATEMENT, "BLOCK_STATEMENT"); }
	)
	;

loop_without_stmt_id :
	loop_stmt s=SEMI^
	  { set($s, LOOP_STATEMENT, "LOOP_STATEMENT"); }
	;

// loop_statement but without the leading [loop_statement_identifier:]
// Common backend for block_or_loop_with_stmt_id and loop_without_stmt_id
// 5.5
loop_stmt : iteration_scheme_opt
		LOOP! sequence_of_statements END! LOOP!  // basic_loop
        ;

// 5.5
iteration_scheme :
	  WHILE^ condition
	| FOR^ IDENTIFIER IN! reverse_opt discrete_subtype_definition
	;

iteration_scheme_opt :  ( iteration_scheme )?
	;

reverse_opt : ( REVERSE )?
	;

id_opt :
	endid=definable_operator_symbol { end_id_matches_def_id ($endid.text) }?
	| n=compound_name { end_id_matches_def_id ($n.text) }?
	  /* Ordinarily we would need to be stricter here, i.e.
	     match compound_name only for the library-level case
	     (and IDENTIFIER otherwise), but end_id_matches_def_id
	     does the right thing for us.  */
	| { pop_def_id(); }
	;

end_id_opt : END! id_opt
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
stmt_id : n=IDENTIFIER COLON!
	;

/* Manual disambiguation of `loop_stmt' from `block'
   in the presence of the statement_identifier in `statement'
   results in this rule. The case of loop_stmt/block given
   without the statement_identifier is coded in the rules
   loop_without_stmt_id / block_without_stmt_id.  */
// 5.1
statement_identifier :
	n=IDENTIFIER s=COLON^
	{ push_def_id($n.text);
	  set($s, STATEMENT_IDENTIFIER, "STATEMENT_IDENTIFIER"); }
	;

block_without_stmt_id :
	block END! s=SEMI^
	  { set($s, BLOCK_STATEMENT, "BLOCK_STATEMENT"); }
	;

// block_statement but without the leading [block_statement_identifier:]
// Common backend for block_or_loop_with_stmt_id and block_without_stmt_id
// 5.6
block : declare_opt block_body
	;

declare_opt : ( DECLARE declarative_part )?
	;

// 5.7
exit_statement : s=EXIT^ ( label_name )? ( WHEN condition )? SEMI!
	{ set($s, EXIT_STATEMENT, "EXIT_STATEMENT"); }
	;

// RM says (label_)name where (label_) is label_ rendered in italics.
// However, since `name' is much too loose, we use IDENTIFIER.
label_name : IDENTIFIER
	;

// 6.5
simple_return_statement : s=RETURN^ ( expression )? SEMI!
	{ set($s, SIMPLE_RETURN_STATEMENT, "SIMPLE_RETURN_STATEMENT"); }
	;

// Not using defining_identifier here because
// 1) this is never lib_level and 2) we don't want to push_def_id()
// 6.5
extended_return_statement :
	s=RETURN^ IDENTIFIER COLON! aliased_opt return_subtype_indication init_opt
	( DO! handled_sequence_of_statements END! RETURN! )?
	SEMI!
	{ set($s, EXTENDED_RETURN_STATEMENT, "EXTENDED_RETURN_STATEMENT"); }
	;

// Not using ( subtype_indication | access_definition ) due to
// ambiguity triggered by null_exclusion_opt.
return_subtype_indication : null_exclusion_opt
	( subtype_ind_no_nullex | access_def_no_nullex )
	// -> ^(RETURN_SUBTYPE_INDICATION $return_subtype_indication)
	;

// 5.8
goto_statement : s=GOTO^ label_name SEMI!
	{ set($s, GOTO_STATEMENT, "GOTO_STATEMENT"); }
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
call_or_assign_aux :
	name
	( ASSIGN expression
	//   -> ^(ASSIGNMENT_STATEMENT $call_or_assign_aux)
	| /* empty */
	//   -> ^(CALL_STATEMENT $call_or_assign_aux)
		/* Preliminary. Use semantic analysis to produce
		   {PROCEDURE|ENTRY}_CALL_STATEMENT.  */
	)
	;

call_or_assignment :  // procedure_call is in here.
	call_or_assign_aux
	SEMI!
	;

entry_body : e=ENTRY^ defining_identifier[false, false] entry_body_formal_part entry_barrier IS!
		body_part SEMI!
	{ set($e, ENTRY_BODY, "ENTRY_BODY"); }
	;

entry_body_formal_part : entry_index_spec_opt formal_part_opt
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
// 9.5.2
entry_index_specification :
	LPAREN! FOR! defining_identifier[false, false] IN! discrete_subtype_definition RPAREN!
	;

entry_index_spec_opt :
	( (LPAREN FOR) => entry_index_specification
	| /* empty */
	)
	;

entry_barrier : WHEN! condition
	;

// 9.5.3
entry_call_statement : name e=SEMI^  // Semantic analysis required, for example
				     // to ensure `name' is an entry.
	{ set($e, ENTRY_CALL_STATEMENT, "ENTRY_CALL_STATEMENT"); }
	;

// 9.5.2
accept_statement :
	a=ACCEPT^ defining_identifier[false, true] entry_index_opt formal_part_opt
		( DO! handled_sequence_of_statements end_id_opt! SEMI!
		| SEMI! { pop_def_id(); }
		)
	{ set($a, ACCEPT_STATEMENT, "ACCEPT_STATEMENT"); }
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
// Auxiliary rule for 9.5.2 accept_statement
parenthesized_entry_index :
	  e=LPAREN^ expression RPAREN!
	    { set($e, ENTRY_INDEX_OPT, "ENTRY_INDEX_OPT"); }
	;

entry_index_opt :
	( (LPAREN expression RPAREN) => parenthesized_entry_index
	// We need the syn pred for the usage context in accept_statement.
	// The formal_part_opt that follows the entry_index_opt there
	// creates ambiguity (due to the opening LPAREN.)
	| /* empty */
	)
	;

// delay_statement directly codes delay_until_statement and
// delay_relative_statement thus those rules are not present.
// delay_until_statement is signaled by non empty until_opt.
// 9.6
delay_statement : d=DELAY^ until_opt expression SEMI!
	{ set($d, DELAY_STATEMENT, "DELAY_STATEMENT"); }
	;

until_opt : ( UNTIL )?
	;

// SELECT_STATEMENT is not modeled in the AST since it is trivially
// reconstructed:
//   select_statement ::= selective_accept | timed_entry_call
//             | conditional_entry_call | asynchronous_select
// 9.7
select_statement : s=SELECT^
	( (triggering_alternative THEN ABORT) =>
		triggering_alternative THEN! ABORT! abortable_part
		{ set($s, ASYNCHRONOUS_SELECT, "ASYNCHRONOUS_SELECT"); }
	| selective_accept
		{ set($s, SELECTIVE_ACCEPT, "SELECTIVE_ACCEPT"); }
	| entry_call_alternative
		( OR! delay_alternative
			{ set($s, TIMED_ENTRY_CALL, "TIMED_ENTRY_CALL"); }
		| ELSE! sequence_of_statements
			{ set($s, CONDITIONAL_ENTRY_CALL, "CONDITIONAL_ENTRY_CALL"); }
		)
	)
	END! SELECT! SEMI!
	;

triggering_alternative : ( delay_statement | entry_call_statement ) stmts_opt
	// -> ^(TRIGGERING_ALTERNATIVE $triggering_alternative)
	;

abortable_part : stmts_opt
	// -> ^(ABORTABLE_PART $abortable_part)
	;

entry_call_alternative : entry_call_statement stmts_opt
	// -> ^(ENTRY_CALL_ALTERNATIVE $entry_call_alternative)
	;

selective_accept : guard_opt select_alternative or_select_opt else_opt
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
// 9.7.1
guard :
	WHEN! condition RIGHT_SHAFT! ( pragma )*
	;

guard_opt : ( guard )?
	;

select_alternative  // Not modeled in AST since it's just a pass-through.
	: accept_alternative
	| delay_alternative
	| t=TERMINATE^ SEMI!
		{ set($t, TERMINATE_ALTERNATIVE, "TERMINATE_ALTERNATIVE"); }
	;

accept_alternative : accept_statement stmts_opt
	// -> ^(ACCEPT_ALTERNATIVE $accept_alternative)
	;

delay_alternative : delay_statement stmts_opt
	// -> ^(DELAY_ALTERNATIVE $delay_alternative)
	;

stmts_opt : ( pragma | statement )*
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
// Auxiliary rule for 9.7.1 selective_accept
or_select :
	OR! guard_opt select_alternative
	;

or_select_opt : ( or_select )*
	;

// 9.8
abort_statement : a=ABORT^ name ( COMMA! name )* SEMI!
	{ set($a, ABORT_STATEMENT, "ABORT_STATEMENT"); }
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
except_handler_part :
	EXCEPTION! ( exception_handler )+
	;

// Auxiliary rule for 11.2 handled_sequence_of_statements
except_handler_part_opt : ( except_handler_part )?
	;

// 11.2
exception_handler :
	w=WHEN^ identifier_colon_opt except_choice_s RIGHT_SHAFT! sequence_of_statements
	{ set($w, EXCEPTION_HANDLER, "EXCEPTION_HANDLER"); }
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
identifier_colon :
	IDENTIFIER COLON!
	;

identifier_colon_opt : ( identifier_colon )?
	;

except_choice_s : exception_choice ( PIPE^ exception_choice )*
	;

exception_choice : compound_name
	| OTHERS
	;

// 11.3
raise_statement : r=RAISE^ ( compound_name )? SEMI!
	{ set($r, RAISE_STATEMENT, "RAISE_STATEMENT"); }
	;

// 9.5.4
requeue_statement : r=REQUEUE^ name ( WITH! ABORT )? SEMI!
	{ set($r, REQUEUE_STATEMENT, "REQUEUE_STATEMENT"); }
	;

operator_call : cs=CHAR_STRING^ operator_call_tail[$cs]
	;

operator_call_tail [Token opstr]
	: LPAREN! { is_operator_symbol(opstr.getText()) }?
		  value_s RPAREN! { opstr.setType(OPERATOR_SYMBOL); }
	;

// Auxiliary rule for ANTLR3
// (cannot mix rewrite syntax with AST operator in single rule)
values_aux :
	value ( COMMA! value )*
	;

value_s : values_aux
	// -> ^(VALUES $value_s)
	;

// Non RM auxiliary rule for indexed_component
expression_s : expression ( COMMA! expression )*
	;

/*
literal : NUMERIC_LIT
	| CHARACTER_LITERAL
	| CHAR_STRING
	| NuLL
	;
 */

// ANTLR3 Warning: Decision can match input such as
// "AND {ABS, CHARACTER_LITERAL..CHAR_STRING, IDENTIFIER, LPAREN, MINUS, NEW, NOT, NUMERIC_LIT..NuLL, PLUS, THEN}"
// using multiple alternatives: 1, 4
expression : relation
		( options { greedy=true; }:
		  a=AND^ ( THEN! { set($a, AND_THEN, "AND_THEN"); } )? relation
		| o=OR^ ( ELSE! { set($o, OR_ELSE, "OR_ELSE"); } )? relation
		| XOR^ relation
		)*
	;

relation : simple_expression
		( IN^ range_or_mark
		| n=NOT^ IN! range_or_mark { set($n, NOT_IN, "NOT_IN"); }
		| EQ^ simple_expression
		| NE^ simple_expression
		| LESSTHAN^ simple_expression
		| LE^ simple_expression
		| GREATERTHAN^ simple_expression
		| GE^ simple_expression
		)?
	;

range_or_mark : (range) => range
	| subtype_mark
	;

simple_expression : signed_term
		( PLUS^ signed_term
		| MINUS^ signed_term
		| CONCAT^ signed_term
		)*
	;

signed_term :
	  p=PLUS^ term
		{ set($p, UNARY_PLUS, "UNARY_PLUS"); }
	| m=MINUS^ term
		{ set($m, UNARY_MINUS, "UNARY_MINUS"); }
	| term
	;

term    : factor ( MUL^ factor
		| DIV^ factor
		| MOD^ factor
		| REM^ factor
		)*
	;

factor : ( NOT^ primary
	| ABS^ primary
	| primary ( EXPON^ primary )?
	)
	;

primary : ( ( name ) => name
	| parenthesized_primary
	| allocator
	| NuLL
	| NUMERIC_LIT
	| CHARACTER_LITERAL
	// | cs=CHAR_STRING^ ( operator_call_tail[$cs] )?
	| ( CHAR_STRING ~LPAREN ) => CHAR_STRING
	)
	;

allocator : n=NEW^ name
	{ set($n, ALLOCATOR, "ALLOCATOR"); }
	;

subunit : sep=SEPARATE^ LPAREN! compound_name RPAREN!
		( subprogram_body
		| package_body
		| task_body
		| protected_body
		)
	{ set($sep, SUBUNIT, "SUBUNIT"); }
	;

// 6.3
// This rule is only used in `subunit', other usage contexts use different rules
// (e.g. proc_decl_or_renaming_or_inst_or_body, subprog_decl_or_body).
subprogram_body
	: overriding_opt
	( p=PROCEDURE^ defining_identifier[false, true] formal_part_opt IS! body_part SEMI!
		{ set($p, PROCEDURE_BODY, "PROCEDURE_BODY"); }
	| f=FUNCTION^ defining_designator[false, true] parameter_and_result_profile IS! body_part SEMI!
		{ set($f, FUNCTION_BODY, "FUNCTION_BODY"); }
	)
	;

package_body : p=PACKAGE^ body_is pkg_body_part end_id_opt! SEMI!
	{ set($p, PACKAGE_BODY, "PACKAGE_BODY"); }
	;

task_body : t=TASK^ body_is body_part SEMI!
	{ set($t, TASK_BODY, "TASK_BODY"); }
	;
 
protected_body : p=PROTECTED^ body_is prot_op_bodies_opt end_id_opt! SEMI!
	{ set($p, PROTECTED_BODY, "PROTECTED_BODY"); }
	;

// TBD
// code_stmt : qualified SEMI!
//  	;


