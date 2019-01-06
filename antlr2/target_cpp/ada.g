/*
 * Ada2005 Recognizer for ANTLR V2
 *
 * Oliver M. Kellogg  <okellogg@users.sourceforge.net>
 *
 * Originally based on lexer9x.l/grammar9x.y,
 *
 ******* A YACC grammar for Ada 9X *********************************
 * Copyright (C) Intermetrics, Inc. 1994 Cambridge, MA  USA        *
 * Copying permitted if accompanied by this statement.             *
 * Derivative works are permitted if accompanied by this statement.*
 * This grammar is thought to be correct as of May 1, 1994         *
 * but as usual there is *no warranty* to that effect.             *
 *******************************************************************
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


header "pre_include_hpp" {
#include <iostream>
#include <cstring>
#include <string>
#include <vector>
#include <antlr/SemanticException.hpp>  // antlr wants this
#include "AdaAST.hpp"
}

options {
  language="Cpp";
}


//-----------------------------------------------------------------------------
// Define a Parser, calling it AdaParser
//-----------------------------------------------------------------------------
class AdaParser extends Parser;
options {
  k = 4;                           // token lookahead
  exportVocab=Ada;                 // Call its vocabulary "Ada"
  defaultErrorHandler = true;     // Generate parser error handlers
  buildAST = true;
  ASTLabelType = "RefAdaAST";
}

{
private:
  std::vector<std::string> m_def_id;
public:
  // Ada support stuff
  void push_def_id (const std::string& defid) {
    m_def_id.push_back(defid);
  }
  std::string pop_def_id () {
    std::string defid;
    if (m_def_id.size()) {
      defid = m_def_id.back();
      m_def_id.pop_back();
    } else {
      std::cerr << "pop_def_id called with empty stack" << std::endl;
    }
    return defid;
  }
  bool end_id_matches_def_id (const std::string& endStr) {
    if (m_def_id.size() < 1)
      return false;
    std::string defStr = pop_def_id();
    int cmpResult = strcasecmp(defStr.c_str(), endStr.c_str());
    if (cmpResult)
      std::cerr << "expecting \"end " << defStr << "\", found \"end " << endStr << "\"" << std::endl;
    return cmpResult == 0;
  }
  bool definable_operator (const std::string& string) { // operator_symbol sans "/="
    if (string.length() < 3 || string[0] != '"')
      return false;
    std::string str = string.substr(1, string.length() - 2);
    static const char *ops[] = {
                          "and", "or", "xor",           // logical
                          "=", "<", "<=", ">", ">=",    // relational (omitting "/=")
                          "+", "\055", "&",             // binary/unary adding - somehow ANTLR does not like "-", had to write as "\055"
                          "*", "/", "mod", "rem",       // multiplying
                          "**", "abs", "not"            // highest precedence
                        };
    for (int i = 0; i < sizeof(ops) / sizeof(char*); i++)
    {
      if (strcasecmp(str.c_str(), ops[i]) == 0)
        return true;
    }
    return false;
  }
  bool is_operator_symbol (const std::string& string) {
    return definable_operator(string) || string == "\"/=\"";
  }
}

// Compilation Unit:  This is the start rule for this parser.
// The rules in this grammar are listed in the order in which
// compilation_unit introduces them, depth first, with the
// exception of the expression related rules which are listed
// towards the end.
// The rule library_unit_declaration is not materialized because
// it acts as a passthrough.
// 10.1.1
compilation_unit
	: context_clause ( library_item | subunit ) ( pragma )*
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
context_item
	: pragma  // RM Annex P neglects pragmas; we include them.
	| ( ( LIMITED )? ( PRIVATE )? WITH ) => with_clause
	  /* The syntactic predicate has not helped here, see comment at limited_private_opt */
	| use_clause
	;

context_clause
	: ( context_item )*
	{ #context_clause =
		#(#[CONTEXT_CLAUSE, "CONTEXT_CLAUSE"], #context_clause); }
		// According to our naming convention the node should really be named
		// CONTEXT_ITEMS_OPT but we stick with the RM wording.
	;

limited_private_opt : ( LIMITED )? ( PRIVATE )?
	/* The ( PRIVATE )? above confuses ANTLR 2.7.7, giving a nondeterminism warning.
	   Syn pred has not helped. IMHO it is a bug in ANTLR.  */
	{ #limited_private_opt = #(#[MODIFIERS, "MODIFIERS"], #limited_private_opt); }
	;

with_clause : limited_private_opt w:WITH^ compound_name_list SEMI!
	{ Set(#w, WITH_CLAUSE); }
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
use_clause : u:USE^
		( TYPE! subtype_mark ( COMMA! subtype_mark )*
			{ Set(#u, USE_TYPE_CLAUSE); }
		| compound_name_list { Set(#u, USE_PACKAGE_CLAUSE); }
		)
	SEMI!
	;

// The RM defines `subtype_mark' as `name'.
// However, this looks overly permissive.
// AARM 3.2.2 4.a says:
// " Note that name includes attribute_reference; thus, S'Base can be used
//   as a subtype_mark. "
// Thus narrowing down the rule, albeit not to the particular Base attribute:
subtype_mark : compound_name ( TIC^ IDENTIFIER )?
	{ #subtype_mark = #(#[SUBTYPE_MARK, "SUBTYPE_MARK"], #subtype_mark); }
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
	( lib_pkg_spec_or_body
	| lib_subprog_decl_or_rename_or_inst_or_body
	| generic_decl[true]
	)
	{ #library_item = #(#[LIBRARY_ITEM, "LIBRARY_ITEM"], #library_item); }
	;

private_opt : ( PRIVATE )?
	{ #private_opt = #(#[MODIFIERS, "MODIFIERS"], #private_opt); }
	;

lib_pkg_spec_or_body
	: pkg:PACKAGE^
		( BODY! defining_identifier[true, true] IS! pkg_body_part end_id_opt! SEMI!
			{ Set(#pkg, PACKAGE_BODY); }
		| defining_identifier[true, true] spec_decl_part[#pkg]
		)
	;

// 8.3.1 overriding_indicator is dissolved into overriding_opt because
//       overriding_indicator is only ever used as an optional item.
overriding_opt :
	( OVERRIDING )?
	  { #overriding_opt = #(#[OVERRIDING_OPT, "OVERRIDING_OPT"],
			       #overriding_opt); }
	;

// 6.1
// This is NOT at the library level.
subprogram_declaration :
	overriding_opt
	( p:PROCEDURE^ defining_identifier[false]
		( generic_subp_inst
			{ Set(#p, GENERIC_PROCEDURE_INSTANTIATION); }
		| formal_part_opt
			( renames { Set(#p, PROCEDURE_RENAMING_DECLARATION); }
			| is_separate_or_abstract_or_decl[#p]
			)
			SEMI!
		)
	| f:FUNCTION^ defining_designator[false]
		( generic_subp_inst
			{ Set(#f, GENERIC_FUNCTION_INSTANTIATION); }
		| parameter_and_result_profile
			( renames { Set(#f, FUNCTION_RENAMING_DECLARATION); }
			| is_separate_or_abstract_or_decl[#f]
			)
			SEMI!
		)
	)
	;

// 3.1
defining_identifier [bool lib_level, bool push_id=false]
	: { lib_level }? cn:compound_name { if (push_id) push_def_id(#cn->getText()); }
	| { !lib_level }? n:IDENTIFIER { if (push_id) push_def_id(#n->getText()); }
	;

// Non RM rule factoring repeated pattern in subprogram_declaration and
// subprog_decl_or_rename_or_inst_or_body
generic_subp_inst : IS! generic_inst SEMI!
	;

// Tail of generic_instantiation, i.e. without "(package|procedure|function) ... is".
// generic_instantiation per se does not exist (optimized away to avoid syn preds).
// 12.3
generic_inst : NEW! compound_name ( LPAREN! generic_actual_part RPAREN! )?
	{ pop_def_id(); }
	;

generic_actual_part : generic_association ( COMMA! generic_association )*
	;

// RM rule generic_association has explicit_generic_actual_parameter which would be:
//  expression | variable_name | subprogram_name | entry_name | subtype_mark | package_instance_name
// but since `expression' contains them all, we just use `expression':

generic_association : ( IDENTIFIER RIGHT_SHAFT^ )? expression
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
	  { #array_aggreg_elem_s =
		#(#[ARRAY_AGGREG_ELEM_S, "ARRAY_AGGREG_ELEM_S"], #array_aggreg_elem_s); }
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
	// { #value = #(#[VALUE, "VALUE"], #value); }
	;

ranged_expr_s : ranged_expr ( PIPE^ ranged_expr )*
	// { #ranged_expr_s =
	// 	#(#[RANGED_EXPRS, "RANGED_EXPRS"], #ranged_expr_s); }
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
	// { #range = #(#[RANGE_EXPR, "RANGE_EXPR"], #range); }
	;

range_dots : simple_expression DOT_DOT^ simple_expression
	;

// 4.1.4
range_attribute_reference :
	prefix TIC! r:RANGE^ ( LPAREN! expression RPAREN! )?
	{ Set(#r, RANGE_ATTRIBUTE_REFERENCE); }
	;

// Here, the definition of `prefix' deviates from the RM.
// This gives us some more strictness than `name' (which the RM uses to
// define `prefix'.)   See also: name
// 4.1
prefix : IDENTIFIER
		( DOT^ ( ALL | IDENTIFIER )
		| p:LPAREN^ expression_s RPAREN!
			{ Set(#p, INDEXEDCOMPONENT_OR_TYPECONVERSION_OR_FUNCTIONCALL); }
		)*
	{ #prefix = #(#[PREFIX, "PREFIX"], #prefix); }
	;

// non RM : 6.1 formal_part, optional
formal_part_opt : ( LPAREN! parameter_specification
		( SEMI! parameter_specification )*
		RPAREN! )?
	{ #formal_part_opt = #(#[FORMAL_PART_OPT, "FORMAL_PART_OPT"],
			       #formal_part_opt); }
	;

// 6.1
parameter_specification : def_ids_colon
	( ( null_exclusion_opt ACCESS ) => access_definition
	| aliased_opt mode null_exclusion_opt subtype_mark
	)
	init_opt
	{ #parameter_specification =
		#(#[PARAMETER_SPECIFICATION,
		   "PARAMETER_SPECIFICATION"], #parameter_specification); }
	;

// non RM rule factoring repeated occurrence of defining_identifier_list followed by colon
def_ids_colon : defining_identifier_list COLON!
	;

// 3.3.1
defining_identifier_list : IDENTIFIER ( COMMA! IDENTIFIER )*
	{ #defining_identifier_list =
		#(#[DEFINING_IDENTIFIER_LIST,
		   "DEFINING_IDENTIFIER_LIST"], #defining_identifier_list); }
	;

// 6.1
mode : ( IN )? ( OUT )?
	{ #mode = #(#[MODE, "MODE"], #mode); }
	;

renames : RENAMES! name
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
		( DOT^	( ALL
			| IDENTIFIER
			| CHARACTER_LITERAL
			| operator_string
			)
		| ( parenthesized_primary ) => parenthesized_primary
		| p:LPAREN^ expression_s RPAREN!
			{ Set(#p, INDEXEDCOMPONENT_OR_TYPECONVERSION_OR_FUNCTIONCALL); }
		| TIC^ ( parenthesized_primary | attribute_id )
		)*
	{ #name = #(#[NAME, "NAME"], #name); }
	;

operator_string
	: { is_operator_symbol(LT(1)->getText()) }?
		op:CHAR_STRING { #op->setType(OPERATOR_SYMBOL); }
	;

definable_operator_symbol returns [std::string d]
	: { definable_operator(LT(1)->getText()) }?
		op:CHAR_STRING { #op->setType(OPERATOR_SYMBOL); d = #op->getText(); }
	;

parenthesized_primary : pp:LPAREN^
		( NuLL RECORD!
		| value_s extension_opt
		)
	RPAREN!
	{ Set(#pp, PARENTHESIZED_PRIMARY); }
	;

extension_opt :  ( WITH! ( NuLL RECORD! | value_s ) )?
	{ #extension_opt =
		#(#[EXTENSION_OPT, "EXTENSION_OPT"], #extension_opt); }
	;

is_separate_or_abstract_or_decl! [RefAdaAST t]
	: IS! separate_or_abstract[t]
	| /* empty */
	  { // based on 6.1 subprogram_declaration
	    if (t->getType() == AdaTokenTypes::PROCEDURE)
	      Set(t, PROCEDURE_DECLARATION);
	    else
	      Set(t, FUNCTION_DECLARATION);
	  }
	;

separate_or_abstract! [RefAdaAST t]
	: SEPARATE!
		{ // based on 10.1.3 subprogram_body_stub
		  if (t->getType() == AdaTokenTypes::PROCEDURE)
		    Set(t, PROCEDURE_BODY_STUB);
		  else
		    Set(t, FUNCTION_BODY_STUB);
		}
	| ABSTRACT!
		{ // based on 3.9.3 abstract_subprogram_declaration
		  if (t->getType() == AdaTokenTypes::PROCEDURE)
		    Set(t, ABSTRACT_PROCEDURE_DECLARATION);
		  else
		    Set(t, ABSTRACT_FUNCTION_DECLARATION);
		}
	| /* empty */
	  { t->getType() == AdaTokenTypes::PROCEDURE }? NuLL!
		{ // 6.7
		  Set(t, NULL_PROCEDURE_DECLARATION);
		}
	;

// 6.1
defining_designator [bool lib_level, bool push_id=false]
	{ std::string d; }
	: { lib_level }? defining_identifier[true, push_id]
	| { !lib_level }? d=designator { if (push_id) push_def_id(d); }
	;

// 6.1
designator returns [std::string d]
	{ std::string op; }
	: op=definable_operator_symbol { d = op; }
	| n:IDENTIFIER { d = #n->getText(); }
	;

parameter_profile : formal_part_opt
	;

parameter_and_result_profile :
	func_formal_part_opt RETURN! null_exclusion_opt ( subtype_mark | access_def_no_nullex )
	;

// formal_part_opt is not strict enough for functions, i.e. it permits
// "in out" and "out" as modes, thus we make an extra rule.
// We are currently on Ada2005; when we go to Ada2012 this can be replaced by formal_part_opt.
func_formal_part_opt : ( LPAREN! func_param ( SEMI! func_param )* RPAREN! )?
	{ #func_formal_part_opt =
		#(#[FORMAL_PART_OPT,
		  "FORMAL_PART_OPT"], #func_formal_part_opt); }
	;

func_param : def_ids_colon
	( ( null_exclusion_opt ACCESS ) => access_definition
	| aliased_opt in_opt! null_exclusion_opt subtype_mark
	)
	init_opt
	{ #func_param =
		#(#[PARAMETER_SPECIFICATION,
		   "PARAMETER_SPECIFICATION"], #func_param); }
	;

in_opt : ( IN )?
	;

spec_decl_part [RefAdaAST pkg]
	: ( IS! ( generic_inst { Set(pkg, GENERIC_PACKAGE_INSTANTIATION);
				 pop_def_id(); }
		| pkg_spec_part { Set(pkg, PACKAGE_SPECIFICATION); }
		)
	| renames { Set(pkg, PACKAGE_RENAMING_DECLARATION);
		    pop_def_id(); }
	)
	SEMI!
	;

pkg_spec_part : basic_declarative_items_opt
		( PRIVATE basic_declarative_items_opt )?
		end_id_opt!
	;

basic_declarative_items_opt : ( basic_declarative_item | pragma )*
	{ #basic_declarative_items_opt =
		#(#[BASIC_DECLARATIVE_ITEMS_OPT,
		   "BASIC_DECLARATIVE_ITEMS_OPT"],
		  #basic_declarative_items_opt); }
	;

basic_declarative_items : ( basic_declarative_item | pragma )+
	{ #basic_declarative_items =
		#(#[BASIC_DECLARATIVE_ITEMS_OPT,
		   "BASIC_DECLARATIVE_ITEMS_OPT"],
		  #basic_declarative_items); }
	;

basic_declarative_item
	: pkg:PACKAGE^ defining_identifier[false, true] spec_decl_part[#pkg]
	| tsk:TASK^ task_type_or_single_decl[#tsk]
	| pro:PROTECTED^ prot_type_or_single_decl[#pro] SEMI!
	| subprogram_declaration
	| decl_common
	;

task_type_or_single_decl [RefAdaAST tsk]
	: TYPE! defining_identifier[false, true] discrim_part_opt task_definition_opt
		{ Set(tsk, TASK_TYPE_DECLARATION); }
	| defining_identifier[false, true] task_definition_opt
		{ Set(tsk, SINGLE_TASK_DECLARATION); }
	;

task_definition_opt
	: IS! new_interfacelist_with_opt task_items_opt private_task_items_opt end_id_opt! SEMI!
	| SEMI! { pop_def_id(); }
	;

discrim_part_opt
	: ( discrim_part_text )?
	{ #discrim_part_opt =
		#(#[DISCRIM_PART_OPT,
		   "DISCRIM_PART_OPT"], #discrim_part_opt); }
	;

discrim_part_text : LPAREN! (BOX | discriminant_specifications) RPAREN!
	;

empty_discrim_opt :  /* empty, constructed only for structural orthogonality
                        in type_def and generic_formal_parameter */
	{ #empty_discrim_opt =
		#(#[DISCRIM_PART_OPT,
		   "DISCRIM_PART_OPT"], #empty_discrim_opt); }
	;

discrim_part
	: discrim_part_text
	{ #discrim_part =
		#(#[DISCRIM_PART_OPT,
		   "DISCRIM_PART_OPT"], #discrim_part); }
	;

discriminant_specifications : discriminant_specification
		( SEMI! discriminant_specification )*
	{ #discriminant_specifications =
		#(#[DISCRIMINANT_SPECIFICATIONS,
		   "DISCRIMINANT_SPECIFICATIONS"],
		  #discriminant_specifications); }
	;

discriminant_specification :
	 def_ids_colon null_exclusion_opt ( subtype_mark | access_def_no_nullex ) init_opt
	{ #discriminant_specification =
		#(#[DISCRIMINANT_SPECIFICATION,
		   "DISCRIMINANT_SPECIFICATION"],
		  #discriminant_specification); }
	;

init_opt : ( ASSIGN! expression )?
	{ #init_opt = #(#[INIT_OPT, "INIT_OPT"], #init_opt); }
	;  // `expression' is of course much too loose;
	   // semantic checks are required in the usage contexts.

new_interfacelist_with_opt : ( NEW! interface_list WITH! )?
	{ #new_interfacelist_with_opt =
		#(#[NEW_INTERFACELIST_WITH_OPT,
		   "NEW_INTERFACELIST_WITH_OPT"], #new_interfacelist_with_opt); }
	;

task_items_opt : ( pragma )* entrydecls_repspecs_opt
	{ #task_items_opt =
		#(#[TASK_ITEMS_OPT, "TASK_ITEMS_OPT"], #task_items_opt); }
	;

entrydecls_repspecs_opt : ( entry_declaration ( pragma | rep_spec )* )*
	;

// 9.5.2
entry_declaration : e:ENTRY^ IDENTIFIER
		discrete_subtype_def_opt formal_part_opt SEMI!
	{ Set (#e, ENTRY_DECLARATION); }
	;

discrete_subtype_def_opt : ( (LPAREN discrete_subtype_definition RPAREN) =>
		LPAREN! discrete_subtype_definition RPAREN!
	| /* empty */
	)
	{ #discrete_subtype_def_opt =
		#(#[DISCRETE_SUBTYPE_DEF_OPT,
		   "DISCRETE_SUBTYPE_DEF_OPT"], #discrete_subtype_def_opt); }
	;

discrete_subtype_definition : ( (range) => range
	| subtype_indication
	)
	// Looks alot like discrete_range but it's not
	// (as soon as we start doing semantics.)
	;

rep_spec : r:FOR^ subtype_mark USE! rep_spec_part[#r] SEMI!
	;

rep_spec_part [RefAdaAST t]
	: RECORD! align_opt comp_loc_s END! RECORD! // record_type_spec
		{ Set(t, RECORD_REPRESENTATION_CLAUSE); }
	| AT! expression                        // address_spec (Ada83)
		{ Set(t, AT_CLAUSE); }
	| expression  // attrib_def. Semantic check must ensure that the
		// respective subtype_mark contains an attribute reference.
		{ Set(t, ATTRIBUTE_DEFINITION_CLAUSE); }
	;

align_opt : ( AT! MOD! expression SEMI! )?
	{ #align_opt = #(#[MOD_CLAUSE_OPT, "MOD_CLAUSE_OPT"], #align_opt); }
	;

comp_loc_s : ( pragma | subtype_mark AT! expression RANGE! range SEMI! )*
	{ #comp_loc_s = #(#[COMPONENT_CLAUSES_OPT, "COMPONENT_CLAUSES_OPT"],
			  #comp_loc_s); }
	;

private_task_items_opt : ( PRIVATE! ( pragma )* entrydecls_repspecs_opt )?
	{ #private_task_items_opt =
		#(#[PRIVATE_TASK_ITEMS_OPT,
		   "PRIVATE_TASK_ITEMS_OPT"], #private_task_items_opt); }
	;

prot_type_or_single_decl [RefAdaAST pro]
	: TYPE! defining_identifier[false] discrim_part_opt protected_definition
		{ Set(pro, PROTECTED_TYPE_DECLARATION); }
	| defining_identifier[false] protected_definition
		{ Set(pro, SINGLE_PROTECTED_DECLARATION); }
	;

protected_definition
	: IS! new_interfacelist_with_opt prot_op_decl_s ( PRIVATE! prot_member_decl_s )? end_id_opt!
	;

prot_op_decl_s : ( protected_operation_declaration )*
	{ #prot_op_decl_s = #(#[PROT_OP_DECLARATIONS,
			      "PROT_OP_DECLARATIONS"], #prot_op_decl_s); }
	;

// 9.4
protected_operation_declaration : entry_declaration
	| p:PROCEDURE^ defining_identifier[false] formal_part_opt SEMI!
		{ pop_def_id(); Set(#p, PROCEDURE_DECLARATION); }
	| f:FUNCTION^ defining_designator[false] parameter_and_result_profile SEMI!
		{ pop_def_id(); Set(#f, FUNCTION_DECLARATION); }
	| rep_spec
	| pragma
	;

protected_element_declaration : ( protected_operation_declaration | component_declaration )
	;

prot_member_decl_s : ( protected_element_declaration )*
	{ #prot_member_decl_s =
		#(#[PROT_MEMBER_DECLARATIONS,
		   "PROT_MEMBER_DECLARATIONS"], #prot_member_decl_s); }
	;

// 3.8
component_declaration : def_ids_colon component_definition init_opt SEMI!
	{ #component_declaration =
		#(#[COMPONENT_DECLARATION,
		   "COMPONENT_DECLARATION"], #component_declaration); }
	;

// decl_common is shared between declarative_item and basic_declarative_item.
// decl_common only contains specifications.
decl_common
	: t:TYPE^ IDENTIFIER
		( IS! type_def[#t]  // type_definition is resolved to its
				    // finer grained rules.
		|	( discrim_part
				( IS! derived_or_private_or_record[#t, true]
				| /* empty */
					{ Set(#t, INCOMPLETE_TYPE_DECLARATION); }
				)
			| empty_discrim_opt
			  { Set(#t, INCOMPLETE_TYPE_DECLARATION); }
			  // NB: In this case, the discrim_part_opt does not
			  //   appear in the INCOMPLETE_TYPE_DECLARATION node.
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
	| s:SUBTYPE^ IDENTIFIER IS! subtype_indication SEMI!  // subtype_declaration
		{ Set(#s, SUBTYPE_DECLARATION); }
	| generic_decl[false]
	| use_clause
	| r:FOR^ ( (local_enum_name USE LPAREN) => local_enum_name USE!
			enumeration_aggregate
			{ Set(#r, ENUMERATION_REPESENTATION_CLAUSE); }
		| subtype_mark USE! rep_spec_part[#r]
		)
		SEMI!
	| (IDENTIFIER COLON EXCEPTION RENAMES) =>
		IDENTIFIER erd:COLON^ EXCEPTION! RENAMES! compound_name SEMI!
			{ Set(#erd, EXCEPTION_RENAMING_DECLARATION); }
	// TBC: The next 3 patterns all lead to OBJECT_RENAMING_DECLARATION,
	//      probably we need separate finer grained tokens?
	| (IDENTIFIER RENAMES) =>
		IDENTIFIER ord1:RENAMES^ name SEMI!
			{ Set(#ord1, OBJECT_RENAMING_DECLARATION); }
	| (IDENTIFIER COLON null_exclusion_opt subtype_mark RENAMES) =>
		IDENTIFIER ord2:COLON^ null_exclusion_opt subtype_mark RENAMES! name SEMI!
			{ Set(#ord2, OBJECT_RENAMING_DECLARATION); }
	| (IDENTIFIER COLON access_definition RENAMES) =>
		IDENTIFIER ord3:COLON^ access_definition RENAMES! name SEMI!
			{ Set(#ord3, OBJECT_RENAMING_DECLARATION); }
	| defining_identifier_list od:COLON^  // object_declaration
		( EXCEPTION!
			{ Set(#od, EXCEPTION_DECLARATION); }
		| (CONSTANT ASSIGN) => CONSTANT! ASSIGN! expression
			{ Set(#od, NUMBER_DECLARATION); }
		| aliased_constant_opt
			( array_type_definition[#od] init_opt
				{ Set(#od, ARRAY_OBJECT_DECLARATION); }
				// Not an RM rule, but simplifies distinction
				// from the non-array object_declaration.
			| subtype_indication init_opt
				{ Set(#od, OBJECT_DECLARATION); }
			)
		)
		SEMI!
	;

// Tail part of type_definition
// Full type_definition is assembled in decl_common.
// 3.2.1
type_def [RefAdaAST t]
	: LPAREN! enum_id_s RPAREN!
		{ Set(t, ENUMERATION_TYPE_DECLARATION); }
	| RANGE! range
		{ Set(t, SIGNED_INTEGER_TYPE_DECLARATION); }
	| MOD! expression
		{ Set(t, MODULAR_TYPE_DECLARATION); }
	| DIGITS! expression range_constraint_opt
		{ Set(t, FLOATING_POINT_DECLARATION); }
	| DELTA! expression
		( RANGE! range
			{ Set(t, ORDINARY_FIXED_POINT_DECLARATION); }
		| DIGITS! expression range_constraint_opt
			{ Set(t, DECIMAL_FIXED_POINT_DECLARATION); }
		)
	| array_type_definition[t]
	| access_type_definition[t]
	| ( ( LIMITED | TASK | PROTECTED | SYNCHRONIZED )? INTERFACE ) =>
	  interface_type_definition[t]
	| empty_discrim_opt derived_or_private_or_record[t, false]
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
array_type_definition [RefAdaAST t]
	: ARRAY! LPAREN! index_or_discrete_range_s RPAREN!
		OF! component_definition
		{ Set(t, ARRAY_TYPE_DECLARATION); }
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
	{ #component_definition =
		#(#[COMPONENT_DEFINITION,
		   "COMPONENT_DEFINITION"], #component_definition); }
	;

aliased_opt : ( ALIASED )?
	{ #aliased_opt = #(#[MODIFIERS, "MODIFIERS"], #aliased_opt); }
	;

// This is subtype_indication without the leading null_exclusion_opt.
// We use this instead of subtype_indication in places where null_exclusion_opt
// would introduce ambiguity.
subtype_ind_no_nullex : subtype_mark constraint_opt
	;

// 3.2.2
subtype_indication : null_exclusion_opt subtype_ind_no_nullex
	{ #subtype_indication = #(#[SUBTYPE_INDICATION, "SUBTYPE_INDICATION"],
			           #subtype_indication); }
	;

constraint_opt : ( range_constraint
	| digits_constraint
	| delta_constraint
	| (index_constraint) => index_constraint
	| discriminant_constraint
	)?
	;

// 3.5.9
digits_constraint : d:DIGITS^ expression range_constraint_opt
	{ Set(#d, DIGITS_CONSTRAINT); }
	;

// J.3
delta_constraint : d:DELTA^ expression range_constraint_opt
	{ Set(#d, DELTA_CONSTRAINT); }
	;

// 3.6.1
index_constraint : p:LPAREN^ discrete_range ( COMMA! discrete_range )* RPAREN!
	{ Set(#p, INDEX_CONSTRAINT); }
	;

// 3.6.1
discrete_range
	: (range) => range
	| subtype_indication
	;

// 3.7.1
discriminant_constraint : p:LPAREN^ discriminant_association 
		( COMMA! discriminant_association )* RPAREN!
	{ Set(#p, DISCRIMINANT_CONSTRAINT); }
	;

// 3.7.1
discriminant_association : selector_names_opt expression
	{ #discriminant_association =
		#(#[DISCRIMINANT_ASSOCIATION,
		   "DISCRIMINANT_ASSOCIATION"], #discriminant_association); }
	;

selector_names_opt : ( (association_head) => association_head
	| /* empty */
	)
	{ #selector_names_opt =
		#(#[SELECTOR_NAMES_OPT,
		   "SELECTOR_NAMES_OPT"], #selector_names_opt); }
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
	{ #null_exclusion_opt =
		#(#[NULL_EXCLUSION_OPT,
		   "NULL_EXCLUSION_OPT"], #null_exclusion_opt); }
	;

constant_opt : ( CONSTANT )?
	{ #constant_opt =
	  #(#[MODIFIERS, "MODIFIERS"], #constant_opt); }
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
access_type_definition [RefAdaAST t]
	: null_exclusion_opt ACCESS!
		( protected_opt
			( PROCEDURE! formal_part_opt
				{ Set(t, ACCESS_TO_PROCEDURE_DECLARATION); }
			| FUNCTION! func_formal_part_opt RETURN! subtype_mark
				{ Set(t, ACCESS_TO_FUNCTION_DECLARATION); }
			)
		| general_access_modifier_opt subtype_indication
			{ Set(t, ACCESS_TO_OBJECT_DECLARATION); }
		)
	;

limited_task_protected_synchronized_opt
	: ( LIMITED | TASK | PROTECTED | SYNCHRONIZED )?
	  { #limited_task_protected_synchronized_opt =
		#(#[MODIFIERS, "MODIFIERS"],
		   #limited_task_protected_synchronized_opt); }
	;

// 3.9.4
interface_list
	: subtype_mark ( AND subtype_mark )*
	;

and_interface_list_opt
	: ( AND interface_list )?
	  { #and_interface_list_opt =
	      #(#[AND_INTERFACE_LIST_OPT, "AND_INTERFACE_LIST_OPT"],
	         #and_interface_list_opt); }
	;

interface_type_definition [RefAdaAST t]
	: limited_task_protected_synchronized_opt INTERFACE! and_interface_list_opt
		{ Set(t, INTERFACE_TYPE_DEFINITION); }
	;

protected_opt : ( PROTECTED )?
	{ #protected_opt = #(#[MODIFIERS, "MODIFIERS"], #protected_opt); }
	;

// Modification of general_access_modifier supporting optionality
// 3.10
general_access_modifier_opt : ( CONSTANT | ALL )?
	{ #general_access_modifier_opt =
		#(#[MODIFIERS, "MODIFIERS"], #general_access_modifier_opt); }
	;

// type Indefinite_New_W_Disc (ND : Sub_Type) is new Indefinite_Tag_W_Disc (ND) with record
derived_or_private_or_record [RefAdaAST t, bool has_discrim]
	: abstract_tagged_limited_synchronized_opt
		( PRIVATE! { Set(t, PRIVATE_TYPE_DECLARATION); }
		| record_definition[has_discrim]
			{ Set(t, RECORD_TYPE_DECLARATION); }
		| NEW! subtype_indication
			( ( and_interface_list_opt WITH ) =>
			  and_interface_list_opt WITH!
				( PRIVATE!  { Set(t, PRIVATE_EXTENSION_DECLARATION); }
				| record_definition[has_discrim]
					{ Set(t, DERIVED_RECORD_EXTENSION); }
				)
			| /* empty */
				{ Set(t, ORDINARY_DERIVED_TYPE_DECLARATION); }
			)
		)
	;

// 3.8
record_definition [bool has_discrim]
	: RECORD! component_list[has_discrim] END! RECORD!
	| NuLL! RECORD!  // Thus the component_list is optional in the tree.
			 // If it is absent then we have `null record'.
	;

// 3.8
component_list [bool has_discrim]
	: NuLL! SEMI!  // Thus the component_list is optional in the tree.
	| component_items ( variant_part { has_discrim }? )?
	| empty_component_items variant_part { has_discrim }?
	;

component_items : ( pragma | component_declaration )+
	{ #component_items =
		#(#[COMPONENT_ITEMS,
		   "COMPONENT_ITEMS"], #component_items); }
	;

empty_component_items :
	{ #empty_component_items =
		#(#[COMPONENT_ITEMS,
		   "COMPONENT_ITEMS"], #empty_component_items); }
	;

// 3.8.1
variant_part : c:CASE^ discriminant_direct_name IS! variant_s END! CASE! SEMI!
	{ Set (#c, VARIANT_PART); }
	;

discriminant_direct_name : IDENTIFIER  // TBD: symtab lookup.
	;

variant_s : ( variant )+
	{ #variant_s = #(#[VARIANTS, "VARIANTS"], #variant_s); }
	;

// 3.8.1
variant : w:WHEN^ choice_s RIGHT_SHAFT! component_list[true]
	{ Set (#w, VARIANT); }
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
	{ #mark_with_constraint =
		#(#[MARK_WITH_CONSTRAINT,
		   "MARK_WITH_CONSTRAINT"], #mark_with_constraint); }
	;

// Slightly loose , "tagged" shall not appear on derived_type_definition.
abstract_tagged_limited_synchronized_opt
	: ( ABSTRACT TAGGED! | TAGGED )?
	  ( LIMITED | SYNCHRONIZED )?
	{ #abstract_tagged_limited_synchronized_opt =
	  #(#[MODIFIERS, "MODIFIERS"], #abstract_tagged_limited_synchronized_opt); }
	;

local_enum_name : IDENTIFIER  // to be refined: do a symbol table lookup
	;

enumeration_aggregate : array_aggregate
	;

aliased_constant_opt : ( ALIASED )? ( CONSTANT )?
	{ #aliased_constant_opt =
	  #(#[MODIFIERS, "MODIFIERS"], #aliased_constant_opt); }
	;

generic_decl [bool lib_level]
	: g:GENERIC^ generic_formal_part_opt
	( PACKAGE! defining_identifier[lib_level, true]
		( renames { Set(#g, GENERIC_PACKAGE_RENAMING);
			    pop_def_id(); }
		| IS! pkg_spec_part { Set(#g, GENERIC_PACKAGE_DECLARATION); }
		)
	| PROCEDURE! defining_identifier[lib_level] formal_part_opt
		( renames { Set(#g, GENERIC_PROCEDURE_RENAMING); }
		  // ^^^ Semantic check must ensure that the (generic_formal)*
		  //     after GENERIC is not given here.
		| /* empty */  { Set(#g, GENERIC_PROCEDURE_DECLARATION); }
		)
	| FUNCTION! defining_designator[lib_level] parameter_and_result_profile
		( renames { Set(#g, GENERIC_FUNCTION_RENAMING); }
		  // ^^^ Semantic check must ensure that the (generic_formal)*
		  //     after GENERIC is not given here.
		| /* empty */  { Set(#g, GENERIC_FUNCTION_DECLARATION); }
		)
	)
	SEMI!
	;

// This is generic_formal_part without the leading keyword `generic'
// (which is handled in generic_decl).
// 12.1
generic_formal_part_opt : ( use_clause | pragma | generic_formal_parameter )*
		{ #generic_formal_part_opt =
			#(#[GENERIC_FORMAL_PART,
			   "GENERIC_FORMAL_PART"],
			  #generic_formal_part_opt); }
	;

generic_formal_parameter :
	( t:TYPE^ defining_identifier[false]
		( IS!
			( LPAREN! BOX! RPAREN!
				{ Set (#t, FORMAL_DISCRETE_TYPE_DECLARATION); }
			| RANGE! BOX!
				{ Set (#t, FORMAL_SIGNED_INTEGER_TYPE_DECLARATION); }
			| MOD! BOX!
				{ Set (#t, FORMAL_MODULAR_TYPE_DECLARATION); }
			| DELTA! BOX!
				( DIGITS! BOX!
					{ Set (#t, FORMAL_DECIMAL_FIXED_POINT_DECLARATION); }
				| { Set (#t, FORMAL_ORDINARY_FIXED_POINT_DECLARATION); }
				)
			| DIGITS! BOX!
				{ Set (#t, FORMAL_FLOATING_POINT_DECLARATION); }
			| array_type_definition[#t]
			| access_type_definition[#t]
			| empty_discrim_opt discriminable_type_definition[#t]
			)
		| discrim_part IS! discriminable_type_definition[#t]
		)
		{ pop_def_id(); }
	| w:WITH^ ( PROCEDURE! defining_identifier[false] formal_part_opt subprogram_default_opt
			{ Set(#w, FORMAL_PROCEDURE_DECLARATION); }
		| FUNCTION! defining_designator[false] parameter_and_result_profile subprogram_default_opt
			{ Set(#w, FORMAL_FUNCTION_DECLARATION); }
		| PACKAGE! defining_identifier[false] IS! NEW! compound_name formal_package_actual_part_opt
			{ Set(#w, FORMAL_PACKAGE_DECLARATION); }
		)
		{ pop_def_id(); }
	| defining_identifier_list fod:COLON^ mode null_exclusion_opt
		( subtype_mark
		| access_def_no_nullex
		)
		init_opt
			{ Set(#fod, FORMAL_OBJECT_DECLARATION); }
	)
	SEMI!
	;

discriminable_type_definition [RefAdaAST t]
	: abstract_tagged_limited_synchronized_opt
	  ( PRIVATE! { Set(t, FORMAL_PRIVATE_TYPE_DECLARATION); }
	  | NEW! subtype_indication
		( ( and_interface_list_opt WITH ) =>
		  and_interface_list_opt WITH! PRIVATE!
			{ Set(t, FORMAL_PRIVATE_EXTENSION_DECLARATION); }
		| /* empty */
			{ Set(t, FORMAL_ORDINARY_DERIVED_TYPE_DECLARATION); }
		)
	  | /* empty, i.e. abstract_tagged_limited_synchronized_opt is probably TAGGED */
		{ Set (t, FORMAL_INCOMPLETE_TYPE_DECLARATION); }
	  )
	;

subprogram_default_opt : ( IS! ( BOX | name ) )?
	;

formal_package_actual_part_opt
	: ( LPAREN! ( BOX | defining_identifier_list ) RPAREN! )?
	;

// Auxiliary to (lib_)subprog_decl_or_rename_or_inst_or_body
proc_decl_or_renaming_or_inst_or_body  [RefAdaAST p] :
	  generic_subp_inst
		{ Set(#p, GENERIC_PROCEDURE_INSTANTIATION);
		  pop_def_id(); }
	| formal_part_opt
		( renames { Set(#p, PROCEDURE_RENAMING_DECLARATION);
		            pop_def_id(); }
		| IS!	( separate_or_abstract[#p] { pop_def_id(); }
			| body_part { Set(#p, PROCEDURE_BODY); }
			)
		| { pop_def_id();
		    Set(#p, PROCEDURE_DECLARATION); }
		)
		SEMI!
	;

// Auxiliary to (lib_)subprog_decl_or_rename_or_inst_or_body
func_decl_or_renaming_or_inst_or_body  [RefAdaAST f] :
	  generic_subp_inst
		{ Set(#f, GENERIC_FUNCTION_INSTANTIATION);
		  pop_def_id(); }
	| parameter_and_result_profile
		( renames { Set(#f, FUNCTION_RENAMING_DECLARATION);
		            pop_def_id(); }
		| IS!	( separate_or_abstract[#f] { pop_def_id(); }
			| body_part { Set(#f, FUNCTION_BODY); }
			)
		| { pop_def_id();
		    Set(#f, FUNCTION_DECLARATION); }
		)
		SEMI!
	;

lib_subprog_decl_or_rename_or_inst_or_body :
	  p:PROCEDURE^ defining_identifier[true, true] proc_decl_or_renaming_or_inst_or_body[#p]
	| f:FUNCTION^  defining_designator[true, true] func_decl_or_renaming_or_inst_or_body[#f]
	// Passing true to defining_{identifier,designator} argument push_id means
	// all non body alternatives must pop_def_id().
	;

subprog_decl_or_rename_or_inst_or_body :
	overriding_opt
	( p:PROCEDURE^ defining_identifier[false, true] proc_decl_or_renaming_or_inst_or_body[#p]
	| f:FUNCTION^  defining_designator[false, true] func_decl_or_renaming_or_inst_or_body[#f]
	)
	// Passing true to defining_{identifier,designator} argument push_id means
	// all non body alternatives must pop_def_id().
	;

body_part : declarative_part block_body end_id_opt!
	;

// 3.11
declarative_part : ( pragma | declarative_item )*
	{ #declarative_part =
		#(#[DECLARATIVE_PART, "DECLARATIVE_PART"],
		  #declarative_part); }
	;

// A declarative_item may appear in the declarative part of any body.
// 3.11
declarative_item :
	( pkg:PACKAGE^ ( body_is
			( separate { Set(#pkg, PACKAGE_BODY_STUB); }
			| pkg_body_part end_id_opt!
				{ Set(#pkg, PACKAGE_BODY); }
			)
			SEMI!
		| defining_identifier[false, true] spec_decl_part[#pkg]
		)
	| tsk:TASK^ ( body_is
			( separate { Set(#tsk, TASK_BODY_STUB); }
			| body_part { Set(#tsk, TASK_BODY); }
			)
			SEMI!
		| task_type_or_single_decl[#tsk]
		)
	| pro:PROTECTED^
		( body_is
			( separate { Set(#pro, PROTECTED_BODY_STUB); }
	       		| prot_op_bodies_opt end_id_opt!
				{ Set(#pro, PROTECTED_BODY); }
			)
		| prot_type_or_single_decl[#pro]
		)
		SEMI!
	| subprog_decl_or_rename_or_inst_or_body
	| decl_common
	)
	/* DECLARATIVE_ITEM is just a pass-thru node so we omit it.
	 { #declarative_item =
		#(#[DECLARATIVE_ITEM,
		   "DECLARATIVE_ITEM"], #declarative_item); }
	 */
	;

body_is : BODY! defining_identifier[false, true] IS!
	;

separate : SEPARATE! { pop_def_id(); }
	;

pkg_body_part : declarative_part block_body_opt
	;

block_body_opt : ( BEGIN! handled_sequence_of_statements )?
	{ #block_body_opt =
		#(#[BLOCK_BODY_OPT,
		   "BLOCK_BODY_OPT"], #block_body_opt); }
	;

prot_op_bodies_opt :
	( entry_body
	| subprog_decl_or_body
	| pragma
	)*
	{ #prot_op_bodies_opt =
		#(#[PROT_OP_BODIES_OPT,
		   "PROT_OP_BODIES_OPT"], #prot_op_bodies_opt); }
	;

subprog_decl_or_body
	: p:PROCEDURE^ defining_identifier[false, true] formal_part_opt
		( IS! body_part { Set(#p, PROCEDURE_BODY); }
		| { pop_def_id(); Set(#p, PROCEDURE_DECLARATION); }
		)
		SEMI!
	| f:FUNCTION^ defining_designator[false, true] parameter_and_result_profile
		( IS! body_part { Set(#f, FUNCTION_BODY); }
		| { pop_def_id(); Set(#f, FUNCTION_DECLARATION); }
		)
		SEMI!
	;

block_body : b:BEGIN^ handled_sequence_of_statements
	{ Set(#b, BLOCK_BODY); }
	;

// 11.2
handled_sequence_of_statements : sequence_of_statements except_handler_part_opt
	{ #handled_sequence_of_statements =
		#(#[HANDLED_SEQUENCE_OF_STATEMENTS,
		   "HANDLED_SEQUENCE_OF_STATEMENTS"], #handled_sequence_of_statements); }
	;

// 5.1
sequence_of_statements : ( pragma | statement )+
	{ #sequence_of_statements =
		#(#[SEQUENCE_OF_STATEMENTS,
	 	   "SEQUENCE_OF_STATEMENTS"], #sequence_of_statements); }
	;

statement : def_labels_opt
	( null_statement
	| exit_statement
	| simple_return_statement
	| extended_return_statement
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
	{ #statement = #(#[STATEMENT, "STATEMENT"], #statement); }
	;

def_labels_opt : ( LT_LT! IDENTIFIER GT_GT! )*
	{ #def_labels_opt = #(#[LABELS_OPT, "LABELS_OPT"], #def_labels_opt); }
	;

// 5.1
null_statement : s:NuLL SEMI!
	{ Set(#s, NULL_STATEMENT); }
	;

// 5.3
if_statement : s:IF^ cond_clause elsifs_opt
	  else_opt
	  END! IF! SEMI!
	{ Set(#s, IF_STATEMENT); }
	;

cond_clause : condition c:THEN^ sequence_of_statements
	{ Set(#c, COND_CLAUSE); }
	;

condition : expression
	// { #condition = #(#[CONDITION, "CONDITION"], #condition); }
	;

elsifs_opt : ( ELSIF! cond_clause )*
	{ #elsifs_opt = #(#[ELSIFS_OPT, "ELSIFS_OPT"], #elsifs_opt); }
	;

else_opt : ( ELSE! sequence_of_statements )?
	{ #else_opt = #(#[ELSE_OPT, "ELSE_OPT"], #else_opt); }
	;

// 5.4
case_statement : s:CASE^ expression IS! alternative_s END! CASE! SEMI!
	{ Set(#s, CASE_STATEMENT); }
	;

alternative_s : ( case_statement_alternative )+
	;

// 5.4
case_statement_alternative : s:WHEN^ choice_s RIGHT_SHAFT! sequence_of_statements
	{ Set(#s, CASE_STATEMENT_ALTERNATIVE); }
	;

block_or_loop_with_stmt_id :
	statement_identifier
	( loop_stmt id_opt! SEMI!
	  { #block_or_loop_with_stmt_id =
	      #(#[LOOP_STATEMENT, "LOOP_STATEMENT"], #block_or_loop_with_stmt_id); }
	| block end_id_opt! SEMI!
	  { #block_or_loop_with_stmt_id =
	      #(#[BLOCK_STATEMENT, "BLOCK_STATEMENT"], #block_or_loop_with_stmt_id); }
	)
	;

loop_without_stmt_id :
	empty_stmt_id loop_stmt SEMI!
	{ #loop_without_stmt_id =
	     #(#[LOOP_STATEMENT, "LOOP_STATEMENT"], #loop_without_stmt_id); }
	;

// loop_statement but without the leading [loop_statement_identifier:]
// Common backend for block_or_loop_with_stmt_id and loop_without_stmt_id
// 5.5
loop_stmt : iteration_scheme_opt
		LOOP! sequence_of_statements END! LOOP!  // basic_loop
        ;

iteration_scheme_opt : ( WHILE^ condition
	| FOR^ IDENTIFIER IN! reverse_opt discrete_subtype_definition
	)?
	{ #iteration_scheme_opt =
		#(#[ITERATION_SCHEME_OPT,
		   "ITERATION_SCHEME_OPT"], #iteration_scheme_opt); }
	;

reverse_opt : ( REVERSE )?
	{ #reverse_opt = #(#[MODIFIERS, "MODIFIERS"], #reverse_opt); }
	;

id_opt { std::string endid; } :
	endid=definable_operator_symbol { end_id_matches_def_id (endid) }?
	| n:compound_name { end_id_matches_def_id (#n->getText()) }?
	  /* Ordinarily we would need to be stricter here, i.e.
	     match compound_name only for the library-level case
	     (and IDENTIFIER otherwise), but end_id_matches_def_id
	     does the right thing for us.  */
	| { pop_def_id(); }
	;

end_id_opt : END! id_opt
	;

/* Note: This rule should really be `statement_identifier_opt'.
   However, manual disambiguation of `loop_stmt' from `block'
   in the presence of the statement_identifier in `statement'
   results in this rule. The case of loop_stmt/block given
   without the statement_identifier is coded in the rules
   loop_without_stmt_id / block_without_stmt_id.  */
// 5.1
statement_identifier : n:IDENTIFIER t:COLON!
	{ push_def_id(#n->getText());
	  #statement_identifier =
	  	#(#[STATEMENT_IDENTIFIER_OPT,
		   "STATEMENT_IDENTIFIER_OPT"], #statement_identifier); }
	;

empty_stmt_id :
	{ #empty_stmt_id =
	  	#(#[STATEMENT_IDENTIFIER_OPT,
		   "STATEMENT_IDENTIFIER_OPT"], #empty_stmt_id); }
	;

block_without_stmt_id :
	empty_stmt_id block END! SEMI!
	{ #block_without_stmt_id =
	     #(#[BLOCK_STATEMENT, "BLOCK_STATEMENT"], #block_without_stmt_id); }
	;

// block_statement but without the leading [block_statement_identifier:]
// Common backend for block_or_loop_with_stmt_id and block_without_stmt_id
// 5.6
block : declare_opt block_body
	;

declare_opt : ( DECLARE! declarative_part )?
	{ #declare_opt = #(#[DECLARE_OPT, "DECLARE_OPT"], #declare_opt); }
	;

// 5.7
exit_statement : s:EXIT^ ( label_name )? ( WHEN condition )? SEMI!
	{ Set(#s, EXIT_STATEMENT); }
	;

// RM says (label_)name where (label_) is label_ rendered in italics.
// However, since `name' is much too loose, we use IDENTIFIER.
label_name : IDENTIFIER
	;

// 6.5
simple_return_statement : s:RETURN^ ( expression )? SEMI!
	{ Set(#s, SIMPLE_RETURN_STATEMENT); }
	;

// Not using defining_identifier here because this is never lib_level.
// 6.5
extended_return_statement :
	s:RETURN^ IDENTIFIER COLON! aliased_opt return_subtype_indication init_opt
	( DO! handled_sequence_of_statements END! RETURN! )?
	SEMI!
	{ Set(#s, EXTENDED_RETURN_STATEMENT); }
	;

// Not using ( subtype_indication | access_definition ) due to
// ambiguity triggered by null_exclusion_opt.
return_subtype_indication : null_exclusion_opt
	( subtype_ind_no_nullex | access_def_no_nullex )
	{ #return_subtype_indication =
		#(#[RETURN_SUBTYPE_INDICATION,
		   "RETURN_SUBTYPE_INDICATION"], #return_subtype_indication); }
	;

// 5.8
goto_statement : s:GOTO^ label_name SEMI!
	{ Set(#s, GOTO_STATEMENT); }
	;

call_or_assignment :  // procedure_call is in here.
	name ( ASSIGN! expression
		{ #call_or_assignment =
			#(#[ASSIGNMENT_STATEMENT,
			   "ASSIGNMENT_STATEMENT"], #call_or_assignment); }
	     |  { #call_or_assignment =
			#(#[CALL_STATEMENT,
			   "CALL_STATEMENT"], #call_or_assignment); }
		/* Preliminary. Use semantic analysis to produce
		   {PROCEDURE|ENTRY}_CALL_STATEMENT.  */
	     )
	SEMI!
	;

entry_body : e:ENTRY^ defining_identifier[false, true] entry_body_formal_part entry_barrier IS!
		body_part SEMI!
	{ Set (#e, ENTRY_BODY); }
	;

entry_body_formal_part : entry_index_spec_opt formal_part_opt
	;

entry_index_spec_opt :
	( (LPAREN FOR) =>
		LPAREN! FOR! defining_identifier[false] IN! discrete_subtype_definition RPAREN!
	| /* empty */
	)
	{ #entry_index_spec_opt =
		#(#[ENTRY_INDEX_SPECIFICATION,
		   "ENTRY_INDEX_SPECIFICATION"], #entry_index_spec_opt); }
	;

entry_barrier : WHEN! condition
	;

// 9.5.3
entry_call_statement : name SEMI!  // Semantic analysis required, for example
				   // to ensure `name' is an entry.
	{ #entry_call_statement =
		#(#[ENTRY_CALL_STATEMENT,
		   "ENTRY_CALL_STATEMENT"], #entry_call_statement); }
	;

// 9.5.2
accept_statement : a:ACCEPT^ defining_identifier[false, true] entry_index_opt formal_part_opt
		( DO! handled_sequence_of_statements end_id_opt! SEMI!
		| SEMI! { pop_def_id(); }
		)
	{ Set (#a, ACCEPT_STATEMENT); }
	;

entry_index_opt : ( (LPAREN expression RPAREN) => LPAREN! expression RPAREN!
	// Looks alot like parenthesized_expr_opt but it's not.
	// We need the syn pred for the usage context in accept_statement.
	// The formal_part_opt that follows the entry_index_opt there
	// creates ambiguity (due to the opening LPAREN.)
	| /* empty */
	)
	{ #entry_index_opt =
		#(#[ENTRY_INDEX_OPT,
		   "ENTRY_INDEX_OPT"], #entry_index_opt); }
	;

// delay_statement directly codes delay_until_statement and
// delay_relative_statement thus those rules are not present.
// delay_until_statement is signaled by non empty until_opt.
// 9.6
delay_statement : d:DELAY^ until_opt expression SEMI!
	{ Set (#d, DELAY_STATEMENT); }
	;

until_opt : ( UNTIL )?
	{ #until_opt = #(#[MODIFIERS, "MODIFIERS"], #until_opt); }
	;

// SELECT_STATEMENT is not modeled in the AST since it is trivially
// reconstructed:
//   select_statement ::= selective_accept | timed_entry_call
//             | conditional_entry_call | asynchronous_select
// 9.7
select_statement : s:SELECT^
	( (triggering_alternative THEN ABORT) =>
		triggering_alternative THEN! ABORT! abortable_part
		{ Set (#s, ASYNCHRONOUS_SELECT); }
	| selective_accept
		{ Set (#s, SELECTIVE_ACCEPT); }
	| entry_call_alternative
		( OR! delay_alternative { Set (#s, TIMED_ENTRY_CALL); }
		| ELSE! sequence_of_statements { Set (#s, CONDITIONAL_ENTRY_CALL); }
		)
	)
	END! SELECT! SEMI!
	// { Set (#s, SELECT_STATEMENT); }
	;

triggering_alternative : ( delay_statement | entry_call_statement ) stmts_opt
	{ #triggering_alternative =
		#(#[TRIGGERING_ALTERNATIVE,
		   "TRIGGERING_ALTERNATIVE"], #triggering_alternative); }
	;

abortable_part : stmts_opt
	{ #abortable_part =
		#(#[ABORTABLE_PART,
		   "ABORTABLE_PART"], #abortable_part); }
	;

entry_call_alternative : entry_call_statement stmts_opt
	{ #entry_call_alternative =
		#(#[ENTRY_CALL_ALTERNATIVE,
		   "ENTRY_CALL_ALTERNATIVE"], #entry_call_alternative); }
	;

selective_accept : guard_opt select_alternative or_select_opt else_opt
	;

guard_opt : ( WHEN! condition RIGHT_SHAFT! ( pragma )* )?
	{ #guard_opt = #(#[GUARD_OPT, "GUARD_OPT"], #guard_opt); }
	;

select_alternative  // Not modeled in AST since it's just a pass-through.
	: accept_alternative
	| delay_alternative
	| t:TERMINATE SEMI!  { Set(#t, TERMINATE_ALTERNATIVE); }
	;

accept_alternative : accept_statement stmts_opt
	{ #accept_alternative =
		#(#[ACCEPT_ALTERNATIVE,
		   "ACCEPT_ALTERNATIVE"], #accept_alternative); }
	;

delay_alternative : delay_statement stmts_opt
	{ #delay_alternative =
		#(#[DELAY_ALTERNATIVE,
		   "DELAY_ALTERNATIVE"], #delay_alternative); }
	;

stmts_opt : ( pragma | statement )*
	;

or_select_opt : ( OR! guard_opt select_alternative )*
	{ #or_select_opt =
		#(#[OR_SELECT_OPT, "OR_SELECT_OPT"], #or_select_opt); }
	;

// 9.8
abort_statement : a:ABORT^ name ( COMMA! name )* SEMI!
	{ Set (#a, ABORT_STATEMENT); }
	;

except_handler_part_opt : ( EXCEPTION! ( exception_handler )+ )?
	{ #except_handler_part_opt =
		#(#[EXCEPT_HANDLER_PART_OPT,
		   "EXCEPT_HANDLER_PART_OPT"], #except_handler_part_opt); }
	;

exception_handler : w:WHEN^ identifier_colon_opt except_choice_s RIGHT_SHAFT!
		sequence_of_statements
	{ Set (#w, EXCEPTION_HANDLER); }
	;

identifier_colon_opt : ( IDENTIFIER COLON! )?
	{ #identifier_colon_opt =
		#(#[IDENTIFIER_COLON_OPT,
		   "IDENTIFIER_COLON_OPT"], #identifier_colon_opt); }
	;

except_choice_s : exception_choice ( PIPE^ exception_choice )*
	;

exception_choice : compound_name
	| OTHERS
	;

// 11.3
raise_statement : r:RAISE^ ( compound_name )? SEMI!
	{ Set (#r, RAISE_STATEMENT); }
	;

// 9.5.4
requeue_statement : r:REQUEUE^ name ( WITH! ABORT )? SEMI!
	{ Set (#r, REQUEUE_STATEMENT); }
	;

operator_call : cs:CHAR_STRING^ operator_call_tail[#cs]
	;

operator_call_tail [RefAdaAST opstr]
	: LPAREN! { is_operator_symbol(opstr->getText()) }?
		  value_s RPAREN! { opstr->setType(OPERATOR_SYMBOL); }
	;

value_s : value ( COMMA! value )*
	{ #value_s = #(#[VALUES, "VALUES"], #value_s); }
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

expression : relation
		( a:AND^ ( THEN! { Set (#a, AND_THEN); } )? relation
		| o:OR^ ( ELSE! { Set (#o, OR_ELSE); } )? relation
		| XOR^ relation
		)*
	;

relation : simple_expression
		( IN^ range_or_mark
		| n:NOT^ IN! range_or_mark { Set (#n, NOT_IN); }
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

signed_term
	: p:PLUS^ term { Set(#p, UNARY_PLUS); }
	| m:MINUS^ term { Set(#m, UNARY_MINUS); }
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
	// | cs:CHAR_STRING^ ( operator_call_tail[#cs] )?
	| ( CHAR_STRING ~LPAREN ) => CHAR_STRING
	)
	;

allocator : n:NEW^ name
	{ Set(#n, ALLOCATOR); }
	;

subunit : sep:SEPARATE^ LPAREN! compound_name RPAREN!
	{ Set(#sep, SUBUNIT); }
		( subprogram_body
		| package_body
		| task_body
		| protected_body
		)
	;

// 6.3
// This rule is only used in `subunit', other usage contexts use different rules
// (e.g. proc_decl_or_renaming_or_inst_or_body, subprog_decl_or_body).
subprogram_body
	: overriding_opt
	( p:PROCEDURE^ defining_identifier[false, true] formal_part_opt IS! body_part SEMI!
		{ Set(#p, PROCEDURE_BODY); }
	| f:FUNCTION^ defining_designator[false, true] parameter_and_result_profile IS! body_part SEMI!
		{ Set(#f, FUNCTION_BODY); }
	)
	;

package_body : p:PACKAGE^ body_is pkg_body_part end_id_opt! SEMI!
	{ Set(#p, PACKAGE_BODY); }
	;

task_body : t:TASK^ body_is body_part SEMI!  // end_id_opt is done in body_part
	{ Set(#t, TASK_BODY); }
	;
 
protected_body : p:PROTECTED^ body_is prot_op_bodies_opt end_id_opt! SEMI!
	{ Set(#p, PROTECTED_BODY); }
	;

// TBD
// code_stmt : qualified SEMI!
//  	;

//----------------------------------------------------------------------------
// The Ada scanner
//----------------------------------------------------------------------------

class AdaLexer extends Lexer;

options {
  charVocabulary = '\3'..'\377';
  exportVocab = Ada;      // call the vocabulary "Ada"
  testLiterals = false;   // don't automatically test for literals
  k = 4;                  // number of characters of lookahead
  caseSensitive = false;
  caseSensitiveLiterals = false;
  defaultErrorHandler = true;
}

tokens {
  // part 1: keywords
  ABORT            = "abort"      ;
  ABS              = "abs"        ;
  ABSTRACT         = "abstract"   ;
  ACCEPT           = "accept"     ;
  ACCESS           = "access"     ;
  ALIASED          = "aliased"    ;
  ALL              = "all"        ;
  AND              = "and"        ;
  ARRAY            = "array"      ;
  AT               = "at"         ;
  BEGIN            = "begin"      ;
  BODY             = "body"       ;
  CASE             = "case"       ;
  CONSTANT         = "constant"   ;
  DECLARE          = "declare"    ;
  DELAY            = "delay"      ;
  DELTA            = "delta"      ;
  DIGITS           = "digits"     ;
  DO               = "do"         ;
  ELSE             = "else"       ;
  ELSIF            = "elsif"      ;
  END              = "end"        ;
  ENTRY            = "entry"      ;
  EXCEPTION        = "exception"  ;
  EXIT             = "exit"       ;
  FOR              = "for"        ;
  FUNCTION         = "function"   ;
  GENERIC          = "generic"    ;
  GOTO             = "goto"       ;
  IF               = "if"         ;
  IN               = "in"         ;
  INTERFACE        = "interface"  ;
  IS               = "is"         ;
  LIMITED          = "limited"    ;
  LOOP             = "loop"       ;
  MOD              = "mod"        ;
  NEW              = "new"        ;
  NOT              = "not"        ;
  NuLL             = "null"       ;
  OF               = "of"         ;
  OR               = "or"         ;
  OTHERS           = "others"     ;
  OUT              = "out"        ;
  OVERRIDING       = "overriding" ;
  PACKAGE          = "package"    ;
  PRAGMA           = "pragma"     ;
  PRIVATE          = "private"    ;
  PROCEDURE        = "procedure"  ;
  PROTECTED        = "protected"  ;
  RAISE            = "raise"      ;
  RANGE            = "range"      ;
  RECORD           = "record"     ;
  REM              = "rem"        ;
  RENAMES          = "renames"    ;
  REQUEUE          = "requeue"    ;
  RETURN           = "return"     ;
  REVERSE          = "reverse"    ;
  SELECT           = "select"     ;
  SEPARATE         = "separate"   ;
  SUBTYPE          = "subtype"    ;
  SYNCHRONIZED     = "synchronized";
  TAGGED           = "tagged"     ;
  TASK             = "task"       ;
  TERMINATE        = "terminate"  ;
  THEN             = "then"       ;
  TYPE             = "type"       ;
  UNTIL            = "until"      ;
  USE              = "use"        ;
  WHEN             = "when"       ;
  WHILE            = "while"      ;
  WITH             = "with"       ;
  XOR              = "xor"        ;

  // part 2: RM tokens (synthetic)
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
  ENTRY_INDEX_SPECIFICATION;
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
  /* FUNCTION_CALL => INDEXEDCOMPONENT_OR_TYPECONVERSION_OR_FUNCTIONCALL */
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
  /* INDEXED_COMPONENT => INDEXEDCOMPONENT_OR_TYPECONVERSION_OR_FUNCTIONCALL */
  INDEX_CONSTRAINT;
  INTERFACE_TYPE_DEFINITION;
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
  /* STATEMENT_IDENTIFIER => STATEMENT_IDENTIFIER_OPT */
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
  /* TYPE_CONVERSION => INDEXEDCOMPONENT_OR_TYPECONVERSION_OR_FUNCTIONCALL */
  TYPE_DECLARATION;   /* not used, replaced by the corresponding
			 finer grained declarations  */
  USE_CLAUSE;
  USE_PACKAGE_CLAUSE;
  USE_TYPE_CLAUSE;
  VARIANT;
  VARIANT_PART;
  WITH_CLAUSE;

  // part 3: Non-RM synthetic tokens.
  // They exist mainly to normalize the node structure with respect to
  // optional items. (Without them, the presence or absence of an optional
  // item would change the node layout, but we want a fixed layout.)
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
  DISCRIMINANT_SPECIFICATIONS;
  DISCRIM_PART_OPT;
  ELSE_OPT;
  ELSIFS_OPT;
  ENTRY_INDEX_OPT;
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
  STATEMENT_IDENTIFIER_OPT;
  TASK_ITEMS_OPT;
  /* We cannot currently distinguish between
     INDEXED_COMPONENT, TYPE_CONVERSION, FUNCTION_CALL */
  INDEXEDCOMPONENT_OR_TYPECONVERSION_OR_FUNCTIONCALL;
  UNARY_MINUS;
  UNARY_PLUS;
  VALUE;
  VALUES;
  VARIANTS;
}

//----------------------------------------------------------------------------
// OPERATORS
//----------------------------------------------------------------------------
COMMENT_INTRO      :       "--"    ;
DOT_DOT            :       ".."    ;
LT_LT              :       "<<"    ;
BOX                :       "<>"    ;
GT_GT              :       ">>"    ;
ASSIGN             :       ":="    ;
RIGHT_SHAFT        :       "=>"    ;
NE                 :       "/="    ;
LE                 :       "<="    ;
GE                 :       ">="    ;
EXPON              :       "**"    ;
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

TIC    : { LA(3) != '\'' || (LA(2) == '(' && LA(5) == '\'') }?    "'" ;
	// condition needed to disambiguate from CHARACTER_LITERAL


// Literals.

// Rule for IDENTIFIER: testLiterals is set to true.  This means that
// after we match the rule, we look in the literals table to see if
// it's a keyword or really an identifier.
IDENTIFIER
	options {testLiterals=true;}
            : ( 'a'..'z' ) ( ('_')? ( 'a'..'z'|'0'..'9' ) )*
	;

CHARACTER_LITERAL : { LA(3) == '\'' && !(LA(2) == '(' && LA(5) == '\'') }?
	// condition needed to disambiguate from TIC
	"'" . "'"
	;

CHAR_STRING : '"' ("\"\"" | ~('"'))* '"'
	;

NUMERIC_LIT : ( DIGIT )+
		( '#' BASED_INTEGER ( '.' BASED_INTEGER )? '#'
		| ( '_' ( DIGIT )+ )+  // INTEGER
		)?
		( { LA(2)!='.' }?  //&& LA(3)!='.' }?
			( '.' ( DIGIT )+ ( '_' ( DIGIT )+ )* ( EXPONENT )?
			| EXPONENT
			)
		)?
	;

// a couple protected methods to assist in matching the various numbers

protected
DIGIT   :  ( '0'..'9' ) ;

protected
EXPONENT           :  ('e') ('+'|'-')? ( DIGIT )+ ;

protected
EXTENDED_DIGIT     :  ( DIGIT | 'a'..'f' ) ;

protected
BASED_INTEGER      :  ( EXTENDED_DIGIT ) ( ('_')? EXTENDED_DIGIT )* ;


// Whitespace -- ignored
WS_	:	(	' '
		|	'\t'
		|	'\f'
		// handle newlines
		|	(	"\r\n"  // Evil DOS
			|	'\r'    // Macintosh
			|	'\n'    // Unix (the right way)
			)
			{ newline(); }
		)
		{ $setType(antlr::Token::SKIP); }
	;

// Single-line comments
COMMENT :	( COMMENT_INTRO (~('\n'|'\r'))* ('\n'|'\r'('\n')?) )
		{ $setType(antlr::Token::SKIP); newline(); }
	;

// end of AdaLexer

