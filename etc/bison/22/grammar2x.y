/*************** A Bison grammar for Ada 2022 **********************
 *                                                                 *
 * Copyright (C) 2022, O. Kellogg <okellogg@users.sourceforge.net> *
 *                                                                 *
 * based on                                                        *
 *                                                                 *
 ******* A YACC grammar for Ada 9X *********************************
 * Copyright (C) Intermetrics, Inc. 1994 Cambridge, MA  USA        *
 * Copying permitted if accompanied by this statement.             *
 * Derivative works are permitted if accompanied by this statement.*
 * This grammar is thought to be correct as of May 1, 1994         *
 * but as usual there is *no warranty* to that effect.             *
 *******************************************************************/

%token TIC
%token DOT_DOT
%token LT_LT
%token BOX
%token LT_EQ
%token EXPON
%token NE
%token GT_GT
%token GE
%token IS_ASSIGNED
%token RIGHT_SHAFT
%token ABORT
%token ABS
%token ABSTRACT
%token ACCEPT
%token ACCESS
%token ALIASED
%token ALL
%token AND
%token ARRAY
%token AT
%token BEGiN
%token BODY
%token CASE
%token CONSTANT
%token DECLARE
%token DELAY
%token DELTA
%token DIGITS
%token DO
%token ELSE
%token ELSIF
%token END
%token ENTRY
%token EXCEPTION
%token EXIT
%token FOR
%token FUNCTION
%token GENERIC
%token GOTO
%token IF
%token IN
%token INTERFACE
%token IS
%token LIMITED
%token LOOP
%token MOD
%token NEW
%token NOT
%token NuLL
%token OF
%token OR
%token OTHERS
%token OUT
%token OVERRIDING
%token PACKAGE
%token PARALLEL
%token PRAGMA
%token PRIVATE
%token PROCEDURE
%token PROTECTED
%token RAISE
%token RANGE
%token RECORD
%token REM
%token RENAMES
%token REQUEUE
%token RETURN
%token REVERSE
%token SELECT
%token SEPARATE
%token SOME
%token SUBTYPE
%token SYNCHRONIZED
%token TAGGED
%token TASK
%token TERMINATE
%token THEN
%token TYPE
%token UNTIL
%token USE
%token WHEN
%token WHILE
%token WITH
%token XOR
/* pseudo keywords */
%token UNSPECIFIED            /* global_aspect_definition */
%token SYNCHRONIZATION        /* 9.5 aspect_specification on declaration of a
                                 primitive procedure of a synchronized tagged
                                 type (followed by synchronization_kind) */
%token BY_ENTRY               /* synchronization_kind */
%token BY_PROTECTED_PROCEDURE /* synchronization_kind */
%token OPTIONAL               /* synchronization_kind */
%token CLASS                  /* aspect_mark */
/* end of pseudo keywords */
%token char_lit
%token identifier
%token string_lit
%token numeric_lit

%{
#define YYDEBUG 1

extern int soft_keywords;

/* functions defined in lexer2x.l */
extern void yyerror(const char *);
extern int  yylex();
%}

%glr-parser
%verbose

%%

// Non RM rule (bison: first rule is %start)
goal_symbol : compilation
	;

// 2.8
pragma  : PRAGMA identifier ';'
	| PRAGMA simple_name '(' pragma_arg_s ')' ';'
	;

pragma_arg_s : pragma_arg
	| pragma_arg_s ',' pragma_arg
	;

pragma_arg : expression
	| simple_name RIGHT_SHAFT expression
	;

pragma_s :
	| pragma_s pragma
	;

// 3.1  basic_declaration
basic_decl :
	  type_decl
	| subtype_decl
	| object_decl
	| number_decl
	| abstract_or_nonabstract_or_null_or_expr_subprog_decl
	| pkg_decl
	| rename_decl
	| exception_decl
	| generic_decl
	| generic_inst
	;

// 3.1  defining_identifier
def_id  : identifier
	;

init_opt :
	| IS_ASSIGNED expression
	;

// 3.2.1  type_declaration
type_decl : full_type_decl
	| incomplete_type_decl
	| private_type_decl
	| private_extension_decl
	;

// 3.2.1  full_type_declaration
full_type_decl :
	  TYPE def_id known_discrim_part_opt IS type_def aspect_spec_opt ';'
	| task_type_decl
	| protected_type_decl
	;

discrim_part_opt :
	| discrim_part
	;

// 3.2.1  type_definition
type_def : enumeration_type
	| integer_type
	| real_type
	| array_type
	| record_type
	| access_type
	| derived_type
	| interface_type
	;

// 3.2.2  subtype_declaration
subtype_decl : SUBTYPE def_id IS subtype_ind aspect_spec_opt ';'
	;

// 3.2.2  subtype_indication
subtype_ind : null_exclusion_opt subtype_mark constraint_opt
	;

tic_attr_id_opt :
	| tic_attr_id_opt TIC attribute_id
	;

// 3.2.2  subtype_mark
// Deviation from RM: `name` is unnecessarily loose.
// TODO sem pred checking compound_name is a subtype name
subtype_mark : compound_name tic_attr_id_opt
	;

constraint_opt :
	| constraint
	;

// 3.2.2
constraint : scalar_constraint
	| composite_constraint
	;

// 3.2.2
scalar_constraint : range_constraint
	| digits_constraint
	| delta_constraint
	;

// 3.2.2
composite_constraint : index_constraint
	| discriminant_constraint
	;

object_qualifier_opt :
	| ALIASED
	| CONSTANT
	| ALIASED CONSTANT
	;

// 3.3.1  object_declaration
object_decl :
	  def_id_s ':' object_qualifier_opt subtype_ind    init_opt aspect_spec_opt ';'
	| def_id_s ':' object_qualifier_opt access_def     init_opt aspect_spec_opt ';'
	| def_id_s ':' object_qualifier_opt array_type_def init_opt aspect_spec_opt ';'
	| single_task_decl
	| single_prot_decl
	;

// 3.3.1  defining_identifier_list
def_id_s : def_id
	| def_id_s ',' def_id
	;

// 3.3.2  number_declaration
number_decl : def_id_s ':' CONSTANT IS_ASSIGNED expression ';'
	;

// Auxiliary rule for derived_type_definition
derivation_common : abstract_opt limited_opt NEW subtype_ind
	;

// Auxiliary rule for derived_type_definition
rec_ext_part_opt :
	| and_interfacelist_opt record_extension_part
	;

// 3.4  derived_type_definition
//      TODO: Sem pred checking subtype_ind is parent subtype indication
derived_type : derivation_common rec_ext_part_opt
	;

// 3.5
range_constraint : RANGE range
	;

// 3.5
range : range_attribute_reference
	| simple_expression DOT_DOT simple_expression
	;

// 3.5.1  enumeration_type_definition
enumeration_type : '(' enum_id_s ')'

enum_id_s : enum_id
	| enum_id_s ',' enum_id
	;

// 3.5.1  enumeration_literal_specification
enum_id : identifier
	| char_lit
	;

range_spec : range_constraint
	;

range_spec_opt :
	| range_spec
	;

// 3.5.4 integer_type_definition
integer_type : range_spec
	| MOD expression
	;

// 3.5.6 real_type_definition
real_type : float_type
	| fixed_type
	;

// 3.5.7 floating_point_definition
float_type : DIGITS expression range_spec_opt
	;

// 3.5.9 fixed_point_definition
fixed_type : DELTA expression range_spec
	| DELTA expression DIGITS expression range_spec_opt
	;

// 3.5.9
// TODO: Sem pred checking simple_expression is static
digits_constraint : DIGITS simple_expression range_constr_opt
	;

// 3.6  array_type_definition
array_type_def : unconstrained_array_def
	| constrained_array_def
	;

// 3.6  unconstrained_array_definition
unconstrained_array_def : ARRAY '(' index_subtype_def_s ')' OF identifier // component_def
	;

index_subtype_def_s : index_subtype_def
	| index_subtype_def_s ',' index_subtype_def
	;

// 3.6  index_subtype_definition
index_subtype_def : subtype_mark RANGE BOX
	;

// 3.6  constrained_array_definition
constrained_array_def : ARRAY '(' discrete_subtype_def_s ')' OF identifier // component_def
	;

discrete_subtype_def_s : discrete_subtype_def
	| discrete_subtype_def_s ',' discrete_subtype_def
	;

// 3.6  discrete_subtype_definition
discrete_subtype_def : identifier // discrete_subtype_indication | range
	;

// 3.6  array_type_definition
array_type : unconstr_array_type
	| constr_array_type
	;

unconstr_array_type : ARRAY '(' index_s ')' OF component_subtype_def
	;

constr_array_type : ARRAY index_constraint OF component_subtype_def
	;

// 3.6
discrete_subtype_definition : subtype_ind | range
   ;

component_subtype_def : aliased_opt subtype_ind
	;

aliased_opt :
	| ALIASED
	;

index_s : index
	| index_s ',' index
	;

index : name RANGE BOX
	;

// 3.6.1
index_constraint : '(' iter_discrete_range_s ')'
	;

iter_discrete_range_s : discrete_range
	| iter_discrete_range_s ',' discrete_range
	;

// 3.6.1
discrete_range : name range_constr_opt
	| range
	;

range_constr_opt :
	| range_constraint
	;

// 3.7  discriminant_part
discrim_part : unknown_discrim_part | known_discrim_part
	;

// 3.7  unknown_discriminant_part
unknown_discrim_part : '(' BOX ')'
	;

discrim_spec_s : discrim_spec
	| discrim_spec_s ';' discrim_spec
	;

known_discrim_part_opt :
	| known_discrim_part

// 3.7  known_discriminant_part
known_discrim_part : '(' discrim_spec_s ')'
	;

// 3.7
// default_expression : expression

// 3.7.1
// (discriminant_association {, discriminant_association})
discriminant_constraint : identifier
	| discriminant_constraint ',' identifier
	;

// 3.7  discriminant_specification
discrim_spec :
	  def_id_s ':' null_exclusion_opt subtype_mark init_opt aspect_spec_opt
	| def_id_s ':' access_def init_opt aspect_spec_opt
	;

record_type : tagged_opt limited_opt record_def
	;

record_def : RECORD pragma_s comp_list END RECORD
	| NuLL RECORD
	;

abstract_opt :
	| ABSTRACT
	;

tagged_opt :
	| TAGGED
	| ABSTRACT TAGGED
	;

comp_list : comp_decl_s variant_part_opt
	| variant_part pragma_s
	| NuLL ';' pragma_s
	;

comp_decl_s : comp_decl
	| comp_decl_s pragma_s comp_decl
	;

variant_part_opt : pragma_s
	| pragma_s variant_part pragma_s
	;

comp_decl : def_id_s ':' component_subtype_def init_opt ';'
	| error ';'
	;

variant_part : CASE simple_name IS pragma_s variant_s END CASE ';'
	;

variant_s : variant
	| variant_s variant
	;

variant : WHEN choice_s RIGHT_SHAFT pragma_s comp_list
	;

choice_s : choice
	| choice_s '|' choice
	;

// 3.8.1
discrete_choice_list : discrete_choice
	| discrete_choice_list '|' discrete_choice
	;

// 3.8.1
discrete_choice : expression  // TODO change to choice_expression
	| subtype_ind
	| range
	| OTHERS
	;

choice : expression
	| discrete_with_range
	| OTHERS
	;

discrete_with_range : name range_constraint
	| range
	;

// 3.9.1
record_extension_part : WITH record_def
	;

is_abstract_or_null_opt :
	  /* empty */
	| IS ABSTRACT
	| IS NuLL
	;

is_abstract_or_aggregate_opt :
	  /* empty */
	| IS ABSTRACT
	| IS aggregate
	;

// 3.9.3  abstract_subprogram_declaration
/* This rule is only used in basic_declaration.
abstract_subprog_decl :
	overriding_ind_opt subprog_spec IS ABSTRACT aspect_spec_opt ';'
	;
 ****
 Combining this with 6.1 subprogram_declaration,
                     6.7 null_procedure_declaration,
                     6.8 expression_function_declaration:
           overriding_ind_opt function_specification IS
              ( LPAREN expression RPAREN   // creates ambiguity with `aggregate`
              | aggregate
              )
   Since `aggregate` subsumes `LPAREN expression RPAREN` we leave away the latter.
 */
abstract_or_nonabstract_or_null_or_expr_subprog_decl :
	  overriding_ind_opt procedure_spec is_abstract_or_null_opt aspect_spec_opt ';'
	| overriding_ind_opt function_spec is_abstract_or_aggregate_opt aspect_spec_opt ';'
	;

limited_task_protected_sync_opt :
	| LIMITED
	| TASK
	| PROTECTED
	| SYNCHRONIZED
	;

// 3.9.4  interface_type_definition
interface_type : limited_task_protected_sync_opt INTERFACE and_interfacelist_opt
	;

// TODO: Sem pred to ensure subtype_mark are interface_subtype_mark
and_interface_subtype_mark_s :
	| and_interface_subtype_mark_s AND subtype_mark
	;

// 3.9.4
// TODO: Sem pred to ensure subtype_mark is interface_subtype_mark
interface_list : subtype_mark and_interface_subtype_mark_s
	;

access_type : ACCESS subtype_ind
	| ACCESS CONSTANT subtype_ind
	| ACCESS ALL subtype_ind
	| ACCESS prot_opt PROCEDURE formal_part_opt
	| ACCESS prot_opt FUNCTION formal_part_opt RETURN subtype_mark
	;

const_opt :
	| CONSTANT
	;

null_exclusion_opt :
	| null_exclusion
	;

// 3.10
null_exclusion : NOT NuLL
	;

// 3.10  access_definition
access_def :
	  null_exclusion_opt ACCESS const_opt subtype_mark
	| null_exclusion_opt ACCESS prot_opt PROCEDURE parameter_profile
	| null_exclusion_opt ACCESS prot_opt FUNCTION parameter_and_result_profile
	;

prot_opt :
	| PROTECTED
	;

is_tagged_opt :
	| IS TAGGED
	;

// 3.10.1  incomplete_type_declaration
incomplete_type_decl : TYPE def_id discrim_part_opt is_tagged_opt ';'
	;

// 3.11  declarative_part
decl_part :
	| decl_item_s
	;

decl_item_s : decl_item
	| decl_item_s decl_item
	;

// 3.11 declarative_item
decl_item : basic_decl_item
	| body
	;

basic_decl_item_s : basic_decl_item
	| basic_decl_item_s basic_decl_item
	;

// 3.11 basic_declarative_item
basic_decl_item : basic_decl
	| aspect_clause
	| use_clause
	| pragma
	;

// 3.11
body : proper_body | body_stub
	;

// 3.11
proper_body : subprog_body
	| pkg_body
	| task_body
	| prot_body
	;

all_or_id_idxcomp_slice_typeconv_funcall_genrlidx :
	  ALL              // from explicit_dereference
	| selector_name idxcomp_slice_typeconv_funcall_genrlidx
	;

name_comma_opt : /* empty */
	| name ','
	;

lparen_name_comma_opt_expression_rparen_opt : /* empty */
	| '(' name_comma_opt expression ')'
	;

tic_suffix :
	  /* from attribute_reference/qualified_expression/reduction_specification */
	  identifier lparen_name_comma_opt_expression_rparen_opt
	  /* from attribute_designator */
	| ACCESS | DELTA | DIGITS | MOD
	;

dot_or_tic_suffix : '.' all_or_id_idxcomp_slice_typeconv_funcall_genrlidx
	| TIC tic_suffix
	;

dot_or_tic_suffix_s : /* empty */
	| dot_or_tic_suffix_s dot_or_tic_suffix
	;

// 4.1
name : operator_symbol       // from direct_name
	| identifier dot_or_tic_suffixes_or_idxcomp_slice_typeconv_funcall_genrlidx
	| char_lit
	| target_name
	| value_seq_reduction_attribute_reference
	;

dot_or_tic_suffixes_or_idxcomp_slice_typeconv_funcall_genrlidx :
	  dot_or_tic_suffix_s  // from direct_name, selected_component, et al.
	| idxcomp_slice_typeconv_funcall_genrlidx
	;

/* 4.1
direct_name : identifier
	| operator_symbol
	;
 */
// 4.1
/* Deviation from RM : Avoid `prefix`, it is indistinguishable from `name`.
prefix : name
	| implicit_dereference
	;
 */

/* 4.1  merged into `name`
explicit_dereference : name '.' ALL
	;
 */
// 4.1
/* Deviation from RM : Avoid `implicit_dereference`, it is indistinguishable from `name`.
implicit_dereference : name
	;
 */

/* indexed_comp is merged into `name`
expression_s : expression
	| expression_s ',' expression
	;

// 4.1.1  indexed_component
// Deviation from RM : Avoid `prefix`, it is indistinguishable from `name`.
indexed_comp : name '(' expression_s ')'
	;
 */

/* 4.1.2  merged into `name`
// Deviation from RM : Avoid `prefix`, it is indistinguishable from `name`.
slice : name '(' discrete_range ')'
	;
 */

param_association_s : parameter_association
	| param_association_s ',' parameter_association
	;

param_associations_or_discrete_range : param_association_s   // from actual_parameter_part [1]
	| discrete_range                                     // from slice
	;

param_associations_or_discrete_range_opt : /* empty */
	| '(' param_associations_or_discrete_range ')'
	;

// Auxiliary rule for `name`:
// indexed_component OR slice OR type_conversion OR function_call OR generalized_indexing
idxcomp_slice_typeconv_funcall_genrlidx :
	param_associations_or_discrete_range_opt
	;

// [1] typeconv etc are subsumed by the actual_parameter_part of function_call

/* 4.1.3  merged into `name`
// Deviation from RM : Avoid `prefix`, it is indistinguishable from `name`.
selected_comp : name '.' selector_name
	;
 */

// 4.1.3
selector_name : identifier
	| char_lit
	| operator_symbol
	;

simple_name : identifier
	;

// Strangely, the RM never defines this rule, which however is
// required for tightening up the syntax of certain names
// (library unit names etc.)
compound_name : simple_name
	| compound_name '.' simple_name
	;

c_name_list : compound_name
	 | c_name_list ',' compound_name
	;

// 4.1.4
// Deviation from RM : Avoid `prefix`, it is indistinguishable from `name`.
range_attribute_reference : name TIC range_attribute_designator
	;

// 4.1.4
// TODO: Sem pred checking `expression` is static
range_attribute_designator : RANGE
	| RANGE '(' expression ')'
	;

value_s : value
	| value_s ',' value
	;

value : expression
	| comp_assoc
	| discrete_with_range
	| error
	;

// 4.5.10 TODO (reduction_)identifier
reduction_attribute_designator : identifier '(' reduction_specification ')' ;

// 4.5.10 TODO (reducer_)name, (initial_value_)expression
reduction_specification : name ',' expression ;

attribute_id : identifier
	| DIGITS
	| DELTA
	| ACCESS
	;

// 4.3
/*
    record_aggregate | extension_aggregate | array_aggregate
  | delta_aggregate | container_aggregate
 */
aggregate : '(' comp_assoc ')'
	| '(' value_s_2 ')'
	| '(' expression WITH value_s ')'
	| '(' expression WITH NuLL RECORD ')'
	| '(' NuLL RECORD ')'
	;

value_s_2 : value ',' value
	| value_s_2 ',' value
	;

// 4.3.2
ancestor_part : subtype_mark // TODO: | expression
	;

// 4.3.3
iterated_component_association :
	  FOR def_id IN discrete_choice_list RIGHT_SHAFT expression
	| FOR iterator_specification RIGHT_SHAFT expression
	;

choices_opt : /* empty */
	| choice_s RIGHT_SHAFT 
	;

comp_assoc : choice_s RIGHT_SHAFT BOX
	| choices_opt expression
	;

comp_assoc_s : comp_assoc
	| comp_assoc_s ',' comp_assoc
	;

expression : relation
	| expression logical relation
	| expression short_circuit relation
	;

logical : AND
	| OR
	| XOR
	;

short_circuit : AND THEN
	| OR ELSE
	;

relation : simple_expression
	| simple_expression relational simple_expression
	| simple_expression membership range
	| simple_expression membership name
	;

relational : '='
	| NE
	| '<'
	| LT_EQ
	| '>'
	| GE
	;

membership : IN
	| NOT IN
	;

simple_expression : unary term
	| term
	| simple_expression adding term
	;

unary   : '+'
	| '-'
	;

adding  : '+'
	| '-'
	| '&'
	;

term    : factor
	| term multiplying factor
	;

multiplying : '*'
	| '/'
	| MOD
	| REM
	;

factor : primary
	| NOT primary
	| ABS primary
	| primary EXPON primary
	;

// 4.4
/*
    numeric_literal | null | string_literal | aggregate
  | name | allocator | (expression)
  | (conditional_expression) | (quantified_expression)
  | (declare_expression)
 */
primary : numeric_lit
	| NuLL
	| aggregate     // TODO Ambiguity (r/r conflict) - moved to before parenthesized_primary
	| name
	| string_lit    // TODO Ambiguity - moved to after `name` due to conflict w/ operator_symbol
	| allocator
	| parenthesized_primary
//	| '[' bracketed_primary ']'  // TODO
	;

parenthesized_primary : '(' parenth_primary_content ')'
	;

parenth_primary_content :
	 /* aggregate::record_aggregate record_component_association is subsumed by array_component_association
	  record_component_association ( ',' record_component_association )*
	         i.e. ( component_choice_list RIGHT_SHAFT )? expression
	              | component_choice_list RIGHT_SHAFT BOX
	                          // component_selector_name : IDENTIFIER | CHARACTER_LITERAL ;
	                i.e. component_selector_name ( PIPE component_selector_name )*
	                   | OTHERS
	  */

	NuLL RECORD   // aggregate::record_aggregate

	ancestor_part WITH comp_assoc          // aggregate::extension_aggregate

	/* head of aggregate::array_aggregate::positional_array_aggregate alt-1
	   is subsumed by aggregate::array_aggregate::named_array_aggregate alt-1
	 */
	// | expression_s     // aggregate::array_aggregate::positional_array_aggregate
	// subsumed by next line? (comp_assoc_s)

	| comp_assoc_s  // array_component_association_list     // aggregate::array_aggregate::named_array_aggregate alt-1
	   // adding tail from aggregate::array_aggregate::positional_array_aggregate alt-1
	             others_opt

	/* aggregate::delta_aggregate::record_delta_aggregate is subsumed by
	   aggregate::delta_aggregate::array_delta_aggregate alt-1
	| expression WITH DELTA record_component_association_list
	 */

	| expression WITH DELTA comp_assoc_s // array_component_association_list   // aggregate::delta_aggregate::array_delta_aggregate alt-1

	// aggregate::container_aggregate::{positional,named}_container_aggregate do not start with LPAREN
	// LPAREN expression RPAREN from top level is subsumed by aggregate::array_aggregate::named_array_aggregate

/*	| conditional_expression
	| quantified_expression
	| declare_expression      */
	;

expr_or_box : expression
	| BOX
	;

others_opt : /* empty */
	| ',' OTHERS RIGHT_SHAFT expr_or_box
	;

//bracketed_primary :
//	/* head of aggregate::array_aggregate::positional_array_aggregate alt-2
//	   is subsumed by aggregate::array_aggregate::named_array_aggregate alt-2
//	  expression ( ',' expression )*
//	 */
//
//	  array_component_association_list    // aggregate::array_aggregate::named_array_aggregate alt-2
//	   // adding tail from aggregate::array_aggregate::positional_array_aggregate alt-2
//	             ( ',' OTHERS RIGHT_SHAFT ( expression | BOX ) )?
//
//	| expression WITH DELTA array_component_association_list // aggregate::array_aggregate::array_delta_aggregate a2
//
//	/* aggregate::container_aggregate::positional_container_aggregate
//	   is subsumed by aggregate::array_aggregate::positional_array_aggregate alt-2
//	 */
//	| expression ( ',' expression )*
//
//	/* In aggregate::container_aggregate::named_container_aggregate alt-2,
//	   container_element_association_list alternatives 1 and 2 (involving key_choice_list)
//	   are subsumed by array_component_association_list from named_array_aggregate alt-2.
//	   This leaves named_container_aggregate alt-3 (involving iterated_element_association) :
//	 */
//	| iterated_element_association ( ',' iterated_element_association )*
//
//	| /* empty : aggregate::array_aggregate::null_array_aggregate
//	          OR aggregate::container_aggregate::null_container_aggregate */
//	;


qualified : name TIC parenthesized_primary
	;

allocator : NEW name
	| NEW qualified
	;

/* 4.5.10  reduction_attribute_reference
           Second alternative is dissolved into `name`
           First alt:
 */
value_seq_reduction_attribute_reference :
	value_sequence TIC reduction_attribute_designator
	;

chunk_spec_opt : /* empty */
	| '(' chunk_specification ')'
	;

parallel_opt : /* empty */
	| PARALLEL chunk_spec_opt
	;

// 4.5.10
value_sequence :
	'[' parallel_opt iterated_component_association ']'
	;

statement_s : statement
	| statement_s statement
	;

statement : unlabeled
	| label statement
	;

unlabeled : simple_stmt
	| compound_stmt
	| pragma
	;

simple_stmt : null_stmt
	| assign_stmt
	| exit_stmt
	| return_stmt
	| goto_stmt
	| procedure_call
	| delay_stmt
	| abort_stmt
	| raise_stmt
	| code_stmt
	| requeue_stmt
	| error ';'
	;

compound_stmt : if_stmt
	| case_stmt
	| loop_stmt
	| block
	| accept_stmt
	| select_stmt
	;

label : LT_LT identifier GT_GT
	;

null_stmt : NuLL ';'
	;

assign_stmt : name IS_ASSIGNED expression ';'
	;

// 5.2.1
target_name : AT
	;

if_stmt : IF cond_clause_s else_opt END IF ';'
	;

cond_clause_s : cond_clause
	| cond_clause_s ELSIF cond_clause
	;

cond_clause : cond_part statement_s
	;

cond_part : condition THEN
	;

condition : expression
	;

else_opt :
	| ELSE statement_s
	;

case_stmt : case_hdr pragma_s alternative_s END CASE ';'
	;

case_hdr : CASE expression IS
	;

alternative_s :
	| alternative_s alternative
	;

alternative : WHEN choice_s RIGHT_SHAFT statement_s
	;

loop_stmt : label_opt iteration basic_loop id_opt ';'
	;

label_opt :
	| identifier ':'
	;

iteration :
	| WHILE condition
	| iter_part reverse_opt discrete_range
	;

iter_part : FOR identifier IN
	;

// 5.5
iterator_filter : WHEN condition
	;

iterator_filter_opt : /* empty */
	| iterator_filter
	;

// 5.5
chunk_specification :
	  simple_expression
	| def_id IN discrete_subtype_definition
	;

loop_param_subtype_ind_opt : /* empty */
	| ':' loop_parameter_subtype_indication
	;

in_or_of : IN | OF
	;

// iterator_specification auxiliary rule: (iterator_)name
iterator_name :  name
   ;

// 5.5.2
iterator_specification :
	def_id loop_param_subtype_ind_opt
	     in_or_of reverse_opt iterator_name iterator_filter_opt
	;

// 5.5.2
loop_parameter_subtype_indication : subtype_ind | access_def
	;

// parameter_association_with_box auxiliary rule: (formal_parameter_)selector_name
// We don't use selector_name on the RHS because it includes operator_symbol
// which is not applicable in this context.
formal_parameter_selector_name : identifier
	| char_lit ;
	;

reverse_opt :
	| REVERSE
	;

basic_loop : LOOP statement_s END LOOP
	;

id_opt :
	| designator
	;

block : label_opt block_decl block_body END id_opt ';'
	;

block_decl :
	| DECLARE decl_part
	;

block_body : BEGiN handled_stmt_s
	;

handled_stmt_s : statement_s except_handler_part_opt
	;

except_handler_part_opt :
	| except_handler_part
	;

exit_stmt : EXIT name_opt when_opt ';'
	;

name_opt :
	| name
	;

when_opt :
	| WHEN condition
	;

return_stmt : RETURN ';'
	| RETURN expression ';'
	;

goto_stmt : GOTO name ';'
	;

// 6.1  subprogram_declaration
subprog_decl : overriding_ind_opt subprog_spec aspect_spec_opt ';';
	;

// 6.1  subprogram_specification
subprog_spec : procedure_spec | function_spec
	;

// 6.1  procedure_specification
procedure_spec : PROCEDURE def_prog_unit_name parameter_profile
	;

// 6.1
function_spec : FUNCTION defining_designator parameter_and_result_profile
	;

// 6.1
designator : parent_unit_prefix_opt identifier
	| operator_symbol
	;

// 6.1
defining_designator : def_prog_unit_name
	| defining_operator_symbol
	;

parent_unit_prefix_opt :
	| parent_unit_name '.'
	;

// 6.1  defining_program_unit_name
//      Ambiguous RHS:  parent_unit_prefix_opt def_id
//      Ambiguous because compound_name could consume the def_id
//      Just this one change removes _10_ S/R conflicts !
def_prog_unit_name : compound_name
	;

// 6.1
/* TODO bison sem pred checking string_lit to be valid as operator symbol
operator_symbol : string_lit
	;
//operator_string
//	: { is_operator_symbol(yychar) }?
 */

operator_symbol :  "and" | "or"  | "xor"                // logical_operator
	| "="   | "/="  | "<"   | "<=" | ">" | ">="  // relational_operator
	| "+"   | "-"   | "&"             // {binary,unary}_adding_operator
	| "*"   | "/"   | "mod" | "rem"             // multiplying_operator
	| "**"  | "abs" | "not"              // highest_precedence_operator
	;

// 6.1
defining_operator_symbol : operator_symbol
	;

// 6.1
parameter_profile : formal_part_opt
	;

// 6.1
parameter_and_result_profile :
	  formal_part_opt RETURN null_exclusion_opt subtype_mark
	| formal_part_opt RETURN access_def
	;

formal_part_opt :
	| formal_part
	;

formal_part : '(' param_s ')'
	;

param_s : param
	| param_s ';' param
	;

// 6.1  parameter_specification
param : def_id_s ':' aliased_opt mode null_exclusion_opt subtype_mark init_opt aspect_spec_opt
	| def_id_s ':' access_def init_opt aspect_spec_opt
	;

// 6.1
mode :
	| IN
	| OUT
	| IN OUT
	;

// 6.1.2  global_aspect_definition
global_aspect_def :
	NuLL
	| global_mode global_designator
	| '(' global_aspect_elem_s ')'
	| { soft_keywords = 1; } UNSPECIFIED { soft_keywords = 0; }
	;

global_aspect_elem_s : global_aspect_element
	| global_aspect_elem_s ';' global_aspect_element
	;

// 6.1.2
global_aspect_element :
	global_mode global_set
	| global_mode ALL
	| global_mode SYNCHRONIZED
	;

// 6.1.2
global_mode :
	basic_global_mode
	| extended_global_mode
	;

// 6.1.2
basic_global_mode : IN | IN OUT | OUT
	;

// 6.1.2
global_set : global_name
	| global_set ',' global_name
	;

// 6.1.2
global_designator : ALL | SYNCHRONIZED | global_name
	;

// TODO Sem pred checking that `name` is an object_name
object_name : name
	;

// TODO Sem pred checking that compound_name is a package name
package_name : compound_name
	;

// 6.1.2  ( object_name | package_name )
global_name : name
	;

subprog_spec_is_push : subprog_spec IS
	;

subprog_body : subprog_spec_is_push
	       decl_part block_body END id_opt ';'
	;

procedure_call : name ';'
	;

formal_param_sel_opt : /* empty */
	| formal_parameter_selector_name RIGHT_SHAFT
	;

// 6.4
// Auxiliary rule formal_parameter_selector_name is defined at 5.5.3
// (parameter_association_with_box).
parameter_association :
   formal_param_sel_opt expression  //explicit_actual_parameter
   ;


// 6.7  null_procedure_declaration
null_proc_decl :
	overriding_ind_opt procedure_spec IS NuLL aspect_spec_opt ';'
	;

// 6.8  expression_function_declaration
expr_func_decl :
	  overriding_ind_opt function_spec IS '(' expression ')' aspect_spec_opt ';'
	| overriding_ind_opt function_spec IS aggregate aspect_spec_opt ';'
	;

pkg_decl : pkg_spec ';'
	;

pkg_spec : PACKAGE def_prog_unit_name aspect_spec_opt IS
	     basic_decl_item_s private_part END c_id_opt
	;

private_part :
	| PRIVATE basic_decl_item_s
	;

c_id_opt :
	| identifier
	| parent_unit_name '.' identifier
	;

pkg_body : PACKAGE BODY compound_name IS
	       decl_part body_opt END c_id_opt ';'
	;

body_opt :
	| block_body
	;

// 7.3
private_type_decl :
	TYPE def_id discrim_part_opt IS tagged_opt limited_opt PRIVATE
		aspect_spec_opt ';'
	;

limited_or_synchronized_opt :
	| LIMITED
	| SYNCHRONIZED
	;

and_interfacelist_opt :
	| AND interface_list
	;

// 7.3  private_extension_declaration
//      TODO sem pred to enforce subtype_ind is ancestor_subtype_ind
private_extension_decl :
	TYPE def_id discrim_part_opt IS
	     abstract_opt limited_or_synchronized_opt NEW subtype_ind
	     and_interfacelist_opt WITH PRIVATE aspect_spec_opt ';'

private_type : tagged_opt limited_opt PRIVATE
	;

limited_opt :
	| LIMITED
	;

overriding_ind_opt :
	| overriding_indicator
	;

// 8.3.1
overriding_indicator : OVERRIDING
	| NOT OVERRIDING
	;

// 8.4
use_clause : use_package_clause | use_type_clause
	;

// TODO: Sem pred checking compound names are package names
package_name_s : compound_name
	| package_name_s compound_name
	;

// 8.4
use_package_clause : USE package_name_s ';'
	;

subtype_mark_s : subtype_mark
	| subtype_mark_s ',' subtype_mark
	;

// 8.4
use_type_clause : USE TYPE subtype_mark_s ';'
	| USE ALL TYPE subtype_mark_s ';'
	;

name_s : name
	| name_s ',' name
	;

// 8.5  renaming_declaration
rename_decl : object_rename_decl
	| exception_rename_decl
	| pkg_rename_decl
	| subprog_rename_decl
	| generic_rename_decl
	;

obj_subtype_spec_opt :
	| ':' null_exclusion subtype_mark
	;

// 8.5.1  object_renaming_declaration
// TODO: Sem pred checking `name` is an object name
object_rename_decl : def_id obj_subtype_spec_opt RENAMES name ';'
	;

// 8.5.2  exception_renaming_declaration
// TODO: Sem pred checking `renames` references exception name
exception_rename_decl : def_id ':' EXCEPTION renames ';'
	;

/* rename_unit : PACKAGE compound_name renames ';'
	| subprog_spec renames ';'
	| generic_formal_part PACKAGE compound_name renames ';'
	| generic_formal_part subprog_spec renames ';'
	;
 */

renames : RENAMES compound_name aspect_spec_opt
	;

// 8.5.3  package_renaming_declaration
// TODO: Sem pred checking compound_name is package name
pkg_rename_decl : PACKAGE def_prog_unit_name renames ';'
	;

// 8.5.4  subprogram_renaming_declaration
// TODO: Sem pred checking `name` is callable entity
subprog_rename_decl :
	overriding_ind_opt subprog_spec RENAMES name aspect_spec_opt ';'
	;

// 8.5.5  generic_renaming_declaration
// TODO: Sem pred checking compound_name is generic (package|procedure|function)
//       name, respectively.
generic_rename_decl :
	  GENERIC PACKAGE   def_prog_unit_name renames ';'
	| GENERIC PROCEDURE def_prog_unit_name renames ';'
	| GENERIC FUNCTION  def_prog_unit_name renames ';'
	;

new_interfacelist_with_opt :
	| NEW interface_list WITH
	;

// 9.1  task_type_declaration
task_type_decl : TASK TYPE def_id known_discrim_part_opt aspect_spec_opt ';'
	| TASK TYPE def_id discrim_part_opt aspect_spec_opt
	  IS new_interfacelist_with_opt task_definition ';'

// 9.1  single_task_declaration
single_task_decl : TASK def_id aspect_spec_opt ';'
	| TASK def_id aspect_spec_opt IS new_interfacelist_with_opt task_definition ';'
	;

task_definition :
	| task_item_s task_private_opt END id_opt
	;

task_item_s :
	| task_item_s task_item
	;

task_item : entry_decl
	| aspect_clause
	;

task_private_opt :
	| PRIVATE task_item_s
	;

task_body : TASK BODY def_id IS
	       decl_part block_body END id_opt ';'
	;

// 9.4  protected_type_declaration
protected_type_decl :
	PROTECTED TYPE def_id known_discrim_part_opt aspect_spec_opt IS
		       new_interfacelist_with_opt prot_def ';'
	;

// 9.4  single_protected_declaration
single_prot_decl :
	PROTECTED def_id aspect_spec_opt IS
		  new_interfacelist_with_opt prot_def ';'
	;

// 9.4  protected_definition
prot_def : prot_op_decl_s prot_private_opt END id_opt
	;

prot_private_opt :
	| PRIVATE prot_elem_decl_s

prot_op_decl_s :
	| prot_op_decl_s prot_op_decl
	;

prot_op_decl : subprog_decl
	| entry_decl
	| aspect_clause
	| pragma
	;

prot_elem_decl_s :
	| prot_elem_decl_s prot_elem_decl
	;

prot_elem_decl : prot_op_decl | comp_decl
	;

prot_body : PROTECTED BODY def_id aspect_spec_opt IS prot_op_item_s END id_opt ';'
	;

prot_op_item_s : pragma_s
	| prot_op_item_s prot_op_item pragma_s
	;

// 9.4  protected_operation_item
prot_op_item : subprog_decl
	| subprog_body
	| null_proc_decl
	| expr_func_decl
	| entry_body
	| aspect_clause
	;

// 9.5
/* Deactivated because not referenced within this grammar
synchronization_kind : BY_ENTRY | BY_PROTECTED_PROCEDURE | OPTIONAL
	;
 */

entry_decl : ENTRY identifier formal_part_opt ';' pragma_s
	| ENTRY identifier '(' discrete_range ')' formal_part_opt ';' pragma_s
	;

entry_body : ENTRY identifier formal_part_opt WHEN condition entry_body_part
	| ENTRY identifier '(' iter_part discrete_range ')'
		formal_part_opt WHEN condition entry_body_part
	;

entry_body_part : ';'
	| IS decl_part block_body END id_opt ';'
	;

entry_call : procedure_call
	;

accept_stmt : accept_hdr ';'
	| accept_hdr DO handled_stmt_s END id_opt ';'
	;

accept_hdr : ACCEPT entry_name formal_part_opt
	;

entry_name : simple_name
	| entry_name '(' expression ')'
	;

delay_stmt : DELAY expression ';'
	| DELAY UNTIL expression ';'
	;

select_stmt : select_wait
	| async_select
	| timed_entry_call
	| cond_entry_call
	;

select_wait : SELECT guarded_select_alt or_select else_opt
	      END SELECT ';'
	;

guarded_select_alt : select_alt
	| WHEN condition RIGHT_SHAFT select_alt
	;

or_select :
	| or_select OR guarded_select_alt
	;

select_alt : accept_stmt stmts_opt
	| delay_stmt stmts_opt
	| TERMINATE ';'
	;

delay_or_entry_alt : delay_stmt stmts_opt
	| entry_call stmts_opt

async_select : SELECT delay_or_entry_alt
	       THEN ABORT statement_s
	       END SELECT ';'
	;

timed_entry_call : SELECT entry_call stmts_opt
		   OR delay_stmt stmts_opt
	           END SELECT ';'
	;

cond_entry_call : SELECT entry_call stmts_opt
		  ELSE statement_s
	          END SELECT ';'
	;

stmts_opt :
	| statement_s
	;

abort_stmt : ABORT name_s ';'
	;

// 10.1.1
compilation :
	| compilation comp_unit
	;

// 10.1.1  compilation_unit
comp_unit : context_clause lib_item // pragma_s
	| context_clause subunit
	;

// 10.1.1  library_item
lib_item : private_opt lib_unit_decl
	| lib_unit_body
	| private_opt lib_unit_rename_decl
	;

// 10.1.1  library_unit_declaration
lib_unit_decl : subprog_decl
	| pkg_decl
	| generic_decl
	| generic_inst
	;

// 10.1.1  library_unit_renaming_declaration
lib_unit_rename_decl : pkg_rename_decl
	| generic_rename_decl
	| subprog_rename_decl
	;

// 10.1.1  library_unit_body
lib_unit_body : subprog_body
	| pkg_body
	;

private_opt :
	| PRIVATE
	;

// Non RM rule for accomodating pragmata
context_item_or_pragma : context_item | pragma
	;

// 10.1.2  (adjusted for pragmata)
context_clause :
	| context_item_or_pragma_s
	;

context_item_or_pragma_s : context_item_or_pragma
	| context_item_or_pragma_s context_item_or_pragma
	;

context_item : with_clause | use_clause
	;

with_clause : limited_with_clause | nonlimited_with_clause
	;

// 10.1.2
// TODO: Sem pred checking that names of c_name_list are library units
limited_with_clause : LIMITED private_opt WITH c_name_list ';'
	;

// 10.1.2
// TODO: Sem pred checking that names of c_name_list are library units
nonlimited_with_clause : private_opt WITH c_name_list ';'
	;

// 10.1.3
// TODO: Sem pred checking that compound_name is eligible parent unit
subunit : SEPARATE '(' compound_name ')' proper_body
	;

body_stub : TASK BODY simple_name IS SEPARATE ';'
	| PACKAGE BODY compound_name IS SEPARATE ';'
	| subprog_spec IS SEPARATE ';'
	| PROTECTED BODY simple_name IS SEPARATE ';'
	;

// 10.1.1
// TODO: Sem pred to check that `name` designates valid parent(s)
parent_unit_name : compound_name
	;

exception_decl : def_id_s ':' EXCEPTION ';'
	;

except_handler_part : EXCEPTION exception_handler
	| except_handler_part exception_handler
	;

exception_handler : WHEN except_choice_s RIGHT_SHAFT statement_s
	| WHEN identifier ':' except_choice_s RIGHT_SHAFT statement_s
	;

except_choice_s : except_choice
	| except_choice_s '|' except_choice
	;

except_choice : name
	| OTHERS
	;

raise_stmt : RAISE name_opt ';'
	;

requeue_stmt : REQUEUE name ';'
	| REQUEUE name WITH ABORT ';'
	;

generic_decl : generic_formal_part subprog_spec ';'
	| generic_formal_part pkg_spec ';'
	;

generic_formal_part : GENERIC
	| generic_formal_part generic_formal
	;

generic_formal : param ';'
	| TYPE simple_name generic_discrim_part_opt IS generic_type_def ';'
	| WITH PROCEDURE simple_name
	    formal_part_opt subp_default ';'
	| WITH FUNCTION designator
	    formal_part_opt RETURN name subp_default ';'
	| WITH PACKAGE simple_name IS NEW name '(' BOX ')' ';'
	| WITH PACKAGE simple_name IS NEW name ';'
	| use_clause
	;

generic_discrim_part_opt :
	| discrim_part
	| '(' BOX ')'
	;

subp_default :
	| IS name
	| IS BOX
	;

generic_type_def : '(' BOX ')'
	| RANGE BOX
	| MOD BOX
	| DELTA BOX
	| DELTA BOX DIGITS BOX
	| DIGITS BOX
	| array_type
	| access_type
	| private_type
	| generic_derived_type
	;

generic_derived_type : NEW subtype_ind
	| NEW subtype_ind WITH PRIVATE
	| ABSTRACT NEW subtype_ind WITH PRIVATE
	;

// Auxiliary rule for generic_instantiation
subprog_inst :
         PROCEDURE def_prog_unit_name IS
	         NEW compound_name generic_actual_part_opt aspect_spec_opt ';'
	| FUNCTION defining_designator IS
	         NEW compound_name generic_actual_part_opt aspect_spec_opt ';'
	;

// 12.3  generic_instantiation
// TODO: Sem pred checking that compound name is generic package/procedure/
//       function name respectively.
generic_inst : PACKAGE def_prog_unit_name IS
	         NEW compound_name generic_actual_part_opt aspect_spec_opt ';'
	| overriding_ind_opt subprog_inst
	;

generic_actual_part_opt :
	| '(' generic_association_s ')'
	;

generic_association_s : generic_association
	| generic_association_s ',' generic_association
	;

// 12.3
// TODO: Sem pred checking that selector_name is a generic formal parameter
//       selector.
generic_association : selector_name RIGHT_SHAFT explicit_generic_actual_parameter
	| explicit_generic_actual_parameter
	;

// 12.3  explicit_generic_actual_parameter
// The original rhs is:
//    expression | variable_name | subprogram_name | entry_name
//    | subtype_mark | package_instance_name
// but that creates ambiguities.
// Reducing the different name flavors to just `name` we have:
//    expression | name | subtype_mark
// where
// * `expression` resolves down to `primary` and from there may also
//   resolve to `name`;
// * `subtype_mark` is also `name`.
// Therefore, to avoid the ambiguities we just use a single `name`.
// Semantic checks are required for ensuring that `name` is a variable,
// subprogram, entry, package_instance, or subtype name.
//
explicit_generic_actual_parameter : name
	;

rep_spec : attrib_def
	| record_type_spec
	| address_spec
	;

// 13.1
/* aspect_clause : attribute_definition_clause
      | enumeration_representation_clause
      | record_representation_clause
      | at_clause */
aspect_clause : rep_spec
	;

aspect_def_opt :
	| RIGHT_SHAFT aspect_def

aspect_mark_s : aspect_mark aspect_def_opt
	| aspect_mark_s ',' aspect_mark aspect_def_opt
	;

// 13.1.1
aspect_spec : WITH aspect_mark_s
	;

aspect_spec_opt :
	| aspect_spec
	;

// 13.1.1
// TODO Sem pred checking identifier is aspect_identifier
aspect_mark : identifier
	| identifier TIC { soft_keywords = 1; } CLASS { soft_keywords = 0; }
	;

// 13.1.1  aspect_definition
aspect_def : name
	| expression
	| identifier
	| aggregate
	| global_aspect_def
	;

attrib_def : FOR subtype_mark USE expression ';'
	;

record_type_spec : FOR subtype_mark USE RECORD align_opt comp_loc_s END RECORD ';'
	;

align_opt :
	| AT MOD expression ';'
	;

comp_loc_s :
	| comp_loc_s subtype_mark AT expression RANGE range ';'
	;

address_spec : FOR subtype_mark USE AT expression ';'
	;

code_stmt : qualified ';'
	;

// H.7
extended_global_mode : OVERRIDING basic_global_mode
	;

// J.3
// TODO: Sem pred checking simple_expression is static
delta_constraint : DELTA simple_expression range_constr_opt
	;

%%
