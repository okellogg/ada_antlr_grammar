#ifndef __ADA_AST_H__
#define __ADA_AST_H__

#include <antlr/CommonAST.hpp>

class AdaAST;

typedef antlr::ASTRefCount<AdaAST> RefAdaAST;

/** Custom AST class that adds line numbers to the AST nodes.
 * easily extended with columns. Filenames will take more work since
 * you'll need a custom token class as well (one that contains the
 * filename)
 */
class AdaAST : public antlr::CommonAST {
public:
   // copy constructor
   AdaAST( const AdaAST& other )
   : CommonAST(other)
   , line(other.line)
   {
   }
   // Default constructor
   AdaAST( void ) : CommonAST(), line(0) {}
   virtual ~AdaAST( void ) {}

   /*
    Function set() repurposes a node to different type and text.
    Usage pattern:
    Promote a syntatic sugar token to root, then repurpose it as a
    meaningful node.  This is an optimization circumventing a heap
    allocation of a new root node.

    Example without the optimization:

      lib_pkg_spec_or_body
        : PACKAGE!
                BODY! defining_identifier[true, true] IS! pkg_body_part end_id_opt! SEMI!
          { #lib_pkg_spec_or_body =
	       #(#[PACKAGE_BODY, "PACKAGE_BODY"], #lib_pkg_spec_or_body); }
        ;

    Actual rule including the optimization:

      lib_pkg_spec_or_body
        : pkg:PACKAGE^
                ( BODY! defining_identifier[true, true] IS! pkg_body_part end_id_opt! SEMI!
                        { #pkg->set(PACKAGE_BODY, "PACKAGE_BODY"); }

		// this part was elided above
                | defining_identifier[true, true] spec_decl_part[#pkg]
                )
        ;

    * In the plain version the token PACKAGE is discarded (PACKAGE!) and the
      result node is heap allocated as a root node.
    * In the optimized version the token PACKAGE is promoted to root (PACKAGE^)
      and anchored with a label (pkg). The token is then repurposed as PACKAGE_BODY.
    * In the part which was elided in the plain version, the anchor label is passed
      to a parameterized subrule (spec_decl_part) which uses the parameter to do
      similar repurposing.
    */
   void set(int tokenType, const char *tokenText)
   {
      setType(tokenType);
      setText(tokenText);
   }

   // get the line number of the node (or try to derive it from the child node
   virtual int getLine( void ) const
   {
      // most of the time the line number is not set if the node is a
      // imaginary one. Usually this means it has a child. Refer to the
      // child line number. Of course this could be extended a bit.
      // based on an example by Peter Morling.
      if ( line != 0 )
         return line;
      if( getFirstChild() )
         return ( RefAdaAST(getFirstChild())->getLine() );
      return 0;
   }

   virtual void setLine( int l )
   {
      line = l;
   }

   /** the initialize methods are called by the tree building constructs
    * depending on which version is called the line number is filled in.
    * e.g. a bit depending on how the node is constructed it will have the
    * line number filled in or not (imaginary nodes!).
    */
   virtual void initialize(int t, const ANTLR_USE_NAMESPACE(std)string& txt)
   {
      CommonAST::initialize(t,txt);
      line = 0;
   }

   virtual void initialize( antlr::RefToken t )
   {
      CommonAST::initialize(t);
      line = t->getLine();
   }

   virtual void initialize( RefAdaAST ast )
   {
      CommonAST::initialize(antlr::RefAST(ast));
      line = ast->getLine();
   }

   // Cannot name this getFirstChild due to covariant return type
   // of reimplementation
   RefAdaAST firstChild() const
   {
      return RefAdaAST(getFirstChild());
   }

   // Cannot name this getNextSibling due to covariant return type
   // of reimplementation
   RefAdaAST nextSibling() const
   {
      return RefAdaAST(getNextSibling());
   }

   // for convenience, will also work without
   void addChild( RefAdaAST c )
   {
      BaseAST::addChild( antlr::RefAST(c) );
   }

   // for convenience, will also work without
   void setNextSibling( RefAdaAST c )
   {
      BaseAST::setNextSibling( antlr::RefAST(c) );
   }

   // provide a clone of the node (no sibling/child pointers are copied)
   virtual antlr::RefAST clone( void )
   {
      return antlr::RefAST(new AdaAST(*this));
   }

   static antlr::RefAST factory( void )
   {
      return antlr::RefAST(RefAdaAST(new AdaAST()));
   }

private:
   int line;
};

// CAVEAT: nullAdaAST is only intended for use in the scope stack,
//         it is NOT to be used in or on the AdaAST.
//         For comparing regular AdaAST nodes against null, use the
//         ANTLR defined symbol `nullAST'.
//         Reason: ANTLR deploys nullAST in the generated trees and
//         tests for null nodes will only work with that symbol.
extern RefAdaAST nullAdaAST;

#endif
