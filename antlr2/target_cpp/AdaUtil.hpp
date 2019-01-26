#ifndef _ADA_UTIL_HPP
#define _ADA_UTIL_HPP

#include <string>
#include <vector>
#include <map>
#include "AdaAST.hpp"

namespace AdaUtil {

   // General purpose type for mapping a name to its AST node
   typedef std::map<std::string, RefAdaAST> NameToNodeMap_t;

   // General purpose type for vector of nodes
   typedef std::vector<RefAdaAST> NodeVec_t;

   /**
    * An element of the scope stack.
    */
   struct Scope_t {
      /**
       * Pointer to any of the following node types:
       * PACKAGE_SPECIFICATION
       * PACKAGE_BODY
       * PROCEDURE_BODY
       * FUNCTION_BODY
       * TASK_BODY
       * ENTRY_BODY
       * DECLARATIVE_PART (of local blocks)
       */
      RefAdaAST decl;

      /**
       * Pointers to PACKAGE_SPECIFICATION nodes mentioned in "use" clauses.
       */
      NodeVec_t useVisible;

      /**
       * Pointer to the immediate enclosing scope.
       * This is for convenience; the enclosing scope could also be
       * computed as the previous index in the scope stack.
       * Accesses are simplified by having the back pointer directly
       * available.
       */
      Scope_t *parentScope;
   };

   void pushScope(RefAdaAST nodeWithDeclarativeItems);

   void popScope();

   void usePkg(RefAdaAST pkgName);

}

#endif
