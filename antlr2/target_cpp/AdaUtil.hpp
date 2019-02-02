#ifndef _ADA_UTIL_HPP
#define _ADA_UTIL_HPP

#include <string>
#include <vector>
#include <map>
#include "AdaAST.hpp"

namespace AdaUtil {

   /**
    * Set the path of the GNAT runtime's adainclude directory.
    * The path of the adainclude directory can be found by running
    * `gnatls -v` and looking at the output in the section
    * "Source Search Path".
    * This switches the semantic analysis mode on.
    */
   void setGnatAdaincludePath(const char *absolutePath);

   /**
    * By default, Ada source files are expected to be in the current
    * working directory.
    * Use this function to add another path where Ada source files
    * may be located.
    * The function may be called multiple times to add more search
    * locations.
    * It is equivalent to the "-I" switch of gcc.
    * This switches the semantic analysis mode on.
    */
   void addDirectoryToSourceSearchPath(const char *dir);

   /**
    * Switch semantic analysis mode on or off.
    */
   void doSemanticAnalysis(bool arg = true);

   /**
    * Return the semantic analysis mode (true means "on", false
    * means "off").
    */
   bool sem();

   bool findCU(RefAdaAST compilation_unit);

   /**
    * Call this after calling findCU().
    * Each call to findChild() advances the library unit pointer
    * farther "inside" the package hierarchy.
    * Example: Given a full package name A.B.C,
    * findCU(A) looks for the library level unit A and if found,
    *           sets the library unit pointer to A;
    * findChild(B) expects the library unit pointer to point to A,
    *           looks for a first child B under A, and if it is
    *           found sets the library unit pointer to B;
    * findChild(C) expects the library unit pointer to point to A.B,
    *           looks for a child C under A.B, and if it is found
    *           sets the library unit pointer to C.
    * Thus the library unit pointer is initialized by calling findCU()
    * and is advanced by calling findChild().
    */
   bool findChild(RefAdaAST child_unit);

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

   bool lookup(RefAdaAST ident);

}

#endif
