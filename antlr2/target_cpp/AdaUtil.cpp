/* AdaUtil.cpp */

#include "AdaUtil.hpp"

namespace AdaUtil {

   // Key:   fully qualified compilation unit name in Ada syntax, lowercased,
   //        stripped of possible prefix "standard."
   // Value: compilation_unit AST node
   NameToNodeMap_t compUnitMap;

   typedef std::vector<Scope_t*> ScopeStack_t;

   ScopeStack_t scopeStack;

   void pushScope(RefAdaAST nodeWithDeclarativeItems) {
      Scope_t *parentScope = 0;
      if (scopeStack.size())
         parentScope = scopeStack.back();
      Scope_t *newScope = new Scope_t;
      newScope->decl        = nodeWithDeclarativeItems;
      newScope->parentScope = parentScope;
      std::cout << "AdaUtil::pushScope: " << newScope->decl->toStringList() << std::endl;
      scopeStack.push_back(newScope);
   }

   void popScope() {
      if (scopeStack.size()) {
         Scope_t *top = scopeStack.back();
         scopeStack.pop_back();
         std::cout << "AdaUtil::popScope: " << top->decl->toStringList() << std::endl;
         delete top;
      } else {
         std::cerr << "AdaUtil::popScope: scopeStack is empty" << std::endl;
      }
   }

   void usePkg(RefAdaAST pkgName) {
      if (scopeStack.size()) {
         Scope_t *top = scopeStack.back();
         top->useVisible.push_back(pkgName);
      } else {
         std::cerr << "AdaUtil::usePkg: scopeStack is empty" << std::endl;
      }
   }

}

