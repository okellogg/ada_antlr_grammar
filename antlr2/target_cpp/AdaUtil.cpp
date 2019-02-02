/* AdaUtil.cpp */

#include "AdaUtil.hpp"
#include <iostream>

namespace AdaUtil {

   std::string toLower(std::string s)
   {
      std::basic_string<char> res = s;
      for (std::basic_string<char>::iterator p = res.begin(); p != res.end(); ++p) {
         *p = tolower(*p);
      }
      return res;
   }

   void addDirectoryToSourceSearchPath(const char *dir)
   {
   }

   bool doSem = false;

   void doSemanticAnalysis(bool arg)
   {
      doSem = arg;
   }

   bool sem()
   {
      return doSem;
   }

   const RefAdaAST nullAST;

   // The library unit pointer
   RefAdaAST currentLibUnit;

   bool findCU(RefAdaAST compilation_unit)
   {
      currentLibUnit = nullAST;
      std::string nm = toLower(compilation_unit->getText());
      std::cout << "findCU : " << nm << std::endl;
      return true;
   }

   bool findChild(RefAdaAST child_unit)
   {
      return true;
   }

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

   bool lookup(RefAdaAST ident) {
      if (!doSem)
         return true;
      // TODO
      return true;
   }

}

