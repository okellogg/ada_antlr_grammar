/* AdaUtil.cpp */

#include "AdaUtil.hpp"
#include "AdaTokenTypes.hpp"
#include <deque>
#include <sstream>
#include <iostream>

// Declared `extern' in AdaAST.hpp
RefAdaAST nullAdaAST;

namespace AdaUtil {

   std::string toLower(std::string s)
   {
      std::basic_string<char> res = s;
      for (std::basic_string<char>::iterator p = res.begin(); p != res.end(); ++p) {
         *p = tolower(*p);
      }
      return res;
   }

   void setGnatAdaIncludePath(const char *absolutePath)
   {
   }

   void addDirectoryToSourceSearchPath(const char *dir)
   {
   }

   bool doSem = false;
   bool quitSemOnError = false;

   void doSemanticAnalysis(bool switchOn, bool relinquishOnError)
   {
      doSem = switchOn;
      quitSemOnError = relinquishOnError;
   }

   bool sem()
   {
      return doSem;
   }

   // Key:   fully qualified compilation unit name in Ada syntax, lowercased,
   //        stripped of possible prefix "standard."
   // Value: compilation_unit AST node
   NameToNodeMap_t compUnitMap;

   // The library unit pointer
   RefAdaAST currentLibUnit;

   bool findUnit(RefAdaAST compilation_unit)
   {
      if (!doSem)
         return true;
      currentLibUnit = nullAdaAST;
      std::string nm = toLower(compilation_unit->getText());
      NameToNodeMap_t::iterator cup = compUnitMap.find(nm);
      bool res = (cup != compUnitMap.end());
      if (res) {
         currentLibUnit = cup->second;
      } else if (quitSemOnError) {
         std::cerr << "findUnit(" << nm << ") : unit not found" << std::endl;
         doSem = false;
         res = true;
      }
      return res;
   }

   bool findChild(RefAdaAST child_unit)
   {
      if (!doSem)
         return true;
      std::string nm = child_unit->getText();
      if (currentLibUnit == nullAdaAST) {
         std::cerr << "findChild(" << nm << ") : currentLibUnit is null" << std::endl;
         if (quitSemOnError) {
            doSem = false;
            return true;
         }
         return false;
      }
      for (RefAdaAST wrk = currentLibUnit->firstChild();
           antlr::RefAST(wrk) != antlr::nullAST; wrk = wrk->nextSibling()) {
         if (wrk->getType() == AdaTokenTypes::PACKAGE_SPECIFICATION) {
            RefAdaAST idNode = wrk->firstChild();
            if (idNode->getText() == nm) {
               std::cout << "findChild(" << nm << ") found" << std::endl;
               currentLibUnit = wrk;
               break;
            } else {
               std::cout << "findChild(" << nm << ") : skipping " << idNode->getText() << std::endl;
            }
         }
      }
      return true;
   }

   typedef std::deque<std::string> stringQ_t;

   void compoundNameFromAST(RefAdaAST cNameAST, stringQ_t& svec)
   {
      if (cNameAST->getType() == AdaTokenTypes::DOT) {
         compoundNameFromAST(cNameAST->firstChild(), svec);
         RefAdaAST tailId = cNameAST->nextSibling();
         if (tailId == nullAdaAST)
            std::cout << "compoundNameFromAST(" << cNameAST->getText() << ") : nextSibling is null" << std::endl;
         else
            svec.push_back(tailId->getText());
      } else {
         svec.push_back(cNameAST->getText());
      }
   }

   void getCompoundName(RefAdaAST idNode, std::string& compoundName)
   {
      stringQ_t svec;
      compoundNameFromAST(idNode, svec);
      std::stringstream ss;
      ss << svec.front();
      svec.pop_front();
      for (stringQ_t::const_iterator it = svec.begin(); it != svec.end(); ++it) {
         ss << "." << *it;
      }
      compoundName = ss.str();
   }

   bool defUnit(RefAdaAST unit, RefAdaAST name)
   {
      bool res = true;
      std::string nm;
      if (name->getType() == AdaTokenTypes::IDENTIFIER) {
         nm = name->getText();
      } else {
         getCompoundName(name, nm);
      }
      NameToNodeMap_t::iterator cup = compUnitMap.find(nm);
      if (cup == compUnitMap.end()) {
         compUnitMap[nm] = unit;
      } else {
         std::cerr << "defUnit(" << nm << ") : duplicate definition (unit is already in compUnitMap)" << std::endl;
         if (quitSemOnError)
            doSem = false;
         else
            res = false;
      }
      return res;
   }

   typedef std::vector<Scope_t*> ScopeStack_t;

   ScopeStack_t scopeStack;

   void pushScope(RefAdaAST nodeWithDeclarativeItems)
   {
      if (!doSem)
         return;
      Scope_t *parentScope = 0;
      if (scopeStack.size())
         parentScope = scopeStack.back();
      Scope_t *newScope = new Scope_t;
      newScope->decl        = nodeWithDeclarativeItems;
      newScope->parentScope = parentScope;
      std::cout << "AdaUtil::pushScope: " << newScope->decl->toStringList() << std::endl;
      scopeStack.push_back(newScope);
   }

   void popScope()
   {
      if (!doSem)
         return;
      if (scopeStack.size()) {
         Scope_t *top = scopeStack.back();
         scopeStack.pop_back();
         std::cout << "AdaUtil::popScope: " << top->decl->toStringList() << std::endl;
         delete top;
      } else {
         std::cerr << "AdaUtil::popScope: scopeStack is empty" << std::endl;
      }
   }

   void usePkg(RefAdaAST pkgName)
   {
      if (!doSem)
         return;
      if (scopeStack.size()) {
         Scope_t *top = scopeStack.back();
         top->useVisible.push_back(pkgName);
      } else {
         std::cerr << "AdaUtil::usePkg: scopeStack is empty" << std::endl;
      }
   }

   bool lookup(RefAdaAST ident)
   {
      if (!doSem)
         return true;
      // TODO
      return true;
   }

}

