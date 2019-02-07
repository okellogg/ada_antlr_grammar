/* AdaUtil.cpp */

#include "AdaUtil.hpp"
#include "AdaTokenTypes.hpp"
#include <deque>
#include <sstream>
#include <iostream>
#include <cstring>

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

   bool adaEquiv(std::string s1, std::string s2)
   {
      return strcasecmp(s1.c_str(), s2.c_str()) == 0;
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
            if (adaEquiv(idNode->getText(), nm)) {
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

   // Auxiliary to lookup()
   bool lookupInDeclarativePart(std::string nm, RefAdaAST node)
   {
      bool found = false;
      for (RefAdaAST wrk = node;
           antlr::RefAST(wrk) != antlr::nullAST; wrk = wrk->nextSibling()) {
         RefAdaAST idNode = wrk->firstChild();
         switch (wrk->getType()) {
            // from subprog_decl_or_rename_or_inst_or_body: Skip overriding_opt
            case AdaTokenTypes::PROCEDURE_RENAMING_DECLARATION :    // TBC: in assignment ?
            case AdaTokenTypes::PROCEDURE_BODY :                    // TBC: in assignment ?
            case AdaTokenTypes::PROCEDURE_DECLARATION :             // TBC: in assignment ?
            case AdaTokenTypes::GENERIC_FUNCTION_INSTANTIATION :
            case AdaTokenTypes::FUNCTION_RENAMING_DECLARATION :
            case AdaTokenTypes::FUNCTION_BODY :
            case AdaTokenTypes::FUNCTION_DECLARATION :
               idNode = idNode->nextSibling();
               // do not break, fall through 
            case AdaTokenTypes::PACKAGE_SPECIFICATION :
            case AdaTokenTypes::PACKAGE_BODY :                     // TBC: in assignment ?
            case AdaTokenTypes::TASK_BODY :                        // TBC: in assignment ?
            case AdaTokenTypes::PROTECTED_TYPE_DECLARATION :       // TBC: in assignment ?
            case AdaTokenTypes::SINGLE_PROTECTED_DECLARATION :     // TBC: in assignment ?
            // from decl_common
            case AdaTokenTypes::INCOMPLETE_TYPE_DECLARATION :
            case AdaTokenTypes::SUBTYPE_DECLARATION :
            case AdaTokenTypes::EXCEPTION_RENAMING_DECLARATION :
            case AdaTokenTypes::OBJECT_RENAMING_DECLARATION :
               if (adaEquiv(idNode->getText(), nm)) {
                  std::cout << "lookupInDeclarativePart(" << nm << ") found" << std::endl;
                  // TODO: Save wrk in some sort of global state for setting the definition-use link.
                  //       Probably this will need to be more than a scalar buffer due to homographs.
                  //       Deciding which is the right one will require more earth shaking changes.
                  found = true;
                  break;
               } else {
                  std::cout << "lookupInDeclarativePart(" << nm << ") : skipping " << idNode->getText() << std::endl;
               }
               break;

            /* decl_common defining_identifier_list */
            case AdaTokenTypes::EXCEPTION_DECLARATION :
            case AdaTokenTypes::NUMBER_DECLARATION :
            case AdaTokenTypes::ARRAY_OBJECT_DECLARATION :
            case AdaTokenTypes::OBJECT_DECLARATION :
               idNode = idNode->firstChild();  // idNode now points to defining_identifier_list
               for (idNode = idNode->firstChild();
                    antlr::RefAST(idNode) != antlr::nullAST; idNode = idNode->nextSibling()) {
                  if (adaEquiv(idNode->getText(), nm)) {
                     std::cout << "lookupInDeclarativePart(" << nm << ") found" << std::endl;
                     // TODO: Save wrk in some sort of global state for setting the definition-use link.
                     found = true;
                     break;
                  } else {
                     std::cout << "lookupInDeclarativePart(" << nm << ") : skipping " << idNode->getText() << std::endl;
                  }
               }
               break;
            default:
               std::cout << "lookupInDeclarativePart(" << nm << ") : ignoring " << wrk->getText() << std::endl;
               break;
         }
      }
      return found;
   }

   bool lookup(RefAdaAST ident)
   {
      if (!doSem)
         return true;
      bool res = false;
      std::string nm = ident->getText();
      if (scopeStack.size() < 1) {
         std::cerr << "AdaUtil::lookup(" << nm << ") : scopeStack is empty" << std::endl;
         if (quitSemOnError) {
            doSem = false;
            res = true;
         }
         return res;
      }
      for (int i = scopeStack.size() - 1; i >= 0; i--) {
         RefAdaAST node = scopeStack[i]->decl;
         if (node == nullAdaAST)  // required due to declare_opt 2nd alt
            continue;
         switch (node->getType()) {
            case AdaTokenTypes::DECLARATIVE_PART :
               res = lookupInDeclarativePart(nm, node->firstChild());
               break;
            default:
               break;
         }
      }
      // TODO: interrogate compUnitMap
      return res;
   }

   bool requireAccess()
   {
      // TODO
      return true;
   }

}

