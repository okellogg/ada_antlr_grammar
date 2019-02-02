#include <string>
#include <fstream>
#include <iostream>
#include <cstdlib>
#include "AdaUtil.hpp"
#include "AdaLexer.hpp"
#include "AdaParser.hpp"

int main(int argc, char *argv[])
{
   if (argc < 2)
   {
      std::cerr << "Provide Ada input file name(s) as last command line argument" << std::endl;
      return 1;
   }

   AdaUtil::doSemanticAnalysis(true, true);

   bool printTree = false;
   bool firstFile = true;
   for (int i = 1; i < argc; i++)
   {
      if (argv[i][0] == '-')
      {
         bool good = (strlen(argv[i]) > 3);
         if (good)
         {
            if (strncmp(argv[i], "-debugParser", strlen(argv[i])) == 0)
               antlr::DEBUG_PARSER = true;
            else if (strncmp(argv[i], "-noSemanticAnalysis", strlen(argv[i])) == 0)
               AdaUtil::doSemanticAnalysis(false);
            else if (strncmp(argv[i], "-semanticAnalysis", strlen(argv[i])) == 0)
               AdaUtil::doSemanticAnalysis(true, false);
            else if (strncmp(argv[i], "-softSemanticAnalysis", strlen(argv[i])) == 0)
               AdaUtil::doSemanticAnalysis(true, true);
            else if (strncmp(argv[i], "-printTree", strlen(argv[i])) == 0)
               printTree = true;
            else
               good = false;
         }
         if (!good)
         {
            std::cerr << "Unknown option: " << argv[i] << std::endl;
            std::cout << "Available options:" << std::endl;
            std::cout << "-deb[ugParser]          Enable debug messages." << std::endl;
            std::cout << "-soft[SemanticAnalysis] Enable semantics, relinquish on first error." << std::endl;
            std::cout << "                        This is the default operating mode." << std::endl;
            std::cout << "-sem[anticAnalysis]     Enable semantic analysis with hard errors." << std::endl;
            std::cout << "-noSem[anticAnalysis]   Disable semantic analysis." << std::endl;
            std::cout << "-print[Tree]            Print LISP like representation of generated AST." << std::endl;
         }
      }
      else
      {
         std::string f(argv[i]);
         if (firstFile)
            firstFile = false;
         else
            std::cout << std::endl;
         std::cout << "Parsing " << f << std::endl;

         std::ifstream s(f.c_str());
         
         // Create a scanner that reads from the input stream
         AdaLexer lexer(s);
         lexer.setFilename(f);
      
         // Create a parser that reads from the scanner
         AdaParser parser(lexer);
         parser.setFilename(f);
      
         // make an ast factory
         antlr::ASTFactory ast_factory;
      
         // initialize and put it in the parser...
         parser.initializeASTFactory(ast_factory);
         parser.setASTFactory(&ast_factory);
      
         // start parsing at the compilation_unit rule
         try {
            parser.compilation_unit();
         } catch (const antlr::ANTLRException& ae) {
            std::cerr << ae.toString() << std::endl;
            return 1;
         } catch (...) {
            std::cerr << "Exception from parser.compilation_unit()" << std::endl;
            return 1;
         }
      
         antlr::RefAST t = parser.getAST();
         if (t)
         {
            if (!printTree)
            {
               const char *print_tree = getenv ("ANTLR_TREE");
               printTree = (print_tree && *print_tree && *print_tree != '0');
            }
            if (printTree)
            {
               // Print the resulting tree out in LISP notation
               std::cout << t->toStringList() << std::endl;
            }
            else
            {
               std::cout << "Tree was produced" << std::endl;
            }
         }
         else
         {
            std::cerr << "No tree produced" << std::endl;
            return 1;
         }
      }
   }

   return 0;
}

