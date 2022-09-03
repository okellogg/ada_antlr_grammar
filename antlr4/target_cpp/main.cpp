#include <iostream>

#include "antlr4-runtime.h"
#include "AdaLexer.h"
#include "AdaParser.h"

// using namespace adalr;
// using namespace antlr4;

int main(int argc, const char **argv) {
  if (argc < 2) {
    std::cerr << "Provide Ada input filename" << std::endl;
    exit (1);
  }
  antlr4::ANTLRFileStream input;
  input.loadFromFile(argv[1]);
  adalr::AdaLexer lexer(&input);
  antlr4::CommonTokenStream tokens(&lexer);

  tokens.fill();
  for (auto token : tokens.getTokens()) {
    std::cout << token->toString() << std::endl;
  }

  adalr::AdaParser parser(&tokens);
  antlr4::tree::ParseTree* tree = parser.compilation();

  std::cout << tree->toStringTree(&parser) << std::endl << std::endl;

  return 0;
}
