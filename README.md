# ada_antlr_grammar
ANTLR grammars for Ada

**Status**
* antlr2 contains a functioning Ada2005 grammar currently being updated to [Ada2012](http://www.ada-auth.org/standards/12rm/html/RM-P.html)
* antlr3 is an ongoing project to port the antlr2 grammar to [ANTLR](https://www.antlr3.org) version [3](https://github.com/antlr/antlr3)
* antlr4 is a direct translation of the [Ada202x draft grammar](http://www.ada-auth.org/standards/2xrm/html/RM-P.html) to [ANTLR](https://www.antlr.org)-[4](https://github.com/antlr/antlr4) in its early stages (not yet usable)
* Work is currently focusing on [branches/experimental](https://github.com/okellogg/ada_antlr_grammar/tree/branches/experimental) to extend the ANTLR2 grammar by enough semantic analysis to distinguish `indexed_component`, `type_conversion`, `function_call`.
