# Language Name
## Language Features:
- functional
- ADT
- Pattern Matching
- Effects (hard)
## Syntax
To do
typing
functionName :: TypeExpr -> TypeExpr
functionName arg1 arg2 ... = expr
expr <- expr BINOP expr
    |   UNOP expr
    |   let id = expr in expr
    |   if expr then expr else expr
    |   expr; expr
    |   expr := expr
---
## Compiler
### Parser
Arrow parser
### Semantic Analysis
type inference???
look into T&PL
### Translation to IR
Maybe to LLVM or Keiko
### Optimizations
Should use [this](http://compileroptimizations.com/)
#### Superoptimization
> Using A*, todo find way of checking equivalence
#### Garbage optimizations
> recomended [paper](https://courses.cs.washington.edu/courses/cse590p/05au/p50-bacon.pdf)

### Architecture Ideas
- Arrows as stages
- Should implement interpreter as form of description

## Books
- Appel, Andrew W. Modern Compiler Implementation in ML. Cambridge ; New York, NY, USA: Cambridge UP, 1998. Cambridge Core. Web.
- Pierce, Benjamin C. Types and Programming Languages. Cambridge, Mass.: MIT, 2002. Ebook Central. Web.
## Useful resources
+ [Homemade compiler reflections](https://borretti.me/article/lessons-writing-compiler)
+ [Guide to LLVM](https://mukulrathi.com/create-your-own-programming-language/llvm-ir-cpp-api-tutorial/)
## OFFTOP
- Wetherall David, Tanenbaum Andrew. Computer Networks, Fifth Edition. 5th ed. 2010. Web.
- Encrypter script
> encrypt/decrypt files using password (store it hashed, together with configs) 
