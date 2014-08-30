# Flub Assembler Design

There are quite a few pieces to this, more than I’d like, really.

## Parsing

Flub uses instaparse and an EBNF grammar. The source is read into a string, then normalized to remove trailing characters and fix new lines. It’s then fed into instaparse to produce an AST.

### Includes

After the AST has been read, includes need to be handled. The AST is traversed with `core.walk` to locate `:INCLUDE` nodes. There are two kinds of includes: Regular programs and pod data. These have different syntaxes, and therefor different parsers. Parser selection occurs based on the extension of the included file — a `.pod` file will use the pod parser, and anything else will use the source parser. Include nodes for pods are replaced with `:PODDEF` trees, while nodes for source are replaced with `:INCLUDED` trees.

This produces the complete AST.

### Symbols

After includes, global symbols need to be resolved; these apply to the entire source. The tree is walked again, this time looking for `:DECL_ASSIGN` and `:SYMBOL` nodes. Any assignment node is put into a symtable, and any matching `:SYMBOL` node has its value replaced with the value from the symtab. Symbols which do not match the global declarations are left unchanged. All `:DECLARATIONS` nodes are removed from the tree during this process.

## Assembling

At this point, the AST is ready to be assembled. The assembler is a recursive multimethod which dispatches on the 0th element of a vector.

The output is a vector of vectors of integers. The inner vector of integers is a single record to be output, and each inner integer is a byte value for that record. The bytes represent specific keys on the Fluke 9010a — there is a mapping in `flub.keys`.

### Programs

Programs can have forward references to other programs they with to execute. The first step of assembly is to scan for `:PROGRAM` trees and assign them an integer starting from zero. This information is pushed into the assembler’s state, then the rest of the tree is processed and concatenated together.

### Labels and program symbols

Programs may have labels and local definitions. These are handled similarly to global symbols; the program is scanned for labels.
