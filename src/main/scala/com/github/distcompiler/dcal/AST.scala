package com.github.distcompiler.dcal

object AST {
  enum Definition {
    case TODO
  }

  /*
  module ::= `module` <name> <imports>? <definition>*

  imports ::= `import` <name> (`,` <name>)*

  definition ::=
    | `def` <name> `(` (<name> (`,` <name>)* )? `)` <block>

  block ::= `{` <statement>* `}

  statement ::=
    | `await` <expression>
    | <assign_pair> (`||` <assign_pair>)*
    | `let` <name> `=` <expression>
    | `var` <name> ((`=` | `\in`) <expression>)?

  // e.g `x := y || y := x` swaps x and y
  assign_pair ::= <name> `:=` <expression>

  expression_base ::=
    | <int_literal>
    | <string_literal>
    | <name>
  
  expression_binop ::= <expression_base> (<binop> <expression_base>)?
  
  expression ::= <expression_binop>
  */

  final case class DCalModule(name: String, imports: List[String], definitions: List[Definition])
}
