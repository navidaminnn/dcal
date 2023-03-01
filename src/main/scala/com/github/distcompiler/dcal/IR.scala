package com.github.distcompiler.dcal

/**
 * An low-level IR that resembles TLA+ but preserves identifiers and let expressions in structures for further
 * optimization.
 */
object IR {
  /*
  module ::= `MODULE` <name> <definition>*

  definition ::= <name> `(` (<name> (`,` <name>)* )? `)` `==` <node>*

  let ::= `LET` ( <name> `==` <node>+ )+ `IN` <node>+

  node ::=
  | <name>
  | <let>
  | <uninterpreted>
  */
  enum Node {
    case Name(name: String)
    case Let(name: String, binding: List[Node], body: List[Node])
    case Uninterpreted(text: String)
  }

  final case class Definition(name: String, params: List[String], body: List[List[Node]])

  final case class Module(name: String, definitions: List[Definition])

  // TODO: Ask what UNION is for
  // let z == y + 1
  // (LET s5 == ?) UNION { LET z == s.y + 1 IN ... : s \in s4 }

  // if x < 2 {
  //   ...
  // } else {
  //   ...
  // }
  // (LET s6 == ?) UNION { IF s.x < 2 THEN ... ELSE ... : s \in s5 }
}
