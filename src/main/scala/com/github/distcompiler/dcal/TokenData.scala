package com.github.distcompiler.dcal

enum TokenData {
    case IntLiteral(value: BigInt)
    case StringLiteral(value: String)
    case Name(name: String)
    case Let
    case Var
    case Equals
    case Walrus
    case DoublePipe
    case BinOpPlaceholder
    case SlashIn
    case Await
    case Def
    case Import
    case Module
    case OpenCurlyBracket
    case CloseCurlyBracket
    case OpenParenthesis
    case ClosParenthesis
    case Comma
}