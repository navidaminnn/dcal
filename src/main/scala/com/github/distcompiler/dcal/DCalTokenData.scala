package com.github.distcompiler.dcal

enum DCalTokenData {
    case IntLiteral(value: BigInt)
    case StringLiteral(value: String)
    case True
    case False
    case Name(name: String)
    case Let
    case Var
    case If
    case Then
    case Else
    case EqualTo
    case Walrus
    case DoublePipe
    case Plus
    case Minus
    case NotEqualTo
    case LesserThan
    case GreaterThan
    case LesserThanOrEqualTo
    case GreaterThanOrEqualTo
    case And
    case Or
    case SlashIn
    case Await
    case Def
    case Import
    case Module
    case OpenCurlyBracket
    case CloseCurlyBracket
    case OpenParenthesis
    case CloseParenthesis
    case Comma
    case Semicolon
    case Dot
}