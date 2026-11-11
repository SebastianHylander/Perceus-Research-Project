module Lex

open FSharp.Text.Lexing
open Parse/// Rule Token
val Token: lexbuf: LexBuffer<char> -> token
/// Rule SkipComment
val SkipComment: lexbuf: LexBuffer<char> -> token
