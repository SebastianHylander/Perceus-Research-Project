# Perceus-Research-Project

## Parsing and Lexing
Parsing and lexing is done with fslexyacc, defined in the Parsing/Lex.fsl and Parsing/Parse.fsy. They can be compiled to fs files with the following commands:

`fslex --unicode --module Lex Parsing/Lex.fsl`
`fsyacc --module Parse Parsing/Parse.fsy`