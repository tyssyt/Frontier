grammar Frontier;

//heaviliy inspired by https://github.com/antlr/grammars-v4/blob/master/java/Java.g4

@parser::header {
package tys.frontier.parser.antlr;
}

@lexer::header {
package tys.frontier.parser.antlr;

import java.util.*;
}

//insert a map that allows to dynamically look up what the keywords are
@lexer::members {
private Map<String, Integer> keywords;

public FrontierLexer (CharStream input, Map<String, Integer> keywords) {
    this(input);
    this.keywords = keywords;
}
}

//Keywords--------------------------------------------------------------------
//keep them up to date with the keywords.java
tokens {
    IMPORT,
    CLASS,
    CONSTRUCTORS,
    EXPORT,
    PRIVATE,
    STATIC,
    NATIVE,
    DELEGATE,
    NEW,
    THIS,
    NULL,
    BOOL,
    INT,
    CHAR,
    INT32,
    INT64,
    FLOAT32,
    FLOAT64,
    TYPE,
    IF,
    THEN,
    ELSE,
    WHILE,
    FOR,
    SWITCH,
    CASE,
    DEFAULT,
    FALLTHROUGH,
    CONTINUE,
    BREAK,
    RETURN,
    TRUE,
    FALSE
}

// starting point -------------------------------------------------------

file
    :   importStatement*
        classDeclaration*
        EOF
    ;

importStatement
    :   IMPORT TypeIdentifier SEMI
    ;

classDeclaration
    :   visibilityModifier? CLASS TypeIdentifier typeParameters? COLON
        classDeclaratives
        (methodDeclaration|nativeMethodDeclaration|fieldDeclaration)*
    ;

typeParameters
    :   LT TypeIdentifier (COMMA TypeIdentifier)* GT
    ;

classDeclaratives
    :   constructorsDeclarative?
    ;

constructorsDeclarative
    :   visibilityModifier? CONSTRUCTORS SEMI
    ;

modifier
    :   STATIC
    ;

visibilityModifier
    :   EXPORT
    |   PRIVATE
    ;

nativeMethodDeclaration
    :   methodHeader SEMI
    ;

methodDeclaration
    :   methodHeader block
    ;

methodHeader
    :   visibilityModifier? NATIVE? STATIC? LCIdentifier typeParameters? formalParameters (ARROW typeType)?
    ;

fieldDeclaration
    :   (DELEGATE nameSelector COLON)? visibilityModifier? modifier? typeType identifier (DECL expression)? SEMI
    ;

formalParameters
    :   LPAREN (formalParameter (COMMA formalParameter)*)? RPAREN
    ;

formalParameter
    : typeType identifier (DECL expression)?
    ;

nameSelector
    :   STAR
    |   STAR BACKSLASH (LCIdentifier (COMMA LCIdentifier)*)?
    |   LCIdentifier (COMMA LCIdentifier)*
    ;

//types ------------------------------------------------------------------------------------

typeType
    :   typeType Array
    |   typeType QUESTION
    |   typeType (COMMA typeType)* ARROW typeType
    |   predefinedType (LT typeList GT)?
    |   TypeIdentifier (LT typeList GT)?
    |   LPAREN typeType RPAREN
    |   LPAREN RPAREN
    ;

typeList
    :   typeType (COMMA typeType)*
    ;

predefinedType
    :   BOOL
    |   INT
    |   CHAR
    |   INT32
    |   INT64
    |   FLOAT32
    |   FLOAT64
    |   TYPE
    ;


// STATEMENTS / BLOCKS ---------------------------------------------------------------------
block
    :   LBRACE statement* RBRACE
    ;


statement
    :   block                                                                               #blockStatement
    |   localVariableDeclaration SEMI                                                       #localVariableDeclarationStatement
    |   ifStatement                                                                         #ifStatement_
    |   FOR  localVariableDeclaration? SEMI expression? SEMI expression2?  block            #forStatement
    |   FOR  LCIdentifier COLON expression  block                                           #foreachStatement
    |   WHILE  expression  block                                                            #whileStatement
    |   RETURN expression? SEMI                                                             #returnStatement
    |   BREAK SEMI                                                                          #breakStatement
    |   CONTINUE SEMI                                                                       #continueStatement
    |   SEMI                                                                                #emptyStatement
    |   expression SEMI                                                                     #expressionStatement
    |   <assoc=right> expression
        (   ASSIGN
        |   ADD_ASSIGN
        |   SUB_ASSIGN
        |   MUL_ASSIGN
        |   DIV_ASSIGN
        |   AND_ASSIGN
        |   OR_ASSIGN
        |   XOR_ASSIGN
        |   MOD_ASSIGN
        )
        expression SEMI                                                                     #assignment
    ;

localVariableDeclaration
    :  typeType? identifier (DECL expression)?
    ;

ifStatement
    :  IF expression block (ELSE block | ELSE ifStatement)?
    ;

// EXPRESSIONS -----------------------------------------------------------------------------

expressionList
    :   expression (COMMA expression)*
    ;

expression
    :   LPAREN expression RPAREN                                   #bracketsExpr
    |   expression EXMARK                                          #cast
    |   expression LBRACK expression RBRACK                        #arrayAccess
    |   expression DOT identifier                                  #fieldAccess
    |   expression DOT LCIdentifier LPAREN expressionList? RPAREN  #externalFunctionCall
    |   LCIdentifier LPAREN expressionList? RPAREN                 #internalFunctionCall
    |   typeType DOT LCIdentifier STAR (LPAREN typeList RPAREN)?   #functionAddress
    |   LCIdentifier STAR (LPAREN typeList RPAREN)?                #internalFunctionAddress
    |   NEW typeType LPAREN expressionList? RPAREN                 #newObject
    |   NEW typeType (LBRACK expression RBRACK)                    #newArray
    |   (EXMARK|SUB|INC|DEC) expression                            #preUnaryOp
    |   LPAREN typeType RPAREN expression                          #cast
    |   expression (STAR|DIV|MOD) expression                       #binaryOp
    |   expression (ADD|SUB) expression                            #binaryOp
    |   expression (LE|GE|LT|GT) expression                        #binaryOp
    |   expression (EQUAL|NOTEQUAL) expression                     #binaryOp
    |   expression (EQUAL_ID|NOTEQUAL_ID) expression               #binaryOp
    |   expression (EQUAL_CONTAINER|NOTEQUAL_CONTAINER) expression #binaryOp
    |   expression (AAND|AOR|XOR|AND|OR) expression                #binaryOp
    |   expression COLON expression                                #optionalElse
    |   THIS                                                       #thisExpr
    |   literal                                                    #literalExpr
    |   typeType                                                   #typeTypeExpr
    |   identifier                                                 #variableExpr
    ;

expression2
    : expression
    ;

//Literals------------------------------------------------------------------------------------

literal
    :   IntegerLiteral
    |   FloatingPointLiteral
    |   StringLiteral
    |   booleanLiteral
    |   NULL
    ;

booleanLiteral
    :   TRUE
    |   FALSE
    ;

//-------------------------------------------------------------------------------------------
//-------------------------------Lexer-------------------------------------------------------
//-------------------------------------------------------------------------------------------

QUESTION        : '?';
EXMARK          : '!';

//Lambdas-----------------------------------------------------------------
UNDERSCORE      : '_';
ARROW           : '->';

//Separators--------------------------------------------------------------
LPAREN          : '(';
RPAREN          : ')';
LBRACE          : '{';
RBRACE          : '}';
LBRACK          : '[';
RBRACK          : ']';
SEMI            : ';';
COMMA           : ',';
DOT             : '.';
COLON           : ':';
BACKSLASH       : '\\';


//Operators-----------------------------------------------------------------
ASSIGN          : '=';
DECL            : ':=';
GT              : '>';
LT              : '<';
EQUAL           : '==';
NOTEQUAL        : '=!=';
EQUAL_ID        : '=*=';
NOTEQUAL_ID     : '=!*=';
EQUAL_CONTAINER : '=[]=';
NOTEQUAL_CONTAINER : '=![]=';
LE              : '<=';
GE              : '>=';
AND             : '&&';
OR              : '||';
AAND            : '&';
AOR             : '|';
XOR             : '^';
INC             : '++';
DEC             : '--';
ADD             : '+';
SUB             : '-';
STAR            : '*';
DIV             : '/';
MOD             : '%';

ADD_ASSIGN      : '+=';
SUB_ASSIGN      : '-=';
MUL_ASSIGN      : '*=';
DIV_ASSIGN      : '/=';
AND_ASSIGN      : '&=';
OR_ASSIGN       : '|=';
XOR_ASSIGN      : '^=';
MOD_ASSIGN      : '%=';

Array           : LBRACK RBRACK;

//Datatypes---------------------------------------------------------------------

//Integer Literals--------------------------------------------------------------
IntegerLiteral
    :   DecimalNumeral
    |   HexNumeral
    |   OctalNumeral
    |   BinaryNumeral
    ;

fragment
DecimalNumeral
    :   '0'
    |   NonZeroDigit (Digits? | Underscores Digits)
    ;

fragment
Digits
    :   Digit (DigitOrUnderscore* Digit)?
    ;

fragment
Digit
    :   '0'
    |   NonZeroDigit
    ;

fragment
NonZeroDigit
    :   [1-9]
    ;

fragment
DigitOrUnderscore
    :   Digit
    |   '_'
    ;

fragment
Underscores
    :   '_'+
    ;

fragment
HexNumeral
    :   '0' [xX] HexDigits
    ;

fragment
HexDigits
    :   HexDigit (HexDigitOrUnderscore* HexDigit)?
    ;

fragment
HexDigit
    :   [0-9a-fA-F]
    ;

fragment
HexDigitOrUnderscore
    :   HexDigit
    |   '_'
    ;

fragment
OctalNumeral
    :   '0' Underscores? OctalDigits
    ;

fragment
OctalDigits
    :   OctalDigit (OctalDigitOrUnderscore* OctalDigit)?
    ;

fragment
OctalDigit
    :   [0-7]
    ;

fragment
OctalDigitOrUnderscore
    :   OctalDigit
    |   '_'
    ;

fragment
BinaryNumeral
    :   '0' [bB] BinaryDigits
    ;

fragment
BinaryDigits
    :   BinaryDigit (BinaryDigitOrUnderscore* BinaryDigit)?
    ;

fragment
BinaryDigit
    :   [01]
    ;

fragment
BinaryDigitOrUnderscore
    :   BinaryDigit
    |   '_'
    ;


//Floating-Point Literals--------------------------------------------------------------------
FloatingPointLiteral
    :   DecimalFloatingPointLiteral
    |   HexadecimalFloatingPointLiteral
    ;

fragment
DecimalFloatingPointLiteral
    :   Digits '.' Digits? ExponentPart? FloatTypeSuffix?
    |   '.' Digits ExponentPart? FloatTypeSuffix?
    |   Digits ExponentPart FloatTypeSuffix?
    |   Digits FloatTypeSuffix
    ;

fragment
ExponentPart
    :   ExponentIndicator SignedInteger
    ;

fragment
ExponentIndicator
    :   [eE]
    ;

fragment
SignedInteger
    :   Sign? Digits
    ;

fragment
Sign
    :   [+-]
    ;

fragment
FloatTypeSuffix
    :   [fFdD]
    ;

fragment
HexadecimalFloatingPointLiteral
    :   HexSignificand BinaryExponent FloatTypeSuffix?
    ;

fragment
HexSignificand
    :   HexNumeral '.'?
    |   '0' [xX] HexDigits? '.' HexDigits
    ;

fragment
BinaryExponent
    :   BinaryExponentIndicator SignedInteger
    ;

fragment
BinaryExponentIndicator
    :   [pP]
    ;

//String Literals------------------------------------------------------------------------
StringLiteral
    :   '"' StringCharacter* '"'
    ;

fragment
StringCharacter
    :   ~["\\]
    |   EscapeSequence
    ;


//Escape Sequences for Character and String Literals--------------------------------------
fragment
EscapeSequence
    :   '\\' [n'\\]
    ;

fragment
OctalEscape
    :   '\\' OctalDigit
    |   '\\' OctalDigit OctalDigit
    |   '\\' ZeroToThree OctalDigit OctalDigit
    ;

fragment
UnicodeEscape
    :   '\\' 'u' HexDigit HexDigit HexDigit HexDigit
    ;

fragment
ZeroToThree
    :   [0-3]
    ;


//Identifiers-------------------------------------------------------------------------------
identifier
    :   (LCIdentifier | TypeIdentifier)
    ;

LCIdentifier
    :   '_'* LowerCaseLetter LetterOrDigit*
    {
        String text = getText();
        Integer type = keywords.get(text);
        if (type != null) {
            setType(type);
        }
    }
    ;

TypeIdentifier //Class names
    :   '_'* UpperCaseLetter LetterOrDigit*
    {
        String text = getText();
        Integer type = keywords.get(text);
        if (type != null) {
            setType(type);
        }
    }
    ;

fragment LetterOrDigit   : [a-zA-Z0-9_];
fragment LowerCaseLetter : [a-z];
fragment UpperCaseLetter : [A-Z];


//    |   // covers all characters above 0x7F which are not a surrogate
//        ~[\u0000-\u007F\uD800-\uDBFF]
//    |   // covers UTF-16 surrogate pairs encodings for U+10000 to U+10FFFF
//        [\uD800-\uDBFF] [\uDC00-\uDFFF]
//    ;

// Whitespace and comments--------------------------------------------------
WS  :  [ \t\r\n\u000C]+ -> skip
    ;

COMMENT
    :   '/*' .*? '*/' -> channel(HIDDEN)
    ;

LINE_COMMENT
    :   '//' ~[\r\n]* -> channel(HIDDEN)
    ;