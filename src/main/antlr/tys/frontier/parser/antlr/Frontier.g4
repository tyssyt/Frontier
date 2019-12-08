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
    INCLUDE,
    CLASS,
    CONSTRUCTORS,
    EXPORT,
    PRIVATE,
    STATIC,
    NATIVE,
    DELEGATE,
    IN,
    OUT,
    WHERE,
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
        includeStatement*
        classDeclaration*
        EOF
    ;

importStatement //TODO this is an ugly mess, I should just change modes (also can't handle paths that contain spaces etc)
    :   IMPORT identifier SEMI
    ;

includeStatement
    :   NATIVE? INCLUDE path SEMI
    ;

path
    :   folder? identifier DOT identifier   #filePath
    |   folder STAR STAR? (DOT identifier)? #folderPath
    ;

folder
    :   (DOT DOT SLASH)+ (identifier SLASH)*
    |   (DOT DOT SLASH)* (identifier SLASH)+
    ;

classDeclaration
    :   visibilityModifier? CLASS TypeIdentifier typeParameters? COLON
        typeParameterSpecification*
        classDeclaratives
        (methodDeclaration|nativeMethodDeclaration|fieldDeclaration)*
    ;

typeParameters
    :   LT typeParamer (COMMA typeParamer)* GT
    ;

typeParamer
    :   (IN|OUT)? TypeIdentifier STAR?  //TODO it would be nice to not have in & out as keyword anywhere, as they can only appear within type parameters
    ;

typeParameterSpecification
    :   WHERE upperBound? TypeIdentifier lowerBound?
    ;

upperBound
    : typeList GT
    ;

lowerBound
    : GT typeList
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

methodHeader //Tuple-ize
    :   visibilityModifier? NATIVE? STATIC? LCIdentifier typeParameters? formalParameters (ARROW typeList)? typeParameterSpecification*
    ;

fieldDeclaration
    :   (DELEGATE nameSelector COLON)? visibilityModifier? modifier? identifier COLON typeType (ASSIGN expression)? SEMI //TODO change to match local var declaration
    ;

formalParameters
    :   LPAREN (formalParameter (COMMA formalParameter)*)? RPAREN
    ;

formalParameter
    : identifier COLON typeType (ASSIGN expression)?
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
    |   LPAREN typeList ARROW typeList RPAREN
    |   predefinedType (LT typeOrTuple (COMMA typeOrTuple)* GT)?
    |   TypeIdentifier (LT typeOrTuple (COMMA typeOrTuple)* GT)?
    ;

typeOrTuple
    :   typeType
    |   LPAREN typeList RPAREN
    |   LPAREN RPAREN
    ;

typeList
    :   typeType (COMMA typeType)*
    |   LPAREN RPAREN
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
    |   ifStatement                                                                         #ifStatement_
    |   FOR  LCIdentifier (COMMA LCIdentifier)* COLON expression block                      #foreachStatement
    |   WHILE  expression  block                                                            #whileStatement
    |   RETURN tupleExpression? SEMI                                                        #returnStatement
    |   BREAK SEMI                                                                          #breakStatement
    |   CONTINUE SEMI                                                                       #continueStatement
    |   SEMI                                                                                #emptyStatement
    |   expression SEMI                                                                     #expressionStatement
    |   assignLhss
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
        tupleExpression SEMI                                                                #assignment
    ;

assignLhss
    :   assignLhs (COMMA assignLhs)*
    ;

assignLhs
    :   identifier COLON typeType?
    |   expression
    ;

ifStatement
    :   IF expression block (ELSE block | ELSE ifStatement)?
    ;

// EXPRESSIONS -----------------------------------------------------------------------------

expression
    :   LPAREN expression RPAREN                                   #bracketsExpr
    |   expression EXMARK                                          #cast
    |   expression LBRACK expression RBRACK                        #arrayAccess
    |   expression DOT identifier                                  #fieldAccess
    |   expression DOT LCIdentifier LPAREN arguments? RPAREN       #externalFunctionCall
    |   LCIdentifier LPAREN arguments? RPAREN                      #internalFunctionCall
    |   typeType DOT LCIdentifier STAR (LPAREN typeList RPAREN)?   #functionAddress
    |   LCIdentifier STAR (LPAREN typeList RPAREN)?                #internalFunctionAddress
    |   NEW typeType LPAREN namedExpressions? RPAREN            #newObject
    |   NEW typeType (LBRACK expression RBRACK)                    #newArray
    |   (EXMARK|SUB|INC|DEC) expression                            #preUnaryOp
    |   LPAREN typeType RPAREN expression                          #cast //TODO change syntax because brackets are ambigious
    |   expression (STAR|SLASH|MOD) expression                       #binaryOp
    |   expression (ADD|SUB) expression                            #binaryOp
    |   expression (LE|GE|LT|GT) expression                        #binaryOp
    |   expression (EQUAL|NOTEQUAL) expression                     #binaryOp
    |   expression (EQUAL_ID|NOTEQUAL_ID) expression               #binaryOp
    |   expression (EQUAL_CONTAINER|NOTEQUAL_CONTAINER) expression #binaryOp
    |   expression (AAND|AOR|XOR|AND|OR) expression                #binaryOp
    |   expression COLON expression                                #optionalElse
    |   lambda                                                     #lambdaExpr
    |   THIS                                                       #thisExpr
    |   literal                                                    #literalExpr
    |   typeType                                                   #typeTypeExpr
    |   identifier                                                 #variableExpr
    ;

arguments
    :   tupleExpression (COMMA namedExpressions)?
    |   namedExpressions
    ;

tupleExpression
    :   expression (COMMA expression)*
    ;

namedExpressions
    :   namedExpression (COMMA namedExpression)*
    ;

namedExpression
    :   identifier ASSIGN tupleExpression
    ;

lambda
    :   lambdaHeader (expression|block)
    ;

lambdaHeader
    :   BACKSLASH lambdaParam* ARROW
    ;

lambdaParam
    :   typeType? identifier
    |   UNDERSCORE
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
SLASH           : '/';
BACKSLASH       : '\\';


//Operators-----------------------------------------------------------------
ASSIGN          : '=';
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