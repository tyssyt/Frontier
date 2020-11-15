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
    NAMESPACE,
    CONSTRUCTORS,
    EXPORT,
    PRIVATE,
    STATIC,
    NATIVE,
    OPEN,
    OPERATOR,
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
    INT16,
    INT32,
    INT64,
    FLOAT32,
    FLOAT64,
    PRIMITIVES,
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
        (classDeclaration|namespaceDeclaration)*
        EOF
    ;

importStatement //TODO this is an ugly mess, I should just change modes (also can't handle paths that contain spaces etc)
    :   IMPORT IDENTIFIER SEMI
    ;

includeStatement
    :   NATIVE? INCLUDE path OUT? SEMI
    ;

path
    :   folder? IDENTIFIER DOT IDENTIFIER   #filePath
    |   folder STAR STAR? (DOT IDENTIFIER)? #folderPath
    ;

folder
    :   (DOT DOT SLASH)+ (IDENTIFIER SLASH)*
    |   (DOT DOT SLASH)* (IDENTIFIER SLASH)+
    ;

classDeclaration
    :   visibilityModifier? NATIVE? CLASS IDENTIFIER typeParameters? COLON
        typeParameterSpecification*
        classDeclaratives
        (methodDeclaration|nativeMethodDeclaration|fieldDeclaration)*
    ;

namespaceDeclaration
    :   visibilityModifier? NAMESPACE IDENTIFIER COLON
        (methodDeclaration|nativeMethodDeclaration)*
    ;


typeParameters
    :   LT typeParamer (COMMA typeParamer)* GT
    ;

typeParamer
    :   (IN|OUT)? IDENTIFIER STAR?  //TODO it would be nice to not have in & out as keyword anywhere, as they can only appear within type parameters
    ;

typeParameterSpecification
    :   WHERE upperBound? IDENTIFIER lowerBound?
    ;

upperBound
    : typeList GT
    ;

lowerBound
    : GT typeList
    ;

classDeclaratives
    :   constructorsDeclarative? forDeclarative?
    ;

constructorsDeclarative
    :   visibilityModifier? CONSTRUCTORS SEMI
    ;

forDeclarative
    :   FOR COLON expression COMMA expression SEMI
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
    :   visibilityModifier? NATIVE? STATIC?
        ((OPEN | typeType DOT)? IDENTIFIER | OPERATOR operator)
        typeParameters? formalParameters
        (ARROW typeList | BACKARROW typedIdentifiers)? typeParameterSpecification*
    ;

operator
    :   EXMARK
    |   ADD
    |   SUB
    |   STAR
    |   SLASH
    |   MOD
    |   LE
    |   GE
    |   LT
    |   GT
    |   EQUAL
    |   NOTEQUAL
    |   EQUAL_CONTAINER
    |   NOTEQUAL_CONTAINER
    |   AAND
    |   AOR
    |   XOR
    |   AND
    |   OR
    |   Array
    ;

fieldDeclaration
    :   (DELEGATE nameSelector COLON)? visibilityModifier? modifier? IDENTIFIER COLON typeType (ASSIGN expression)? SEMI //TODO change to match local var declaration
    ;

formalParameters
    :   LPAREN (formalParameter (COMMA formalParameter)*)? RPAREN
    ;

formalParameter
    : typedIdentifier (ASSIGN expression)?
    ;

typedIdentifiers
    : typedIdentifier (COMMA typedIdentifier)*
    ;

typedIdentifier
    : IDENTIFIER COLON typeType
    ;

nameSelector
    :   STAR
    |   STAR BACKSLASH (IDENTIFIER (COMMA IDENTIFIER)*)?
    |   IDENTIFIER (COMMA IDENTIFIER)*
    ;

//types ------------------------------------------------------------------------------------

typeType
    :   LBRACK typeOrTuple RBRACK
    |   NATIVE LBRACK  typeOrTuple RBRACK
    |   typeType QUESTION
    |   LPAREN typeList ARROW typeList RPAREN
    |   predefinedType (LT typeOrTuple (COMMA typeOrTuple)* GT)?
    |   IDENTIFIER (LT typeOrTuple (COMMA typeOrTuple)* GT)?
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
    |   INT16
    |   INT32
    |   INT64
    |   FLOAT32
    |   FLOAT64
    ;


// STATEMENTS / BLOCKS ---------------------------------------------------------------------
block
    :   LBRACE statement* RBRACE
    ;

lamdaBlock
    :   LBRACE lambdaHeader? statement* RBRACE
    ;

statement
    :   block                                                                               #blockStatement
    |   ifStatement                                                                         #ifStatement_
    |   FOR  IDENTIFIER (COMMA IDENTIFIER)* COLON expression block                      #foreachStatement
    |   WHILE  expression  block                                                            #whileStatement
    |   RETURN tupleExpression? SEMI                                                        #returnStatement
    |   BREAK SEMI                                                                          #breakStatement
    |   CONTINUE SEMI                                                                       #continueStatement
    |   SEMI                                                                                #emptyStatement
    |   expression SEMI                                                                     #expressionStatement
    |   assignLhss ASSIGN tupleExpression SEMI                                              #assignment
    ;

assignLhss
    :   assignLhs (COMMA assignLhs)*
    ;

assignLhs
    :   IDENTIFIER COLON typeType?
    |   expression
    ;

ifStatement
    :   IF expression lamdaBlock (ELSE block | ELSE ifStatement)?
    ;

// EXPRESSIONS -----------------------------------------------------------------------------

//TODO the STAR STAR in adress is a temporary fix to avoid clash with multiplication

expression
    :   LPAREN expression RPAREN                                   #bracketsExpr
    |   expression LBRACK arguments RBRACK                         #arrayAccess
    |   expression DOT IDENTIFIER (LPAREN arguments? RPAREN)?      #externalFunctionCall
    |   IDENTIFIER LPAREN arguments? RPAREN                        #internalFunctionCall
    |   typeType DOT IDENTIFIER STAR STAR (LPAREN typeList RPAREN)?   #functionAddress
    |   typeType DOT OPERATOR operator STAR STAR (LPAREN typeList RPAREN)? #functionAddress
    |   IDENTIFIER STAR STAR (LPAREN typeList RPAREN)?             #internalFunctionAddress
    |   OPERATOR operator STAR STAR (LPAREN typeList RPAREN)?      #internalFunctionAddress
    |   NEW typeType LPAREN namedExpressions? RPAREN               #newObject
    |   NEW typeOrTuple (LBRACK expression RBRACK)                 #newArray
    |   (EXMARK|SUB) expression                                    #preUnaryOp
    |   expression EXMARK                                          #cast
    |   expression (STAR|SLASH|MOD) expression                     #binaryOp
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
    |   IDENTIFIER                                                 #variableExpr
    |   typeType                                                   #typeTypeExpr
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
    :   IDENTIFIER ASSIGN tupleExpression
    ;

lambda
    :   lambdaHeader (expression|block)
    ;

lambdaHeader
    :   BACKSLASH lambdaParam? (COMMA lambdaParam)* ARROW
    ;

lambdaParam
    :   IDENTIFIER (COLON typeType)?
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
BACKARROW           : '<-';

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
ADD             : '+';
SUB             : '-';
STAR            : '*';
MOD             : '%';

Array           : '[]';

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
    :   '\\' [n0'\\]
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
IDENTIFIER
    :   '_'* Letter LetterOrDigit*
    {
        String text = getText();
        Integer type = keywords.get(text);
        if (type != null) {
            setType(type);
        }
    }
    ;

fragment LetterOrDigit   : [a-zA-Z0-9_];
fragment Letter : [A-Za-z];


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