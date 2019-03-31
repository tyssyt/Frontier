# Frontier

Self-made Programming Language

## Getting Started

### Prerequisites

Clang needs to be installed and in your PATH.

For Windows user Visual Studio needs to be installed on your sytem (as it required by LLVM).

### Installing

After the initial Gradle Setup run the following task to generate the Parser from the ANTLR Grammar

```
gradlew generateGrammarSource
```
Whenever the [Grammar](https://github.com/tyssyt/Frontier/blob/master/src/main/antlr/tys/frontier/parser/antlr/Frontier.g4) is modified this tasks needs to be rerun.

## Project Structure

* [Frontier Libs](https://github.com/tyssyt/Frontier/tree/master/Frontier%20Libs): contains basic core libraries that can be used by every Frontier Program with *import* statements
* src
  * main
    * antlr: contains the [Grammar](https://github.com/tyssyt/Frontier/blob/master/src/main/antlr/tys/frontier/parser/antlr/Frontier.g4)
    * java/tys/frontier
      * [code](https://github.com/tyssyt/Frontier/tree/master/src/main/java/tys/frontier/code): data structures for intermediate representation of the code
      * [parser](https://github.com/tyssyt/Frontier/tree/master/src/main/java/tys/frontier/parser): transforms the ANTLR AST into the intermediate representation
      * [backend](https://github.com/tyssyt/Frontier/tree/master/src/main/java/tys/frontier/backend): handles generating LLVM IR from the intermediate representation
      * [passes](https://github.com/tyssyt/Frontier/tree/master/src/main/java/tys/frontier/passes): transformation/analysis passes on the intermediate representation
      * ...
  * test
    * java/tys/frontier
      * [main](https://github.com/tyssyt/Frontier/tree/master/src/test/java/tys/frontier/main): tests that compile frontier code and execute the binaries
      * [parser](https://github.com/tyssyt/Frontier/tree/master/src/test/java/tys/frontier/parser): tests that evoke errors during compilation
    * [resources](https://github.com/tyssyt/Frontier/tree/master/src/test/resources) frontier files for tests

## Frontier Syntax & Semantics

Note that I currently focus on completing major language features, thus most of the syntax is temporary and will be revisited.

### Types

#### Builtin Types
  * int32, int64: signed integer types
  * float32, float64: floating point types
  * bool: boolean type
  * char: character type, implemented as 8 bit integer, literals have the form "c"
  
#### Arrays
  For every type T, T[] is an array of T. Currently there is no support for array literals yet.
  Arrays have a fixed size, to create an int array of size 5:
```
new int[5]
```
  Arrays have a field called *size* of type int32.
  The [List Library](https://github.com/tyssyt/Frontier/blob/master/Frontier%20Libs/List.front) shows the usage of Arrays.
  
#### Strings
  Strings are implemented as char[], and string literals are supported. "this is a string literal!"
  
#### Optionals
  For every type T, T? is the optional of T.
  
  * The value *null* can only be assigned to optionals.
  * T casts implicitly to T?.
  * optionals can be implicitly cast to boolean, being false if they are null and true otherwise
  * The unary ! operator casts from T? to T or causes a runtime error if the cast value is *null*.
  * The binary : (orElse) operator takes an T? on the left side, a T on the right side and a returns a result of type T.
    If the optional is not *null*, it is returned, otherwise the right side is returned.
  * All functions of the base types can be called on the optional type. The return value of this function call is always an optional.
    If the value exist, the function is called, and its result returned. If it is *null*, no function is called and *null* is returned.
  
  The [Optional Test](https://github.com/tyssyt/Frontier/blob/master/src/test/resources/Parser/Main/Optional.front) and the [Optional Test with Generics](https://github.com/tyssyt/Frontier/blob/master/src/test/resources/Parser/Main/OptGeneric.front) show some of the usage of Optionals.

#### User defined types
  See Classes

#### Function Types
  Functions have special types of the form T1, ..., TN -> R, where T1, ..., TN are the types of the arguments and R is the return type.  
  For example, the type of the plus operator on 32 bit integers is: int32, int32 -> int32.
  For functions without return values, we use the special type ().

### Structure of .front Files
  A frontier file starts with import statements. Note that importing will be modified in the near future.
  
  Currently, if the imported name has no file extension, it will be looked up in the standard Library, otherwise it will be looked up in the file system.
  The imported files are parsed first, and all identifiers (classes, functions & fields) that are marked with the visibility modifier *export* are visible to the importer.

  After the import statement, a file may contain any number of class definitions.

### Classes
Class declarations are of the form:
```
VisibilityModifier class Classname:
``` 
  The class name must start with a uppercase letter.
  There are two visibilites, export or default.
  Classes that are marked with export are visible outside of the file when importet by another file.
  Every class defines a type of the same name.
  
  The body of a class declaration may contain field- and function declarations.

#### Generic Classes
  After the class name, we can specify type parameters in angle brackets.
  These parameters can be used like regular types within the body of the class declaration.
  When the class is used somewhere, they have to be instantiated with actual types by the user.
  For every different instantiation, we create a copy of the class.
  
  The [List Library](https://github.com/tyssyt/Frontier/blob/master/Frontier%20Libs/List.front) is an example of a generic class.
  
  A Note on [Variance:](https://en.wikipedia.org/wiki/Covariance_and_contravariance_(computer_science))
  The keywords *in* and *out* can be used to mark a type parameter as co- or contravariant.
  This can allow implicit casts of generic type.
  Optionals are Covariant to the base type, arrays invariant, and function types covariant in the return and contravariant in the arguments.

  A where clause can be used to create bounds for type parameters. For example where T > List<int32> requires T to be a type that can be implicitly cast to List<int32>.
  

### Fields
Field declarations have the form:

```
VisibilityModifier static? Type FieldName;
```
The field name must start with a lowercase letter.
There are two visibilites, export or default.
Classes that are marked with export are visible outside of the file when importet by another file.

If a field is marked static, it belongs to the class and only one copy of it will ever exist (a global variable).
Otherwise, every instance of the class has its own field.

Fields may be assigned an intial value via ':='.

#### Delegate
The delegate keyword together with a name selector can be used in front of a field declaration:
```
delegate size,get : List<int32> list;
```
Name declarators are a comma seperated list of function names.
The specified functions need to exist in the type of the field.
The selected functions are copied into the current class, and calling them delegates the call to the field.

Furthermore, '\*' is also a valid name selector that copies all functions and additionally will allow the current type to be cast to the type of the field.
Finally, '\*\\' followed by a comma seperated list of function names will delegate all but the specified functions.

The [Delegate Test](https://github.com/tyssyt/Frontier/blob/master/src/test/resources/Parser/Main/Delegate.front) shows examples pf using delegate.

### Functions
Function declarations have the form:

```
VisibilityModifier static? FunctionName (ParamType1 ParamName1, ...) -> ReturnType {...}
```
The function name must start with a lowercase letter.
There are two visibilites, export or default.
The arrow and return type may be omitted if nothing is returned.
The body of a function is a list of statements.

If a function is not marked static, an implict first argument called 'this', of type of the current class is added.

Parameters may be assigned a default value via ':=', allowing them to be omitted by callers.

#### Generic Functions
After the function name we can specify type parameters in angle brackets.
These parameters can be used like regular types within the body of the function declaration.
The instantiations of the type parameters are inferred autmatically by the compiler on function call.
For every different instantiation, we create a copy of the function.

Similar to generic classes, where clauses can be used to restrict types.

The [Higher Order Function Test](https://github.com/tyssyt/Frontier/blob/master/src/test/resources/Parser/Main/HigherOrder.front) uses generic functions.

### Constructors
Constructors are static methods of the class that create instances.
They are generated automatically, but can only be called by other function from the class.
The parameters of constructors are the instance fields of the class.
If a field has an initial value, it will be used as default value for the parameter.

### Statements
The following are allowed statements:
* block: { StatementList }
* expression ;
* return expression ;
* variable assignment: Varname = expr; (also supports +=, -= etc.)
* variable declaration:
  * just declaration: Type Varname;
  * declaration with assignment: Type Varname := expr;
  * declaration with assignment and type inference: Varname := expr; 
* conditionals
```
if (expr) {...}
if (expr) {...} else {...}
if (expr) {...} else if (expr) ...
```

#### Loops
* break; to leave the innermost loop
* continue; to continue with the next iteration of the innmermost loop
* while(expr) {...}  
a nornmal while loop
* for(VarDecl;expr;expr) {...}  
a normal for loop with declaration, condition and increment
* for(Varname : expr) {...}  
expr needs to be an array. Iterates over all elements of that array.


### Expressions
The following are allowed expression:
* variable
* literal
* type
* array access: expression[expression]
* field access: expression.expression  
For accessing fields in the same class the first part may be omitted
* function call: expression.expression(expression...)  
For calling functions in the same class the first part may be omitted
* function address: expression.expression*(type...)  
For addresses in the same class the first part may be omitted.
If there is only one function of the name the brackets may be omitted.
* contructor call: new type (expression...)
* unary operators: ++ -- - (all prefix)
* binary operators:
  * arith: + - * / %
  * bool: | & ^
  * bool short circuit: || &&
  * comparison: > < >= <=
  * equality: == =!= =\*= =!\*=  
  The non star variants are meant to be definable by the user, as we don't have custom operators yet they don't work.
  * optional else: ':'
* cast: (type)expression
* cast remove optional: !expression

## Authors

**Thies Strothmann**

