# tiny-graphql-scala ![Build status](https://travis-ci.org/styczynski/tiny-graphql-scala.svg?branch=master) [![Coverage status](https://coveralls.io/repos/github/styczynski/tiny-graphql-scala/badge.svg?branch=master)](https://coveralls.io/github/styczynski/tiny-graphql-scala?branch=master) ![License status](https://img.shields.io/github/license/styczynski/tiny-graphql-scala.svg)

Tiny, fast GraphQL parser written in Scala
** This is yet experimental software, use it for your own resposibility **

Contents of this Document
 * Introduction
 * GraphQL schema parser
   * Parsing schema text
   * Builing AST manually
   * Types relations
 * GraphQL queries parser
 * TODOs
 
## Introduction
 
This project was implemented as a part of Warsaw university course and is yet highly experimental software without any guarantee it works and that it will still work in the future.
 
Parsers are based on Scala [parboiled2](https://github.com/sirthias/parboiled2) super-fast parser generator.
 
## GraphQL Schema parser

The parser supports all features of GraphQL except fragments.

### Parsing schema text

The most basic working snippet of code is presented below.

```scala
import parser.SchemaParser

val code = """|type MyType {
              |    x: ID
              |    t: Boolean
              |    y: Int
              |    z: Float!
              |}
              |""".stripMargin
val schema = SchemaParser().parse(code)
```

Parser returns instance of `GraphQLSchema` that contains representations of all loaded types.

### Builiding AST manually

You can build types manually using provided case classes:
```scala

val myType = GraphQLCompositeType(Some("MyTypeName"))
          .withField("x", DefaultGraphQLSchema.STRING.withNullability(false))
          .withField("y", DefaultGraphQLSchema.INT.withNullability(false))
          .withField("z", DefaultGraphQLSchema.FLOAT)

```
which is equivalent to the following schema:
```graphql
type MyTypeName {
  x: String!
  y: Int!
  z: Float
}
```

The `DefaultGraphQLSchema` provides implementations of basic GraphQL types.
You can also create circular dependencies using string references to types:

```scala
val schema = new GraphQLSchema()
val myType = GraphQLCompositeType(Some("MyTypeName"))
          .withField("x", GraphQLRefType(schema, Some("TypeX"))
```

Which is equivalent to:
```graphql
  type MyTypeName {
    x: TypeX
  }
```

When you use references to other types you must pass schema object that contains the required definiotions.
In the mentioned case the code will not throw any error, because type references are not checked.

To check if your types are valid you must call `schema.validate` (validates all types withing schema) or `myType.validateType` (only validates single type) to validate if the type really exist and if the structure of your types are valid.

You can learn more about building AST manually by looking at the tests [there](https://github.com/styczynski/tiny-graphql-scala/blob/master/src/test/scala/parser/schema/SchemaBasicTest.scala)

### Types relations

Let us introduce relation `~<` that for `A ~< B` means that B is generalization for A.
That in other words means that if you have an object `obj` and it's valid object of type `A` that it must be valid object of type `B`.

In similar manner we define `~>` and equality `~=` which is defined by: `A ~= B is equivalent to A ~< B && A ~> B`
Equality means the types represent exactly the same effective types (i.e. there exist no object that could differentiate these types).

```graphql
union UnionC = Int | Float | String
union UnionB = String | Float
```

In this case we have `String ~< UnionB && UnionB ~< UnionC`
So also we have `String ~< UnionC`

It's obvious as `UnionC` contains floats, ints and especially strings.

Types ordering may be useful in case of validating types.
For example:
```graphql
type X implements Y { ... }
```
means that `X ~> Y`

### GraphQL queries parser

There exists incomplete parser for GraphQL which supports only plain fields without fragments, annotations, fields arguments.

The parser can be used in similar way as the schema parser:
```scala
val code =
    """|
       |query {
       |  fieldA {
       |    fieldX {
       |      someData
       |    }
       |  }
       |}
       |""".stripMargin

println(RequestParser().parse(code))
```

`parse()` method will return AST-representation of the query

### TODOs

What was achieved:
 * Implemented full GraphQL schema langauge support with types validation and ability to manually create in-code queries
   * There are many features of GraphQL that parser has support for, yay!
   * The fragments are the last functionality that is not yet supported
 * Easy way to add custom types in future with own validation, field format etc.
 * Working SBT build with coverage collection on Travis CI
 * >70 tests for parsing various schemas
 * Very simple GraphQL query parser (not yet finished)
 
What left:
 * Implementing fully-compatible query parser
 * Implementing resolver for resolving the query
