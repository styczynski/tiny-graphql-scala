package parser.schema

import org.scalatest.FunSpec
import parser.SchemaParser
import parser.exceptions.{MixedTypesDirectionError, NotCompatibleTypesError, ParserError}
import parser.schema.types._

class SchemaFieldArgsTest extends FunSpec {

  describe("A schema parser") {
    it("should handle basic scalar args on fields") {
      val code = """|
                    |type TypeWithArgs {
                    |  x(a: String, b: String, c: String!): String
                    |  y(a: Int, b: Float!, c: Float): [[Int]!]!
                    |  z(a: Float!, c: ID): String
                    |  d(d: ID): ID!
                    |}
                    |
                    |""".stripMargin
      val schema = SchemaParser().parse(code)

      val TypeWithArgs = GraphQLCompositeType(Some("TypeWithArgs"))
        .withField("x", Map(
          "a" -> DefaultGraphQLSchema.STRING,
          "b" -> DefaultGraphQLSchema.STRING,
          "c" -> DefaultGraphQLSchema.STRING.withNullability(false)),
          DefaultGraphQLSchema.STRING
        )
        .withField("y",
          Map(
            "a" -> DefaultGraphQLSchema.INT,
            "b" -> DefaultGraphQLSchema.FLOAT.withNullability(false),
            "c" -> DefaultGraphQLSchema.FLOAT
          ),
          GraphQLArrayType(
            GraphQLArrayType(DefaultGraphQLSchema.INT).withNullability(false)
          ).withNullability(false)
        )
        .withField("z",
          Map(
            "a" -> DefaultGraphQLSchema.FLOAT.withNullability(false),
            "c" -> DefaultGraphQLSchema.ID
          ),
          DefaultGraphQLSchema.STRING
        )
        .withField("d",
          Map(
            "d" -> DefaultGraphQLSchema.ID
          ),
          DefaultGraphQLSchema.ID.withNullability(false)
        )

      val TypeWithArgsNotCorrect = GraphQLCompositeType(Some("TypeWithArgs"))
        .withField("x", Map(
          "a" -> DefaultGraphQLSchema.STRING,
          "b" -> DefaultGraphQLSchema.STRING,
          "c" -> DefaultGraphQLSchema.STRING.withNullability(false)),
          DefaultGraphQLSchema.STRING
        )
        .withField("y",
          Map(
            "a" -> DefaultGraphQLSchema.INT,
            "b" -> DefaultGraphQLSchema.FLOAT,
            "c" -> DefaultGraphQLSchema.FLOAT
          ),
          GraphQLArrayType(
            GraphQLArrayType(DefaultGraphQLSchema.INT).withNullability(false)
          ).withNullability(false)
        )
        .withField("z",
          Map(
            "a" -> DefaultGraphQLSchema.FLOAT.withNullability(false),
            "c" -> DefaultGraphQLSchema.ID.withNullability(false)
          ),
          DefaultGraphQLSchema.STRING
        )
        .withField("d",
          Map(
            "d" -> DefaultGraphQLSchema.ID
          ),
          DefaultGraphQLSchema.ID.withNullability(false)
        )


      assert(schema.findType("TypeWithArgs").equivalentType(TypeWithArgs).hasNotFailedOrThrow)
      assert(schema.findType("TypeWithArgs") ~!= TypeWithArgsNotCorrect)
    }

    it("should handle input/output type mixing in args") {
      val code = """|
                    |type X {
                    | x: String
                    |}
                    |
                    |type TypeWithArgs {
                    |  z(x: X): X
                    |}
                    |
                    |""".stripMargin

      assertThrows[ParserError[MixedTypesDirectionError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on union in args") {
      val code = """|
                    |union Union1 = String | Int
                    |
                    |type X {
                    | s: Int!
                    |}
                    |
                    |type TypeWithArgs {
                    |  z(x: Union1): X
                    |}
                    |
                    |""".stripMargin

      assertThrows[ParserError[MixedTypesDirectionError]] {
        SchemaParser().parse(code)
      }
    }

    it("should handle enum in args") {
      val code = """|
                    |enum E { E1 E2 E3 }
                    |
                    |type TypeWithArgs {
                    |  z(x: E): [Int]
                    |}
                    |
                    |""".stripMargin

      val schema = SchemaParser().parse(code)

      val TypeWithArgs = GraphQLCompositeInputType(Some("TypeWithArgs"))
        .withField("z",
          Map(
            "x" -> GraphQLEnumType(Some("E"))
                .withValue("E1")
                .withValue("E2")
                .withValue("E3")
          ),
          GraphQLArrayType(DefaultGraphQLSchema.INT)
        )

      assert(schema.findType("TypeWithArgs") ~= TypeWithArgs)
    }

    it("should handle input type in args") {
      val code = """|
                    |input TypeInput {
                    | x: String!
                    |}
                    |
                    |type Type {
                    | x: String!
                    |}
                    |
                    |type TypeWithArgs {
                    |  getSth(arg: TypeInput): Type
                    |}
                    |
                    |""".stripMargin

      val schema = SchemaParser().parse(code)

      val TypeWithArgs = GraphQLCompositeInputType(Some("TypeWithArgs"))
        .withField("getSth",
          Map(
            "arg" -> GraphQLCompositeInputType(Some("TypeInput"))
              .withField("x", DefaultGraphQLSchema.STRING.withNullability(false))
          ),
          GraphQLCompositeType(Some("Type"))
            .withField("x", DefaultGraphQLSchema.STRING.withNullability(false))
        )

      assert(schema.findType("TypeWithArgs") ~= TypeWithArgs)
    }

    it("should fail on incompatible arg types when checking if implements is satisfied") {
      val code = """|
                    |interface I {
                    |  x(a: String): String
                    |}
                    |
                    |type A implements I {
                    |  x(a: Int): String
                    |}
                    |
                    |""".stripMargin

      assertThrows[ParserError[MixedTypesDirectionError]] {
        SchemaParser().parse(code)
      }
    }

    it("should handle exact arg types when checking if implements is satisfied") {
      val code = """|
                    |interface I {
                    |  x(a: String): String
                    |}
                    |
                    |type A implements I {
                    |  x(a: String): String
                    |}
                    |
                    |""".stripMargin

      val schema = SchemaParser().parse(code)

      assert(schema.findType("A") match {
        case c: GraphQLCompositeType => c.getField("x").get.getArgs.size == 1
        case  _ => false
      })
    }

    it("should handle array types in args") {
      val code = """|
                    |type Type {
                    |  x(z: [Int]): String
                    |}
                    |
                    |""".stripMargin

      val schema = SchemaParser().parse(code)

      val Type = GraphQLCompositeInputType(Some("Type"))
          .withField("x",
            Map(
              "z" -> GraphQLArrayType(DefaultGraphQLSchema.INT)
            ),
            DefaultGraphQLSchema.STRING
          )

      assert(schema.findType("Type") ~= Type)
    }

    it("should handle nested array types with nullabilities in args") {
      val code = """|
                    |input Inp {
                    |  w: Int!
                    |}
                    |
                    |type Type {
                    |  x(z: [[ID!]]!, q: [[String]!], p: [Inp!]): String
                    |}
                    |
                    |""".stripMargin

      val schema = SchemaParser().parse(code)

      val Type = GraphQLCompositeInputType(Some("Type"))
        .withField("x",
          Map(
            "z" -> GraphQLArrayType(
              GraphQLArrayType(
                DefaultGraphQLSchema.ID.withNullability(false)
              )
            ).withNullability(false),
            "q" -> GraphQLArrayType(
              GraphQLArrayType(
                DefaultGraphQLSchema.STRING
              ).withNullability(false)
            ),
            "p" -> GraphQLArrayType(
              GraphQLCompositeInputType(Some("Inp"))
                .withField("w", DefaultGraphQLSchema.INT.withNullability(false))
                .withNullability(false)
            )
          ),
          DefaultGraphQLSchema.STRING
        )

      assert(schema.findType("Type") ~= Type)
    }
  }
}