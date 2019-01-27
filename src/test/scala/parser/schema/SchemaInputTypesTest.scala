package parser.schema

import org.scalatest.FunSpec
import parser.SchemaParser
import parser.exceptions.{MixedTypesDirectionError, ParserError}
import parser.schema.types.{GraphQLCompositeInputType, GraphQLEnumType}

class SchemaInputTypesTest extends FunSpec {

  describe("A schema parser") {
    it("should handle basic union types compatibility calculations") {
      val code = """|input InputType {
                    |    x: String
                    |    y: Int!
                    |    z: Float
                    |}
                    |""".stripMargin
      val schema = SchemaParser().parse(code)

      val InputType = GraphQLCompositeInputType(Some("InputType"))
        .withField("x", DefaultGraphQLSchema.STRING)
        .withField("y", DefaultGraphQLSchema.INT.withNullability(false))
        .withField("z", DefaultGraphQLSchema.FLOAT)

      assert(schema.findType("InputType") ~= InputType)
    }



    it("should handle input/output type mixing in unions") {
      val code = """|
                    |type X {
                    | x: String
                    |}
                    |
                    |input Y {
                    | y: String!
                    |}
                    |
                    |union Z = X | Y
                    |
                    |""".stripMargin

      assertThrows[ParserError[MixedTypesDirectionError]] {
        SchemaParser().parse(code)
      }
    }

    it("should handle input/output type mixing in interfaces") {
      val code = """|
                    |input X {
                    | a: String
                    |}
                    |
                    |interface Y {
                    | y: X!
                    |}
                    |
                    |""".stripMargin

      assertThrows[ParserError[MixedTypesDirectionError]] {
        SchemaParser().parse(code)
      }
    }

    it("should handle enums in input types") {
      val code = """|
                    |enum E {
                    |  E1
                    |  E2
                    |  E3
                    |}
                    |
                    |input X {
                    |  e: E
                    |}
                    |
                    |""".stripMargin

      val schema = SchemaParser().parse(code)

      val InputType = GraphQLCompositeInputType(Some("X"))
        .withField("e",
          GraphQLEnumType(Some("E"))
            .withValue("E1")
            .withValue("E2")
            .withValue("E3")
        )

      assert(schema.findType("X") ~= InputType)
    }


    it("should handle correct input types ordering") {
      val code = """|
                    |input X {
                    | x: Y
                    |}
                    |
                    |input Y {
                    | x: X
                    |}
                    |
                    |input Z {
                    | x: String
                    |}
                    |
                    |input Q {
                    | x: String
                    |}
                    |
                    |input P {
                    | x: String!
                    |}
                    |
                    |""".stripMargin

      val schema = SchemaParser().parse(code)

      assert(schema.findType("X") ~= schema.findType("Y"))
      assert(schema.findType("Z") ~!= schema.findType("Y"))
      assert(schema.findType("Z") ~= schema.findType("Q"))
      assert(schema.findType("P") ~< schema.findType("Q"))
      assert(!(schema.findType("P") ~> schema.findType("Q")))
    }

    it("should handle unions inside input type") {
      val code = """|
                    |
                    |union U = String | Float
                    |
                    |input I {
                    |  x: U
                    |}
                    |
                    |""".stripMargin

      assertThrows[ParserError[MixedTypesDirectionError]] {
        SchemaParser().parse(code)
      }
    }
  }
}