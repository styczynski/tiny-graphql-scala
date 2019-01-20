package parser.schema

import org.scalatest.FunSpec
import parser.SchemaParser
import parser.schema.types.{GraphQLArrayType, GraphQLCompositeType}

class SchemaBasicTest extends FunSpec {

  describe("A schema parser") {
    describe("on empty input") {
      val schema = SchemaParser().parse("")
      it("should contain ID scalar") {
        assert(schema.findType("ID").getTypeKeyword.equals("scalar"))
      }
      it("should contain String scalar") {
        assert(schema.findType("String").getTypeKeyword.equals("scalar"))
      }
      it("should contain Float scalar") {
        assert(schema.findType("Float").getTypeKeyword.equals("scalar"))
      }
      it("should contain Int scalar") {
        assert(schema.findType("Int").getTypeKeyword.equals("scalar"))
      }
    }

    it("should handle basic primitives in types definitions") {
      val code = """|type Custom {
                    |    x: ID
                    |    t: Boolean
                    |    y: Int
                    |    z: Float
                    |}
                    |""".stripMargin
      val schema = SchemaParser().parse(code)
      assert(schema.findType("Custom") ==
        GraphQLCompositeType(Some("Custom"))
          .withField("x", DefaultGraphQLSchema.ID)
          .withField("t", DefaultGraphQLSchema.BOOLEAN)
          .withField("y", DefaultGraphQLSchema.INT)
          .withField("z", DefaultGraphQLSchema.FLOAT)
      )
    }

    it("should handle nullability in types definitions") {
      val code = """|type CustomNullability {
                    |    x: String!
                    |    y: Int!
                    |    z: Float
                    |}
                    |""".stripMargin
      val schema = SchemaParser().parse(code)
      assert(schema.findType("CustomNullability") ==
        GraphQLCompositeType(Some("CustomNullability"))
          .withField("x", DefaultGraphQLSchema.STRING.withNullability(false))
          .withField("y", DefaultGraphQLSchema.INT.withNullability(false))
          .withField("z", DefaultGraphQLSchema.FLOAT)
      )
    }

    it("should handle one-level primitive-based composite types") {
      val code = """|type CustomPrimitives {
                    |    x: String
                    |    y: Int
                    |}
                    |""".stripMargin
      val schema = SchemaParser().parse(code)
      assert(schema.findType("CustomPrimitives") ==
        GraphQLCompositeType(Some("CustomPrimitives"))
          .withField("x", DefaultGraphQLSchema.STRING)
          .withField("y", DefaultGraphQLSchema.INT)
      )
    }

    it("should handle composite types with arrays") {
      val code = """|type CustomArray {
                    |    x: String
                    |    y: [Int]
                    |}
                    |""".stripMargin
      val schema = SchemaParser().parse(code)

      assert(schema.findType("CustomArray") ==
        GraphQLCompositeType(Some("CustomArray"))
          .withField("x", DefaultGraphQLSchema.STRING)
          .withField("y", GraphQLArrayType(DefaultGraphQLSchema.INT))
      )
    }

    it("should handle composite types with nested arrays") {
      val code = """|type CustomNestedArrays {
                    |    x: String
                    |    y: [[[Int!]]!]!
                    |}
                    |""".stripMargin
      val schema = SchemaParser().parse(code)
      assert(schema.findType("CustomNestedArrays") ==
        GraphQLCompositeType(Some("CustomNestedArrays"))
          .withField("x", DefaultGraphQLSchema.STRING)
          .withField("y", GraphQLArrayType(
              GraphQLArrayType(
                GraphQLArrayType(
                  DefaultGraphQLSchema.INT.withNullability(false)
                )
              ).withNullability(false)
            ).withNullability(false)
          )
      )
    }

  }
}