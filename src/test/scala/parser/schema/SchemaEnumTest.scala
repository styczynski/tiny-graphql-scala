package parser.schema

import org.scalatest.FunSpec
import parser.SchemaParser
import parser.schema.types.{GraphQLArrayType, GraphQLCompositeType, GraphQLEnumType}

class SchemaEnumTest extends FunSpec {

  describe("A schema parser") {
    it("should handle basic enum") {
      val code = """|enum ENUM0 {
                    |    value1
                    |    value2
                    |    value3
                    |    value4
                    |}
                    |""".stripMargin
      val schema = SchemaParser().parse(code)
      assert(schema.findType("ENUM0") ==
        GraphQLEnumType(Some("ENUM0"))
          .withValue("value1")
          .withValue("value2")
          .withValue("value3")
          .withValue("value4")
      )
    }

    it("should handle enum nested in type") {
      val code = """|enum EnumType {
                    |    value1
                    |}
                    |
                    |type Nested {
                    |    x: [EnumType]
                    |    y: EnumType
                    |    z: EnumType!
                    |}
                    |""".stripMargin
      val schema = SchemaParser().parse(code)

      val enumType = GraphQLEnumType(Some("EnumType"))
        .withValue("value1")

      assert(schema.findType("EnumType") == enumType)

      assert(schema.findType("Nested") ==
        GraphQLCompositeType(Some("Nested"))
          .withField("x", GraphQLArrayType(enumType))
          .withField("y", enumType)
          .withField("z", enumType.withNullability(false))
      )
    }
  }
}