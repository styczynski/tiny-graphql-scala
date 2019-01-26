package parser.schema

import org.scalatest.FunSpec
import parser.SchemaParser
import parser.exceptions.{NotCompatibleTypesError, ParserError, TypeMissingError}
import parser.schema.types.{GraphQLArrayType, GraphQLCompositeType, GraphQLInterfaceType}

class SchemaUnionTest extends FunSpec {

  describe("A schema parser") {
    it("should handle basic union types compatibility calculations") {
      val code = """|union X = String | ID
                    |union Y = Float
                    |union Z = String | ID | Float
                    |""".stripMargin
      val schema = SchemaParser().parse(code)

      assert(schema.findType("X") ~!= schema.findType("Y"))
      assert(schema.findType("Z") ~> schema.findType("X"))
      assert(schema.findType("Z") ~> schema.findType("Y"))
    }


    it("should handle union positive compatibility with nested unions") {
      val code = """|union Text = String
                    |union StringCustom = String
                    |union StringCustom2 = String | StringCustom
                    |union Union1 = Text
                    |union Union2 = StringCustom | StringCustom2
                    |
                    |""".stripMargin
      val schema = SchemaParser().parse(code)

      assert(schema.findType("Text") ~= schema.findType("String"))
      assert(schema.findType("StringCustom") ~= schema.findType("Text"))
      assert(schema.findType("StringCustom2") ~= schema.findType("StringCustom"))
      assert(schema.findType("Union1") ~= schema.findType("StringCustom2"))
      assert(schema.findType("Union1") ~= schema.findType("Union2"))
    }

    it("should handle union negative compatibility with nested unions") {
      val code = """|union Text = String
                    |union StringCustom = String
                    |union StringCustom2 = String | StringCustom
                    |union Union1 = Text
                    |union Union2 = StringCustom | StringCustom2 | Float
                    |
                    |""".stripMargin
      val schema = SchemaParser().parse(code)

      assert(schema.findType("Union1") ~!= schema.findType("Union2"))
      assert(schema.findType("Union1") ~< schema.findType("Union2"))
      assert(schema.findType("Float") ~!= schema.findType("Union2"))
      assert(schema.findType("String") ~!= schema.findType("Union2"))
      assert(schema.findType("Float") ~< schema.findType("Union2"))
      assert(schema.findType("String") ~< schema.findType("Union2"))
      assert(schema.findType("String") ~= schema.findType("Text"))

    }

    it("should handle cyclic union") {
      val code = """|union X = X
                    |union Y = Y
                    |""".stripMargin
      val schema = SchemaParser().parse(code)

      assert(schema.findType("X") ~= schema.findType("Y"))
    }

  }
}