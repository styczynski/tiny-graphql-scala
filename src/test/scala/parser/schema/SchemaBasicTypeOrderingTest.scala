package parser.schema

import org.scalatest.FunSpec
import parser.SchemaParser
import parser.schema.types.{GraphQLArrayType, GraphQLCompositeType}

class SchemaBasicTypeOrderingTest extends FunSpec {

  describe("A schema") {

    it("should correctly order scalars by type") {
      val code =
        """|
           |scalar X
           |scalar Y
           |
           |""".stripMargin
      val schema = SchemaParser().parse(code)

      assert(schema.findType("X") ~!= schema.findType("Y"))
      assert(!(schema.findType("X") ~< schema.findType("Y")))
      assert(!(schema.findType("X") ~> schema.findType("Y")))
      assert(!(schema.findType("X") ~= schema.findType("Y")))
    }

    it("should correctly order unions by type") {
      val code =
        """|
           |union C = Int | Float | String
           |union A = String | Float
           |union B = String
           |
           |""".stripMargin
      val schema = SchemaParser().parse(code)

      assert(schema.findType("A") ~!= schema.findType("B"))
      assert(schema.findType("A") ~> schema.findType("B"))
      assert(!(schema.findType("A") ~< schema.findType("B")))

      assert(schema.findType("C") ~!= schema.findType("A"))
      assert(schema.findType("C") ~> schema.findType("A"))
      assert(!(schema.findType("C") ~< schema.findType("A")))

      assert(schema.findType("C") ~!= schema.findType("B"))
      assert(schema.findType("C") ~> schema.findType("B"))
      assert(!(schema.findType("C") ~< schema.findType("B")))
    }

    it("should correctly order types by nullability") {
      val code =
        """|
           |type B {
           |  x: Int!
           |}
           |
           |type A {
           |  x: Int
           |}
           |
           |""".stripMargin
      val schema = SchemaParser().parse(code)

      assert(schema.findType("A") ~!= schema.findType("B"))
      assert(schema.findType("A") ~> schema.findType("B"))
      assert(!(schema.findType("A") ~< schema.findType("B")))
    }

    it("should correctly order array-types by fields") {
      val code =
        """|
           |union U1 = String
           |union U2 = U1 | Float
           |union U1Rec = U1 | Type1
           |union U2Rec = U2 | Type2
           |
           |type Type1 {
           |  x: U1Rec
           |}
           |
           |type Type2 {
           |  x: U2Rec
           |}
           |
           |""".stripMargin
      val schema = SchemaParser().parse(code)

      assert(schema.findType("Type2") ~!= schema.findType("Type1"))
      assert(schema.findType("Type2") ~> schema.findType("Type1"))
      assert(!(schema.findType("Type2") ~< schema.findType("Type1")))

      assert(schema.findType("U1Rec") ~< schema.findType("U2Rec"))
      assert(schema.findType("U2") ~> schema.findType("U1"))
    }
  }

}