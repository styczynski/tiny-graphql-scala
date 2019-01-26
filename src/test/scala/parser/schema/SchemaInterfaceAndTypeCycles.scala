package parser.schema

import org.scalatest.FunSpec
import parser.SchemaParser
import parser.exceptions.{NotCompatibleTypesError, ParserError, TypeMissingError}
import parser.schema.types._

class SchemaInterfaceAndTypeCycles extends FunSpec {

  describe("A schema parser") {
    it("should handle two infinite separate type cycles") {
      val code = """|
                    |type TypeX {
                    | x: TypeX
                    |}
                    |
                    |type TypeY {
                    | x: TypeY
                    |}
                    |
                    |""".stripMargin

      val schema = SchemaParser().parse(code)
      assert(schema.findType("TypeY") ~= schema.findType("TypeX"))
    }

    it("should throw on cyclic interface with incompatible nullability") {
      val code = """|type TypeA implements InterfaceA {
                    |  x: TypeB
                    |  y: InterfaceA
                    |}
                    |
                    |interface InterfaceA {
                    |  x: InterfaceB
                    |  y: InterfaceA
                    |}
                    |
                    |interface InterfaceB {
                    |  x: InterfaceA
                    |  y: InterfaceB!
                    |}
                    |
                    |type TypeB implements InterfaceB {
                    |  x: TypeA
                    |  y: TypeB
                    |}
                    |
                    |""".stripMargin

      assertThrows[ParserError[NotCompatibleTypesError]] {
        SchemaParser().parse(code)
      }
    }

    it("should handle trivial-cycle type") {
      val code = """|
                    |type TypeX {
                    | x: TypeX!
                    |}
                    |
                    |""".stripMargin

      val schema = SchemaParser().parse(code)
      assert(schema.findType("TypeX") ~=
        GraphQLCompositeType(Some("TypeX"))
          .withField("x", GraphQLRefType(schema, Some("TypeX")).withNullability(false))
      )
    }

    it("should throw on cyclic interface with unions") {
      val code = """|type TypeA implements InterfaceA {
                    |  x: TypeB
                    |  y: InterfaceA
                    |}
                    |
                    |union UnionB = TypeA | TypeA
                    |
                    |interface InterfaceA {
                    |  x: InterfaceB
                    |  y: InterfaceA
                    |}
                    |
                    |union UnionD = TypeA | UnionC
                    |
                    |interface InterfaceB {
                    |  x: InterfaceA
                    |  y: InterfaceB
                    |}
                    |
                    |union UnionA = TypeA | TypeB
                    |
                    |union UnionC = UnionA | UnionB | TypeB
                    |
                    |type TypeB implements InterfaceB {
                    |  x: TypeA
                    |  y: UnionA
                    |}
                    |
                    |""".stripMargin

      val schema = SchemaParser().parse(code)
      (
        schema.findType("TypeA")
          :: schema.findType("TypeB")
          :: schema.findType("UnionA")
          :: schema.findType("UnionB")
          :: schema.findType("UnionC")
          :: schema.findType("UnionD")
          :: Nil
      ).foreach(typeValue => {
        assert(typeValue ~= schema.findType("TypeA"))
      })
    }
  }
}