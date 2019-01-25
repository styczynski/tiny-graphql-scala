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
  }
}