package parser.schema

import org.scalatest.FunSpec
import parser.SchemaParser
import parser.exceptions.{SyntaxError, ParserError}
import parser.schema.types.{GraphQLArrayType, GraphQLCompositeType}

class SchemaSyntaxErrorsTest extends FunSpec {

  describe("A schema parser") {
    it("should fail on missing type information on field") {
      val code = """|
                    |type {
                    |  x:
                    |}
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on missing type information and comma on field") {
      val code = """|type {x}
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on missing type name") {
      val code = """|type { x: String }
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on unfinished union") {
      val code = """|union X = String |
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on invalid union start") {
      val code = """|union X = |Float
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on union single pipe") {
      val code = """|union X = |
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on union without any type") {
      val code = """|union X =
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on union without body") {
      val code = """|union X
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on type without body") {
      val code = """|type T
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on input without body") {
      val code = """|input X
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on interface without body") {
      val code = """|interface AAAAAAAAAAAAAAAAAInterface
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on scalar with body") {
      val code = """|scalar S {
                    |  i: Int
                    |}
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on scalar with union-like equality") {
      val code = """|scalar S = String | Int
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on enum without body") {
      val code = """|enum E
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on enum with type-like body") {
      val code = """|enum E {
                    |  x: String
                    |  y: Int
                    |}
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on enum with commas") {
      val code = """|enum SomeSpecialKindOfEnum { 2, 3, 4 }
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on enum with single type label") {
      val code = """|enum SomeSpecialKindOfEnum{ 2 3 4 XXXX: Int }
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on trailing semicolon") {
      val code = """|type X { x: String; }
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on missing interface name") {
      val code = """|interface { x: String }
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on missing bracket in array type") {
      val code = """|interface X { x: [String }
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on too much brackets in array type") {
      val code = """|interface X { x: [String]]] }
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on double nullability") {
      val code = """|type X { x: String!! }
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on no implements identifier") {
      val code = """|type X implements { x: Int! }
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on missing type keyword") {
      val code = """|X implements { x: String! }
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on only-implements specification") {
      val code = """|scalar X
                    |implements X { x: String! }
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on missing scalar name") {
      val code = """|scalar
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

    it("should fail on two neigbouring pipes in union type") {
      val code = """|union X = String | | Float | Int
                    |""".stripMargin

      assertThrows[ParserError[SyntaxError]] {
        SchemaParser().parse(code)
      }
    }

  }
}