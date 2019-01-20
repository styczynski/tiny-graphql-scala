package parser.schema

import org.scalatest.FunSpec
import parser.SchemaParser
import parser.exceptions.{NotCompatibleTypesError, ParserError, TypeMissingError}
import parser.schema.types.{GraphQLArrayType, GraphQLCompositeType, GraphQLEnumType, GraphQLInterfaceType}

class SchemaInterfaceTest extends FunSpec {

  describe("A schema parser") {
    it("should handle basic primitives in interface definitions") {
      val code = """|interface Custom {
                    |    x: ID
                    |    t: Boolean
                    |    y: Int
                    |    z: Float
                    |}
                    |""".stripMargin
      val schema = SchemaParser().parse(code)

      assert(schema.findType("Custom") ==
        GraphQLInterfaceType(Some("Custom"))
          .withField("x", DefaultGraphQLSchema.ID)
          .withField("t", DefaultGraphQLSchema.BOOLEAN)
          .withField("y", DefaultGraphQLSchema.INT)
          .withField("z", DefaultGraphQLSchema.FLOAT)
      )
    }

    it("should handle composite interfaces with nested arrays") {
      val code = """|interface CustomNestedArrays {
                    |    x: String
                    |    y: [[[Int!]]!]!
                    |}
                    |""".stripMargin
      val schema = SchemaParser().parse(code)
      assert(schema.findType("CustomNestedArrays") ==
        GraphQLInterfaceType(Some("CustomNestedArrays"))
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

    it("should handle composite nested interfaces") {
      val code = """|interface NestedInterface {
                    |    data: [String!]
                    |}
                    |
                    |interface MyInterface {
                    |    x: [Int]
                    |    field: [NestedInterface!]
                    |}
                    |""".stripMargin
      val schema = SchemaParser().parse(code)

      val nestedInterfaceType = GraphQLInterfaceType(Some("NestedInterface"))
        .withField("data", GraphQLArrayType(DefaultGraphQLSchema.STRING.withNullability(false)))

      val myInterfaceType = GraphQLInterfaceType(Some("MyInterface"))
        .withField("x", GraphQLArrayType(DefaultGraphQLSchema.INT))
        .withField("field", GraphQLArrayType(nestedInterfaceType.withNullability(false)))

      assert(schema.findType("NestedInterface") == nestedInterfaceType)
      assert(schema.findType("MyInterface") == myInterfaceType)
    }

    it("should handle interface type implementations") {
      val code = """|interface MyInterface {
                    |    data: [ID]
                    |    z: String
                    |}
                    |
                    |type TypeImpls implements MyInterface {
                    |    data: [ID]
                    |    z: String
                    |    p: Int!
                    |}
                    |
                    |""".stripMargin

      val schema = SchemaParser().parse(code)

      val myInterfaceType = GraphQLInterfaceType(Some("MyInterface"))
        .withField("data", GraphQLArrayType(DefaultGraphQLSchema.ID))
        .withField("z", DefaultGraphQLSchema.STRING)

      val typeImplsType = GraphQLCompositeType(Some("TypeImpls"))
          .withInterface(Some(myInterfaceType))
          .withField("data", GraphQLArrayType(DefaultGraphQLSchema.ID))
          .withField("z", DefaultGraphQLSchema.STRING)
          .withField("p", DefaultGraphQLSchema.INT.withNullability(false))

      assert(schema.findType("MyInterface") == myInterfaceType)
      assert(schema.findType("TypeImpls") == typeImplsType)
    }

    it("should handle nested interface type implementations") {
      val code = """|interface MyInterface {
                    |    data: [Int]
                    |}
                    |
                    |interface MyInterface2 {
                    |    fields: [MyInterface]
                    |}
                    |
                    |type MyInterface2Impl implements MyInterface {
                    |    data: [Int]
                    |}
                    |
                    |type TypeImpls implements MyInterface2 {
                    |    fields: [MyInterface2Impl]
                    |}
                    |
                    |""".stripMargin
      val schema = SchemaParser().parse(code)

      val myInterfaceType = GraphQLInterfaceType(Some("MyInterface"))
        .withField("data", GraphQLArrayType(DefaultGraphQLSchema.INT))

      val myInterface2Type = GraphQLInterfaceType(Some("MyInterface2"))
        .withField("fields", GraphQLArrayType(myInterfaceType))

      val myInterface2ImplType = GraphQLCompositeType(Some("MyInterface2Impl"))
        .withInterface(Some(myInterfaceType))
        .withField("data", GraphQLArrayType(DefaultGraphQLSchema.INT))

      val typeImplsType = GraphQLCompositeType(Some("TypeImpls"))
        .withInterface(Some(myInterface2Type))
        .withField("fields", GraphQLArrayType(myInterface2ImplType))

      assert(schema.findType("MyInterface") == myInterfaceType)
      assert(schema.findType("MyInterface2") == myInterface2Type)
      assert(schema.findType("MyInterface2Impl") == myInterface2ImplType)
      assert(schema.findType("TypeImpls") == typeImplsType)
    }

    it("should throw on interface type missing field") {
      val code = """|interface MyInterface {
                    |    data: [Int]
                    |}
                    |
                    |type TypeImpls implements MyInterface2 {
                    |    fields: [MyInterface2Impl]
                    |}
                    |
                    |""".stripMargin

      assertThrows[ParserError[TypeMissingError]] {
        SchemaParser().parse(code)
      }
    }

    it("should throw on interface type incompatible field") {
      val code = """|interface MyInterface {
                    |    x: Int
                    |}
                    |
                    |type TypeImpls implements MyInterface {
                    |    x: String
                    |}
                    |
                    |""".stripMargin

      assertThrows[ParserError[NotCompatibleTypesError]] {
        SchemaParser().parse(code)
      }
    }

    it("should throw on interface type field with incompatible nullability") {
      val code = """|interface MyInterface {
                    |    x: Int!
                    |}
                    |
                    |type TypeImpls implements MyInterface {
                    |    x: Int
                    |}
                    |
                    |""".stripMargin

      assertThrows[ParserError[NotCompatibleTypesError]] {
        SchemaParser().parse(code)
      }
    }
  }
}