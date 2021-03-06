package parser.schema

import scala.language.implicitConversions
import shapeless.{::, HNil}
import org.parboiled2._

import parser.exceptions._
import parser.core.macros.ParserMacros.parserErr
import parser.schema.types._

final case class AnyIdentifier(name: String = "", isNullableValue: Boolean = true, typeInterface: Option[AnyIdentifier] = None) {
  def getName: String = name
  def isNullable: Boolean = isNullableValue
  def withNullability(shouldBeNullable: Boolean): AnyIdentifier = copy(isNullableValue = shouldBeNullable)
  def withInterface(typeInterfaceName: AnyIdentifier): AnyIdentifier = copy(typeInterface = Some(typeInterfaceName))
}

class SchemaParserCore(val input: ParserInput, val env: Option[GraphQLSchema] = None) extends Parser {
  type EnvE[A] = (GraphQLSchema, A)

  type RuleEnvT = Rule[GraphQLSchema :: HNil, GraphQLSchema :: HNil]
  type RuleEnvTE[A] = RuleT[EnvE[A]]
  type RuleT[A] = Rule[A :: HNil, A :: HNil]
  type RuleEnvE[A] = Rule[GraphQLSchema :: HNil, EnvE[A] :: HNil]
  type RuleEnvP[A] = Rule[GraphQLSchema :: HNil, GraphQLSchema :: A :: HNil]
  type RuleTypeAssignment[T] = Rule[GraphQLField :: GraphQLComposableType[T] :: GraphQLSchema :: HNil, EnvE[GraphQLComposableType[T]] :: HNil]

  def StackSwap[A, B]: () => (A, B) => B :: A :: HNil = () => (a: A, b: B) => b :: a :: HNil
  def StackId[A]: () => A => A :: HNil = () => (a: A) => a :: HNil
  def StackEnvE[A](a: A): GraphQLSchema => (GraphQLSchema, A) :: HNil = (env: GraphQLSchema) => (env, a) :: HNil

  def Input: Rule1[GraphQLSchema] = rule { push(env match {
    case Some(envValue) => envValue
    case None => DefaultGraphQLSchema.implement(new GraphQLSchema())
  }) ~ Whitespaces ~ zeroOrMore(Expression) ~ Whitespaces ~ EOI }

  implicit def wspStr(s: String): Rule0 = rule {
    Whitespaces ~ str(s) ~ Whitespaces
  }

  def printEnv(env: GraphQLSchema): GraphQLSchema = {
    println(env.toString)
    env
  }

  def Expression: RuleEnvT = rule {
    TypeDeclaration | InputDeclaration | EnumDeclaration | ScalarDeclaration | InterfaceDeclaration | UnionDeclaration | DebugStatement
  }

  def DebugStatement: RuleEnvT = rule {
    atomic(str("#print"))  ~> ((env: GraphQLSchema) => printEnv(env))
  }

  def Whitespaces: Rule0 = rule {
    quiet(zeroOrMore(str(" ") | str("\n") | str("\r") | str("\t")))
  }

  def EmptyIdentifier: Rule1[AnyIdentifier] = rule { push(AnyIdentifier()) }

  def Identifier: Rule1[AnyIdentifier] = rule { atomic(capture(Letters)) ~> ((ident: String) => AnyIdentifier(ident)) }

  def NullableToken: Rule0 = rule { Whitespaces ~ "!" ~ Whitespaces }

  def Nullability: RuleT[AnyIdentifier] = rule {
    NullableToken ~> ((ident: AnyIdentifier) => ident.withNullability(false))
  }

  def ImplementsToken: Rule0 = rule { Whitespaces ~ "implements" ~ Whitespaces }

  def Implements: RuleT[AnyIdentifier] = rule {
    ImplementsToken ~ Identifier ~> ((ident: AnyIdentifier, interfaceIdent: AnyIdentifier) => ident.withInterface(interfaceIdent))
  }

  def InterfaceDeclaration: RuleEnvT = rule {
    KeywordInterface ~
      Whitespaces ~
      Identifier ~>
      ((env: GraphQLSchema, ident: AnyIdentifier) => ident :: env :: HNil) ~
      Whitespaces ~
      InterfaceDefinition ~>
      ((typeName: AnyIdentifier, env: EnvE[GraphQLComposableType[GraphQLInterface]]) => parserErr(env._1.registerType(env._2.withName(typeName.getName))))
  }

  def ScalarDeclaration: RuleEnvT = rule {
    KeywordScalar ~
      Whitespaces ~
      Identifier ~>
      ((env: GraphQLSchema, typeName: AnyIdentifier) => parserErr(env.registerType(GraphQLScalarType(Some(typeName.getName)))))
  }

  def UnionDeclaration: RuleEnvT = rule {
    KeywordUnion ~
      Whitespaces ~
      Identifier ~>
      ((env: GraphQLSchema, ident: AnyIdentifier) => ident :: env :: HNil) ~
      Whitespaces ~
      '=' ~
      Whitespaces ~
      UnionDefinition ~>
      ((typeName: AnyIdentifier, env: EnvE[GraphQLType[_]]) => parserErr(env._1.registerType(env._2.withName(typeName.getName))))
  }

  def EnumDeclaration: RuleEnvT = rule {
    KeywordEnum ~
      Whitespaces ~
      Identifier ~>
      ((env: GraphQLSchema, ident: AnyIdentifier) => ident :: env :: HNil) ~
      Whitespaces ~
      EnumDefinition ~>
      ((typeName: AnyIdentifier, env: EnvE[GraphQLType[_]]) => parserErr(env._1.registerType(env._2.withName(typeName.getName))))
  }

  def EnumDefinition: RuleEnvE[GraphQLType[_]] = rule {
    '{' ~
      Whitespaces ~>
      ((env: GraphQLSchema) => (env, GraphQLEnumType())) ~
      zeroOrMore(
        Whitespaces ~ Identifier ~ Whitespaces ~> ((env: EnvE[GraphQLEnumType], valueName: AnyIdentifier) => (env._1, env._2.withValue(valueName.getName)))
      ) ~
      Whitespaces ~
      '}'
  }

  def UnionDefinition: RuleEnvE[GraphQLType[_]] = rule {
      Whitespaces ~>
      ((env: GraphQLSchema) => (env, GraphQLUnionType())) ~
      (
        Whitespaces ~ Identifier ~ Whitespaces ~> ((env: EnvE[GraphQLUnionType], valueName: AnyIdentifier) => (env._1, env._2.withType(GraphQLRefType(env._1, Some(valueName.getName)))))
      ) ~ zeroOrMore(
        '|' ~ Whitespaces ~ Identifier ~ Whitespaces ~> ((env: EnvE[GraphQLUnionType], valueName: AnyIdentifier) => (env._1, env._2.withType(GraphQLRefType(env._1, Some(valueName.getName)))))
      ) ~
      Whitespaces
  }

  def InputDeclaration: RuleEnvT = rule {
    KeywordInput ~
      Whitespaces ~
      Identifier ~>
      ((env: GraphQLSchema, ident: AnyIdentifier) => ident :: env :: HNil) ~
      Whitespaces ~
      InputDefinition ~>
      ((typeName: AnyIdentifier, env: EnvE[GraphQLType[_]]) => {
        val typeWithName = env._2.withName(typeName.getName)
        parserErr(env._1.registerType(typeWithName))
      })
  }

  def TypeDeclaration: RuleEnvT = rule {
    KeywordType ~
      Whitespaces ~
      Identifier ~
      optional(Implements) ~>
      ((env: GraphQLSchema, ident: AnyIdentifier) => ident :: env :: HNil) ~
      Whitespaces ~
      TypeDefinition ~>
      ((typeName: AnyIdentifier, env: EnvE[GraphQLType[_]]) => {
        val typeWithName = env._2.withName(typeName.getName)
        val typeWithInterface = typeName.typeInterface match {
          case Some(interfaceName) => typeWithName.withInterface(Some(GraphQLRefInterface(env._1, Some(interfaceName.getName))/*env._1.findInterface(interfaceName.getName)*/))
          case None => typeWithName
        }
        parserErr(env._1.registerType(typeWithInterface))
      })
  }

  def InterfaceDefinition: RuleEnvE[GraphQLComposableType[GraphQLInterface]] = rule {
    '{' ~
      Whitespaces ~>
      ((env: GraphQLSchema) => (env, GraphQLInterfaceType())) ~
      zeroOrMore(TypeAssignment[GraphQLInterface]) ~
      Whitespaces ~
      '}'
  }

  def InputDefinition: RuleEnvE[GraphQLComposableType[GraphQLCompositeInputType]] = rule {
    '{' ~
      Whitespaces ~>
      ((env: GraphQLSchema) => (env, GraphQLCompositeInputType())) ~
      zeroOrMore(InputTypeAssignment[GraphQLCompositeInputType]) ~
      Whitespaces ~
      '}'
  }

  def TypeDefinition: RuleEnvE[GraphQLComposableType[GraphQLCompositeType]] = rule {
    '{' ~
      Whitespaces ~>
      ((env: GraphQLSchema) => (env, GraphQLCompositeType())) ~
      zeroOrMore(TypeAssignment[GraphQLCompositeType]) ~
      Whitespaces ~
      '}'
  }

  def TypeAssignmentWhitespaces[T]: RuleEnvTE[GraphQLComposableType[T]] = rule {
    Whitespaces ~> ((env: EnvE[GraphQLComposableType[T]]) => env)
  }

  def TypeAssignmentArgs[T]: Rule[ EnvE[GraphQLComposableType[T]] :: GraphQLField :: HNil, EnvE[GraphQLComposableType[T]] :: GraphQLField :: HNil ] = rule {
    '(' ~
      zeroOrMore(
        Whitespaces ~
        Identifier ~
        Whitespaces ~
        ':' ~
        Whitespaces ~>
        ((env: EnvE[GraphQLComposableType[T]], field: GraphQLField, ident: AnyIdentifier) => env :: ident :: field :: env._1 :: HNil) ~
        (TypeNullableIdentifier | TypeArray) ~>
        ((env: EnvE[GraphQLComposableType[T]], ident: AnyIdentifier, field: GraphQLField, envComp: GraphQLSchema, typeValue: GraphQLType[_]) => env :: field.withArg(ident.getName, typeValue) :: HNil)
      ).separatedBy(",") ~
    ')'
  }

  /*zeroOrMore(
        ',' ~  Whitespaces ~ Identifier ~  Whitespaces ~ ':' ~ Whitespaces ~ Identifier ~ optional(Nullability) ~ Whitespaces ~>
        ((env: EnvE[GraphQLComposableType[T]], field: GraphQLField, ident: AnyIdentifier, typeIndent: AnyIdentifier) => env :: field.withArg(ident.getName, GraphQLRefType(env._1, Some(typeIndent.getName)).withNullability(typeIndent.isNullable)) :: HNil)
      ) ~*/

  def TypeAssignmentKey[T]: Rule[ EnvE[GraphQLComposableType[T]] :: HNil, EnvE[GraphQLComposableType[T]] :: GraphQLField :: HNil ] = rule {
    Identifier ~>
      ((env: EnvE[GraphQLComposableType[T]], fieldName: AnyIdentifier) => env :: GraphQLField(fieldName.getName, Map(), null) :: HNil) ~
      Whitespaces
  }

  def TypeAssignmentKeyWithArgs[T]: Rule[ EnvE[GraphQLComposableType[T]] :: HNil, EnvE[GraphQLComposableType[T]] :: GraphQLField :: HNil ] = rule {
    TypeAssignmentKey[T] ~
      optional(TypeAssignmentArgs[T])
  }

  def TypeAssignment[T]: Rule[EnvE[GraphQLComposableType[T]] :: HNil, EnvE[GraphQLComposableType[T]] :: HNil] = rule {
    Whitespaces ~
      TypeAssignmentKeyWithArgs[T] ~
      Whitespaces ~
      ':' ~
      Whitespaces ~>
      ((env: EnvE[GraphQLComposableType[T]], field: GraphQLField) => field :: env._2 :: env._1 :: HNil) ~
      (
        TypeAssignmentInline[T] | TypeAssignmentIdentifier[T] | TypeAssignmentArray[T]
      ) ~
      Whitespaces
  }

  def InputTypeAssignment[T]: Rule[EnvE[GraphQLComposableType[T]] :: HNil, EnvE[GraphQLComposableType[T]] :: HNil] = rule {
    Whitespaces ~
      TypeAssignmentKey[T] ~
      Whitespaces ~
      ':' ~
      Whitespaces ~>
      ((env: EnvE[GraphQLComposableType[T]], field: GraphQLField) => field :: env._2 :: env._1 :: HNil) ~
      (
        TypeAssignmentInline[T] | TypeAssignmentIdentifier[T] | TypeAssignmentArray[T]
        ) ~
      Whitespaces
  }

  def TypeAssignmentInline[T]: RuleTypeAssignment[T] = rule {
    TypeDefinition ~>
      ((field: GraphQLField, topType: GraphQLComposableType[T], env: EnvE[GraphQLType[_]]) => (env._1, topType.withField(field.withType(env._2))))
  }

  def TypeAssignmentIdentifier[T]: RuleTypeAssignment[T] = rule {
    Identifier ~
      optional(Nullability) ~>
      ((field: GraphQLField, topType: GraphQLComposableType[T], env: GraphQLSchema, typeName: AnyIdentifier) => (env, topType.withField(field.withType(GraphQLRefType(env, Some(typeName.getName)).withNullability(typeName.isNullable)))))
  }

  def IdentifierArray: RuleEnvP[GraphQLType[_]] = rule {
    '[' ~
      Whitespaces ~
      Identifier ~
      optional(Nullability) ~
      Whitespaces ~
      ']' ~
      EmptyIdentifier ~
      optional(Nullability) ~>
      ((env: GraphQLSchema, typeName: AnyIdentifier, selfName: AnyIdentifier) => env :: parserErr(GraphQLArrayType(GraphQLRefType(env, Some(typeName.getName)).withNullability(typeName.isNullable)).withNullability(selfName.isNullable)) :: HNil)
  }

  def TypeNullableIdentifier: RuleEnvP[GraphQLType[_]] = rule {
    Identifier ~
      optional(Nullability) ~>
      ((env: GraphQLSchema, typeName: AnyIdentifier) => env :: GraphQLRefType(env, Some(typeName.getName)).withNullability(typeName.isNullable) :: HNil)
  }

  def TypeArray: RuleEnvP[GraphQLType[_]] = rule {
    IdentifierArray | (
      '[' ~
        Whitespaces ~
        (TypeArray | IdentifierArray) ~
        Whitespaces ~
        ']' ~
        EmptyIdentifier ~
        optional(Nullability) ~>
        ((subType: GraphQLType[_], selfName: AnyIdentifier) => parserErr(GraphQLArrayType(subType).withNullability(selfName.isNullable)))
      )
  }

  def TypeAssignmentArray[T]: RuleTypeAssignment[T] = rule {
    TypeArray ~>
      ((field: GraphQLField, topType: GraphQLComposableType[T], env: GraphQLSchema, subType: GraphQLType[_]) => (env, topType.withField(field.withType(subType))))
  }

  def Letters: Rule0 = rule { oneOrMore(CharPredicate.AlphaNum) }

  def KeywordType: Rule0 = rule { atomic("type") }

  def KeywordInput: Rule0 = rule { atomic("input") }

  def KeywordInterface: Rule0 = rule { atomic("interface") }

  def KeywordEnum: Rule0 = rule { atomic("enum") }

  def KeywordUnion: Rule0 = rule { atomic("union") }

  def KeywordScalar: Rule0 = rule { atomic("scalar") }

  def Number: Rule1[Int] = rule { capture(Digits) ~> ((_ : String).toInt) }

  def Digits: Rule0 = rule { oneOrMore(CharPredicate.Digit) }
}