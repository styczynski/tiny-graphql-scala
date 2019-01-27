package parser.schema.types

abstract sealed class GraphQLTypeDirection {
  def getShortName: String
  override def toString = s"$getShortName type"
  def ~~ (graphQLTypeDirection: GraphQLTypeDirection): Boolean = false
  def !~ (graphQLTypeDirection: GraphQLTypeDirection): Boolean = !(this ~~ graphQLTypeDirection)
}

final case class GraphQLDirectionInput() extends GraphQLTypeDirection {
  override def getShortName: String = "input"
  override def ~~ (graphQLTypeDirection: GraphQLTypeDirection): Boolean = graphQLTypeDirection match {
    case GraphQLDirectionInput() => true
    case GraphQLDirectionOutput() => false
    case GraphQLDirectionUni() => true
  }
}

final case class GraphQLDirectionOutput() extends GraphQLTypeDirection {
  override def getShortName: String = "output"
  override def ~~ (graphQLTypeDirection: GraphQLTypeDirection): Boolean = graphQLTypeDirection match {
    case GraphQLDirectionInput() => false
    case GraphQLDirectionOutput() => true
    case GraphQLDirectionUni() => true
  }
}

final case class GraphQLDirectionUni() extends GraphQLTypeDirection {
  override def getShortName: String = "input/output"
  override def ~~ (graphQLTypeDirection: GraphQLTypeDirection): Boolean = true
}