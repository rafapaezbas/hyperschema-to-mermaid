import scala.io.Source
import scala.util.Try
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._

object Main {

  case class Field(name: String, required: Option[Boolean], array: Option[Boolean], `type`: String, version: Int)
  case class Struct(name: String, namespace: String, compact: Boolean, flagsPosition: Int, fields: List[Field])
  case class Schema(version: Int, schema: List[Option[Struct]])

  def main(args: Array[String]): Unit = {
    val content = Try {
      val source = Source.fromFile(args(0))
      try source.getLines().mkString("\n")
      finally source.close()
    }

    content match {
      case scala.util.Success(json)      => println(schemaToMermaid(json))
      case scala.util.Failure(exception) => println(s"Error: $exception")
    }
  }

  def isPrimitive(field: Field): Boolean = !field.`type`.startsWith("@")

  def primitiveToString(field: Field): String = {
    val arraySuffix = if (field.array.getOrElse(false)) "[]" else ""
    val required = if (field.require.getOrElse(false)) "\"Required\"" else ""
    s"\n\t${field.`type`}$arraySuffix ${field.name} $required"
  }

  def relationNotation(struct: Struct, field: Field): String = {
    field.array match {
      case Some(b) =>
        if (b)
          s"""${struct.name.toUpperCase()} ||--o{ ${field.`type`.split("/").last.toUpperCase()} : "has"\n"""
        else
          s"""${struct.name.toUpperCase()} ||--|| ${field.`type`.split("/").last.toUpperCase()} : "has"\n"""
      case None =>
        s"""${struct.name.toUpperCase()} ||--|| ${field.`type`.split("/").last.toUpperCase()} : "has"\n"""
    }
  }

  def fieldsToMermaid(struct: Struct): String =
    struct.fields.filter(isPrimitive).map(primitiveToString).mkString

  def structToMermaid(struct: Option[Struct]): String = struct match {
    case Some(e: Struct) =>
      val definitions: String = s"""\n${e.name.toUpperCase()} { ${fieldsToMermaid(e)}\n}\n\n"""
      val relations: String =
        s"""${e.fields.filterNot(isPrimitive).map(ee => relationNotation(e, ee)).mkString}"""
      definitions + relations
    case _ => ""
  }

  def schemaToMermaid(json: String): String = {
    decode[Schema](json) match {
      case Right(schema) => s"erDiagram\n${schema.schema.map(structToMermaid).mkString}"
      case Left(error)   => s"Failed to decode: $error"
    }
  }
}
