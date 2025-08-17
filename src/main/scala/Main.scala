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
      case scala.util.Success(text) => println(convertToMermaid(text))
      case scala.util.Failure(err)  => println(s"Error: $err")
    }
  }

  def fieldIsPrimitive(field: Field): Boolean = {
    !field.`type`.startsWith("@")
  }

  def primitiveToString(field: Field): String = {
    if (!field.array.getOrElse(false))
      "\n\t" + field.`type` + " " + field.name
    else
      "\n\t" + field.`type` + "[] " + field.name
  }

  def relationToNotation(struct: Struct, field: Field): String = {
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

  def structFieldsToMermaidNotation(struct: Struct): String = {
    s"""${struct.fields.filter(fieldIsPrimitive).map(primitiveToString).mkString}"""
  }

  def structToMermaidNotation(struct: Option[Struct]): String = struct match {
    case Some(e: Struct) =>
      val definitions: String = s"""\n${e.name.toUpperCase()} { ${structFieldsToMermaidNotation(e)}\n}\n\n"""
      val relations: String =
        s"""${e.fields.filterNot(fieldIsPrimitive).map(ee => relationToNotation(e, ee)).mkString}"""
      definitions + relations
    case _ => ""
  }

  def convertToMermaid(jsonString: String): String = {
    val result = decode[Schema](jsonString)
    result match {
      case Right(schema) =>
        s"""erDiagram\n${schema.schema.map(structToMermaidNotation).mkString}"""
      case Left(error) =>
        s"Failed to decode: $error"
    }
  }
}
