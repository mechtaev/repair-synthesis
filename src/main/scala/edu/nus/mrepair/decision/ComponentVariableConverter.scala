package edu.nus.mrepair.decision

import edu.nus.mrepair._
import edu.nus.mrepair.synthesis.{ProgramFormula, Formula, ComponentFormula}
import Formula._
import ProgramFormula._
import ComponentFormula._

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.token.StdTokens

object ComponentVariableConverter {

  var components: List[Component] = Nil

  def compById(id: Int): Component = {
    components.find(_.id == id) match {
      case Some(result) => result
      case None =>
        if (Utils.verbose) println("[warn] converting: failed to find component by id=" + id)
        // components.map({ 
        //   case c => 
        //     if (Utils.verbose) print("id " + c.id + " ")
        //     if (Utils.verbose) println(c)
        // })
        FunctionComponent(Nil, IntegerType(), Variable(ProgramVariable("<unknown component>", IntegerType())))
    }
  }

  private object Prefix {
    def loc       = 'l'
    def out       = 'o'
    def in        = 'i'
    def variable  = 'v'
    def component = 'c'
    def test      = 't'
    def stmt      = 's'
    def exe       = 'e'
    def testexe   = 'x'
    def sep       = '!'
    def int       = 'n'
    def pointer   = 'p'
    def bool      = 'b'
    def sepvar    = 'q'
  }

  def printVariableName(v: ComponentVariable): String = {
    v match {
      case Location(rv) =>
        "%s%s".format(Prefix.loc, printVariableName(rv))
      case TestInstance(rv, test) =>
        "%s%d%s%s".format(Prefix.test, test, Prefix.sep, printConnectionVariableName(rv))
      case StatementInstance(rv, stmt) =>
        "%s%d%s%s".format(Prefix.stmt, stmt, Prefix.sep, printConnectionVariableName(rv))
      case ExecutionInstance(rv, statement, execution) =>
        "%s%d%s%d%s%s".format(Prefix.exe, statement, Prefix.sep, execution, Prefix.sep, printConnectionVariableName(rv))
      case TestExecutionInstance(rv, test, statement, execution) =>
        "%s%d%s%d%s%d%s%s".format(Prefix.testexe, test, Prefix.sep, statement, Prefix.sep, execution, Prefix.sep, printConnectionVariableName(rv))
      case SeparatorVariable(index, groupId) =>
        "%s%s%s%s".format(Prefix.sepvar, "" + index, Prefix.sep, "" + groupId)
    }
  }

  def printProgramVariableName(v: ProgramVariable): String = {
    v match {
      case ProgramVariable(name, IntegerType()) =>
        "%s%s%s%s".format(Prefix.int, Prefix.variable, Prefix.sep, name)
      case ProgramVariable(name, BooleanType()) =>
        "%s%s%s%s".format(Prefix.bool, Prefix.variable, Prefix.sep, name)
      case ProgramVariable(name, PointerType()) =>
        "%s%s%s%s".format(Prefix.pointer, Prefix.variable, Prefix.sep, name)
    }
  }

  def printConnectionVariableName(v: ConnectionVariable): String = {
    v match {
      case BindingVariable(v: ProgramVariable) =>
        "%s".format(printProgramVariableName(v))
      case ComponentInput(comp, index) =>
        "%s%s%d%s%d".format(Prefix.component, Prefix.in, index, Prefix.sep, comp.id)
      case ComponentOutput(comp) =>
        "%s%s%s%d".format(Prefix.component, Prefix.out, Prefix.sep, comp.id)
    }
  }

  trait Z3Tokens extends StdTokens {
    case class VariableToken(v: ComponentVariable) extends Token {
      def chars: String = ComponentVariableConverter.printVariableName(v)
    }

    case class IntegerToken(i: Int) extends Token {
      def chars: String = i.toString
    }

    case class BooleanToken(b: Boolean) extends Token {
      def chars: String = b.toString
    }

    case class ArrowToken() extends Token {
      def chars: String = "->"
    }
  }

  object VariableLexical extends StdLexical with Z3Tokens {

    override def token: Parser[Token] =
      // Location variable:
      ( accept(Prefix.loc) ~> instanceWithLocation ^^ { case v => VariableToken(Location(v)) }
      | (accept(Prefix.sepvar) ~> num) ~ (accept(Prefix.sep) ~> num) ^^ { case index ~ groupId => VariableToken(SeparatorVariable(index, groupId)) }
      // Real variable:
      | instance ^^ { case v => VariableToken(v) }
      | num ^^ { case i => IntegerToken(i) }
      | bool ^^ { case b => BooleanToken(b) }
      | '(' ~> '-' ~> whitespaceChar.+ ~> num <~ ')' ^^ { case i => IntegerToken(-i) }
      | '-' ~ '>' ^^ { case _ => ArrowToken() })

    def ident: Parser[String] = (letter | '$' | '^') ~ (letter | digit | '_' | '@' | '$' | '^' | '#').* ^^ {
      case first ~ rest => first :: rest mkString ""
    }

    def num: Parser[Int] = digit.+ ^^ { case l => Integer.parseInt(l mkString "")}

    def bool: Parser[Boolean] =
      ('t' ~ 'r' ~ 'u' ~ 'e' ^^ {
        case _ => true
      }) |
        ('f' ~ 'a' ~ 'l' ~ 's' ~ 'e') ^^ {
          case _ => false
        }

    def instanceWithLocation: Parser[InstanceWithLocation] = 
      ( (accept(Prefix.stmt) ~> num) ~ (accept(Prefix.sep) ~> aux) ^^ {
          case stmt ~ rv => StatementInstance(rv, stmt)
        }
       |  (accept(Prefix.exe) ~> num) ~ (accept(Prefix.sep) ~> num) ~ (accept(Prefix.sep) ~> aux) ^^ {
             case stmt ~ exe ~ rv => ExecutionInstance(rv, stmt, exe)
         } )

    def instance: Parser[Instance] =
      // Test instance:
      ( accept(Prefix.test) ~> (num ~ (accept(Prefix.sep) ~> aux)) ^^ {
          case test ~ rv => TestInstance(rv, test)
        }
      | instanceWithLocation
        // Test execution instance:
      | ( accept(Prefix.testexe) ~> num) ~ (accept(Prefix.sep) ~> num) ~ (accept(Prefix.sep) ~> num) ~ (accept(Prefix.sep) ~> aux) ^^ {
            case test ~ stmt ~ testexe ~ rv => TestExecutionInstance(rv, test, stmt, testexe)
          } )

    def prVar: Parser[ProgramVariable] =
    ( (Prefix.int ~> accept(Prefix.variable)) ~> (accept(Prefix.sep) ~> ident) ^^ {
        case v => ProgramVariable(v mkString "", IntegerType())
      }
    | (Prefix.bool ~> accept(Prefix.variable)) ~> (accept(Prefix.sep) ~> ident) ^^ {
        case v => ProgramVariable(v mkString "", BooleanType())
      }
    | (Prefix.pointer ~> accept(Prefix.variable)) ~> (accept(Prefix.sep) ~> ident) ^^ {
        case v => ProgramVariable(v mkString "", PointerType())
      } )


    def aux: Parser[ConnectionVariable] =
      // Binding variable:
      (prVar ^^ { case v => BindingVariable(v) }
        // Component input:
      | accept(Prefix.component) ~> accept(Prefix.in) ~> (num ~ (accept(Prefix.sep) ~> num)) ^^ {
          case index ~ id => ComponentInput(compById(id).asInstanceOf[FunctionComponent], index)
        }
      // Component output:
      | accept(Prefix.component) ~> accept(Prefix.out) ~> accept(Prefix.sep) ~> num ^^ {
          case id => ComponentOutput(compById(id))
        })
  }

  object Z3OutputParser extends StandardTokenParsers {
    override val lexical = VariableLexical

    import lexical._

    def newline: Parser[Any] = ("\n" | "\r").+

    def formula: Parser[List[(ComponentVariable, Some[Value[ComponentVariable]])]] = assignment.*

    def assignment: Parser[(ComponentVariable, Some[Value[ComponentVariable]])] =
      variable ~ (arrow ~> (integer | boolean)) ^^ {
        case v ~ n => (v, Some(n))
      }

    def variable: Parser[ComponentVariable] =
      elem("variable", _.isInstanceOf[VariableToken]) ^^ {
        case a: VariableToken => a.v
      }

    def integer: Parser[Value[ComponentVariable]] =
      elem("integer", _.isInstanceOf[IntegerToken]) ^^ { case x: IntegerToken => IntegerValue(x.i) }

    def boolean: Parser[Value[ComponentVariable]] =
      elem("boolean", _.isInstanceOf[BooleanToken]) ^^ { case x: BooleanToken => BooleanValue(x.b) }

    def arrow: Parser[Any] = elem("arrow", _.isInstanceOf[ArrowToken])

    //TODO this part should be moved to Z3Text
    def parse(input: String, cs: List[Component]): List[(ComponentVariable, Some[Value[ComponentVariable]])] = {
      components = cs
      Z3OutputParser.formula(new lexical.Scanner(input)).get
    }

    def parseVariable(input: String, cs: List[Component]): ComponentVariable = {
      components = cs
      Z3OutputParser.variable(new lexical.Scanner(input)).get
    }
  }

}
