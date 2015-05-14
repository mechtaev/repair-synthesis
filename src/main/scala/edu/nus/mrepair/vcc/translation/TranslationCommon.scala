package edu.nus.mrepair.vcc.translation

import org.smtlib.IExpr
import edu.nus.vctrans.ast._
import org.smtlib.IExpr._
import scala.collection.JavaConverters._
import org.smtlib.impl.SMTExpr
import edu.nus.mrepair.Utils._
import edu.nus.vctrans.SimpleKeyword
import org.smtlib.command.{C_assert, C_define_fun}
import org.smtlib.sexpr.ISexpr
import org.smtlib.sexpr.ISexpr.ISeq
import org.smtlib.sexpr.Sexpr
import org.smtlib.sexpr.ISexpr.IToken
import org.smtlib.sexpr.Lexer.LexSymbol


/**
 * Created by seryozha on 4/10/14.
 */
object TranslationCommon {

  def LblPos = new SimpleKeyword() {
    override def value() = {
      ":lblpos"
    }
  }

  def LblNeg = new SimpleKeyword() {
    override def value() = {
      ":lblneg"
    }
  }

  def CmpKind = new SimpleKeyword() {
    override def value() = {
      ":cmp-kind"
    }
  }

  def CmpType = new SimpleKeyword() {
    override def value() = {
      ":cmp-type"
    }
  }

  def CmpId = new SimpleKeyword() {
    override def value() = {
      ":cmp-id"
    }
  }

  def CmpBound = new SimpleKeyword() {
    override def value() = {
      ":cmp-bound"
    }
  }

  def CmpExe = new SimpleKeyword() {
    override def value() = {
      ":cmp-exe"
    }
  }  
  
  def Pattern = new SimpleKeyword() {
    override def value() = {
      ":pattern"
    }
  }


  case class Procedure(name: String,
                       assertion: CFunctionAst,
                       instances: List[DefineFunAst],
                       instancesOrder: List[Int]) {
    override def toString: String = {
      "procedure " + name + "\n" +
        "\nassertion:\n" + assertion.toString +
        "\n\ninstances:\n" + prettyList(instances)
    }

  }

  case class TF(sorts: List[DeclareSortAst],
                declarations: List[DeclareFunAst],
                asserts: List[AssertAst],
                procedures: List[Procedure],
                globalDecls: List[C_assert]) {
    override def toString: String = {
      "sorts:\n" + prettyList(sorts) +
        "\n\ndecls:\n" + prettyList(declarations) +
        "\n\nasserts:\n" + prettyList(asserts) +
        "\n\nprocedures:\n" + prettyList(procedures)
    }
  }

  //FIXME temporary function
  def getAllDefines(tf: TF): List[DefineFunAst] = {
    tf.procedures.map((p: Procedure) => p.instances).flatten
  }

  object SMTLIBExpr {

    // do not traverse child symbols such as functions names, semantics for let is a bit wierd
    def fold[T](e: IExpr, f: IExpr => List[T] => T) : T = {
      e match {
        case exp: IAttributedExpr => f(e)(List[T](fold(exp.expr(), f)))
        case exp: IBinaryLiteral => ???
        case exp: IDecimal => ???
        case exp: IError => ???
        case exp: IExists => ???
        case exp: IFcnExpr => f(e)(exp.args().asScala.toList.map((arg: IExpr) => fold(arg, f)))
        case exp: IForall => f(e)(List[T](fold(exp.expr(), f)))
        case exp: IHexLiteral => ???
        case exp: ILet =>
          val bindings = exp.bindings().asScala.toList
          val bindingsResults = bindings.map((b: IBinding) => fold(b.expr(), f))
          f(e)(bindingsResults ++ List[T](fold(exp.expr(), f)))
        case exp: INumeral => f(e)(List[T]())
        case exp: IParameterizedIdentifier => ???
        case exp: IAsIdentifier => ???
        case exp: IStringLiteral => ???
        case exp: ISymbol => f(e)(List[T]())
      }
    }
    
    //hack
    def mapSexpression(f: IExpr => IExpr, e: ISexpr): ISexpr = {
      e match {
        case seq: ISeq => new Sexpr.Seq(seq.sexprs().asScala.toList.map(mapSexpression(f, _)).asJava)
        case t: IToken[_] => 
          new Sexpr.Token(f(new SMTExpr.Symbol(t.value().toString)).asInstanceOf[ISymbol].headSymbol().value())
      }
    }

    def mapT(e: IExpr, f: IExpr => IExpr): IExpr = {
      def t: (IExpr => List[IExpr] => IExpr) = {
        case exp: IAttributedExpr => {
          case inner :: Nil =>
            val newAttributes = exp.attributes().asScala.map({
              case a => 
                if (a.keyword().equals(Pattern)) 
                  new SMTExpr.Attribute(a.keyword(), mapSexpression(f, a.attrValue().asInstanceOf[ISexpr]))
                else
                  a
            }).asJava
            f(new SMTExpr.AttributedExpr(inner, newAttributes))
        }
        case exp: IFcnExpr => {
          case args =>
            val newHead = f(exp.head().headSymbol()).asInstanceOf[ISymbol]
            f(new SMTExpr.FcnExpr(newHead, args.asJava))
        }
        case exp: IForall => {
          case inner :: Nil =>
           	val newParams = exp.parameters().asScala.toList.map({
           	  case decl: IDeclaration => 
           	     new SMTExpr.Declaration(f(decl.parameter()).asInstanceOf[ISymbol], decl.sort()).asInstanceOf[IDeclaration]
           	}).asJava
           	f(new SMTExpr.Forall(newParams, inner))
        }
        case exp: ILet => {
          case args =>
            val params = exp.bindings().asScala.toList.map((b: IBinding) => b.parameter())
            val newParams = params.map({ case p => f(p) })
            val (bindingExprs, body :: Nil) = args.splitAt(args.length - 1)
            val newBindings =
              newParams.zip(bindingExprs).map({
                case (p, b) => new SMTExpr.Binding(p.asInstanceOf[ISymbol], b).asInstanceOf[IBinding]
              })
            f(new SMTExpr.Let(newBindings.asJava, body))
        }
        case exp: ISymbol => {
          case Nil => f(exp)
        }
        case exp: INumeral => {
          case Nil => f(exp)
        }
        case _ => ???
      }
      fold[IExpr](e, t)
    }

  }

}
