package edu.nus.mrepair.vcc.translation

import org.smtlib._
import org.smtlib.IExpr
import org.smtlib.command.C_assert
import org.smtlib.command.C_declare_fun
import org.smtlib.command.C_declare_sort
import org.smtlib.command.C_define_fun
import org.smtlib.command.C_set_info
import edu.nus.vctrans.ast.AssertAst
import edu.nus.vctrans.util.TransUtil
import edu.nus.vctrans.ast.CFunctionAst
import edu.nus.vctrans.ast.DeclareFunAst
import edu.nus.vctrans.ast.DeclareSortAst
import edu.nus.vctrans.ast.DefineFunAst
import edu.nus.vctrans.ast.IVcAst
import org.smtlib.IExpr.IFcnExpr
import edu.nus.vctrans.util.MutableBoolean
import edu.nus.vctrans._
import org.smtlib.impl.SMTExpr
import org.smtlib.ICommand.IScript
import scala.collection.JavaConversions._
import edu.nus.mrepair.vcc.translation.TranslationCommon.{TF, Procedure}
import org.smtlib.command.{C_assert, C_define_fun}
import edu.nus.mrepair.Utils

object TFGenerator {

  def BoogieVcId = new SimpleKeyword() {
    override def value(): String = {
      ":boogie-vc-id"
    }
  }

  protected var funMap: java.util.Map[String, CFunctionAst] = new java.util.HashMap[String, CFunctionAst]
  protected var expectFunctionBody: Boolean = false

  def translate(script: IScript, needUnfoldLet: Boolean, gDecls: List[C_assert]): TF = {
    try {

      var curFun: CFunctionAst = null
      var curFunName: String = null

      def simplifyBodyFormula(expr: IExpr): IExpr = expr.accept(new FormulaSimplifier)

      def unfoldFunBodyFormula(expr: IExpr): IExpr = {
        assert(expr.isInstanceOf[IExpr.IFcnExpr])
        val funExpr = expr.asInstanceOf[IExpr.IFcnExpr]
        val args: java.util.List[IExpr] = funExpr.args
        assert(args.size == 1)
        val arg: IExpr = args.get(0)
        assert(arg.isInstanceOf[IExpr.ILet])
        val unfolded: IExpr = unfoldLet(arg.asInstanceOf[IExpr.ILet], new java.util.Stack[java.util.List[IExpr.IBinding]])
        assert(unfolded != null)
        unfolded
      }


      def transformBodyFormula(formula: IExpr): IExpr = {
        if (needUnfoldLet) formula.accept(new FormulaTransformer(curFun))
        else formula.accept(new LetFormulaTransformer(curFun))
      }

      def transformCommand(e: ICommand): IVcAst = {
        e match {
          case setInfo: C_set_info =>
            val option: IExpr.IKeyword = setInfo.option
            if (option == BoogieVcId) {
              val value: IExpr.IAttributeValue = setInfo.value
              assert(value != null)
              curFunName = value.toString
              curFun = new CFunctionAst(curFunName)
              funMap.put(curFunName, curFun)
              expectFunctionBody = true
            }
            return null
          case asrt: C_assert =>
            if (expectFunctionBody) {
              //FIXME: I disabled unfolding to improve performance
              val negated: IExpr = 
                if (needUnfoldLet) unfoldFunBodyFormula(asrt.expr)
                else asrt.expr.asInstanceOf[IFcnExpr].args().get(0)
              assert(TransUtil.isNotExpr(asrt.expr.asInstanceOf[IFcnExpr]))
              val filtered: IExpr = transformBodyFormula(negated)
              val simplified: IExpr = simplifyBodyFormula(filtered)
              curFun.setExpr(simplified)
              expectFunctionBody = false
              return curFun
            }
            else {
              val expr: IExpr = asrt.expr
              expr match {
                case attExp: SMTExpr.AttributedExpr =>
                  for (att <- attExp.attributes()) {
                    val keyword = att.keyword
                    if (keyword.value == ":assert-type") {
                      val attVal = att.attrValue()
                      if ((attVal.toString == "GlobalConstraint") || (attVal.toString == "GlobalArrayConstraint")) {
                        val ast: AssertAst = new AssertAst(asrt)
                        return ast
                      }
                    }
                  }
                case _ => ()
              }
            }
          case decFun: C_declare_fun =>
            val ast: DeclareFunAst = new DeclareFunAst(decFun)
            return ast
          case decSort: C_declare_sort =>
            val ast: DeclareSortAst = new DeclareSortAst(decSort)
            return ast
          case defFun: C_define_fun =>
            val ast: DefineFunAst = new DefineFunAst(defFun)
            return ast
          case _ =>
        }
        null
      }


      var sorts = List[DeclareSortAst]()
      var declarations = List[DeclareFunAst]()
      var asserts =  List[AssertAst]()
      var funAssertions = List[(String, CFunctionAst)]()
      var defines = List[(DefineFunAst, Int)]()
      var defIndex = 0

      // transforming commands
      for (command <- script.commands) {
        val ast: IVcAst = transformCommand(command)
        if (ast != null) {
          ast match {
            case cfun: CFunctionAst =>
              funAssertions = funAssertions ++ ((cfun.getName, cfun) :: Nil)
            case declare: DeclareFunAst =>
              declarations = declarations ++ (declare :: Nil)
            case sort: DeclareSortAst =>
              sorts = sorts ++ (sort :: Nil)
            case defFunAst: DefineFunAst =>
              var bodyExp: IExpr = defFunAst.getBodyExp
              bodyExp = bodyExp match {
                //FIXME: I disabled unfolding to improve performance
                case letExp: IExpr.ILet if needUnfoldLet =>
                   unfoldLet(letExp, new java.util.Stack[java.util.List[IExpr.IBinding]])
                case _ =>
                  bodyExp
              }
              val filtered = transformBodyFormula(bodyExp)
              val simplified: IExpr = simplifyBodyFormula(filtered)
              defFunAst.setBody(simplified)
              defines = defines ++ ((defFunAst, defIndex) :: Nil)
              defIndex = defIndex + 1
            case assertAst: AssertAst =>
              asserts = asserts ++ (assertAst :: Nil)
            case _ =>
              if (Utils.verbose) println("[warn] skipping command:")
              if (Utils.verbose) println(command)
          }
        }
      }

      //constructing TF
      val procedures = funAssertions.map({
        case (name, cast) =>
          val (instances, order) = defines.filter({
            case (define, index) =>
              define.getName.startsWith(name) && define.getName.charAt(name.length) == '@'
          }).unzip
          Procedure(name, cast, instances, order)

      })

      val tf = TF(sorts, declarations, asserts, procedures, gDecls)

      tf

    }
    catch {
      case e: IVisitor.VisitorException => {
        throw new Error("Failed for VC translation")
      }
    }
  }


  def unfoldLet(letExp: IExpr.ILet, bindingsStack: java.util.Stack[java.util.List[IExpr.IBinding]]): IExpr = {
    val bindings: java.util.List[IExpr.IBinding] = letExp.bindings
    bindingsStack.push(bindings)
    val rst: IExpr = letExp.accept(new DefaultVisitor[IExpr] {
      override def visit(e: IExpr.ILet): IExpr = {
        val expr: IExpr = e.expr
        expr match {
          case letExpr: IExpr.ILet =>
            unfoldLet(letExpr, bindingsStack)
          case _ =>
            unfoldLetExpr(expr)
        }
      }

      private def unfoldLetExpr(original: IExpr): IExpr = {
        val changed: MutableBoolean = new MutableBoolean(false)
        val unfolded: IExpr = original.accept(new IdIExprVisitor {
          override def visit(e: IExpr.IFcnExpr): IExpr = {
            val args: java.util.List[IExpr] = e.args
            val newArgs: java.util.List[IExpr] = new java.util.LinkedList[IExpr]
            for (arg <- args) {
              val alreadyChecked: Boolean = checked.contains(arg)
              val oldHash: Int = arg.hashCode
              val newArg: IExpr = if (alreadyChecked) arg else arg.accept(this)
              assert(newArg != null)
              newArgs.add(newArg)
              if (oldHash != newArg.hashCode) {
                changed.setValue(true)
              }
              else {
                checked.add(arg)
              }
            }
            if (changed.value) {
              val head: IExpr.IQualifiedIdentifier = e.head
              new SMTExpr.FcnExpr(head, newArgs)
            }
            else {
              e.setArgs(newArgs)
              e
            }
          }

          override def visit(e: IExpr.ISymbol): IExpr = {
            val term: IExpr = getTerm(e)
            changed.setValue(true)
            if (term == null) {if (Utils.verbose) println("IMPOSSIBLE!!!"); e} else term
          }

          def getTerm(e: IExpr.ISymbol): IExpr = {
            for (bindingList <- bindingsStack) {
              for (binding <- bindingList) {
                val param: IExpr.ISymbol = binding.parameter
                if (param == e) {
                  val expr: IExpr = binding.expr
                  return expr
                }
              }
            }
            e
          }
        })
        assert(unfolded != null)
        if (changed.value) {
          unfoldLetExpr(unfolded)
        }
        else {
          unfolded
        }
      }

      private final val checked: java.util.Set[IExpr] = new java.util.HashSet[IExpr]
    })
    assert(rst != null)
    rst
  }

}
