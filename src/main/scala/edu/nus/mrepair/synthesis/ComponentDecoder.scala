package edu.nus.mrepair.synthesis

import Formula._
import ProgramFormula._
import ComponentFormula._
import edu.nus.mrepair.Utils
import edu.nus.mrepair.StatementLevelRepair

object ComponentDecoder {

  /**
    * Construct expressions from location variables assignment
    * @param assignment mapping from component variables to concrete values
    * @return mapping from binding variables to repaired expressions semantics
    */
  def decode(assignment: List[(ComponentVariable, Option[Value[ComponentVariable]])]):
      List[(ProgramVariable, ProgramFormulaExpression)] = {
    val locations =
      assignment.foldRight(List[(ConnectionVariable, Int, Int, Int)]())(
        (t: (ComponentVariable, Option[Value[ComponentVariable]]),
         l: List[(ConnectionVariable, Int, Int, Int)]) => t match {
          case (Location(ExecutionInstance(v, stmt, exe)), Some(IntegerValue(loc))) => (v, stmt, exe, loc) :: l
          case _ => l
        })

    //FIXME this is a very bad hack. global components currently correspond to stmtId=0, so, we need to add them to all other groups
    val tmpStmts = locations.groupBy({case (_, s, e, _) => (s, e)}).toList.sortBy({ case ((n, _), _) => n})
    val globalComponentsVars = tmpStmts.filter({ case ((n, _), _ ) => n == 0 }).map({ case (_, vars ) => vars }).flatten
    val stmts = tmpStmts.filter({ case ((n, e), _) => n != 0 && e != StatementLevelRepair.phantomExeId }).map({ case (n, vars) => (n, vars ++ globalComponentsVars)})

    stmts.map({ case ((stmt, exe), locals) =>
      val Some((BindingVariable(outputVar), _, _, inputLoc)) = locals.find({
        case (BindingVariable(_), _, _, _) => true
        case _ => false
      })
      def connectToInput(loc: Int): ProgramFormulaExpression = {
        locals.find({
          case (_: ComponentOutput, _, _, l) => l == loc
          case _ => false
        }) match {
          case Some((ComponentOutput(comp), _, _, _)) =>
            comp match {
              case vc: VariableComponent => Variable(vc.variable)
              case ic: IntegerConstantComponent => {
                assignment.find({
                  //TODO do we need to compare stmtId?
                  case (StatementInstance(ComponentOutput(c), _), _) => c.id == ic.id// && s == stmt 
                  case _ => false
                }) match {
                  case Some((_, Some(IntegerValue(value)))) => IntegerValue(value)
                  case None => 
                    if (Utils.verbose) println("[error] decoding: failed to find value of " + ic)
                    ???
                }
              }
              case bc: BooleanConstantComponent => {
                assignment.find({
                  //TODO do we need to compare stmtId?
                  case (StatementInstance(ComponentOutput(c), _), _) => c.id == bc.id// && s == stmt
                  case _ => false
                }) match {
                   case Some((_, Some(BooleanValue(value)))) => BooleanValue(value)
                  case None => 
                    if (Utils.verbose) println("[error] decoding: failed to find value of " + bc)
                    ???
                }
              }
              case f: FunctionComponent => {
                val inputsLocations = locals.filter({
                  case (ComponentInput(c, i), _, _, _) => c.id == f.id
                  case _ => false
                })
                val subexpressions = inputsLocations.map({
                  case (ComponentInput(c, i), _, _, l) => (i, connectToInput(l))
                })
                ProgramFormulaUtils.substitute[ProgramVariable](
                  f.semantics,
                  (v: ProgramVariable) => {
                    val index = f.inputs.indexOf(v)
                    val Some((_, expr)) = subexpressions.find({
                      case (i, _) => i == index
                    })
                    expr
                  },
                  (v: ProgramVariable) => Variable(v))
              }
            }
          case _ => {
            if (Utils.verbose) println("[error] decoding: reference to non-existing component (loc = " + loc + ")")
            Variable(ProgramVariable("?", BooleanType())) // for debugging
          }
        }
      }
      (outputVar, connectToInput(inputLoc))
    })
  }

}
