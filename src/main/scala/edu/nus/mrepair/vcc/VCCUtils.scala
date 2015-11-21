package edu.nus.mrepair.vcc

import edu.nus.mrepair.synthesis.ProgramFormula._
import edu.nus.mrepair._
import org.smtlib.IExpr
import org.smtlib.IExpr.ISymbol
import org.smtlib._
import edu.nus.mrepair.synthesis.Formula._
import edu.nus.mrepair.vcc.translation.TranslationCommon._
import scala.collection.JavaConverters._
import edu.nus.vctrans.ast.DefineFunAst
import edu.nus.vctrans.DefaultVisitor
import edu.nus.vctrans.ast.DeclareFunAst
import org.smtlib.impl.SMTExpr
import edu.nus.mrepair.Utils
import edu.nus.mrepair.StatementLevelRepair.RepairableBindings


object VCCUtils {

  val BoolTypeName = "Bool"
  val IntTypeName  = "Int"
  val PointerTypeName = "T@$ptr"

  case class TFRepairableExpressions(inGlobalDecls: List[RepairableBindings],
                                     byFunction: Map[String, RepairableBindings])

  def buildDeclsIndex(decls: List[DeclareFunAst]): Map[String, DeclareFunAst] = {
    decls.map({ case ast: DeclareFunAst => (ast.getName, ast) }).toMap
  }

  private def builtinSymbols =
    Set("+", "-", "*", "/", ">", "<", ">=", "<=", "=",
        "and", "or", "iff", "ite", "not", "=>", "xor",
        "select", "true", "false", "store")

  def renameExpr(e: IExpr, r: (String => String)): IExpr = {
    SMTLIBExpr.mapT(e, {
      case s: ISymbol =>
        if (builtinSymbols.contains(s.value())) s
        else new SMTExpr.Symbol(r(s.value()))
      case o => o
    })
  }

  def collectVarNames(e: IExpr): List[String] = {
    SMTLIBExpr.fold[List[String]](e, {
      case s: ISymbol =>
        if (builtinSymbols.contains(s.value())) { case l => l.flatten }
        else { case l => s.value() :: l.flatten }
      case o => { case l => l.flatten }
    })
  }

  def getVariableRenamerByType(id: Int, typ: Option[Type]): (String => String) = {
    (name: String) => typ match {
      //FIXME hardcoded values
      //these three are for synthesis:
      case Some(IntegerType()) => "|" + "t" + id.toString + "!" + "n" + "v!" + name + "|"
      case Some(BooleanType()) => "|" + "t" + id.toString + "!" + "b" + "v!" + name + "|"
      case Some(PointerType()) => "|" + "t" + id.toString + "!" + "p" + "v!" + name + "|"
      //this is for other symbols:
      case None => genericRenamer(id)(name)
    }
  }

  def genericRenamer(id: Int): (String => String) = {
    (name: String) => "|" + "test" + id.toString + "!" + name + "|"
  }

  /**
    * Rename variables for different test instances
    */
  def getVariableRenamer(id: Int, typeOf: String => Option[Type]): (String => String) = {
    (name: String) => getVariableRenamerByType(id, typeOf(name))(name)
  }

  /**
   * Rename variables for different test instances
   */
  def getProgramVariableRenamer(id: Int): (ProgramVariable => String) = {
    (p: ProgramVariable) => getVariableRenamerByType(id, Some(p.typ))(p.name)
  }


  /**
   * Rename variables and functions for different test instances
   */
  def getSymbolRenamer(id: Int, tf: TF, declsIndex: Map[String, DeclareFunAst], repairable: RepairableBindings): (String => String) = {
    case name =>
      getAllDefines(tf).find({
        case d: DefineFunAst => d.getName == name
        case _ => false
      }) match {
        case Some(d) => genericRenamer(id)(name)
        case _ =>
          val typeOf = getTypeOf(declsIndex, repairable)
          getVariableRenamer(id, typeOf)(name)
      }
  }


  def getDeclBuilder(id: Int): (ProgramVariable => String) = {
    val rename = getProgramVariableRenamer(id)
    (p: ProgramVariable) => p.typ match {
      case IntegerType() => "(declare-fun " + rename(p) + " () " + IntTypeName + ")"
      case BooleanType() => "(declare-fun " + rename(p) + " () " + BoolTypeName + ")"
      case PointerType() => "(declare-fun " + rename(p) + " () " + PointerTypeName + ")"
      case _ =>
        if (Utils.verbose) println("[error] cannot create declaration for variable " + p.name)
        ???
    }
  }

  //TODO support true/false which are instances of ISymbol
  /**
    * Check whether repairable and translate to our AST
    */
  class RepairableTranslator(typeOf: String => Option[Type]) extends DefaultVisitor[ProgramFormulaExpression] {

    def binOpByString(op: String): BinaryOperator = {
      op match {
        case "or"  => Or()
        case "and" => And()
        case "="   => Equal()
        case "<"   => Less()
        case "<="  => LessOrEqual()
        case ">"   => Greater()
        case ">="  => GreaterOrEqual()
        case "+"   => Add()
        case "-"   => Sub()
        case "*"   => Mult()
        case "/"   => Div()
        case "=>"  => Impl()
        case "iff" => Iff()
      }
    }

    def unOpByString(op: String): UnaryOperator = {
      op match {
        case "not" => Not()
        case "-"   => Neg()
      }
    }

    override def visit(e: IExpr.IFcnExpr): ProgramFormulaExpression = {
      val op = e.head.headSymbol.value
      if(builtinSymbols.contains(op)) {
        e.args.size() match {
          case 1 => UnaryOperation(unOpByString(op), e.args.get(0).accept(this))
          case 2 => BinaryOperation(binOpByString(op), e.args.get(0).accept(this), e.args.get(1).accept(this))
          case 3 => 
            assert(op == "ite")
            Ite(e.args.get(0).accept(this), e.args.get(1).accept(this), e.args.get(2).accept(this))
        }
      } else {
        if (Utils.verbose) println("[warn] using uninterpreted function: " + op)
        UninterpretedFunctionApplication(Variable(ProgramVariable(op, typeOf(op).get)), 
                                         e.args().asScala.toList.map(_.accept(this)))
      }
    }

    override def visit(e: IExpr.ISymbol): ProgramFormulaExpression = {
      if (e.value == "true") return BooleanValue(true)
      if (e.value == "false") return BooleanValue(false)
      Variable(ProgramVariable(e.value, typeOf(e.value).get))
    }

    override def visit(e: IExpr.INumeral): ProgramFormulaExpression = {
      IntegerValue(e.intValue)
    }

  }

  /*
   * Find all variables that should be of the boolean type. Get expected outout type. Return actual output type.
   */
  class RepairableBooleanInference(topType: Boolean) extends DefaultVisitor[Boolean] {

    val booleans = scala.collection.mutable.SortedSet[String]()
    var justNowWasBooleanOperation = topType

    def getConstraints(): Set[String] = {
      booleans.toSet
    }

    //TODO: I should refactor this (too many repetitions)
    def binOpByString(op: String): BinaryOperator = {
      op match {
        case "or"  => Or()
        case "and" => And()
        case "="   => Equal()
        case "<"   => Less()
        case "<="  => LessOrEqual()
        case ">"   => Greater()
        case ">="  => GreaterOrEqual()
        case "+"   => Add()
        case "-"   => Sub()
        case "*"   => Mult()
        case "/"   => Div()
        case "=>"  => Impl()
        case "iff" => Iff()
      }
    }

    def unOpByString(op: String): UnaryOperator = {
      op match {
        case "not" => Not()
        case "-"   => Neg()
      }
    }

    def setFlagByOp(op: Operator): Unit = {
      Types.opArgsType(op) match {
        case BooleanType() =>
          justNowWasBooleanOperation = true
        case _ =>
          justNowWasBooleanOperation = false
      }
    }


    override def visit(e: IExpr.IFcnExpr): Boolean = {
      val op = e.head.headSymbol.value
      if(builtinSymbols.contains(op)) {
        e.args.size() match {
          case 1 =>
            setFlagByOp(unOpByString(op))
            e.args.get(0).accept(this)
            Types.opOutputType(unOpByString(op)) == BooleanType()
          case 2 =>
            setFlagByOp(binOpByString(op))
            e.args.get(0).accept(this)
            setFlagByOp(binOpByString(op))
            e.args.get(1).accept(this)
            Types.opOutputType(binOpByString(op)) == BooleanType()
          case 3 =>
            assert(op == "ite")
            justNowWasBooleanOperation = true
            e.args.get(0).accept(this)
            justNowWasBooleanOperation = false
            e.args.get(1).accept(this)
            justNowWasBooleanOperation = false
            e.args.get(2).accept(this)
            true
        }
      } else {
        if (Utils.verbose) println("[warn] using uninterpreted function: " + op)
        println("[synthesis] Error: unsupported SMTLIB symbol: " + op)
        println("[synthesis] the following symbols are currently supported: " + builtinSymbols)
        ???
      }
    }

    override def visit(e: IExpr.ISymbol): Boolean = {
      if (justNowWasBooleanOperation) {
        booleans += e.value
      }
      justNowWasBooleanOperation
    }

    override def visit(e: IExpr.INumeral): Boolean = {
      false
    }

    //TODO here should be something about booleans:
    // override def visit(e: ): ProgramFormulaExpression = {
    //   BooleanValue(???)
    // }
  
  }

  /**
    * TODO: what is it for?
    */
  class RepairableAnalyzer(typeOf: String => Option[Type]) extends DefaultVisitor[(Int, Int)] {

    def binOpByString(op: String): BinaryOperator = {
      op match {
        case "or"  => Or()
        case "and" => And()
        case "="   => Equal()
        case "<"   => Less()
        case "<="  => LessOrEqual()
        case ">"   => Greater()
        case ">="  => GreaterOrEqual()
        case "+"   => Add()
        case "-"   => Sub()
        case "*"   => Mult()
        case "/"   => Div()
        case "=>"  => Impl()
        case "iff" => Iff()
      }
    }

    def unOpByString(op: String): UnaryOperator = {
      op match {
        case "not" => Not()
        case "-"   => Neg()
      }
    }

    override def visit(e: IExpr.IFcnExpr): (Int, Int) = {
      val op = e.head.headSymbol.value
      if(builtinSymbols.contains(op)) {
        e.args.size() match {
          case 1 => 
            val (ios, bos) = e.args.get(0).accept(this)
            val outtype = Types.opOutputType(unOpByString(op))
            outtype match {
              case IntegerType() => (ios + 1, bos)
              case BooleanType() => (ios, bos + 1)
              case _ => (ios, bos)
            }
          case 2 =>
            val (ios1, bos1) = e.args.get(0).accept(this)
            val (ios2, bos2) = e.args.get(1).accept(this)
            val outtype = Types.opOutputType(binOpByString(op))
            outtype match {
              case IntegerType() => (ios1 + ios2 + 1, bos1 + bos2)
              case BooleanType() => (ios1 + ios2, bos1 + bos2 + 1)
              case _ => (ios1 + ios2, bos1 + bos2)
            }
          case 3 => 
            //here we assume that types are known
            assert(op == "ite")
            val (ios1, bos1) = e.args.get(0).accept(this)
            val (ios2, bos2) = e.args.get(1).accept(this)
            val (ios3, bos3) = e.args.get(2).accept(this)
            (ios1 + ios2 + ios3 + 1, bos1 + bos2 + bos3)
        }
      } else {
        if (Utils.verbose) println("[warn] using uninterpreted function: " + op)
        //dont know what to do here :)
        val ls = e.args().asScala.toList.map(_.accept(this))
        val (ios, bos) = ls.foldLeft((0, 0))({ case ((a, b), (aa, bb)) => (a + aa, b + bb)})
        val outtype = typeOf(op).get
        outtype match {
          case IntegerType() => (ios + 1, bos)
          case BooleanType() => (ios, bos + 1)
          case _ => (ios, bos)
        }
      }
    }

    override def visit(e: IExpr.ISymbol): (Int, Int) = {
      if (e.value == "true" || e.value == "false") return (0, 1)
      val outtype = typeOf(e.value).get
      outtype match {
        case IntegerType() => (1, 0)
        case BooleanType() => (0, 1)
        case _ => (0, 0)
      }
    }

    override def visit(e: IExpr.INumeral): (Int, Int) = {
      (1, 0)
    }

  }


  def translateIfRepairable(expr: IExpr, typeOf: String => Option[Type]): Option[ProgramFormulaExpression] = {
    try {
      val result = expr.accept(new RepairableTranslator(typeOf))
      Some(result)
    } catch {
      case e: Exception =>
        if (Utils.verbose) println("[warn] expression is not repairable:")
        if (Utils.verbose) println(expr)
        None
    }
  }

  def getTypeConstraints(expr: IExpr, expectedTopTypeIsBool: Boolean): (Set[String], Boolean) = {
    val visitor = new RepairableBooleanInference(expectedTopTypeIsBool)
    val topType = expr.accept(visitor)
    (visitor.getConstraints(), topType)
  }

  def castIntToBool(expr: IExpr): IExpr = {
    val not: IExpr.IQualifiedIdentifier = new SMTExpr.Symbol("not")
    val eq: IExpr.IQualifiedIdentifier = new SMTExpr.Symbol("=")
    val zero = new SMTExpr.Numeral(0)
    val eqSubExpr = new java.util.ArrayList[IExpr]()
    eqSubExpr.add(expr)
    eqSubExpr.add(zero)
    val eqExpr = new SMTExpr.FcnExpr(eq, eqSubExpr)
    val notSubExpr = new java.util.ArrayList[IExpr]()
    notSubExpr.add(eqExpr)
    val notExpr = new SMTExpr.FcnExpr(not, notSubExpr)
    notExpr
  }

  def isInterestingExpression(expr: IExpr, typeOf: String => Option[Type], config: SynthesisConfig): Boolean = {
    try {
      val (intouts, boolouts) = expr.accept(new RepairableAnalyzer(typeOf))
      //TODO should we take into consideration mutiple components occurences?
      config.componentLevel match {
        case Alternatives()          => (boolouts >= 2) || (intouts >= 2)
        case IntegerConstants()      => (intouts >= 1)
        case BooleanConstants()      => (boolouts >= 1)
        case Variables()             => true
        case BasicArithmetic()       => (intouts >= 1)
        case BasicLogic()            => (boolouts >= 1) || (intouts >= 1) // because we use int2bool
        case BasicInequalities()     => (boolouts >= 1) && (intouts >= 1)
        case ExtendedArithmetic()    => (intouts >= 1)
        case ExtendedLogic()         => (boolouts >= 1)
        case ExtendedInequalities()  => (boolouts >= 1) && (intouts >= 1)
        case MixedConditional()      => (boolouts >= 1)
        case ConditionalArithmetic() => (intouts >= 1)
      }
    } catch {
      case e: Exception =>
        if (Utils.verbose) println("[warn] cannot analyze expression:")
        if (Utils.verbose) println(expr)
        ???
    }
  }

  def typeStringToAst(typ: String): Option[Type] = {
    typ match {
      case BoolTypeName    => Some(BooleanType())
      case IntTypeName     => Some(IntegerType())
      case PointerTypeName => Some(PointerType())
      case _               => None
    }
  }

  def lookupGlobalDeclarations(declsIndex: Map[String, DeclareFunAst], varName: String): Option[Type] = {
    if (declsIndex.contains(varName)) {
      declsIndex(varName) match {
        case d: DeclareFunAst => typeStringToAst(d.getType)
        case _ =>
          if (Utils.verbose) println("[error] Variable " + varName + " is not declared as declare-fun")
          ???
      }
    } else {
      //if (Utils.verbose) println("[error] Variable " + varName + " is not declared")
      None
    }
  }

  def getTypeOf(declsIndex: Map[String, DeclareFunAst], repairable: RepairableBindings): (String => Option[Type]) = {
    val subsType = (name: String) => {
        repairable.find({ case (v, _, _, _, _) => v.name == name}) match {
          case Some((ProgramVariable(_, typ), _, _, _, _)) => Some(typ)
          case None => None
        }
    }
    def typeOf(varName: String): Option[Type] = {
      subsType(varName) match {
        case Some(typ) => Some(typ)
        case None => lookupGlobalDeclarations(declsIndex, varName)
      }
    }
    typeOf
  }

}
