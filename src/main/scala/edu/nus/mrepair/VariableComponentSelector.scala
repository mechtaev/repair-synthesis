package edu.nus.mrepair

import edu.nus.mrepair.synthesis._
import Formula._
import ProgramFormula._
import ComponentFormula._

object VariableComponentSelector {

  var angelicfixVariables: Map[Int, List[String]] = Map[Int, List[String]]()

  def select(stmtId: Int, exeId: Int): List[VariableComponent] = {

    if (angelicfixVariables.exists(_._1 == stmtId)) {
      return angelicfixVariables(stmtId).map({
        case name => VariableComponent(ProgramVariable(name + "#" + stmtId + "#" + exeId, IntegerType()))
      })
    } else {
      return Nil
    }

//     (Utils.logBenchmarkName, Utils.logBenchmarkVersion) match {       

//       case ("schedule", "1") =>
//         (stmtId, exeId) match {
//           case (4543, 0) => 
//             VariableComponent(ProgramVariable("f_ele_iter0", BooleanType())) :: Nil
//           case (4543, 1) => 
//             VariableComponent(ProgramVariable("f_ele_iter1", BooleanType())) :: Nil
//           case (4543, 2) => 
//             VariableComponent(ProgramVariable("f_ele_iter2", BooleanType())) :: Nil
//           case _ => Nil
//         }

//       case ("schedule", "6") =>
//         (stmtId, exeId) match {
//           case (4360, 0) => 
//             VariableComponent(ProgramVariable("f_ele_iter0", BooleanType())) :: Nil
//           case (4360, 1) => 
//             VariableComponent(ProgramVariable("f_ele_iter1", BooleanType())) :: Nil
//           case (4360, 2) => 
//             VariableComponent(ProgramVariable("f_ele_iter2", BooleanType())) :: Nil
//           case _ => Nil
//         }

//       case ("tcas", _) => 
//         (stmtId, exeId) match {
//           case (0, 0) =>
//             (VariableComponent(ProgramVariable("Global#Cur_Vertical_Sep@$s", IntegerType()))
// // these are actually booleans:
// // :: VariableComponent(ProgramVariable("Global#High_Confidence@$s", IntegerType()))
// // :: VariableComponent(ProgramVariable("Global#Two_of_Three_Reports_Valid@$s", IntegerType()))

//           :: VariableComponent(ProgramVariable("Global#Own_Tracked_Alt@$s", IntegerType()))
//           :: VariableComponent(ProgramVariable("Global#Own_Tracked_Alt_Rate@$s", IntegerType()))
//           :: VariableComponent(ProgramVariable("Global#Other_Tracked_Alt@$s", IntegerType()))
//           :: VariableComponent(ProgramVariable("Global#Alt_Layer_Value@$s", IntegerType()))
//           :: VariableComponent(ProgramVariable("Global#Up_Separation@$s", IntegerType()))
//           :: VariableComponent(ProgramVariable("Global#Down_Separation@$s", IntegerType()))
//           :: VariableComponent(ProgramVariable("Global#Other_RAC@$s", IntegerType()))
//           :: VariableComponent(ProgramVariable("Global#Other_Capability@$s", IntegerType()))
//           :: VariableComponent(ProgramVariable("Global#Climb_Inhibit@$s", IntegerType()))
//           :: Nil)
//           case _ => Nil
//         }

//       case ("coreutils", "72d05289") =>
//         (stmtId, exeId) match {
//           case (777777, _) => 
//             VariableComponent(ProgramVariable("my-scontext", PointerType())) :: 
//             VariableComponent(ProgramVariable("my-optarg", PointerType())) :: 
//             Nil
//           case _ => Nil
//         }

//       case ("coreutils", "a0851554") =>
//         (stmtId, exeId) match {
//           case (777777, 12) => 
//             VariableComponent(ProgramVariable("local.strptr@8", PointerType())) :: Nil
//           case (777777, 11) => 
//             VariableComponent(ProgramVariable("local.strptr@10", PointerType())) :: Nil
//           case (777777, 10) => 
//             VariableComponent(ProgramVariable("local.strptr@8", PointerType())) :: Nil
//           case (777777, 7) => 
//             VariableComponent(ProgramVariable("local.strptr@4", PointerType())) :: Nil
//           case (777777, 6) => 
//             VariableComponent(ProgramVariable("local.strptr@5", PointerType())) :: Nil
//           case (777777, 5) => 
//             VariableComponent(ProgramVariable("local.strptr@4", PointerType())) :: Nil
//           case (777777, 3) => 
//             VariableComponent(ProgramVariable("local.strptr@0", PointerType())) :: Nil
//           case (777777, 2) => 
//             VariableComponent(ProgramVariable("local.strptr@1", PointerType())) :: Nil
//           case (777777, 1) => 
//             VariableComponent(ProgramVariable("local.strptr@0", PointerType())) :: Nil
//           case _ => Nil
//         }

//       case ("coreutils", "b58a8b4e") =>
//         (stmtId, exeId) match {
//           case (777777, 8) => 
//             VariableComponent(ProgramVariable("local.strptr@7", PointerType())) :: Nil
//           case (777777, 7) => 
//             VariableComponent(ProgramVariable("local.strptr@4", PointerType())) :: Nil
//           case (777777, 6) => 
//             VariableComponent(ProgramVariable("L#__temp22329@3", PointerType())) :: Nil
//           case (777777, 3) => 
//             VariableComponent(ProgramVariable("local.strptr@2", PointerType())) :: Nil
//           case (777777, 2) => 
//             VariableComponent(ProgramVariable("local.strptr@0", PointerType())) :: Nil
//           case (777777, 1) => 
//             /*VariableComponent(ProgramVariable("L#__temp22329@0", PointerType())) ::*/ Nil
//           case _ => Nil
//         }


//       case ("replace", "4") =>
//         (stmtId, exeId) match {
//           case (41831, _) => 
//             //actially, i should specify id:
//             VariableComponent(ProgramVariable("L#lastm@0", IntegerType())) :: 
//             VariableComponent(ProgramVariable("L#lastm@1", IntegerType())) :: 
//             Nil
//           case _ => Nil //what is here?
//         }

//       case _ => Nil

//     }
  }

}
