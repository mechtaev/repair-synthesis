package edu.nus.mrepair;

import java.io.File;
import java.util.List;
import java.util.Map;
import java.util.Set;

import junit.framework.Assert;

import org.junit.Test;
import org.smtlib.IExpr;

import edu.nus.vctrans2.VCTranslator;
import edu.nus.vctrans2.ast.CFunctionAst;
import edu.nus.vctrans2.ast.DeclareAst;
import edu.nus.vctrans2.ast.DeclareFunAst;
import edu.nus.vctrans2.ast.DeclareSortAst;
import edu.nus.vctrans2.ast.RootAst;

public class FrontEndTest {

	static final String DIR_Simple = "Subjects" + File.separator + "Simple";
	static final String DIR_TCAS = "Subjects" + 
			File.separator + "Tcas" + 
			File.separator + "versions.alt" + 
			File.separator + "versions.orig";	

	@Test
	public void testInc() throws Exception {
		File file = new File(DIR_Simple, "inc.smt2.script");
		Assert.assertTrue("source file does not exist", file.exists());
		
		boolean shouldUnfoldLetExpr = true;
		RootAst ast = VCTranslator.translate(file, shouldUnfoldLetExpr, false);
		Assert.assertTrue(ast != null);				
	}


    // TODO what is vertical lines in LexLiterals names? (for example, "|L#output_o@0|")
    @Test
	public void testIncDec() throws Exception {
		File file = new File(DIR_Simple, "inc_dec.smt2.script");
		Assert.assertTrue("source file does not exist", file.exists());
		
		boolean shouldUnfoldLetExpr= true;
		RootAst root = VCTranslator.translate(file, shouldUnfoldLetExpr, false);
		Assert.assertTrue(root != null);
		
		DeclareFunAst decFunAst = root.getFunctionDeclaration("%lbl%+34186");
		Assert.assertTrue(decFunAst != null);
		System.out.println(decFunAst);
		
		List<DeclareSortAst> allDecSorts = root.getAllDeclareSorts();
		Assert.assertTrue(allDecSorts != null);
		Assert.assertTrue(allDecSorts.size() > 0);
		
		// get all declarations (function declarations + sort declarations)
		List<DeclareAst> allDecs = root.getAllDeclares();
		Assert.assertTrue(allDecs != null);
		Assert.assertTrue(allDecs.size() > 0);
		// root.printAllDeclares();
		
		CFunctionAst funAst = root.getCFunctionAst("inc_dec");
		Assert.assertTrue(funAst != null);
		System.out.println("\n" + funAst);

		
		Map<String, IExpr> condSet = funAst.getConditions();
		Assert.assertTrue(condSet != null);
		Assert.assertEquals(2, condSet.size());
		// System.out.println("\nCondition components:");
		// System.out.println(condSet);
		
//		List<IExpr> condList = new LinkedList<IExpr>(condSet);
//		IExpr firstCond = condList.get(0);
//		Assert.assertTrue(firstCond != null);
		// Assert.assertTrue(firstCond + "should be a condition.", funAst.hasCondition(firstCond));		

		Map<String, IExpr> assgSet = funAst.getAssignments();
		Assert.assertTrue(assgSet != null);
		Assert.assertEquals(4, assgSet.size());
		// System.out.println("\nAssignment components:");
		// System.out.println(assgSet);	
		
//		List<IExpr> assgList = new LinkedList<IExpr>(assgSet);
//		IExpr firstAssg = assgList.get(0);
//		Assert.assertTrue(firstAssg != null);
//		Assert.assertTrue(firstAssg + "should be an assignment.", funAst.hasAssignment(firstAssg));

		Map<String, IExpr> repComp = funAst.getRepairableComponents();
		Assert.assertTrue(repComp != null);
		System.out.println("\nRepairable components (totally " + repComp.size() + "):");
		for(Map.Entry<String, IExpr> e: repComp.entrySet()) {
			System.out.println(e.getKey() + " \t=\t " + e.getValue());
		}
		
		Set<IExpr> assumptionSet = funAst.getAssumptions();
		Assert.assertTrue(assumptionSet != null);
		Assert.assertTrue(assumptionSet.size() == 2);
	}
	
	@Test
	public void testIncDec2() throws Exception {
		File file = new File(DIR_Simple, "inc_dec2.smt2.script");		
		Assert.assertTrue("source file does not exist", file.exists());
		
		boolean shouldUnfoldLetExpr = true;
		RootAst root = VCTranslator.translate(file, shouldUnfoldLetExpr, false);
		Assert.assertTrue(root != null);
				
		List<DeclareSortAst> allDecSorts = root.getAllDeclareSorts();
		Assert.assertTrue(allDecSorts != null);
		Assert.assertTrue(allDecSorts.size() > 0);
		
		// get all declarations (function declarations + sort declarations)
		List<DeclareAst> allDecs = root.getAllDeclares();
		Assert.assertTrue(allDecs != null);
		Assert.assertTrue(allDecs.size() > 0);
		// root.printAllDeclares();
		
		CFunctionAst funAst = root.getCFunctionAst("inc_dec");
		Assert.assertTrue(funAst != null);
		System.out.println("\n" + funAst);
						
//		Set<IExpr> repComp = funAst.getRepairableComponents();
//		Assert.assertTrue(repComp != null);
		// System.out.println("\nRepairable components (conditions + assignments):");
		// System.out.println(repComp);
	}
	
	
	@Test
	public void testTCAS_v1() throws Exception {
		File file = new File(DIR_TCAS, "v1" + File.separator + "tcas_v1.smt2.script");
		Assert.assertTrue("source file does not exist", file.exists());
		
		boolean shouldUnfoldLetExpr = true;
		RootAst root = VCTranslator.translate(file, shouldUnfoldLetExpr, false);
		Assert.assertTrue(root != null);
				
		List<DeclareSortAst> allDecSorts = root.getAllDeclareSorts();
		Assert.assertTrue(allDecSorts != null);
		Assert.assertTrue(allDecSorts.size() > 0);				
		
		// get all declarations (function declarations + sort declarations)
		List<DeclareAst> allDecs = root.getAllDeclares();
		Assert.assertTrue(allDecs != null);
		Assert.assertTrue(allDecs.size() > 0);
		// root.printAllDeclares();
		
		CFunctionAst funAst = root.getCFunctionAst("Non_Crossing_Biased_Climb");
		Assert.assertTrue(funAst != null);
		System.out.println("\n" + funAst);
						
//		Set<IExpr> repComp = funAst.getRepairableComponents();
//		Assert.assertTrue(repComp != null);
		// System.out.println("\nRepairable components (conditions + assignments):");
		// System.out.println(repComp);
	}

}
