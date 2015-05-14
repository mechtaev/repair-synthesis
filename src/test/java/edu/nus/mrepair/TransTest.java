package edu.nus.mrepair;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.List;

import junit.framework.Assert;

import org.junit.BeforeClass;
import org.junit.Test;

import edu.nus.vctrans2.VCTranslator;
import edu.nus.vctrans2.ast.CFunctionAst;
import edu.nus.vctrans2.ast.RootAst;

public class TransTest extends Experiment {

	static final String DIR_Simple = "Subjects" + File.separator + "Simple";
	static final String DIR_Tcas = "Subjects" + File.separator + "Tcas"
			+ File.separator + "source";
	static final String DIR_Replace = "Subjects" + File.separator + "replace"
			+ File.separator + "versions.alt" + File.separator + "versions.orig";
	static final String DIR_Schedule = "Subjects" + File.separator + "schedule"
			+ File.separator + "source";
	static final String DIR_Schedule_version = "Subjects" + File.separator + "schedule"
			+ File.separator + "versions.alt" + File.separator + "versions.orig";
	static final String DIR_Schedule2_version = "Subjects" + File.separator + "schedule2"
			+ File.separator + "versions.alt" + File.separator + "versions.orig";
	static final String DIR_Grep_version1 = "Subjects" + File.separator + "grep"
			+ File.separator + "versions.alt" + File.separator + "versions.seeded" + File.separator + "v1";	
	static final String DIR_Coreutils = "Subjects" + File.separator + "coreutils"
			+ File.separator + "source";
	
	@BeforeClass
	public static void setup() {
		canSubstituteAnnotated = false;
	}
	
	@Test
	public void testIncDec() throws Exception {
		String name = "inc_dec";
		File smt2File = new File(DIR_Simple, name + ".smt2");
		Assert.assertTrue("source file does not exist", smt2File.exists());

		File suffixFile = new File(DIR_Simple, name + ".suffix");
		Assert.assertTrue("suffix file does not exist", suffixFile.exists());

		boolean shouldUnfoldLetExpr = false;
		RootAst root = VCTranslator.translateSmt2(smt2File, shouldUnfoldLetExpr, false);
		Assert.assertNotNull(root);
		CFunctionAst funAst = root.getCFunctionAst("inc_dec");
		Assert.assertNotNull(funAst);
		// System.out.println("\n" + funAst);

		// ast file
		File astFile = makeAstFile(DIR_Simple, name, root, funAst, new File[] { suffixFile });
		List<String> outputLines = runZ3(astFile);

		// check expected:
		int i = 0;
		for (String line : outputLines) {
			System.out.println(line);
			switch (i) {
			case 2:
				Assert.assertEquals("((|L#output_o@2| 2))", line);
				break;
			case 5:
				Assert.assertEquals("((|L#output_o@2| (- 2)))", line);
				break;
			case 6:
				Assert.assertEquals("unsat", line);
				break;
			}
			i++;
		}		
	}
	
	@Test
	public void testBoolTest() throws Exception {
		List<String> outputLines = run(DIR_Simple, "bool_test", "booltest");
		
		// check expected:
		for (String line : outputLines) {
			System.out.println(line);
		}
	}
	
	@Test
	public void testLoopTest() throws Exception {
		List<String> outputLines = run(DIR_Simple + File.separator + "loop", "loop", "loop_test");
		
		for (String line : outputLines) {
			System.out.println(line);
		}
	}
	
	@Test
	public void transMotivating_ex2() throws Exception {		
		String fileName = "ex2";
		String funName = "ex2"; 		
		List<String> outputLines = run(DIR_Simple + File.separator + "motivating", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void transCalleeTest() throws Exception {
		List<String> outputLines = run(DIR_Simple + File.separator + "callee", "callee_test", "callee_test");
		
		// check expected:
		String[][] expected = {
				{"assert output==112", "((|L#output_o@0| 112))"}
		};
		checkExpected(outputLines, expected);		
	}

	@Test
	public void transGlobalTest() throws Exception {
		List<String> outputLines = run(DIR_Simple + File.separator + "global", "global_test", "global_test");
		
		// check expected:
		String[] expected = {
				"sat",
				"((|L#output_o@0| 23))"
		};
		checkExpected(outputLines, expected);		
	}

	@Test
	public void transArrayTest() throws Exception {
		List<String> outputLines = run(DIR_Simple + File.separator + "array", "array_test", "array_test");
		
		// check expected:
		String[] expected = {
				"sat",
				"((|L#output_o@0| 23))"
		};
		checkExpected(outputLines, expected);		
	}

	@Test
	public void transNoIfTest() throws Exception {
		List<String> outputLines = run(DIR_Simple + File.separator + "if", "no_if", "booltest");
		for (String line : outputLines) {
			System.out.println(line);
		}				
	}

	@Test
	public void transWithIfTest() throws Exception {
		List<String> outputLines = run(DIR_Simple + File.separator + "if", "with_if", "booltest");
		for (String line : outputLines) {
			System.out.println(line);
		}				
	}

	@Test
	public void transCaseBefore() throws Exception {
		List<String> outputLines = run(DIR_Simple + File.separator + "case", "before", "test");
		for (String line : outputLines) {
			System.out.println(line);
		}				
	}

	@Test
	public void transCaseAfter() throws Exception {
		List<String> outputLines = run(DIR_Simple + File.separator + "case", "after", "test");
		for (String line : outputLines) {
			System.out.println(line);
		}				
	}

	@Test
	public void testTcas_v1() throws Exception {
		String fileName = "tcas_v1";
		String funName = "Test_Non_Crossing_Biased_Climb";		
		List<String> outputLines = run(DIR_Tcas, fileName, funName);
		
		// check expected:
		String[][] expected = {
				{"[case 1 result (expect 1)]", "((|L#result@1_F#Non_Crossing_Biased_Climb@0| 1))"}, 
				{"[case 2 result (expect 0)]", "((|L#result@1_F#Non_Crossing_Biased_Climb@0| 0))"}
		};
		checkExpected(outputLines, expected);
	}

	@Test
	public void testReplace_v1() throws Exception {
		String fileName = "replace_v1";
		String funName = "dodash"; // "dodash";		
		List<String> outputLines = run(DIR_Replace + File.separator + "v1", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}

	@Test
	public void testReplace_v2() throws Exception {
		String fileName = "replace_v2";
		String funName = "dodash"; // "dodash";		
		List<String> outputLines = run(DIR_Replace + File.separator + "v2", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}

	@Test
	public void testReplace_v3() throws Exception {
		String fileName = "replace_v3";
		String funName = "subline"; 		
		List<String> outputLines = run(DIR_Replace + File.separator + "v3", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}

	@Test
	public void testReplace_v4() throws Exception {
		String fileName = "replace_v4";
		String funName = "subline"; 		
		List<String> outputLines = run(DIR_Replace + File.separator + "v4", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void testReplace_v5() throws Exception {
		String fileName = "replace_v5";
		String funName = "dodash"; // "dodash";		
		List<String> outputLines = run(DIR_Replace + File.separator + "v5", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void testReplace_v6() throws Exception {
		String fileName = "replace_v6";
		String funName = "test_locate"; 		
		List<String> outputLines = run(DIR_Replace + File.separator + "v6", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}

	@Test
	public void testReplace_v7() throws Exception {
		String fileName = "replace_v7";
		String funName = "in_set_2"; 		
		List<String> outputLines = run(DIR_Replace + File.separator + "v7", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}

	@Test
	public void testReplace_v8() throws Exception {
		String fileName = "replace_v8";
		String funName = "in_set_2_test"; 		
		List<String> outputLines = run(DIR_Replace + File.separator + "v8", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}

	@Test
	public void testReplace_v9() throws Exception {
		String fileName = "replace_v9";
		String funName = "dodash"; // "dodash";		
		List<String> outputLines = run(DIR_Replace + File.separator + "v9", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}

	@Test
	public void testReplace_v10() throws Exception {
		String fileName = "replace_v10";
		String funName = "dodash"; // "dodash";		
		List<String> outputLines = run(DIR_Replace + File.separator + "v10", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}

	@Test
	public void testReplace_v11() throws Exception {
		String fileName = "replace_v11";
		String funName = "dodash"; // "dodash";		
		List<String> outputLines = run(DIR_Replace + File.separator + "v11", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void testReplace_v13() throws Exception {
		String fileName = "replace_v13";
		String funName = "subline"; 		
		List<String> outputLines = run(DIR_Replace + File.separator + "v13", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}

	@Test
	public void testReplace_v14() throws Exception {
		String fileName = "replace_v14";
		String funName = "test_omatch"; 		
		List<String> outputLines = run(DIR_Replace + File.separator + "v14", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}

	@Test
	public void testReplace_v16() throws Exception {
		String fileName = "replace_v16";
		String funName = "in_set_2_test"; 		
		List<String> outputLines = run(DIR_Replace + File.separator + "v16", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void testReplace_v18() throws Exception {
		String fileName = "replace_v18";
		String funName = "test_omatch"; 		
		List<String> outputLines = run(DIR_Replace + File.separator + "v18", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void testReplace_v24() throws Exception {
		String fileName = "replace_v24";
		String funName = "test_omatch"; 		
		List<String> outputLines = run(DIR_Replace + File.separator + "v24", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}

	@Test
	public void testReplace_v25() throws Exception {
		String fileName = "replace_v25";
		String funName = "omatch"; 		
		List<String> outputLines = run(DIR_Replace + File.separator + "v25", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void testReplace_v26() throws Exception {
		String fileName = "replace_v26";
		String funName = "omatch"; 		
		List<String> outputLines = run(DIR_Replace + File.separator + "v26", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void testReplace_v27() throws Exception {
		String fileName = "replace_v27";
		String funName = "in_pat_set_test"; 		
		List<String> outputLines = run(DIR_Replace + File.separator + "v27", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void testReplace_v28() throws Exception {
		String fileName = "replace_v28";
		String funName = "in_set_2"; 		
		List<String> outputLines = run(DIR_Replace + File.separator + "v28", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void testReplace_v29() throws Exception {
		String fileName = "replace_v29";
		String funName = "in_set_2"; 		
		List<String> outputLines = run(DIR_Replace + File.separator + "v29", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}

	@Test
	public void testReplace_v30() throws Exception {
		String fileName = "replace_v30";
		String funName = "in_set_2_test"; 		
		List<String> outputLines = run(DIR_Replace + File.separator + "v30", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void testReplace_v31() throws Exception {
		String fileName = "replace_v31";
		String funName = "test_omatch"; 		
		List<String> outputLines = run(DIR_Replace + File.separator + "v31", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
//	@Test
//	public void transPointer() throws Exception {
//		String fileName = "pointer";
//		String funName = "id";
//		List<String> outputLines = run(DIR_Simple + File.separator + "pointer", fileName, funName);
//		
//		for (String line : outputLines) {
//			System.out.println(line);
//		}		
//	}	
	
	@Test
	public void trans_find_nth() throws Exception {
		String fileName = "find_nth";
		String funName = "find_nth";		
		List<String> outputLines = run(DIR_Schedule, fileName, funName);
		
		// check expected:
		String[][] expected = {
				{ "assert i == 3", "((|L#i@5| 3))" },
				{ "assert i == 2", "((|L#i@5| 2))" }};
		checkExpected(outputLines, expected);
	}	
	
	@Test
	public void trans_find_v1_nth() throws Exception {
		String fileName = "find_nth_v1";
		String funName = "find_nth";		
		List<String> outputLines = run(DIR_Schedule, fileName, funName);
		
		// check expected:
		String[][] expected = {
				{ "assert i == 3", "((|L#i@5| 3))" },
				{ "assert i == 2", "((|L#i@5| 3))" }};
		checkExpected(outputLines, expected);
	}	
	
	@Test
	public void trans_find_v6_nth() throws Exception {
		String fileName = "find_nth_v6";
		String funName = "find_nth";		
		List<String> outputLines = run(DIR_Schedule, fileName, funName);
		
		// check expected:
		String[][] expected = {
				{ "assert i == 3", "((|L#i@5| 3))" },
				{ "assert i == 2", "((|L#i@5| 3))" }};
		checkExpected(outputLines, expected);
	}
	
	@Test
	public void testSchedule_v2() throws Exception {
		String fileName = "schedule_v2";
		String funName = "unblock_process"; 		
		List<String> outputLines = run(DIR_Schedule_version + File.separator + "v2", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}

	@Test
	public void testSchedule_v4() throws Exception {
		String fileName = "schedule_v4";
		String funName = "upgrade_process_prio"; 		
		List<String> outputLines = run(DIR_Schedule_version + File.separator + "v4", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void testSchedule_v5() throws Exception {
		String fileName = "schedule_v5";
		String funName = "upgrade_process_prio"; 		
		List<String> outputLines = run(DIR_Schedule_version + File.separator + "v5", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void testSchedule_v8() throws Exception {
		String fileName = "schedule_v8";
		String funName = "upgrade_process_prio"; 		
		List<String> outputLines = run(DIR_Schedule_version + File.separator + "v8", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void testSchedule_v9() throws Exception {
		String fileName = "schedule_v9";
		String funName = "main"; 		
		List<String> outputLines = run(DIR_Schedule_version + File.separator + "v9", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void testSchedule2_v1() throws Exception {
		String fileName = "schedule2_v1";
		String funName = "upgrade_prio"; 		
		List<String> outputLines = run(DIR_Schedule2_version + File.separator + "v1", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void testSchedule2_v5() throws Exception {
		String fileName = "schedule2_v5";
		String funName = "new_job"; 		
		List<String> outputLines = run(DIR_Schedule2_version + File.separator + "v5", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void testSchedule2_v6() throws Exception {
		String fileName = "schedule2_v6";
		String funName = "get_command"; 		
		List<String> outputLines = run(DIR_Schedule2_version + File.separator + "v6", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void testSchedule2_v7() throws Exception {
		String fileName = "schedule2_v7";
		String funName = "get_process"; 		
		List<String> outputLines = run(DIR_Schedule2_version + File.separator + "v7", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void testSchedule2_v8() throws Exception {
		String fileName = "schedule2_v8";
		String funName = "put_end"; 		
		List<String> outputLines = run(DIR_Schedule2_version + File.separator + "v8", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void testSchedule2_v10() throws Exception {
		String fileName = "schedule2_v10";
		String funName = "enqueue"; 		
		List<String> outputLines = run(DIR_Schedule2_version + File.separator + "v10", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void testGrep_v1_bug1() throws Exception {
		String fileName = "grep_bug1";
		String funName = "_obstack_newchunk"; 		
		List<String> outputLines = run(DIR_Grep_version1, fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}
	
	@Test
	public void trans_new_ele() throws Exception {
		String fileName = "new_ele";
		String funName = "new_ele";		
		List<String> outputLines = run(DIR_Schedule, fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}	

	@Test
	public void trans_field_update() throws Exception {
		String fileName = "field_update";
		String funName = "del_ele";		
		List<String> outputLines = run(DIR_Schedule, fileName, funName);
				
		// check expected:
		String[][] expected = { 
				{ "assert value == 2", "((|L#count1@0| 2))" },
				{ "assert value == 1", "((|L#count2@0| 1))" }};
		checkExpected(outputLines, expected);
	}	

	@Test
	public void trans_upgrade_process_prio() throws Exception {
		String fileName = "upgrade_process_prio";
		String funName = "upgrade_process_prio";		
		List<String> outputLines = run(DIR_Schedule, fileName, funName);
		
		//for (String line : outputLines) {
		//	System.out.println(line);
		//}		

		// check expected:
		String[][] expected = { 				
				{ "assert i == 2", "((|L#i@5_F#find_nth@0| 2))" },
				{ "assert find_nth(src_queue, n) == 300", "((($addr0 ($phys_ptr_cast |L#proc@0| ^_job)) 300))" },
				//{ "src_queue->mem_count changes to 1", "(((select (select ($heap $s@6@@0) list.mem_count) |Input#src_queue|) 1))" },
				//{ "proc' job.priority changes to 1", "(((select (select ($heap $s@1@@5) _job.priority) |L#proc@0|) 1))" }
		};
		checkExpected(outputLines, expected);				
	}		

	@Test
	public void transScheduleOrg_find_nth() throws Exception {
		String fileName = "schedule_org";
		String funName = "find_nth";		
		List<String> outputLines = run(DIR_Schedule, fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}	

	@Test
	public void transScheduleOrg_main() throws Exception {
		String fileName = "schedule_org";
		String funName = "main";		
		List<String> outputLines = run(DIR_Schedule, fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}

	@Test
	public void trans_md5sum_repair() throws Exception {
		String fileName = "md5sum.repair";
		String funName = "bsd_split_3";		
		
		Files.copy(new File(getPath("Z:/Benchmarks/Corebench/checkout/coreutils/7cb24684.dist/coreutils/src/" + fileName + ".c")).toPath(), 
				new File(getPath(DIR_Coreutils+"/7cb24684/" + fileName + ".c")).toPath(),
				StandardCopyOption.REPLACE_EXISTING);		
		Files.copy(new File(getPath("Z:/Benchmarks/Corebench/checkout/coreutils/7cb24684.dist/coreutils/src/" + fileName + ".smt2")).toPath(), 
				new File(getPath(DIR_Coreutils+"/7cb24684/" + fileName + ".smt2")).toPath(),
				StandardCopyOption.REPLACE_EXISTING);

		List<String> outputLines = run(DIR_Coreutils + File.separator + "7cb24684", fileName, funName);
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}	

	@Test
	public void trans_md5sum_buggy() throws Exception {
		String fileName = "md5sum.buggy";
		String funName = "bsd_split_3";		
		
		Files.copy(new File(getPath("Z:/Benchmarks/Corebench/checkout/coreutils/7cb24684.dist/coreutils/src/" + fileName + ".c")).toPath(), 
				new File(getPath(DIR_Coreutils+"/7cb24684/" + fileName + ".c")).toPath(),
				StandardCopyOption.REPLACE_EXISTING);		
		Files.copy(new File(getPath("Z:/Benchmarks/Corebench/checkout/coreutils/7cb24684.dist/coreutils/src/" + fileName + ".smt2")).toPath(), 
				new File(getPath(DIR_Coreutils+"/7cb24684/" + fileName + ".smt2")).toPath(),
				StandardCopyOption.REPLACE_EXISTING);

		List<String> outputLines = run(DIR_Coreutils + File.separator + "7cb24684", fileName, funName);
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}	

	@Test
	public void trans_md5sum_test() throws Exception {
		String fileName = "md5sum.test";
		String funName = "bsd_split_3";		
		
		Files.copy(new File(getPath("Z:/Benchmarks/Corebench/checkout/coreutils/7cb24684.dist/coreutils/src/" + fileName + ".c")).toPath(), 
				new File(getPath(DIR_Coreutils+"/7cb24684/" + fileName + ".c")).toPath(),
				StandardCopyOption.REPLACE_EXISTING);
		Files.copy(new File(getPath("Z:/Benchmarks/Corebench/checkout/coreutils/7cb24684.dist/coreutils/src/" + fileName + ".bpl")).toPath(), 
				new File(getPath(DIR_Coreutils+"/7cb24684/" + fileName + ".bpl")).toPath(),
				StandardCopyOption.REPLACE_EXISTING);				
		Files.copy(new File(getPath("Z:/Benchmarks/Corebench/checkout/coreutils/7cb24684.dist/coreutils/src/" + fileName + ".smt2")).toPath(), 
				new File(getPath(DIR_Coreutils+"/7cb24684/" + fileName + ".smt2")).toPath(),
				StandardCopyOption.REPLACE_EXISTING);

		List<String> outputLines = run(DIR_Coreutils + File.separator + "7cb24684", fileName, funName);
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}	
	
	@Test
	public void trans_pr_buggy() throws Exception {
		String fileName = "pr.buggy";
		String funName = "char_to_clump";		
		List<String> outputLines = run(DIR_Coreutils + File.separator + "6856089f", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}	

	@Test
	public void trans_pr_repair() throws Exception {
		String fileName = "pr.repair";
		String funName = "char_to_clump";		
		List<String> outputLines = run(DIR_Coreutils + File.separator + "6856089f", fileName, funName);
		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}	

	@Test
	public void trans_paste_repair() throws Exception {
		String fileName = "paste.repair.simple";
		String funName = "collapse_escapes";		
		
		Files.copy(new File(getPath("Y:/Benchmarks/Corebench/checkout/coreutils/b58a8b4e.dist/coreutils/src/" + fileName + ".c")).toPath(), 
				new File(getPath(DIR_Coreutils+"/b58a8b4e/" + fileName + ".c")).toPath(),
				StandardCopyOption.REPLACE_EXISTING);		
		Files.copy(new File(getPath("Y:/Benchmarks/Corebench/checkout/coreutils/b58a8b4e.dist/coreutils/src/" + fileName + ".smt2")).toPath(), 
				new File(getPath(DIR_Coreutils+"/b58a8b4e/" + fileName + ".smt2")).toPath(),
				StandardCopyOption.REPLACE_EXISTING);
		
		
		List<String> outputLines = run(DIR_Coreutils + File.separator + "b58a8b4e", fileName, funName);		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}

	@Test
	public void trans_paste_buggy() throws Exception {
		String fileName = "paste.buggy.simple";
		String funName = "collapse_escapes";		
		
		Files.copy(new File(getPath("Z:/Benchmarks/Corebench/checkout/coreutils/b58a8b4e.dist/coreutils/src/" + fileName + ".c")).toPath(), 
				new File(getPath(DIR_Coreutils+"/b58a8b4e/" + fileName + ".c")).toPath(),
				StandardCopyOption.REPLACE_EXISTING);
		Files.copy(new File(getPath("Z:/Benchmarks/Corebench/checkout/coreutils/b58a8b4e.dist/coreutils/src/" + fileName + ".bpl")).toPath(), 
				new File(getPath(DIR_Coreutils+"/b58a8b4e/" + fileName + ".bpl")).toPath(),
				StandardCopyOption.REPLACE_EXISTING);				
		Files.copy(new File(getPath("Z:/Benchmarks/Corebench/checkout/coreutils/b58a8b4e.dist/coreutils/src/" + fileName + ".smt2")).toPath(), 
				new File(getPath(DIR_Coreutils+"/b58a8b4e/" + fileName + ".smt2")).toPath(),
				StandardCopyOption.REPLACE_EXISTING);
		
		
		List<String> outputLines = run(DIR_Coreutils + File.separator + "b58a8b4e", fileName, funName);		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}

	@Test
	public void trans_ptx_buggy() throws Exception {
		String fileName = "ptx.buggy.simple";
		String funName = "copy_unescaped_string";		
		
		Files.copy(new File(getPath("Y:/Benchmarks/Corebench/checkout/coreutils/a0851554.dist/coreutils/src/" + fileName + ".c")).toPath(), 
				new File(getPath(DIR_Coreutils+"/a0851554/" + fileName + ".c")).toPath(),
				StandardCopyOption.REPLACE_EXISTING);		
		Files.copy(new File(getPath("Y:/Benchmarks/Corebench/checkout/coreutils/a0851554.dist/coreutils/src/" + fileName + ".smt2")).toPath(), 
				new File(getPath(DIR_Coreutils+"/a0851554/" + fileName + ".smt2")).toPath(),
				StandardCopyOption.REPLACE_EXISTING);
		
		
		List<String> outputLines = run(DIR_Coreutils + File.separator + "a0851554", fileName, funName);		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}

	@Test
	public void trans_ptx_repair() throws Exception {
		String fileName = "ptx.repair.simple";
		String funName = "copy_unescaped_string";		
		
		Files.copy(new File(getPath("Y:/Benchmarks/Corebench/checkout/coreutils/a0851554.dist/coreutils/src/" + fileName + ".c")).toPath(), 
				new File(getPath(DIR_Coreutils+"/a0851554/" + fileName + ".c")).toPath(),
				StandardCopyOption.REPLACE_EXISTING);		
		Files.copy(new File(getPath("Y:/Benchmarks/Corebench/checkout/coreutils/a0851554.dist/coreutils/src/" + fileName + ".smt2")).toPath(), 
				new File(getPath(DIR_Coreutils+"/a0851554/" + fileName + ".smt2")).toPath(),
				StandardCopyOption.REPLACE_EXISTING);
		
		
		List<String> outputLines = run(DIR_Coreutils + File.separator + "a0851554", fileName, funName);		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}

	@Test
	public void trans_mkfifo_buggy() throws Exception {
		String fileName = "mkfifo.buggy";
		String funName = "main";		
		
		Files.copy(new File(getPath("Z:/Benchmarks/Corebench/checkout/coreutils/72d05289.dist/coreutils/src/" + fileName + ".c")).toPath(), 
				new File(getPath(DIR_Coreutils+"/72d05289/" + fileName + ".c")).toPath(),
				StandardCopyOption.REPLACE_EXISTING);		
		Files.copy(new File(getPath("Z:/Benchmarks/Corebench/checkout/coreutils/72d05289.dist/coreutils/src/" + fileName + ".smt2")).toPath(), 
				new File(getPath(DIR_Coreutils+"/72d05289/" + fileName + ".smt2")).toPath(),
				StandardCopyOption.REPLACE_EXISTING);
		
		
		List<String> outputLines = run(DIR_Coreutils + File.separator + "72d05289", fileName, funName);		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}

	@Test
	public void trans_mkfifo_repair() throws Exception {
		String fileName = "mkfifo.repair";
		String funName = "main";		
		
		Files.copy(new File(getPath("Z:/Benchmarks/Corebench/checkout/coreutils/72d05289.dist/coreutils/src/" + fileName + ".c")).toPath(), 
				new File(getPath(DIR_Coreutils+"/72d05289/" + fileName + ".c")).toPath(),
				StandardCopyOption.REPLACE_EXISTING);		
		Files.copy(new File(getPath("Z:/Benchmarks/Corebench/checkout/coreutils/72d05289.dist/coreutils/src/" + fileName + ".smt2")).toPath(), 
				new File(getPath(DIR_Coreutils+"/72d05289/" + fileName + ".smt2")).toPath(),
				StandardCopyOption.REPLACE_EXISTING);
		
		
		List<String> outputLines = run(DIR_Coreutils + File.separator + "72d05289", fileName, funName);		
		for (String line : outputLines) {
			System.out.println(line);
		}		
	}

	private String getPath(String str) {
		return str.replace("/", File.separator);
	}
	
}
