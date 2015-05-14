package edu.nus.mrepair;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import junit.framework.Assert;

import org.smtlib.IExpr;

import edu.nus.vctrans2.VCTranslator;
import edu.nus.vctrans2.ast.AssertAst;
import edu.nus.vctrans2.ast.CFunctionAst;
import edu.nus.vctrans2.ast.DeclareAst;
import edu.nus.vctrans2.ast.DefineAst;
import edu.nus.vctrans2.ast.DefineFunAst;
import edu.nus.vctrans2.ast.RootAst;

public abstract class Experiment {
	
	protected static boolean canSubstituteAnnotated = true;

	protected List<String> run(String dir, String mainFileName, String funName, String[] suffixFileNames) throws IOException {
		File smt2File = new File(dir, mainFileName + ".smt2");
		Assert.assertTrue(smt2File.getName() + " does not exist", smt2File.exists());

		File[] suffixFiles = new File[suffixFileNames.length];
		int count = 0;
		for (String suffixFileName : suffixFileNames) {
			Assert.assertTrue(suffixFileName.endsWith(".suffix"));
			File suffixFile = new File(dir, suffixFileName);
			Assert.assertTrue("suffix file does not exist: " + suffixFileName, suffixFile.exists());
			suffixFiles[count++] = suffixFile;
		}
		

    // System.out.println("Suffix file: " + dir + "/" + suffixFileName);
		// File suffixFile = new File(dir, mainFileName + ".suffix");
		// Assert.assertTrue("suffix file does not exist", suffixFile.exists());

		boolean shouldUnfoldLetExpr = false;
		RootAst root = VCTranslator.translateSmt2(smt2File, shouldUnfoldLetExpr, canSubstituteAnnotated);
		Assert.assertNotNull(root);
		CFunctionAst funAst = root.getCFunctionAst(funName);	
		Assert.assertNotNull(funAst);
		// System.out.println("\n" + funAst);

		// ast file
		File astFile = makeAstFile(dir, mainFileName, root, funAst, suffixFiles);
		List<String> outputLines = runZ3(astFile);
		
		return outputLines;
	}

	protected List<String> run(String dir, String fileName, String funName) throws IOException {
		return run(dir, fileName, funName, new String[] { fileName + ".suffix" });
	}

	protected List<String> runZ3(File astFile) throws IOException {
		Properties prop = new Properties();
		prop.load(new FileInputStream("z3.properties"));
		String z3exe = prop.getProperty("z3exe");
		ProcessBuilder z3 = new ProcessBuilder(z3exe, "-smt2", astFile.getAbsolutePath());
		z3.redirectErrorStream(true);
		Process p = z3.start();
		Assert.assertNotNull(p);

		BufferedReader br = new BufferedReader(new InputStreamReader(
				p.getInputStream()));
		List<String> output = new ArrayList<String>();
		String line;
		while ((line = br.readLine()) != null) {
			if (!line.startsWith("WARNING")) {
				output.add(line);
			}
		}

		return output;
	}
	
	protected void printOutputLines(List<String> outputLines) {
		for (String line : outputLines) {
			printZ3Response(line);
		}				
	}

	protected void checkExpected(List<String> outputLines, String[] expected) {
		int i = 0;
		for (String line : outputLines) {
			printZ3Response(line);
			Assert.assertEquals(expected[i++], line);
		}		
	}
	
	protected void checkExpected(List<String> outputLines, String[][] expected) {
		for (int h = 0; h < outputLines.size(); h++) {
			String line = outputLines.get(h); 
			printZ3Response(line);
			for (int j = 0; j < expected.length; j++) {
				try {
					Assert.assertEquals(expected[j].length, 2);
				} catch (ArrayIndexOutOfBoundsException e) {
					Assert.fail();
				}
				String cue = expected[j][0];
				if (line.equals(cue)) {
					String actual = outputLines.get(++h);
					printZ3Response(actual);
					String expectedLine = expected[j][1];
					Assert.assertEquals(expectedLine, actual);
				} 
			}		
		}				
	}
	
	protected void printZ3Response(String response) {
		System.out.println(response);
	}
	
	protected File makeAstFile(String dir, String name, RootAst root, CFunctionAst funAst,
			File[] suffixFiles) throws IOException {
		File astFile = new File(dir, name + ".ast.lisp");
		FileWriter fw = new FileWriter(astFile);

		fw.write("(set-option :produce-models true)\n");
		fw.write("(set-option :produce-unsat-cores true)\n");

		List<DeclareAst> allDecs = root.getAllDeclares();
		Assert.assertNotNull(allDecs);
		Assert.assertTrue(allDecs.size() > 0);
		for (DeclareAst dec : allDecs) {
			fw.write(dec + "\n");
		}
				
		List<DefineAst> allDefs = root.getAllDefines();
		Assert.assertNotNull(allDefs);
		for (DefineAst def: allDefs) {
			if (def instanceof DefineFunAst) {
				DefineFunAst defFunAst = (DefineFunAst) def;
				fw.write("\n");
				fw.write(defFunAst.toPrettyString() + "\n");
			} else {
				fw.write(def + "\n");	
			}
		}
		
		List<AssertAst> asrts = root.getAsserts();
		Assert.assertNotNull(asrts);
		for (AssertAst asrt : asrts) {
			fw.write(asrt + "\n");
		}		

		for (IExpr assumption : funAst.getAssumptions()) {
			fw.write("(assert\n");
			fw.write(assumption + "\n");
			fw.write("); assert\n");
		}

		fw.write("\n");		
		fw.write(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n");
		fw.write("; The formula for the function under testing\n");
		fw.write(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n");		
		fw.write("(assert\n");
		fw.write(funAst.getExprStr());
		fw.write("); assert\n\n");

		for (File suffixFile : suffixFiles) {
			copy(fw, suffixFile);
		}
		
		fw.close();
		System.out.println("[INFO] Made " + astFile.getName());
		return astFile;
	}
	
	private void copy(FileWriter fw, File file) throws IOException {
		FileReader fr = new FileReader(file);
		char[] buf = new char[1024];
		int bytesRead;
		while ((bytesRead = fr.read(buf)) > 0) {
			fw.write(buf, 0, bytesRead);
		}
		fr.close();	
	}

}
