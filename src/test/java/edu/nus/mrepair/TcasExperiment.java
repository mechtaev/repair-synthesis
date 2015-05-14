package edu.nus.mrepair;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import junit.framework.Assert;

import org.junit.BeforeClass;
import org.junit.Test;

import com.google.common.base.Charsets;
import com.google.common.collect.ObjectArrays;
import com.google.common.io.Files;

public class TcasExperiment extends Experiment {

	/*
	 * Change this to "correct" when performing repair.
	 */
	static final String OUT_DIR_NAME = "v1_Non_Crossing_Biased_Climb";
	// static final String OUT_DIR_NAME = "correct";

	static final boolean GENERATE_SUFFIXES = true;

	static final boolean IGNORE_UNKONWN_CONSTANT_ERROR = true;

	static final List<String> ONLY_SPECIFIED = new ArrayList<String>();

	static final String DIR_OUT = "Subjects" + File.separator + "Tcas"
			+ File.separator + "outputs" + File.separator + OUT_DIR_NAME;

	static final String DIR_SUFFIX = "Subjects" + File.separator + "Tcas"
			+ File.separator + "suffixes";

	static final String DIR_Tcas = "Subjects" + File.separator + "Tcas"
			+ File.separator + "versions.alt" + File.separator
			+ "versions.orig";

	@BeforeClass
	public static void init() throws Exception {
		if (GENERATE_SUFFIXES) {
			// empty the suffixes dir
			File dirSuffix = new File(DIR_SUFFIX);
			deleteSuffixDir(dirSuffix);
			dirSuffix.mkdir();
			Assert.assertTrue(dirSuffix.getAbsolutePath() + " does not exist.",
					dirSuffix.exists());

			makeCommonSuffixFiles(DIR_SUFFIX, "tcas");
		}
	}

	@Test
	public void tcas_v1() throws Exception {
		String fileName = "tcas_v1";
		String funName = "Test_Non_Crossing_Biased_Climb";
		String dir = DIR_Tcas + File.separator + "v1";

		// ONLY_SPECIFIED.add("tcas_146.suffix");
		// ONLY_SPECIFIED.add("tcas_1460.suffix");

		String chooseFileName = dir + File.separator + fileName
				+ "_choose.suffix";
		makeChooseSuffixFile(chooseFileName);

		String[] suffixFiles = ObjectArrays.concat(dir + File.separator
				+ fileName + "_array.suffix", getCommonSuffixFiles());
		suffixFiles = ObjectArrays.concat(suffixFiles, chooseFileName);
		List<String> outputLines = run(dir, fileName, funName, suffixFiles);

		// check expected:
		String[][] expected = { { "expect sat", "sat" } };
		checkExpected(outputLines, expected);
	}

	private String[] getCommonSuffixFiles() {
		File dir = new File(DIR_SUFFIX);
		Assert.assertTrue(dir.exists());
		List<String> fileNames = new ArrayList<String>();
		for (File file : dir.listFiles()) {
			String name = file.getName();
			if (!name.endsWith(".suffix"))
				continue; // ignore non-suffix files
			if (!ONLY_SPECIFIED.isEmpty()) {
				if (ONLY_SPECIFIED.contains(name)) {
					fileNames.add(file.getAbsolutePath());
				}
			} else {
				fileNames.add(file.getAbsolutePath());
			}
		}
		String[] rst = new String[fileNames.size()];
		fileNames.toArray(rst);
		return rst;
	}

	@Override
	protected void printZ3Response(String response) {
		if (IGNORE_UNKONWN_CONSTANT_ERROR
				&& response.contains("unknown constant")) {
			return;
		} else {
			System.out.println(response);
		}
	}

	private void makeChooseSuffixFile(String chooseFileName) throws IOException {
		List<String> candidates;
		if (!ONLY_SPECIFIED.isEmpty()) {
			candidates = ONLY_SPECIFIED;
		} else {
			File suffixDir = new File(DIR_SUFFIX);
			Assert.assertTrue(suffixDir.exists());
			candidates = new ArrayList<String>();
			for (File suffixFile : suffixDir.listFiles()) {
				String name = suffixFile.getName();
				candidates.add(name);
			}
		}

		File f = new File(chooseFileName);
		FileWriter fw = new FileWriter(f);
		for (String candidate : candidates) {
			if (candidate.endsWith(".suffix")) {
				String name = candidate.split(".suffix")[0];
				String caseNum = name.split("tcas_")[1];
				fw.append("(echo \"case" + caseNum + "\")\n");
				fw.append("(push)\n");
				fw.append("(assert case" + caseNum + ")\n");
				fw.append("(echo \"expect sat\")\n");
				fw.append("(check-sat)\n");
				fw.append("(pop)\n\n");
			}
		}
		fw.close();
	}

	private static void makeCommonSuffixFiles(String dir, String fileName)
			throws IOException {
		File outDir = new File(DIR_OUT);
		Assert.assertTrue(outDir.exists());

		for (File outFile : outDir.listFiles()) {
			String suffixNum = outFile.getName().substring(1);
			List<String> lines = Files.readLines(outFile, Charsets.UTF_8);
			if (lines.get(0).startsWith("Error:"))
				continue; // ignore error case
			if (hasOutput(lines)) {
				File suffixFile = new File(dir, fileName + "_" + suffixNum
						+ ".suffix");
				FileWriter fw = new FileWriter(suffixFile);
				fw.append("\n; case " + suffixNum + "\n");
				fw.append(getSuffixFileContent(lines, suffixNum));
				fw.close();
			}
		}
	}

	private static String getSuffixFileContent(List<String> lines,
			String suffixNum) throws IOException {
		StringBuffer sb = new StringBuffer();
		sb.append("(declare-const case" + suffixNum + " Bool)\n");

		List<String> nonOutputLines = getNonOutputLines(lines);
		String outputValue = getOutputValue(lines);
		sb.append(assertInput(nonOutputLines, suffixNum) + "\n");
		sb.append(assertCase(suffixNum, outputValue) + "\n");

		return sb.toString();
	}

	private static String assertCase(String suffixNum, String outputValue) {
		StringBuffer sb = new StringBuffer();
		sb.append("(assert\n");
		sb.append("(=> case" + suffixNum + "\n");
		sb.append("(= |L#result@1_F#Non_Crossing_Biased_Climb@0| "
				+ outputValue + ")");
		sb.append("))");
		return sb.toString();
	}

	private static String assertInput(List<String> nonOutputLines,
			String suffixNum) {
		StringBuffer sb = new StringBuffer();
		sb.append("(assert \n");
		sb.append("(= case" + suffixNum + "\n");
		sb.append("(and\n");

		for (String line : nonOutputLines) {
			String[] splitted = line.split("=");
			Assert.assertNotNull(splitted);
			Assert.assertTrue(splitted.length == 2);
			String varName = splitted[0];
			String val = splitted[1];

			sb.append("(= |Global#" + varName + "| " + val + ")\n");
		}

		sb.append(")))");

		return sb.toString();
	}

	private static void deleteSuffixDir(File dirSuffix) {
		if (dirSuffix.exists()) {
			for (File file : dirSuffix.listFiles()) {
				if (file.getName().endsWith(".suffix")) {
					file.delete();
				}
			}
		}

	}

	private static String getOutputValue(List<String> lines) {
		for (String line : lines) {
			if (line.startsWith("output")) {
				String[] splitted = line.split("=");
				Assert.assertNotNull(splitted);
				Assert.assertTrue(splitted.length == 2);
				String val = splitted[1];
				return val;
			}
		}
		Assert.fail();
		return null;
	}

	private static List<String> getNonOutputLines(List<String> lines) {
		List<String> rst = new ArrayList<String>();
		for (String line : lines) {
			if (!line.startsWith("output")) {
				rst.add(line);
			}
		}
		return rst;
	}

	private static boolean hasOutput(List<String> lines) {
		for (String line : lines) {
			if (line.startsWith("output")) {
				return true;
			}
		}
		return false;
	}

}
