package edu.nus.mrepair.util;

import java.io.File;

// Common util
public class CUtil {

	public static String fileLoc(String loc) {
		String[] splits = loc.split("/");
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < splits.length - 1; i++) {
			sb.append(splits[i]);
			sb.append(File.separator);
		}
		sb.append(splits[splits.length-1]);
		return sb.toString();
	}
}
