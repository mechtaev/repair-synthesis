package edu.nus.vctrans2.ast;

import org.smtlib.command.C_declare_sort;

public class DeclareSortAst extends DeclareAst {

	private C_declare_sort jSMTLIBAst; 

	public DeclareSortAst(C_declare_sort jSMTLIBAst) {
		this.jSMTLIBAst = jSMTLIBAst;		
	}
	
	@Override
	public String toString() {
		return jSMTLIBAst.toString();
	}

}
