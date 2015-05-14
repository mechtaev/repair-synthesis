package edu.nus.vctrans.ast;

import org.smtlib.command.C_assert;

public class AssertAst extends DeclareAst  {

	private C_assert jSMTLIBAst; 
	
	public AssertAst(C_assert jSMTLIBAst) {
		this.jSMTLIBAst = jSMTLIBAst;
	}

	@Override
	public String toString() {
		return jSMTLIBAst.toString();
	}
}
