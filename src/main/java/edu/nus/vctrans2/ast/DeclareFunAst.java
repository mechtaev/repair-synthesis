package edu.nus.vctrans2.ast;

import org.smtlib.command.C_declare_fun;
import org.smtlib.impl.SMTExpr;
import scala.Function1;

public class DeclareFunAst extends DeclareAst {
	
	private String name;
    private String typ;
	private C_declare_fun jSMTLIBAst; 

	public DeclareFunAst(C_declare_fun jSMTLIBAst) {
		this.jSMTLIBAst = jSMTLIBAst;
		this.name = jSMTLIBAst.symbol().value();
        this.typ = jSMTLIBAst.resultSort().toString();
	}

	public String getName() {
		return this.name;
	}

    public String getType() {
        return this.typ;
    }

    public DeclareFunAst rename(Function1<String, String> modify) {
        String newName = modify.apply(jSMTLIBAst.symbol().value());
        C_declare_fun newFun =
                new C_declare_fun(new SMTExpr.Symbol(newName), jSMTLIBAst.argSorts(), jSMTLIBAst.resultSort());
        return new DeclareFunAst(newFun);
    }
	
	@Override
	public String toString() {
		return jSMTLIBAst.toString();
	}
}
