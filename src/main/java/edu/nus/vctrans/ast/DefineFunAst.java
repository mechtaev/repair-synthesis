package edu.nus.vctrans.ast;

import org.smtlib.IExpr;
import org.smtlib.IVisitor.VisitorException;
import org.smtlib.command.C_define_fun;
import org.smtlib.impl.SMTExpr;
import scala.Function1;

import java.util.LinkedList;
import java.util.List;

public class DefineFunAst extends DefineAst {

	public C_define_fun jSMTLIBAst;
	private String name;

	public DefineFunAst(C_define_fun jSMTLIBAst) {
		this.jSMTLIBAst = jSMTLIBAst;
		this.name = jSMTLIBAst.symbol().value();
	}

	public String getName() {
		return this.name;
	}

	public void setBody(IExpr bodyExp) {
		this.jSMTLIBAst.setExpression(bodyExp);
	}

    public DefineFunAst rename(Function1<String, String> modifyName, Function1<IExpr, IExpr> modifyBody) {
        C_define_fun newDefine = new C_define_fun(new SMTExpr.Symbol(modifyName.apply(name)),
                jSMTLIBAst.parameters(),
                jSMTLIBAst.resultSort(),
                modifyBody.apply(jSMTLIBAst.expression()));
        return new DefineFunAst(newDefine);
    }


    public List<String> getParameters() {
        List<String> params = new LinkedList<>();
        for(IExpr.IDeclaration p: jSMTLIBAst.parameters()) {
            params.add(p.parameter().value());
        }
        return params;
    }

	public IExpr getBodyExp() {
		IExpr exp = this.jSMTLIBAst.expression();
		return exp;
	}
	
	public String toPrettyString() {
		try {
			String rst = jSMTLIBAst.accept(new PrettyPrinter());
			return rst;
		} catch (VisitorException e) {
			throw new Error("Failed to pretty-print");
		}
	}
	
	@Override
	public String toString() {
		return jSMTLIBAst.toString();
	}

}
