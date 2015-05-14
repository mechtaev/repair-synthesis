package edu.nus.vctrans2.ast;

import java.util.List;

import org.smtlib.ICommand;
import org.smtlib.ICommand.IScript;
import org.smtlib.IExpr;
import org.smtlib.IExpr.IAsIdentifier;
import org.smtlib.IExpr.IAttribute;
import org.smtlib.IExpr.IAttributedExpr;
import org.smtlib.IExpr.IBinaryLiteral;
import org.smtlib.IExpr.IBinding;
import org.smtlib.IExpr.IDecimal;
import org.smtlib.IExpr.IDeclaration;
import org.smtlib.IExpr.IError;
import org.smtlib.IExpr.IExists;
import org.smtlib.IExpr.IFcnExpr;
import org.smtlib.IExpr.IForall;
import org.smtlib.IExpr.IHexLiteral;
import org.smtlib.IExpr.IKeyword;
import org.smtlib.IExpr.ILet;
import org.smtlib.IExpr.INumeral;
import org.smtlib.IExpr.IParameterizedIdentifier;
import org.smtlib.IExpr.IStringLiteral;
import org.smtlib.IExpr.ISymbol;
import org.smtlib.ILogic;
import org.smtlib.IResponse;
import org.smtlib.IResponse.IAssertionsResponse;
import org.smtlib.IResponse.IAssignmentResponse;
import org.smtlib.IResponse.IAttributeList;
import org.smtlib.IResponse.IProofResponse;
import org.smtlib.IResponse.IUnsatCoreResponse;
import org.smtlib.IResponse.IValueResponse;
import org.smtlib.ISort.IAbbreviation;
import org.smtlib.ISort.IApplication;
import org.smtlib.ISort.IFamily;
import org.smtlib.ISort.IFcnSort;
import org.smtlib.ISort.IParameter;
import org.smtlib.ITheory;
import org.smtlib.IVisitor;
import org.smtlib.command.C_define_fun;
import org.smtlib.sexpr.Utils;

import edu.nus.vctrans2.util.TransUtil;

public class PrettyPrinter implements IVisitor<String> {

	private int indentCount = 0;
	private static int indentSize = 2;
	
	private String indent() {
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < indentCount; i++) {
			sb.append(" ");
		}
		return sb.toString();
	}
	
	private void incIndent() {
		indentCount += indentSize;
	}
	
	private void decIndent() {
		indentCount -= indentSize;
	}
	
	@Override
	public String visit(IAttribute<?> e)
			throws org.smtlib.IVisitor.VisitorException {
		return indent() + e.toString();
	}

	@Override
	public String visit(IBinding e) throws org.smtlib.IVisitor.VisitorException {
		StringBuffer sb = new StringBuffer();
		sb.append("(");
		sb.append(e.parameter());
		sb.append("\n");
		String exprStr = e.expr().accept(this);
		sb.append("  " + exprStr);
		sb.append(")");		
		return sb.toString();
	}
	
	@Override
	public String visit(IBinaryLiteral e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return e.toString();
	}

	@Override
	public String visit(IDecimal e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return e.toString();
	}

	@Override
	public String visit(IError e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return e.toString();
	}

	@Override
	public String visit(org.smtlib.IResponse.IError e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return e.toString();
	}

	@Override
	public String visit(IExists e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return e.toString();
	}
	
	@Override
	public String visit(IAttributedExpr e)
			throws org.smtlib.IVisitor.VisitorException {
		StringBuffer sb = new StringBuffer();
		sb.append(indent() + "(!");
		incIndent();
		
		String exprStr = e.expr().accept(this);
		sb.append("\n" + exprStr);
		
		for (IAttribute<?> attr : e.attributes()) {
			String attrStr = attr.accept(this);
			sb.append("\n" + attrStr);
		}
		
		sb.append(" )");
		decIndent();
		return sb.toString();
	}

	@Override
	public String visit(IFcnExpr e) throws org.smtlib.IVisitor.VisitorException {
		StringBuffer sb = new StringBuffer();
		List<IExpr> args = e.args();
        if (TransUtil.isImpliesExpr(e)
        		|| TransUtil.isEqExpr(e)
        		|| TransUtil.isGtExpr(e)
        		|| TransUtil.isGeExpr(e)
        		|| TransUtil.isLtExpr(e)
        		|| TransUtil.isLeExpr(e)
        		|| TransUtil.isIteExpr(e)
        		|| TransUtil.isNotExpr(e)
        		|| TransUtil.isAndExpr(e)) {
			sb.append(indent() + "(");
			sb.append(e.head() + " ");
			incIndent();

			for (IExpr arg: args) {
				String argStr = arg.accept(this);
				sb.append("\n" + argStr);
			}

			sb.append(" )");
			decIndent();
		} else {
			sb.append(indent() + e.toString());
		}
		return sb.toString();
	}

	@Override
	public String visit(ILet e) throws org.smtlib.IVisitor.VisitorException {
		StringBuffer sb = new StringBuffer();
		sb.append("(" + Utils.LET + "\n");
		// incIndent();
		sb.append(indent() + "(");
		for (IBinding a: e.bindings()) {
			String argStr = a.accept(this);
			sb.append(argStr);
		}
		sb.append(") ");
		String exprStr = e.expr().accept(this);
		sb.append("\n" + indent() + exprStr + ")");
		// decIndent();
		return sb.toString();
	}

	@Override
	public String visit(IForall e) throws org.smtlib.IVisitor.VisitorException {
		return indent() + e.toString();
	}

	@Override
	public String visit(IHexLiteral e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return indent() + e.toString();
	}

	@Override
	public String visit(IKeyword e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return indent() + e.toString();
	}
	
	@Override
	public String visit(INumeral e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return indent() + e.toString();
	}

	@Override
	public String visit(IDeclaration e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return indent() + e.toString();
	}

	@Override
	public String visit(IParameterizedIdentifier e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return indent() + e.toString();
	}

	@Override
	public String visit(IAsIdentifier e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return indent() + e.toString();
	}

	@Override
	public String visit(IStringLiteral e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return indent() + e.toString();
	}

	@Override
	public String visit(ISymbol e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return indent() + e.toString();
	}

	@Override
	public String visit(IScript e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return indent() + e.toString();
	}

	@Override
	public String visit(ICommand e) throws org.smtlib.IVisitor.VisitorException {
		if (e instanceof C_define_fun) {
			C_define_fun defFun = (C_define_fun) e;
			StringBuilder sb = new StringBuilder();
			sb.append("(define-fun ");
			sb.append(defFun.symbol());
			sb.append(" ");
			sb.append(defFun.getParameterString());
			sb.append(" ");
			sb.append(defFun.resultSort());
			sb.append("\n");			
			sb.append(defFun.expression().accept(this));
			sb.append("); define-fun");
			return sb.toString();
		} else {
			return e.toString();
		}
	}

	@Override
	public String visit(IFamily s) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return s.toString();
	}

	@Override
	public String visit(IAbbreviation s)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return s.toString();
	}

	@Override
	public String visit(IApplication s)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return s.toString();
	}

	@Override
	public String visit(IFcnSort s) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return s.toString();
	}

	@Override
	public String visit(IParameter s)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return s.toString();
	}

	@Override
	public String visit(ILogic s) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return s.toString();
	}

	@Override
	public String visit(ITheory s) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return s.toString();
	}

	@Override
	public String visit(IResponse e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return e.toString();
	}

	@Override
	public String visit(IAssertionsResponse e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return e.toString();
	}

	@Override
	public String visit(IAssignmentResponse e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return e.toString();
	}

	@Override
	public String visit(IProofResponse e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return e.toString();
	}

	@Override
	public String visit(IValueResponse e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return e.toString();
	}

	@Override
	public String visit(IUnsatCoreResponse e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return e.toString();
	}

	@Override
	public String visit(IAttributeList e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return e.toString();
	}

}
