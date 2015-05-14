package edu.nus.vctrans2;

import org.smtlib.ICommand;
import org.smtlib.ICommand.IScript;
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

public class DefaultVisitor<T> implements IVisitor<T> {

	protected static IKeyword BoogieVcId;
	{
		BoogieVcId = new SimpleKeyword() {
			@Override
			public String value() {
				return ":boogie-vc-id";
			}
		};
	}

	protected static IKeyword LblPos;
	{
		LblPos = new SimpleKeyword() {
			@Override
			public String value() {
				return ":lblpos";
			}
		};
	}

	protected static IKeyword LblNeg;
	{
		LblNeg = new SimpleKeyword() {
			@Override
			public String value() {
				return ":lblneg";
			}
		};
	}

	protected static IKeyword CmpKind;
	{
		CmpKind = new SimpleKeyword() {
			@Override
			public String value() {
				return ":cmp-kind";
			}
		};
	}

	protected static IKeyword CmpId;
	{
		CmpId = new SimpleKeyword() {
			@Override
			public String value() {
				return ":cmp-id";
			}
		};
	}
	
	@Override
	public T visit(IAttribute<?> e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IAttributedExpr e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IBinaryLiteral e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IBinding e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IDecimal e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IError e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(org.smtlib.IResponse.IError e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IExists e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IFcnExpr e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IForall e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IHexLiteral e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IKeyword e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(ILet e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(INumeral e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IDeclaration e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IParameterizedIdentifier e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IAsIdentifier e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IStringLiteral e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(ISymbol e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IScript e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(ICommand e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IFamily s) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IAbbreviation s) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IApplication s) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IFcnSort s) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IParameter s) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(ILogic s) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(ITheory s) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IResponse e) throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IAssertionsResponse e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IAssignmentResponse e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IProofResponse e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IValueResponse e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IUnsatCoreResponse e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public T visit(IAttributeList e)
			throws org.smtlib.IVisitor.VisitorException {
		// TODO Auto-generated method stub
		return null;
	}

}
