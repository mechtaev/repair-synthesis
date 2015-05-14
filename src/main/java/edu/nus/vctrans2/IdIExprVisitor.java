package edu.nus.vctrans2;

import org.smtlib.IExpr;
import org.smtlib.IExpr.IAsIdentifier;
import org.smtlib.IExpr.IAttributedExpr;
import org.smtlib.IExpr.IBinaryLiteral;
import org.smtlib.IExpr.IDecimal;
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
import org.smtlib.impl.SMTExpr.StringLiteral;
import org.smtlib.impl.SMTExpr.Symbol;

public class IdIExprVisitor extends DefaultVisitor<IExpr> {

	protected static ISymbol TRUE  = new Symbol("true");
	protected static ISymbol FALSE  = new Symbol("false");
	protected static ISymbol AND   = new Symbol("and");

	protected static IStringLiteral CONDITION = new StringLiteral("CONDITION", false);
    protected static IStringLiteral CALL      = new StringLiteral("CALL", false);
	protected static IStringLiteral RHS       = new StringLiteral("RHS", false);
	protected static IStringLiteral BLOCK     = new StringLiteral("BLOCK", false);
	
	@Override
	public IExpr visit(IAttributedExpr e) throws VisitorException {
		return e;
	}

	@Override
	public IExpr visit(IBinaryLiteral e) throws VisitorException {
		return e;
	}

	@Override
	public IExpr visit(IDecimal e) throws VisitorException {
		return e;
	}

	@Override
	public IExpr visit(IError e) throws VisitorException {
		return e;
	}

	@Override
	public IExpr visit(IExists e) throws VisitorException {
		return e;
	}

	@Override
	public IExpr visit(IFcnExpr e) throws VisitorException {
		return e;
	}

	@Override
	public IExpr visit(IForall e) throws VisitorException {
		return e;
	}

	@Override
	public IExpr visit(IHexLiteral e) throws VisitorException {
		return e;
	}

	@Override
	public IExpr visit(ILet e) throws VisitorException {
		return e;
	}

	@Override
	public IExpr visit(INumeral e) throws VisitorException {
		return e;
	}

	@Override
	public IExpr visit(IParameterizedIdentifier e) throws VisitorException {
		return e;
	}

	@Override
	public IExpr visit(IAsIdentifier e) throws VisitorException {
		return e;
	}

	@Override
	public IExpr visit(IStringLiteral e) throws VisitorException {
		return e;
	}

	@Override
	public IExpr visit(ISymbol e) throws VisitorException {
		return e;
	}
	
}
