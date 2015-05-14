package edu.nus.vctrans.util;

import org.smtlib.IExpr;
import org.smtlib.IExpr.IFcnExpr;
import org.smtlib.IExpr.IQualifiedIdentifier;
import org.smtlib.IExpr.ISymbol;

public class TransUtil {

	public static boolean isNotExpr(IFcnExpr e) {
		return TransUtil.isFunction(e, "not");
	}

	public static boolean isComparison(IFcnExpr e) {
		return TransUtil.isEqExpr(e) || TransUtil.isGeExpr(e)
				|| TransUtil.isGtExpr(e) || TransUtil.isLtExpr(e)
				|| TransUtil.isLeExpr(e);
	}

	public static boolean isImpliesExpr(IFcnExpr e) {
		return TransUtil.isFunction(e, "=>");
	}

	public static boolean isEqExpr(IFcnExpr e) {
		return TransUtil.isFunction(e, "=");
	}

	public static boolean isGeExpr(IFcnExpr e) {
		return TransUtil.isFunction(e, ">=");
	}

	public static boolean isGtExpr(IFcnExpr e) {
		return TransUtil.isFunction(e, ">");
	}

	public static boolean isLeExpr(IFcnExpr e) {
		return TransUtil.isFunction(e, "<=");
	}

	public static boolean isLtExpr(IFcnExpr e) {
		return TransUtil.isFunction(e, "<");
	}

	public static boolean isIteExpr(IFcnExpr e) {
		return TransUtil.isFunction(e, "ite");
	}

	public static boolean isAndExpr(IFcnExpr e) {
		return TransUtil.isFunction(e, "and");
	}

	public static boolean isOrExpr(IFcnExpr e) {
		return TransUtil.isFunction(e, "or");
	}

	public static boolean isFunction(IFcnExpr e, String name) {
		IQualifiedIdentifier head = e.head();
		ISymbol headSym = head.headSymbol();
		String val = headSym.value();
		return val.equals(name);
	}

	public static boolean isCurrentTimestampExpr(IExpr e) {
		if (!(e instanceof IFcnExpr)) {
			return false;
		}
		try {
			IFcnExpr fe = (IFcnExpr) e;
			return fe.head().headSymbol().value().equals("$current_timestamp");
		} catch (Exception ex) {
			return false;
		}
	}
}
