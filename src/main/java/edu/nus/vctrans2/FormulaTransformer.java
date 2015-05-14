package edu.nus.vctrans2;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.smtlib.IExpr;
import org.smtlib.IExpr.IAttribute;
import org.smtlib.IExpr.IAttributedExpr;
import org.smtlib.IExpr.IFcnExpr;
import org.smtlib.IExpr.IForall;
import org.smtlib.IExpr.IKeyword;
import org.smtlib.IExpr.IQualifiedIdentifier;
import org.smtlib.IExpr.ISymbol;
import org.smtlib.impl.SMTExpr;
import org.smtlib.impl.SMTExpr.FcnExpr;
import org.smtlib.impl.SMTExpr.Symbol;

import edu.nus.vctrans2.ast.CFunctionAst;
import edu.nus.vctrans2.util.TransUtil;

public class FormulaTransformer extends IdIExprVisitor {

	protected CFunctionAst curFun;
	protected Map<SMTExpr.Numeral, String> collectedStatements = new HashMap<>();
	
	protected int subsVarCount = 0;
	protected String subsVarName = "subs";	
	
	public FormulaTransformer(CFunctionAst curFun) {
		this.curFun = curFun;
	}

	@Override
	public IExpr visit(IFcnExpr e) throws VisitorException {
		if (transformIntoTrue(e)) {
			return TRUE;
		}

		// transform "=>" into "and" for BLOCKs.
		transformBlocks(e);

		List<IExpr> args = e.args();
		List<IExpr> newArgs = new LinkedList<IExpr>();
		for (IExpr arg : args) {
			IExpr r = arg.accept(this);
			if (r != null) {
				newArgs.add(r);
			}
		}
		e.setArgs(newArgs);
		return e;
	}

	@Override
	public IExpr visit(IForall e) throws VisitorException {
		IExpr innerExpr = e.expr();
		innerExpr.accept(this);
		
		return e;
	}

	// transform "=>" into "and" for BLOCKs.
	protected void transformBlocks(IFcnExpr e) {
		List<IExpr> args = e.args();
		if (TransUtil.isImpliesExpr(e)) {
			IExpr first = args.get(0);
			if (first instanceof IAttributedExpr) {
				List<IAttribute<?>> atts = ((IAttributedExpr) first)
						.attributes();
				for (IAttribute<?> att : atts) {
					IKeyword keyword = att.keyword();
					if (keyword.equals(CmpKind)) {
						Object attVal = att.attrValue();
						if (attVal.equals(BLOCK)) {
							IQualifiedIdentifier head = e.head();
							e.setHead(AND);
						}
					}
				}
			}
		}
	}

	boolean transformIntoTrue(IFcnExpr e) {
		String funName = e.head().headSymbol().value();
		List<IExpr> args = e.args();
		if (funName.equals("$full_stop") || funName.equals("$good_state_ext")
				|| funName.equals("$function_entry")
				|| funName.equals("$modifies")
				|| funName.equals("$call_transition")
				|| funName.equals("$f_closed")
				|| funName.equals("$f_timestamp")
				|| funName.equals("$f_owner")
				|| funName.equals("$roots")) {
			return true;
		} else if (funName.equals("=")) {
			IExpr first = args.get(0);
			IExpr second = args.get(1);

			if (first instanceof Symbol) {
				if (((Symbol) first).startsWith("#wrTime$")) {
					return true;
				}
			}

			if (first instanceof FcnExpr) {
				String subFunName = ((FcnExpr) first).head().headSymbol()
						.value();
				if (subFunName.equals("$writes_at")) {
					return true;
				}
			}

			if (second instanceof ISymbol) {
				ISymbol sym = (ISymbol) second;
				if (sym.value().startsWith("$decreases_level"))
					return true;
			}
		} else if (funName.equals("<=")) {
			if (isMinExpr(args.get(0)) || isMaxExpr(args.get(1))) {
				return true;
			}
			
			if (isCurrentTimestampExpr(args.get(0)) && isCurrentTimestampExpr(args.get(1))) {
				return true;
			}
		} else if (funName.equals("=>")) {
			if (isFalseExpr(args.get(0))) {
				return true;
			}
		}
		return false;
	}

	private boolean isFalseExpr(IExpr iExpr) {
		if (isNotTrueExpr(iExpr)) {
			return true;
		}
		return false;
	}

	private boolean isNotTrueExpr(IExpr iExpr) {
		if (iExpr instanceof FcnExpr) {
			if (((FcnExpr) iExpr).id().toString().equals("not")
					&& isTrueExpr(((FcnExpr) iExpr).args().get(0))) {
				return true;
			}
		}
		return false;
	}

	private boolean isTrueExpr(IExpr iExpr) {
		if (iExpr instanceof Symbol) {
			if (((Symbol) iExpr).value().equals("true")) {
				return true;
			}
		}
		return false;
	}

	private boolean isCurrentTimestampExpr(IExpr iExpr) {
		if (iExpr instanceof FcnExpr) {
			if (((FcnExpr) iExpr).id().toString().equals("$current_timestamp"))
				return true;
		}
		return false;
	}

	private boolean isMaxExpr(IExpr iExpr) {
		if (iExpr instanceof Symbol) {
			if (((Symbol) iExpr).startsWith("$max.")) {
				return true;
			}
		}
		return false;
	}

	private boolean isMinExpr(IExpr iExpr) {
		if (iExpr instanceof Symbol) {
			if (((Symbol) iExpr).startsWith("$min.")) {
				return true;
			}				
		}
		
		return false;
	}

	@Override
	public IExpr visit(ISymbol e) throws VisitorException {
		String val = e.value();
		if (val.startsWith("%lbl%") || val.equals("$position_marker")) {
			return TRUE;
		} else {
			return e;
		}
	}

	@Override
	public IExpr visit(IAttributedExpr e) throws VisitorException {
		List<IAttribute<?>> attrs = e.attributes();
		List<IAttribute<?>> newAttrs = new ArrayList<IAttribute<?>>();
		for (IAttribute<?> att : attrs) {
			IKeyword keyword = att.keyword();
			if (keyword.equals(CmpKind)
					|| keyword.equals(CmpId)) {
				newAttrs.add(att);
			} else if (keyword.equals(LblPos) || keyword.equals(LblNeg)) {
				continue;
			}
		}
		if (newAttrs.size() > 0) {
			e.setAttributes(newAttrs);
			return e;
		} else {
			return e.expr().accept(this);	
		}
	}
}
