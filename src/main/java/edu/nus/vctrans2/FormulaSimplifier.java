package edu.nus.vctrans2;

import java.util.LinkedList;
import java.util.List;

import org.smtlib.IExpr;
import org.smtlib.IExpr.IBinding;
import org.smtlib.IExpr.IFcnExpr;
import org.smtlib.IExpr.ILet;

import edu.nus.vctrans2.util.TransUtil;

public class FormulaSimplifier extends IdIExprVisitor {

	@Override
	public IExpr visit(IFcnExpr e) throws VisitorException {
		if (TransUtil.isComparison(e)) {
			for (IExpr arg: e.args()) {
				if (TransUtil.isCurrentTimestampExpr(arg)) {
					return TRUE;
				}
			}			
		}
		
		if (TransUtil.isImpliesExpr(e)) {
			List<IExpr> args = e.args();
			assert args.size() == 2;

			List<IExpr> newArgs = new LinkedList<IExpr>();
			IExpr first = args.get(0);
			IExpr second = args.get(1);

			IExpr newFirst = first.accept(this);
			if (newFirst.equals(TRUE)) {
				IExpr newSecond = second.accept(this);
				return newSecond;
			} else {
				newArgs.add(newFirst);
				IExpr newSecond = second.accept(this);
				newArgs.add(newSecond);
			}

			e.setArgs(newArgs);
			return e;
		} else if (TransUtil.isAndExpr(e)) {
			List<IExpr> args = e.args();
			List<IExpr> newArgs = new LinkedList<IExpr>();
			boolean allTrue = true;
			for (IExpr arg : args) {
				IExpr newArg = arg.accept(this);
				if (!newArg.equals(TRUE)) {
					allTrue = false;
					newArgs.add(newArg);
				}
			}
			if (allTrue) {
				return TRUE;
			} else if (newArgs.size() == 1) {
				return newArgs.get(0);
			} else {
				e.setArgs(newArgs);
				return e;
			}
		} else if (TransUtil.isOrExpr(e)) {
			List<IExpr> args = e.args();
			List<IExpr> newArgs = new LinkedList<IExpr>();
			for (IExpr arg : args) {
				IExpr newArg = arg.accept(this);
				if (newArg.equals(TRUE)) {
					return TRUE;
				} else {
					newArgs.add(newArg);
				}
			}
			e.setArgs(newArgs);
			return e;
		} else if (TransUtil.isEqExpr(e)) {
			List<IExpr> args = e.args();
			
			IExpr newArg1 = args.get(0).accept(this);
			IExpr newArg2 = args.get(1).accept(this);
			
			if (newArg1.equals(TRUE) && newArg2.equals(TRUE)) {
				return TRUE;
			} else {
				List<IExpr> newArgs = new LinkedList<IExpr>();				
				newArgs.add(newArg1);
				newArgs.add(newArg2);
				e.setArgs(newArgs);
				return e;
			}
		} else if (TransUtil.isNotExpr(e)) {
			List<IExpr> args = e.args();
			IExpr newArg = args.get(0).accept(this);
			if (newArg.equals(TRUE)) {
				return FALSE;
			} else if (newArg.equals(FALSE)) {
				return TRUE;
			} else {
				List<IExpr> newArgs = new LinkedList<IExpr>();
				newArgs.add(newArg);
				e.setArgs(newArgs);
				return e;
			}
		} else {
			for (IExpr arg: e.args()) {
				arg.accept(this);
			}
		}
		return e;
	}
	
	@Override
	public IExpr visit(ILet e) throws VisitorException {
		for (IBinding binding: e.bindings()) {
			binding.accept(this);
		}
		e.expr().accept(this);
		return e;
	}
	
	@Override
	public IExpr visit(IBinding b) throws VisitorException {
		IExpr newExpr = b.expr().accept(this);
		b.setExpr(newExpr);
		return null;
	}
}
