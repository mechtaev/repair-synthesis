package edu.nus.vctrans2;

import java.util.LinkedList;
import java.util.List;

import org.smtlib.IExpr;
import org.smtlib.IExpr.IBinding;
import org.smtlib.IExpr.IFcnExpr;
import org.smtlib.IExpr.ILet;

import edu.nus.vctrans2.ast.CFunctionAst;

public class LetFormulaTransformer extends FormulaTransformer {

	public LetFormulaTransformer(CFunctionAst curFun) {
		super(curFun);
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
	public IExpr visit(ILet e) throws VisitorException {
		List<IBinding> bindings = e.bindings();
		for (IBinding binding: bindings) {
			binding.accept(this);
		}
		e.expr().accept(this);
		return e;
	}
	
	@Override
	public IExpr visit(IBinding binding) throws org.smtlib.IVisitor.VisitorException {
		IExpr newExpr = binding.expr().accept(this);
		binding.setExpr(newExpr);
		return null;
	}


}
