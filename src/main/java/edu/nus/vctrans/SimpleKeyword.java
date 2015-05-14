package edu.nus.vctrans;

import org.smtlib.IExpr.IKeyword;
import org.smtlib.IPos;
import org.smtlib.IVisitor;
import org.smtlib.IVisitor.VisitorException;

public class SimpleKeyword implements IKeyword {

	@Override
	public <T> T accept(IVisitor<T> v) throws VisitorException {
		assert false;
		throw new Error("Unsupported");
	}

	@Override
	public IPos pos() {
		assert false;
		throw new Error("Unsupported");
	}

	@Override
	public void setPos(IPos pos) {
		assert false;
		throw new Error("Unsupported");
	}

	@Override
	public String value() {
		assert false;
		throw new Error("Unsupported");
	}

	@Override
	public String kind() {
		assert false;
		throw new Error("Unsupported");
	}

}
