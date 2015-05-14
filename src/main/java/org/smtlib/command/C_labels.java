package org.smtlib.command;

import org.smtlib.ICommand.Ilabels;
import org.smtlib.IResponse;
import org.smtlib.ISolver;
import org.smtlib.IVisitor;
import org.smtlib.IVisitor.VisitorException;
import org.smtlib.impl.Command;
import org.smtlib.sexpr.Parser;

public class C_labels extends Command implements Ilabels {

	/** The command name */
	public static final String commandName = "labels";

	@Override
	public IResponse execute(ISolver solver) {
		throw new Error("Unsupported");
	}

	@Override
	public <T> T accept(IVisitor<T> v) throws VisitorException {
		return v.visit(this);			
	}
	
	static public /*@Nullable*/ C_labels parse(Parser p) {
		return new C_labels();
	}

	@Override
	public String commandName() {
		return commandName;
	}

}
