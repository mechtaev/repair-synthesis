package edu.nus.vctrans.ast;

import edu.nus.vctrans.DefaultVisitor;
import edu.nus.vctrans.IdIExprVisitor;
import org.smtlib.IExpr;
import org.smtlib.impl.SMTExpr;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by seryozha on 3/21/14.
 */
public class VariableCollector extends DefaultVisitor<Void> {

    public ArrayList<String> getCollectedVariable() {
        return collectedVariable;
    }

    private ArrayList<String> collectedVariable = new ArrayList<>();

    @Override
    public Void visit(IExpr.IAttributedExpr e) throws VisitorException {
        e.expr().accept(this);
        return null;
    }

    @Override
    public Void visit(IExpr.IFcnExpr e) throws VisitorException {
        for(IExpr arg: e.args()) {
            arg.accept(this);
        }
        return null;
    }

    @Override
    public Void visit(IExpr.ISymbol e) throws VisitorException {
        collectedVariable.add(e.value());
        return null;
    }

}
