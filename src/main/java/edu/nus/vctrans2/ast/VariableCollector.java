package edu.nus.vctrans2.ast;

import java.util.ArrayList;

import org.smtlib.IExpr;

import edu.nus.vctrans.DefaultVisitor;

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
