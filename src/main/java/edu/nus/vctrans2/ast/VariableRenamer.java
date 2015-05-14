package edu.nus.vctrans2.ast;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.smtlib.IExpr;
import org.smtlib.impl.SMTExpr;

import scala.Function1;
import edu.nus.vctrans.IdIExprVisitor;

/**
 * Created by seryozha on 2/26/14.
 */
public class VariableRenamer extends IdIExprVisitor {

    private Function1<String, String> modify;

    public VariableRenamer(Function1<String, String> modify) {
        this.modify = modify;

        stdFuncs.add("+");
        stdFuncs.add("-");
        stdFuncs.add("*");
        stdFuncs.add("/");

        stdFuncs.add(">");
        stdFuncs.add("<");
        stdFuncs.add(">=");
        stdFuncs.add("<=");
        stdFuncs.add("=");

        stdFuncs.add("and");
        stdFuncs.add("or");
        stdFuncs.add("iff");
        stdFuncs.add("ite");
        stdFuncs.add("not");
        stdFuncs.add("=>");

        //for tcas:
        stdFuncs.add("select");
    }

    @Override
    public IExpr visit(IExpr.IAttributedExpr e) throws VisitorException {
        IExpr renamedBody = e.expr().accept(this);
        return new SMTExpr.AttributedExpr(renamedBody, e.attributes());
    }


    @Override
    public IExpr visit(IExpr.ISymbol e) throws VisitorException {
        return new SMTExpr.Symbol(modify.apply(e.value()));
    }

    @Override
    public IExpr visit(IExpr.ILet e) throws VisitorException {
        List<IExpr.IBinding> renamedBindings = new ArrayList<>();
        for(IExpr.IBinding b: e.bindings()) {
            IExpr.ISymbol newPar = new SMTExpr.Symbol(modify.apply(b.parameter().value()));
            IExpr.IBinding newB = new SMTExpr.Binding(newPar, b.expr().accept(this));
            renamedBindings.add(newB);
        }
        IExpr renamedExpr = e.expr().accept(this);
        return new SMTExpr.Let(renamedBindings, renamedExpr);
    }

    Set<String> stdFuncs = new TreeSet<String>();

    @Override
    public IExpr visit(IExpr.IFcnExpr e) throws VisitorException {
        String funSymbol = e.head().headSymbol().value();
        if(!stdFuncs.contains(funSymbol)) {
            funSymbol = modify.apply(funSymbol);
        }
        IExpr.IQualifiedIdentifier q = new SMTExpr.Symbol(funSymbol);
        List<IExpr> newExprs = new ArrayList<>();
        for(IExpr arg: e.args()) {
            newExprs.add(arg.accept(this));
        }
        return new SMTExpr.FcnExpr(q, newExprs);
    }



}

