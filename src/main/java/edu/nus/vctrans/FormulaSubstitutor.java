package edu.nus.vctrans;

import java.util.ArrayList;
import java.util.List;

import org.smtlib.IExpr;
import org.smtlib.IExpr.IAttribute;
import org.smtlib.IExpr.IAttributedExpr;
import org.smtlib.IExpr.IKeyword;
import org.smtlib.impl.SMTExpr;
import org.smtlib.impl.SMTExpr.FcnExpr;
import org.smtlib.impl.SMTExpr.Symbol;

import edu.nus.vctrans.ast.CFunctionAst;

public class FormulaSubstitutor extends FormulaTransformer {

	public FormulaSubstitutor(CFunctionAst curFun) {
		super(curFun);
	}

	@Override
	public IExpr visit(IAttributedExpr e) throws VisitorException {
		IExpr updatedNode = null;
		IExpr extractedExpr = null;
		String substitutedVar = null;
		SMTExpr.Numeral stmtId = null;

		List<IAttribute<?>> atts = e.attributes();

		for (IAttribute<?> att : atts) {
			IKeyword keyword = att.keyword();
			if (keyword.equals(CmpId)) {
				assert att.attrValue() instanceof SMTExpr.Numeral;
				stmtId = (SMTExpr.Numeral) att.attrValue();
			}
		}

		for (IAttribute<?> att : atts) {
			IKeyword keyword = att.keyword();
			if (keyword.equals(CmpKind)) {
				IExpr expr = e.expr();
				expr.accept(this);
				assert updatedNode == null : "updatedNode is already set";
				updatedNode = expr;

                Object attVal = att.attrValue();
                if (attVal.equals(CONDITION) || attVal.equals(RHS) || attVal.equals(CALL)) {
                    if (stmtId == null || stmtId.intValue() != 0) {
                        if (stmtId != null
                                && collectedStatements.containsKey(stmtId)) {
                            substitutedVar = collectedStatements.get(stmtId);
                        } else {
                            substitutedVar = subsVarName + subsVarCount;
                            subsVarCount++;
                        }
                        updatedNode = new Symbol(substitutedVar);
                        extractedExpr = expr;
                    } else {
                        updatedNode = expr;
                    }
                }
			} else if (keyword.equals(LblPos) || keyword.equals(LblNeg)) {
				IExpr innerExpr = e.expr();
				assert updatedNode == null : "updatedNode is already set";
                updatedNode = innerExpr.accept(this);
			}
		}

		if (substitutedVar != null && !collectedStatements.containsKey(stmtId)) {
			curFun.addCondition(substitutedVar, extractedExpr);
			collectedStatements.put(stmtId, substitutedVar);
		}

        //TODO case when we don't have kind annotation, for now just ignore
        if (stmtId != null && updatedNode == null) {
            updatedNode = e.expr().accept(this);
        }

        if (updatedNode == null) {
        	throw new Error("there is an unhandled case");
        }

		assert updatedNode != null : "there is an unhandled case";
		return updatedNode;
	}
	
}
