package edu.nus.vctrans.ast;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.smtlib.IExpr;
import org.smtlib.IExpr.IForall;
import org.smtlib.IVisitor.VisitorException;

public class CFunctionAst extends DefaultVcAst {

	private String name;
	private IExpr expr;
	
	private Map<String, IExpr> conditionSet = new HashMap<String, IExpr>();
	private Map<String, IExpr> assignmentSet = new HashMap<String, IExpr>();
	
	private Set<IExpr> assumptionSet= new HashSet<IExpr>();

	public CFunctionAst(String name) {
		this.name = name;
	}
	
	public void setExpr(IExpr expr) {
		this.expr = expr;
	}
	
	public String getName() {
		return this.name;
	}
	
	public IExpr getExpr() {
		return this.expr;
	}

	public void addCondition(String var, IExpr cond) {
		conditionSet.put(var, cond);		
	}
	
	public void addAssignment(String var, IExpr cond) {
		assignmentSet.put(var, cond);		
	}
	
	public void addAssumption(IExpr e) {
		assumptionSet.add(e);		
	}
	
	public boolean hasCondition(IExpr expr) {
		return conditionSet.values().contains(expr);
	}
	
	public boolean hasAssignment(IExpr expr) {
		return assignmentSet.values().contains(expr);
	}

	public Map<String, IExpr> getConditions() {
		return this.conditionSet;
	}
	
	public Map<String, IExpr> getAssignments() {
		return this.assignmentSet;
	}
	
	public Set<IExpr> getAssumptions() {
		return this.assumptionSet;
	}

	public Map<String, IExpr> getRepairableComponents() {
		HashMap<String, IExpr> rst = new HashMap<String, IExpr>(this.conditionSet);
		rst.putAll(this.assignmentSet);
		return rst;
	}
	
	public String getExprStr() {
		try {
			String rst = expr.accept(new PrettyPrinter());
			return rst;
		} catch (VisitorException e) {
			throw new Error("Failed to pretty-print");
		}
	}

	@Override
	public String toString() {		
		try {
			StringBuffer sb = new StringBuffer();
			sb.append(name + ":\n");			
			String rst = expr.accept(new PrettyPrinter());
			sb.append(rst);
			return sb.toString();
		} catch (VisitorException e) {
			throw new Error("Failed to pretty-print");
		}
	}

}
