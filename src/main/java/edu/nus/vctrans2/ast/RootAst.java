package edu.nus.vctrans2.ast;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class RootAst extends DefaultVcAst {

	private Map<String,CFunctionAst> cFunAstMap = new HashMap<String,CFunctionAst>();
	private Map<String,DeclareFunAst> declareFunAstMap = new HashMap<String,DeclareFunAst>();
	private List<DeclareSortAst> declareSortList = new LinkedList<DeclareSortAst>();
	private List<DeclareAst> declareAstList = new LinkedList<DeclareAst>();
	private List<AssertAst> assertList = new LinkedList<AssertAst>();
	private List<DefineAst> defineAstList = new LinkedList<DefineAst>();

	public void addCFunctionAst(CFunctionAst ast) {
		cFunAstMap.put(ast.getName(), ast);
	}

	public void addDeclareFunAst(DeclareFunAst ast) {
		declareFunAstMap.put(ast.getName(), ast);
		declareAstList.add(ast);
	}
	
	public void addDefineFunAst(DefineFunAst ast) {
		defineAstList.add(ast);		
	}	
	
	public void addDeclareSortAst(DeclareSortAst ast) {
		declareSortList.add(ast);
		declareAstList.add(ast);
	}	

	public void addAssertAst(AssertAst ast) {
		assertList.add(ast);
	}
	
	public List<AssertAst> getAsserts() {
		return this.assertList;
	}

	public CFunctionAst getCFunctionAst(String cFunName) {
		CFunctionAst rst = cFunAstMap.get(cFunName);	
		return rst;
	}

	public DeclareFunAst getFunctionDeclaration(String name) {
		DeclareFunAst rst = declareFunAstMap.get(name);	
		return rst;
	}
	
	public List<DeclareSortAst> getAllDeclareSorts() {
		return this.declareSortList;
	}

    //FIXME it is better to separate funs and sorts because it causes sorts duplication problem
	public List<DeclareAst> getAllDeclares() {
		return this.declareAstList;
	}
	
	public List<DefineAst> getAllDefines() {
		return this.defineAstList;
	}
		
}
