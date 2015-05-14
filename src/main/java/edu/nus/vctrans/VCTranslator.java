package edu.nus.vctrans;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.UUID;

import edu.nus.vctrans.subsmode.NoSubstitution;
import edu.nus.vctrans.subsmode.SubstituteAll;
import edu.nus.vctrans.subsmode.SubstituteInFunctions;
import edu.nus.vctrans.subsmode.SubstitutionMode;
import org.smtlib.ICommand;
import org.smtlib.ICommand.IScript;
import org.smtlib.IExpr;
import org.smtlib.IExpr.IAttribute;
import org.smtlib.IExpr.IAttributeValue;
import org.smtlib.IExpr.IBinding;
import org.smtlib.IExpr.IFcnExpr;
import org.smtlib.IExpr.IKeyword;
import org.smtlib.IExpr.ILet;
import org.smtlib.IExpr.IQualifiedIdentifier;
import org.smtlib.IExpr.ISymbol;
import org.smtlib.IParser;
import org.smtlib.IParser.ParserException;
import org.smtlib.ISource;
import org.smtlib.SMT;
import org.smtlib.command.C_assert;
import org.smtlib.command.C_declare_fun;
import org.smtlib.command.C_declare_sort;
import org.smtlib.command.C_define_fun;
import org.smtlib.command.C_set_info;
import org.smtlib.impl.Pos.Source;
import org.smtlib.impl.SMTExpr.AttributedExpr;
import org.smtlib.impl.SMTExpr.FcnExpr;

import edu.nus.vctrans.ast.AssertAst;
import edu.nus.vctrans.ast.CFunctionAst;
import edu.nus.vctrans.ast.DeclareFunAst;
import edu.nus.vctrans.ast.DeclareSortAst;
import edu.nus.vctrans.ast.DefineFunAst;
import edu.nus.vctrans.ast.IVcAst;
import edu.nus.vctrans.ast.RootAst;
import edu.nus.vctrans.util.MutableBoolean;

public class VCTranslator extends DefaultVisitor<IVcAst> {


    protected IScript script;
	protected String curFunName;
	protected Map<String, CFunctionAst> funMap = new HashMap<String, CFunctionAst>();
	protected boolean expectFunctionBody = false;
    private SubstitutionMode substitutionMode;
	private boolean canSubstituteAnnotated = true;

    VCTranslator(IScript script, SubstitutionMode substitutionMode) {
		this.script = script;
        this.substitutionMode = substitutionMode;
	}

	public static RootAst translateSmt2(File smt2File, SubstitutionMode substitutionMode) throws IOException {
    UUID uuid = UUID.randomUUID();
		File tmpFile = new File(uuid.toString() + ".smt2");
		makeScript(smt2File, tmpFile);
		RootAst rst = translate(tmpFile, substitutionMode);
		return rst;
	}

	private static void makeScript(File smt2File, File scriptFile)
			throws IOException {
		FileReader input = null;
		FileWriter output = null;
		try {
			assert smt2File.exists() : smt2File.getName() + " does not exist";
			input = new FileReader(smt2File);
			output = new FileWriter(scriptFile);

			output.write("(\n");

			char[] buf = new char[1024];
			int bytesRead;
			while ((bytesRead = input.read(buf)) > 0) {
				output.write(buf, 0, bytesRead);
			}

			output.write("\n)");
		} finally {
			input.close();
			output.close();
		}
	}

	// translate a script file
	public static RootAst translate(File file, SubstitutionMode substitutionMode) throws FileNotFoundException {

		SMT smt = new SMT();
		// smt.smtConfig.jsmtLibDebug = true;
		ISource src = new Source(smt.smtConfig, file);

		IParser parser = smt.smtConfig.smtFactory.createParser(smt.smtConfig,
				src);
		IScript script = null;
		try {
			script = parser.parseScript();
		} catch (ParserException e) {
			throw new Error("Faield to parse");
		} catch (IOException e) {
			throw new Error("Faield to parse");
		}

		assert script != null;
		VCTranslator vcTrans = new VCTranslator(script, substitutionMode);
		RootAst root = vcTrans.translate();
		return root;
	}

	RootAst translate() {
		try {
			IVcAst rst = script.accept(new DefaultVisitor<IVcAst>() {
				
				protected CFunctionAst curFun;
						
				@Override
				public IVcAst visit(IScript e) throws VisitorException {
					RootAst root = new RootAst();
					for (ICommand command : e.commands()) {
						IVcAst ast = command.accept(this);
						if (ast != null) {
							if (ast instanceof CFunctionAst) {
								root.addCFunctionAst((CFunctionAst) ast);
							} else if (ast instanceof DeclareFunAst) {
								root.addDeclareFunAst((DeclareFunAst) ast);
							} else if (ast instanceof DeclareSortAst) {
								root.addDeclareSortAst((DeclareSortAst) ast);
							} else if (ast instanceof DefineFunAst) {
								DefineFunAst defFunAst = (DefineFunAst) ast;
//                                if ("Non_Crossing_Biased_Climb@0".equals(defFunAst.getName())) {
//                                    System.out.println("HERE");
//                                }
								IExpr bodyExp = defFunAst.getBodyExp();
								if (bodyExp instanceof ILet) {
									bodyExp = unfoldLet((ILet) bodyExp,
											new Stack<List<IBinding>>());
								}
//                                System.out.println(bodyExp);
								IExpr filtered;
                                Boolean oldValue = canSubstituteAnnotated;
                                if (substitutionMode instanceof NoSubstitution) {
                                    canSubstituteAnnotated = false;
                                } else if (substitutionMode instanceof SubstituteAll) {
                                    canSubstituteAnnotated = true;
                                } else if (substitutionMode instanceof SubstituteInFunctions) {
                                    Set<String> set =
                                            ((SubstituteInFunctions) substitutionMode).getInterestingFunctions();
                                    canSubstituteAnnotated = set.contains(defFunAst.getName());
                                }
                                filtered = transformBodyFormula(bodyExp);
								IExpr simplified = simplifyBodyFormula(filtered);
                                canSubstituteAnnotated = oldValue;
                                defFunAst.setBody(simplified);
								root.addDefineFunAst(defFunAst);
							} else if (ast instanceof AssertAst) {
								root.addAssertAst((AssertAst) ast);
							}
						}
					}
					return root;
				}

				@Override
				public IVcAst visit(ICommand e) throws VisitorException {
					if (e instanceof C_set_info) {
						C_set_info setInfo = (C_set_info) e;
						IKeyword option = setInfo.option();
						if (option.equals(BoogieVcId)) {
							IAttributeValue value = setInfo.value();
							assert value != null;
							curFunName = value.toString();
							curFun = new CFunctionAst(curFunName);
							funMap.put(curFunName, curFun);
							expectFunctionBody = true;
						}
						return null;
					} else if (e instanceof C_assert) {
						if (expectFunctionBody) {
							C_assert asrt = (C_assert) e;
							IExpr unfolded = unfoldFunBodyFormula(asrt.expr());
							IExpr filtered = transformBodyFormula(unfolded);
							IExpr simplified = simplifyBodyFormula(filtered);
							curFun.setExpr(simplified);
							expectFunctionBody = false;
							return curFun;
						} else {
							C_assert asrt = (C_assert) e;
							IExpr expr = asrt.expr();
							if (expr instanceof AttributedExpr) {
								AttributedExpr attExp = (AttributedExpr) expr;
								for (IAttribute<?> att: attExp.attributes()) {
									IKeyword keyword = att.keyword();
									if (keyword.value().equals(":assert-type")) {
										IAttributeValue attVal = att.attrValue();
										if (attVal.toString().equals("GlobalConstraint")
												|| attVal.toString().equals("GlobalArrayConstraint")) {
											AssertAst ast = new AssertAst(asrt);
											return ast;											
										}
									}			
								}
							}
						}
					} else if (e instanceof C_declare_fun) {
						C_declare_fun decFun = (C_declare_fun) e;
						DeclareFunAst ast = new DeclareFunAst(decFun);
						return ast;
					} else if (e instanceof C_declare_sort) {
						C_declare_sort decSort = (C_declare_sort) e;
						DeclareSortAst ast = new DeclareSortAst(decSort);
						return ast;
					} else if (e instanceof C_define_fun) {
						C_define_fun defFun = (C_define_fun) e;
						DefineFunAst ast = new DefineFunAst(defFun);
						return ast;
					}

					return null;
				}

				IExpr unfoldFunBodyFormula(IExpr expr) throws VisitorException {
					assert expr instanceof IFcnExpr;
					IFcnExpr funExpr = (IFcnExpr) expr;
					List<IExpr> args = funExpr.args();

					assert args.size() == 1;
					IExpr arg = args.get(0);

					assert arg instanceof ILet;
					IExpr unfolded = unfoldLet((ILet) arg,
							new Stack<List<IBinding>>());

					assert unfolded != null;
					return unfolded;
				}

				IExpr transformBodyFormula(IExpr formula)
						throws org.smtlib.IVisitor.VisitorException {
					IExpr filtered;
					if (canSubstituteAnnotated) {
						filtered = formula.accept(new FormulaSubstitutor(this.curFun));
					} else {
						filtered = formula.accept(new FormulaTransformer(this.curFun));
					}
					return filtered;
				}

				IExpr simplifyBodyFormula(IExpr expr)
						throws org.smtlib.IVisitor.VisitorException {
					IExpr simplified = expr.accept(new FormulaSimplifier());
					return simplified;

				}
			});
			assert rst instanceof RootAst;
			return (RootAst) rst;
		} catch (VisitorException e) {
			throw new Error("Failed for VC translation");
		}
	}

	IExpr unfoldLet(ILet letExp, final Stack<List<IBinding>> bindingsStack)
			throws VisitorException {
		List<IBinding> bindings = letExp.bindings();
		bindingsStack.push(bindings);
		IExpr rst = letExp.accept(new DefaultVisitor<IExpr>() {
			@Override
			public IExpr visit(ILet e) throws VisitorException {
				IExpr expr = e.expr();
				if (expr instanceof ILet) {
					ILet letExpr = (ILet) expr;
					IExpr rst = unfoldLet(letExpr, bindingsStack);
					return rst;
				} else {
					IExpr rst = unfoldLetExpr(expr);
					return rst;
				}
			}

			final Set<IExpr> checked = new HashSet<IExpr>();

			IExpr unfoldLetExpr(IExpr original) throws VisitorException {
				final MutableBoolean changed = new MutableBoolean(false);
				IExpr unfolded = original.accept(new IdIExprVisitor() {
					@Override
					public IExpr visit(IFcnExpr e) throws VisitorException {
						List<IExpr> args = e.args();

						List<IExpr> newArgs = new LinkedList<IExpr>();
						for (IExpr arg : args) {
							boolean alreadyChecked = checked.contains(arg);
							int oldHash = arg.hashCode();
							IExpr newArg = (alreadyChecked) ? arg : arg
									.accept(this);
							assert newArg != null;
							newArgs.add(newArg);
							if (oldHash != newArg.hashCode()) {
								changed.setValue(true);
							} else {
								checked.add(arg);
							}
						}

						if (changed.value()) {
							IQualifiedIdentifier head = e.head();
							e = new FcnExpr(head, newArgs);
						} else {
							e.setArgs(newArgs);
						}
						return e;
					}

					@Override
					public IExpr visit(ISymbol e) throws VisitorException {
						IExpr term = getTerm(e);
						changed.setValue(true);
						return (term == null) ? e : term;
					}

					IExpr getTerm(ISymbol e) throws VisitorException {
						for (List<IBinding> bindingList : bindingsStack) {
							for (IBinding binding : bindingList) {
								ISymbol param = binding.parameter();
								if (param.equals(e)) {
									IExpr expr = binding.expr();
									return expr;
								}
							}
						}

						return e;
					}

				});
				assert unfolded != null;
				if (changed.value()) {
					return unfoldLetExpr(unfolded);
				} else {
					return unfolded;
				}
			}
		});
		assert rst != null;
		return rst;
	}

}
