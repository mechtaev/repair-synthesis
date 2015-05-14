package edu.nus.mrepair;

import edu.nus.vctrans2.VCTranslator;
import edu.nus.vctrans2.ast.*;
import junit.framework.Assert;
import org.junit.Test;
import org.smtlib.IExpr;
import java.io.File;
import java.util.Map;

import scala.runtime.AbstractFunction1;


/**
 * Test SMT formula variables renaming
 */
public class RenamerTest {

    static final String DIR_Simple = "benchmarks" + File.separator + "vcc";

    @Test
    public void testTcasRename() throws Exception {
        File file = new File(DIR_Simple, "tcas_v1.repairable");
        boolean shouldUnfoldLetExpr = true;
        RootAst root = VCTranslator.translateSmt2(file, shouldUnfoldLetExpr, true);
        CFunctionAst funAst = root.getCFunctionAst("Non_Crossing_Biased_Climb");

        IExpr expr = funAst.getExpr();
        System.out.println(expr.accept(new PrettyPrinter()));

        IExpr newExpr = expr.accept(new VariableRenamer(new AbstractFunction1<String, String>() {
            public String apply(String arg) {
                return arg + "foo";
            }
        }));
        System.out.println(newExpr.accept(new PrettyPrinter()));

        Map<String, IExpr> repComp = funAst.getRepairableComponents();
        Assert.assertTrue(repComp != null);
        System.out.println("\nRepairable components (totally " + repComp.size() + "):");
        for(Map.Entry<String, IExpr> e: repComp.entrySet()) {
            System.out.println(e.getKey() + " \t=\t " + e.getValue());
        }

        return;

    }

    @Test
    public void testCallee() throws Exception {
        File file = new File(DIR_Simple, "callee_test.repairable");
        boolean shouldUnfoldLetExpr = true;
        RootAst root = VCTranslator.translateSmt2(file, shouldUnfoldLetExpr, true);
        CFunctionAst funAst = root.getCFunctionAst("callee_test");

        IExpr expr = funAst.getExpr();
        System.out.println(expr.accept(new PrettyPrinter()));

        IExpr newExpr = expr.accept(new VariableRenamer(new AbstractFunction1<String, String>() {
            public String apply(String arg) {
                return arg + "foo";
            }
        }));
        System.out.println(newExpr.accept(new PrettyPrinter()));

        Map<String, IExpr> repComp = funAst.getRepairableComponents();
        Assert.assertTrue(repComp != null);
        System.out.println("\nRepairable components (totally " + repComp.size() + "):");
        for(Map.Entry<String, IExpr> e: repComp.entrySet()) {
            System.out.println(e.getKey() + " \t=\t " + e.getValue());
        }

    }



}
