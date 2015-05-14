package org.smtlib.solvers;

import java.io.IOException;
import java.util.LinkedList;

import org.smtlib.ICommand.Ideclare_fun;
import org.smtlib.ICommand.Ideclare_sort;
import org.smtlib.IExpr;
import org.smtlib.IPrinter;
import org.smtlib.IResponse;
import org.smtlib.IVisitor;
import org.smtlib.IVisitor.VisitorException;
import org.smtlib.SMT.Configuration;

public class Solver_z3_4_3_asynch extends Solver_z3_4_3 {

  private LinkedList<IFun<IResponse>> q;
  private Worker worker;

  public Solver_z3_4_3_asynch(Configuration smtConfig, String executable) {
    super(smtConfig, executable);
    this.q = new LinkedList<IFun<IResponse>>();
    this.worker = new Worker();
  }

  static interface IFun<T> {
    T apply(Object... args) throws Z3SolverException;
  }

  @SuppressWarnings("serial")
  public static class Z3SolverException extends Exception {

    public Z3SolverException(String msg) {
      super(msg);
    }

  }

  class Worker implements Runnable {

    @Override
    public void run() {
      // TODO Auto-generated method stub

    }

  }

  static class VoidResponse implements IResponse {

    @Override
    public <T> T accept(IVisitor<T> v) throws VisitorException {
      throw new Error("Should not be called");
    }

    @Override
    public boolean isOK() {
      return true;
    }

    @Override
    public boolean isError() {
      return false;
    }
    
    // EXT: change contract
    @Override
    public String getValue(IPrinter p) {
      assert false : "Not implemented";
      return null;
    }

  }

  protected VoidResponse voidResponse = new VoidResponse();

  @Override
  public IResponse declare_sort(final Ideclare_sort cmd) {
    q.add(new IFun<IResponse>() {

      @Override
      public IResponse apply(Object... args) throws Z3SolverException {
        if (!logicSet) {
          return smtConfig.responseFactory.error("The logic must be set before a declare-sort command is issued");
        }
        try {
          checkSatStatus = null;
          return parseResponse(solverProcess.sendAndListen(translate(cmd), "\n"));
        } catch (IOException e) {
          return smtConfig.responseFactory.error("Error writing to Z3 solver: " + e);
        } catch (IVisitor.VisitorException e) {
          return smtConfig.responseFactory.error("Error writing to Z3 solver: " + e);
        }
      }
    });
    return voidResponse;

  }

  @Override
  public IResponse declare_fun(final Ideclare_fun cmd) {
    q.add(new IFun<IResponse>() {

      @Override
      public IResponse apply(Object... args) throws Z3SolverException {
        try {
          assert logicSet : "The logic must be set before a declare-fun command is issued";
          return parseResponse(solverProcess.sendAndListen(translate(cmd), "\n"));
        } catch (IOException e) {
          return smtConfig.responseFactory.error("Error writing to Z3 solver: " + e);
        } catch (IVisitor.VisitorException e) {
          return smtConfig.responseFactory.error("Error writing to Z3 solver: " + e);
        }
      }
    });
    return voidResponse;
  }

  @Override
  public IResponse assertExpr(final IExpr sexpr) {
    q.add(new IFun<IResponse>() {

      @Override
      public IResponse apply(Object... args) throws Z3SolverException {
        try {
          assert pushesDepth > 0 : "All assertion sets have been popped from the stack";
          assert logicSet : "The logic must be set before an assert command is issued";
          return parseResponse(solverProcess.sendAndListen("(assert ", translate(sexpr), ")\n"));
        } catch (IVisitor.VisitorException e) {
          return smtConfig.responseFactory.error("Failed to assert expression: " + e + " " + sexpr);
        } catch (Exception e) {
          return smtConfig.responseFactory.error("Failed to assert expression: " + e + " " + sexpr);
        }
      }
    });
    return voidResponse;
  }

  @Override
  public IResponse push(final int number) {
    q.add(new IFun<IResponse>() {

      @Override
      public IResponse apply(Object... args) throws Z3SolverException {
        try {
          assert logicSet : "The logic must be set before a push command is issued";
          assert number >= 0 : "Internal bug: A push command called with a negative argument: " + number;
          if (number == 0)
            return smtConfig.responseFactory.success();

          pushesDepth += number;
          IResponse r = parseResponse(solverProcess.sendAndListen("(push ", new Integer(number).toString(), ")\n"));
          // FIXME - actually only see this problem on Linux
          if (r.isError() && !isWindows)
            return smtConfig.responseFactory.success();
          return r;
        } catch (Exception e) {
          return smtConfig.responseFactory.error("Error writing to Z3 solver: " + e);
        }
      }
    });
    return voidResponse;
  }

  @Override
  public IResponse pop(final int number) {
    q.add(new IFun<IResponse>() {

      @Override
      public IResponse apply(Object... args) throws Z3SolverException {
        try {
          assert logicSet : "The logic must be set before a pop command is issued";
          assert number >= 0 : "Internal bug: A pop command called with a negative argument: " + number;
          if (number > pushesDepth)
            return smtConfig.responseFactory.error("The argument to a pop command is too large: " + number
                + " vs. a maximum of " + (pushesDepth));
          if (number == 0)
            return smtConfig.responseFactory.success();

          checkSatStatus = null;
          pushesDepth -= number;
          return parseResponse(solverProcess.sendAndListen("(pop ", new Integer(number).toString(), ")\n"));
        } catch (Exception e) {
          return smtConfig.responseFactory.error("Error writing to Z3 solver: " + e);
        }
      }
    });
    return voidResponse;
  }

  @Override
  public IResponse comment(final String msg) {
    q.add(new IFun<IResponse>() {

      @Override
      public IResponse apply(Object... args) throws Z3SolverException {
        try {
          solverProcess.log.append(";" + msg + "\n");
          return smtConfig.responseFactory.success();
        } catch (IOException e) {
          return smtConfig.responseFactory.error(e.toString());
        }
      }
    });
    return voidResponse;
  }

}
