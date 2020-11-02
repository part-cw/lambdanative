package LNjScheme;

/** @author Peter Norvig, peter@norvig.com http://www.norvig.com
 * Copyright 1998 Peter Norvig, see http://www.norvig.com/license.html **/

public class Macro extends Closure {

    /** Make a macro from a parameter list, body, and environment. **/
    public Macro (Object parms, Object body, Environment env) {
      super(parms, body, env);
    }

  /** Replace the old cons cell with the macro expansion, and return it. **/
  public Pair expand(Scheme interpreter, Pair oldPair, Object args) {
    Object expansion = apply(interpreter, args);
    if (expansion instanceof Pair) {
      oldPair.first = ((Pair)expansion).first;
      oldPair.rest  = ((Pair)expansion).rest;
    } else {
      oldPair.first = "begin";
      oldPair.rest = cons(expansion, null);
    }
    return oldPair;
  }

  /** Macro expand an expression **/
  public static Object macroExpand(Scheme interpreter, Object x) {
    if (!(x instanceof Pair)) return x;
    Object fn = interpreter.eval(first(x), interpreter.globalEnvironment);
    if (!(fn instanceof Macro)) return x;
    return ((Macro)fn).expand(interpreter, (Pair)x, rest(x));
  }
}
