package jscheme;

/** Environments allow you to look up the value of a variable, given
 * its name.  Keep a list of variables and values, and a pointer to
 * the parent environment.  If a variable list ends in a symbol rather
 * than null, it means that symbol is bound to the remainder of the
 * values list.
 * @author Peter Norvig, peter@norvig.com http://www.norvig.com
 * Copyright 1998 Peter Norvig, see http://www.norvig.com/license.html */

public class Environment extends SchemeUtils {
    public Object vars;
    public Object vals;
    public Environment parent;

    /** A constructor to extend an environment with var/val pairs. */
    public Environment(Object vars, Object vals, Environment parent) {
        this.vars = vars;
        this.vals = vals;
        this.parent = parent;
	if (!numberArgsOK(vars, vals))
	    warn("wrong number of arguments: expected " + vars +
			" got " + vals);
    }

    /** Construct an empty environment: no bindings. **/
    public Environment() {}

    /** Find the value of a symbol, in this environment or a parent. */
    public Object lookup (String symbol) {
	Object varList = vars, valList = vals;
	// See if the symbol is bound locally
	while (varList != null) {
	    if (first(varList) == symbol) {
		return first(valList);
	    } else if (varList == symbol) {
		return valList;
	    } else {
		varList = rest(varList);
		valList = rest(valList);
	    }
	}
	// If not, try to look for the parent
	if (parent != null) return parent.lookup(symbol);
	else return error("Unbound variable: " + symbol);
    }

    /** Add a new variable,value pair to this environment. */
     public Object define(Object var, Object val) {
	 vars = cons(var, vars);
	 vals = cons(val, vals);
	 if (val instanceof Procedure
	     && ((Procedure)val).name.equals("anonymous procedure"))
	     ((Procedure)val).name = var.toString();
	 return var;
     }

  /** Set the value of an existing variable **/
  public Object set(Object var, Object val) {
    if (!(var instanceof String))
      return error("Attempt to set a non-symbol: "
			  + stringify(var));;
    String symbol = (String) var;
    Object varList = vars, valList = vals;
    // See if the symbol is bound locally
    while (varList != null) {
      if (first(varList) == symbol) {
	return setFirst(valList, val);
      } else if (rest(varList) == symbol) {
	return setRest(valList, val);
      } else {
	varList = rest(varList);
	valList = rest(valList);
      }
    }
    // If not, try to look for the parent
    if (parent != null) return parent.set(symbol, val);
    else return error("Unbound variable: " + symbol);
  }

    public Environment defPrim(String name, int id, int minArgs) {
      define(name, new Primitive(id, minArgs, minArgs));
      return this;
    }

    public Environment defPrim(String name, int id, int minArgs, int maxArgs) {
      define(name, new Primitive(id, minArgs, maxArgs));
      return this;
    }

    /** See if there is an appropriate number of vals for these vars. **/
    boolean numberArgsOK(Object vars, Object vals) {
	return ((vars == null && vals == null)
		|| (vars instanceof String)
		|| (vars instanceof Pair && vals instanceof Pair
		    && numberArgsOK(((Pair)vars).rest, ((Pair)vals).rest)));
    }

}
