package jscheme;

/** A closure is a user-defined procedure.  It is "closed" over the
 * environment in which it was created.  To apply the procedure, bind
 * the parameters to the passed in variables, and evaluate the body.
 * @author Peter Norvig, peter@norvig.com http://www.norvig.com
 * Copyright 1998 Peter Norvig, see http://www.norvig.com/license.html **/

public class Closure extends Procedure {

    Object parms;
    Object body;
    Environment env;

    /** Make a closure from a parameter list, body, and environment. **/
    public Closure (Object parms, Object body, Environment env) {
        this.parms = parms;
	this.env = env;
	this.body = (body instanceof Pair && rest(body) == null)
	    ? first(body)
	    : cons("begin", body);
    }

    /** Apply a closure to a list of arguments.  **/
    public Object apply(Scheme interpreter, Object args) {
	return interpreter.eval(body, new Environment(parms, args, env));
    }
}
