package LNjScheme;

/**  @author Peter Norvig, peter@norvig.com http://www.norvig.com
 * Copyright 1998 Peter Norvig, see http://www.norvig.com/license.html **/

import java.io.*;

public abstract class SchemeUtils {

  /** Same as Boolean.TRUE. **/
  public static final Boolean TRUE = Boolean.TRUE;
  /** Same as Boolean.FALSE. **/
  public static final Boolean FALSE = Boolean.FALSE;

  public static Double ZERO = new Double(0.0);
  public static Double ONE = new Double(1.0);
  //////////////// Conversion Routines ////////////////

  // The following convert or coerce objects to the right type.

  /** Convert boolean to Boolean. **/
  public static Boolean truth(boolean x) { return x ? TRUE : FALSE; }

  /** Convert Scheme object to boolean.  Only #f is false, others are true. **/
  public static boolean truth(Object x) { return x != FALSE; }

  /** Convert double to Double. Caches 0 and 1; makes new for others. **/
  public static Double num(double x) {
    return (x == 0.0) ? ZERO : (x == 1.0) ? ONE : new Double(x); }

  /** Converts a Scheme object to a double, or calls error. **/
  public static double num(Object x) {
    if (x instanceof Number) return ((Number)x).doubleValue();
    else return num(error("expected a number, got: " + x));
  }

  /** Converts a Scheme object to a char, or calls error. **/
  public static char chr(Object x) {
    if (x instanceof Character) return ((Character)x).charValue();
    else return chr(error("expected a char, got: " + x));
  }

  /** Converts a char to a Character. **/
  public static Character chr(char ch) {
    return new Character(ch);
  }

  /** Coerces a Scheme object to a Scheme string, which is a char[]. **/
  public static char[] str(Object x) {
    if (x instanceof char[]) return (char[])x;
    else return str(error("expected a string, got: " + x));
  }

  /** Coerces a Scheme object to a Scheme symbol, which is a string. **/
  public static String sym(Object x) {
    if (x instanceof String) return (String)x;
    else return sym(error("expected a symbol, got: " + x));
  }

  /** Coerces a Scheme object to a Scheme vector, which is a Object[]. **/
  public static Object[] vec(Object x) {
    if (x instanceof Object[]) return (Object[])x;
    else return vec(error("expected a vector, got: " + x));
  }

  /** Coerces a Scheme object to a Scheme input port, which is an InputPort.
   * If the argument is null, returns interpreter.input. **/
  public static InputPort inPort(Object x, Scheme interp) {
    if (x == null) return interp.input;
    else if (x instanceof InputPort) return (InputPort)x;
    else return inPort(error("expected an input port, got: " + x), interp);
  }

  /** Coerces a Scheme object to a Scheme input port, which is a PrintWriter.
   * If the argument is null, returns System.out. **/
  public static PrintWriter outPort(Object x, Scheme interp) {
    if (x == null) return interp.output;
    else if (x instanceof PrintWriter) return (PrintWriter)x;
    else return outPort(error("expected an output port, got: " + x), interp);
  }

  //////////////// Error Routines ////////////////

  /** A continuable error. Prints an error message and then prompts for
   * a value to eval and return. **/
  public static Object error(String message) {
    System.err.println("**** ERROR: " + message);
    throw new RuntimeException(message);
  }

  public static Object warn(String message) {
    System.err.println("**** WARNING: " + message);
    return "<warn>";
  }

  //////////////// Basic manipulation Routines ////////////////

  // The following are used throughout the code.

  /** Like Common Lisp first; car of a Pair, or null for anything else. **/
  public static Object first(Object x) {
    return (x instanceof Pair) ? ((Pair)x).first : null;
  }

  /** Like Common Lisp rest; car of a Pair, or null for anything else. **/
  public static Object rest(Object x) {
    return (x instanceof Pair) ? ((Pair)x).rest : null;
  }

  /** Like Common Lisp (setf (first ... **/
  public static Object setFirst(Object x, Object y) {
    return (x instanceof Pair) ? ((Pair)x).first = y
      : error("Attempt to set-car of a non-Pair:" + stringify(x));
  }

  /** Like Common Lisp (setf (rest ... **/
  public static Object setRest(Object x, Object y) {
    return (x instanceof Pair) ? ((Pair)x).rest = y
      : error("Attempt to set-cdr of a non-Pair:" + stringify(x));
  }

  /** Like Common Lisp second. **/
  public static Object second(Object x) {
    return first(rest(x));
  }

  /** Like Common Lisp third. **/
  public static Object third(Object x) {
    return first(rest(rest(x)));
  }

  /** Creates a two element list. **/
  public static Pair list(Object a, Object b) {
    return new Pair(a, new Pair(b, null));
  }

  /** Creates a one element list. **/
  public static Pair list(Object a) {
    return new Pair(a, null);
  }

  /** listStar(args) is like Common Lisp (apply #'list* args) **/
  public static Object listStar(Object args) {
    if (rest(args) == null) return first(args);
    else return cons(first(args), listStar(rest(args)));
  }

  /** cons(x, y) is the same as new Pair(x, y). **/
  public static Pair cons(Object a, Object b) {
    return new Pair(a, b);
  }

  /** Reverse the elements of a list. **/
  public static Object reverse(Object x) {
    Object result = null;
    while (x instanceof Pair) {
      result = cons(first(x), result);
      x = rest(x);
    }
    return result;
  }

  /** Check if two objects are equal. **/
  public static boolean equal(Object x, Object y) {
    if (x == null || y == null) {
      return x == y;
    } else if (x instanceof char[]) {
      if (!(y instanceof char[])) return false;
      char[] xc = (char[])x, yc = (char[])y;
      if (xc.length != yc.length) return false;
      for (int i = xc.length - 1; i >= 0; i--) {
	if (xc[i] != yc[i]) return false;
      }
      return true;
    } else if (x instanceof Object[]) {
      if (!(y instanceof Object[])) return false;
      Object[] xo = (Object[])x, yo = (Object[])y;
      if (xo.length != yo.length) return false;
      for (int i = xo.length - 1; i >= 0; i--) {
	if (!equal(xo[i],yo[i])) return false;
      }
      return true;
    } else {
      return x.equals(y);
    }
  }

  /** Check if two objects are == or are equal numbers or characters. **/
  public static boolean eqv(Object x, Object y) {
    return x == y
      || (x instanceof Double && x.equals(y))
      || (x instanceof Character && x.equals(y));
  }

  /** The length of a list, or zero for a non-list. **/
  public static int length(Object x) {
    int len = 0;
    while (x instanceof Pair) {
      len++;
      x = ((Pair)x).rest;
    }
    return len;
  }

  /** Convert a list of characters to a Scheme string, which is a char[]. **/
  public static char[] listToString(Object chars) {
    char[] str = new char[length(chars)];
    for (int i = 0; chars instanceof Pair; i++) {
      str[i] = chr(first(chars));
      chars = rest(chars);
    }
    return str;
  }

  /** Convert a list of Objects to a Scheme vector, which is a Object[]. **/
  public static Object[] listToVector(Object objs) {
    Object[] vec = new Object[length(objs)];
    for (int i = 0; objs instanceof Pair; i++) {
      vec[i] = first(objs);
      objs = rest(objs);
    }
    return vec;
  }

  /** Write the object to a port.  If quoted is true, use "str" and #\c,
   * otherwise use str and c. **/
  public static Object write(Object x, PrintWriter port, boolean quoted) {
    port.print(stringify(x, quoted));
    port.flush();
    return x;
  }

  /** Convert a vector to a List. **/
  public static Pair vectorToList(Object x) {
    if (x instanceof Object[]) {
      Object[] vec = (Object[])x;
      Pair result = null;
      for (int i = vec.length - 1; i >= 0; i--)
	result = cons(vec[i], result);
      return result;
    } else {
      error("expected a vector, got: " + x);
      return null;
    }
  }

  /** Convert a Scheme object to its printed representation, as
   * a java String (not a Scheme string). If quoted is true, use "str" and #\c,
   * otherwise use str and c. You need to pass in a StringBuffer that is used
   * to accumulate the results. (If the interface didn't work that way, the
   * system would use lots of little internal StringBuffers.  But note that
   * you can still call <tt>stringify(x)</tt> and a new StringBuffer will
   * be created for you. **/

  static void stringify(Object x, boolean quoted, StringBuffer buf) {
    if (x == null)
      buf.append("()");
    else if (x instanceof Double) {
      double d = ((Double)x).doubleValue();
      if (Math.round(d) == d) buf.append((long)d); else buf.append(d);
    } else if (x instanceof Character) {
      if (quoted) buf.append("#\\");
      buf.append(x);
    } else if (x instanceof Pair) {
      ((Pair)x).stringifyPair(quoted, buf);
    } else if (x instanceof char[]) {
      char[] chars = (char[])x;
      if (quoted) buf.append('"');
      for (int i = 0; i < chars.length; i++) {
	if (quoted && chars[i] == '"') buf.append('\\');
	buf.append(chars[i]);
      }
      if (quoted) buf.append('"');
    } else if (x instanceof Object[]) {
	Object[] v = (Object[])x;
	buf.append("#(");
	for (int i=0; i<v.length; i++) {
	    stringify(v[i], quoted, buf);
	    if (i != v.length-1) buf.append(' ');
	}
	buf.append(')');
    } else if (x == TRUE) {
      buf.append("#t");
    } else if (x == FALSE) {
      buf.append("#f");
    } else {
      buf.append(x);
    }
  }

  /** Convert x to a Java String giving its external representation.
   * Strings and characters are quoted. **/
  static String stringify(Object x) { return stringify(x, true); }

  /** Convert x to a Java String giving its external representation.
   * Strings and characters are quoted iff <tt>quoted</tt> is true.. **/
  static String stringify(Object x, boolean quoted) {
    StringBuffer buf = new StringBuffer();
    stringify(x, quoted, buf);
    return buf.toString();
  }

  /** For debugging purposes, prints output. **/
  static Object p(Object x) {
    System.out.println(stringify(x));
    return x;
  }

  /** For debugging purposes, prints output. **/
  static Object p(String msg, Object x) {
    System.out.println(msg + ": " + stringify(x));
    return x;
  }
}
