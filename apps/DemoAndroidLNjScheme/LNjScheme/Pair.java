package jscheme;

/** A Pair has two fields, first and rest (or car and cdr).
 * The empty list is represented by null. The methods that you might
 * expect here, like first, second, list, etc. are instead static methods
 * in class SchemeUtils.
 * @author Peter Norvig, peter@norvig.com http://www.norvig.com
 * Copyright 1998 Peter Norvig, see http://www.norvig.com/license.html */

public class Pair extends SchemeUtils {

    /** The first element of the pair. **/
    public Object first;

    /** The other element of the pair. **/
    public Object rest;

    /** Build a pair from two components. **/
    public Pair(Object first, Object rest) {
	this.first = first; this.rest = rest;
    }

    /** Two pairs are equal if their first and rest fields are equal. **/
    public boolean equals(Object x) {
	if (x == this) return true;
	else if (!(x instanceof Pair)) return false;
	else {
	  Pair that = (Pair)x;
	  return equal(this.first, that.first)
	    && equal(this.rest, that.rest);
	}
    }

  /** Return a String representation of the pair. **/
  public String toString() { return stringify(this, true); }

  /** Build up a String representation of the Pair in a StringBuffer. **/
  void stringifyPair(boolean quoted, StringBuffer buf) {
    String special = null;
    if ((rest instanceof Pair) && rest(rest) == null)
      special = (first == "quote") ? "'" : (first == "quasiquote") ? "`"
	: (first == "unquote") ? "," : (first == "unquote-splicing") ? ",@"
	: null;

    if (special != null) {
      buf.append(special); stringify(second(this), quoted, buf);
    } else {
      buf.append('(');
      stringify(first, quoted, buf);
      Object tail = rest;
      while (tail instanceof Pair) {
	buf.append(' ');
	stringify(((Pair)tail).first, quoted, buf);
	tail = ((Pair)tail).rest;
      }
      if (tail != null) {
	buf.append(" . ");
	stringify(tail, quoted, buf);
      }
      buf.append(')');
    }
  }

}
