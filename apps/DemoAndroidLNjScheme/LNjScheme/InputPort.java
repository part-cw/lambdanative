package LNjScheme;
import java.io.*;

/** InputPort is to Scheme as InputStream is to Java.
 * @author Peter Norvig, peter@norvig.com http://www.norvig.com
 * Copyright 1998 Peter Norvig, see http://www.norvig.com/license.html **/

public class InputPort extends SchemeUtils {

  static  String EOF = "#!EOF";
  boolean isPushedToken = false;
  boolean isPushedChar = false;
  Object  pushedToken = null;
  int     pushedChar = -1;
  Reader  in;
  StringBuffer buff = new StringBuffer();

  /** Construct an InputPort from an InputStream. **/
  public InputPort(InputStream in) { this.in = new InputStreamReader(in);}

  /** Construct an InputPort from a Reader. **/
  public InputPort(Reader in) { this.in = in;}

  /** Read and return a Scheme character or EOF. **/
  public Object readChar() {
    try {
      if (isPushedChar) {
	isPushedChar = false;
	if (pushedChar == -1) return EOF; else return chr((char)pushedChar);
      } else {
	int ch = in.read();
	if (ch == -1) return EOF; else return chr((char)ch);
      }
    } catch (IOException e) {
      warn("On input, exception: " + e);
      return EOF;
    }
  }

  /** Peek at and return the next Scheme character (or EOF).
   * However, don't consume the character. **/
  public Object peekChar() {
    int p = peekCh();
    if (p == -1) return EOF; else return chr((char)p);
  }

  /** Push a character back to be re-used later. **/
  int pushChar(int ch) {
    isPushedChar = true;
    return pushedChar = ch;
  }

  /** Pop off the previously pushed character. **/
  int popChar() {
    isPushedChar = false;
    return pushedChar;
  }

  /** Peek at and return the next Scheme character as an int, -1 for EOF.
   * However, don't consume the character. **/
  public int peekCh() {
    try { return isPushedChar ? pushedChar : pushChar(in.read()); }
    catch (IOException e) {
      warn("On input, exception: " + e);
      return -1;
    }
  }

  /** Read and return a Scheme expression, or EOF. **/
  public Object read() {
    try {
      Object token = nextToken();
      if (token == "(")
	return readTail(false);
      else if (token == ")")
	{ warn("Extra ) ignored."); return read(); }
      else if (token == ".")
	{ warn("Extra . ignored."); return read(); }
      else if (token == "'")
	return list("quote", read());
      else if (token == "`")
	return list("quasiquote", read());
      else if (token == ",")
	return list("unquote", read());
      else if (token == ",@")
	return list("unquote-splicing", read());
      else
	return token;
    } catch (IOException e) {
      warn("On input, exception: " + e);
      return EOF;
    }
  }

  /** Close the port.  Return TRUE if ok. **/
  public Object close() {
    try { this.in.close(); return TRUE; }
    catch (IOException e) { return error("IOException: " + e); }
  }

  /** Is the argument the EOF object? **/
  public static boolean isEOF(Object x) { return x == EOF; }

  Object readTail(boolean dotOK) throws IOException {
    Object token = nextToken();
    if (token == EOF)
      return error("EOF during read.");
    else if (token == ")")
      return null;
    else if (token == ".") {
      Object result = read();
      token = nextToken();
      if (token != ")") warn("Where's the ')'? Got " +
			     token + " after .");
      return result;
    } else {
      isPushedToken = true;
      pushedToken = token;
      return cons(read(), readTail(true));
    }
  }

  Object nextToken() throws IOException {
    int ch;

    // See if we should re-use a pushed char or token
    if (isPushedToken) {
      isPushedToken = false;
      return pushedToken;
    } else if (isPushedChar) {
      ch = popChar();
    } else {
      ch = in.read();
    }

    // Skip whitespace
    while (Character.isWhitespace((char)ch)) ch = in.read();

    // See what kind of non-white character we got
    switch(ch) {
    case -1: return EOF;
    case '(' : return "(";
    case ')':  return ")";
    case '\'': return "'";
    case '`':  return "`";
    case ',':
      ch = in.read();
      if (ch == '@') return ",@";
      else { pushChar(ch); return ","; }
    case ';':
      // Comment: skip to end of line and then read next token
      while(ch != -1 && ch != '\n' && ch != '\r') ch = in.read();
      return nextToken();
    case '"':
      // Strings are represented as char[]
      buff.setLength(0);
      while ((ch = in.read()) != '"' && ch != -1) {
	buff.append((char) ((ch == '\\') ? in.read() : ch));
      }
      if (ch == -1) warn("EOF inside of a string.");
      return buff.toString().toCharArray();
    case '#':
      switch (ch = in.read()) {
      case 't': case 'T': return TRUE;
      case 'f': case 'F': return FALSE;
      case '(':
	pushChar('(');
	return listToVector(read());
      case '\\':
	ch = in.read();
	if (ch == 's' || ch == 'S' || ch == 'n' || ch == 'N') {
	  pushChar(ch);
	  Object token = nextToken();
	  if (token == "space") return chr(' ');
	  else if (token == "newline") return chr('\n');
	  else {
	    isPushedToken = true;
	    pushedToken = token;
	    return chr((char)ch);
	  }
	} else {
	  return chr((char)ch);
	}
      case 'e': case 'i': case 'd': return nextToken();
      case 'b': case 'o': case 'x':
	warn("#" + ((char)ch) + " not implemented, ignored.");
	return nextToken();
      default:
	warn("#" + ((char)ch) + " not recognized, ignored.");
	return nextToken();
      }
    default:
      buff.setLength(0);
      int c = ch;
      do {
	buff.append((char)ch);
	ch = in.read();
      } while (!Character.isWhitespace((char)ch) && ch != -1 &&
	       ch != '(' && ch != ')' && ch != '\'' && ch != ';'
	       && ch != '"' && ch != ',' && ch != '`');
      pushChar(ch);
      // Try potential numbers, but catch any format errors.
      if (c == '.' || c == '+' || c == '-' || (c >= '0' && c <= '9')) {
	try { return new Double(buff.toString()); }
	catch (NumberFormatException e) { ; }
      }
      return buff.toString().toLowerCase().intern();
    }
  }
}
