package LNjScheme;
import java.lang.reflect.*;

/** @author Peter Norvig, peter@norvig.com http://www.norvig.com
 * Copyright 1998 Peter Norvig, see http://www.norvig.com/license.html */

public class JavaMethod extends Procedure {

  Class[] argClasses;
  Method method;
  boolean isStatic;

  public JavaMethod(String methodName, Object targetClassName,
		    Object argClassNames) {
    this.name = targetClassName + "." + methodName;
    try {
      argClasses = classArray(argClassNames);
      method = toClass(targetClassName).getMethod(methodName, argClasses);
      isStatic = Modifier.isStatic(method.getModifiers());
    } catch (ClassNotFoundException e) {
      error("Bad class, can't get method " + name);
    } catch (NoSuchMethodException e) {
      error("Can't get method " + name);
    }

  }

  private Object raiseJavaMethodError(String msg, Exception e, Object args) {
    return error(msg + " " + e + " on " + this + stringify(args) + ";");
  }

  /** Apply the method to a list of arguments. **/
  public Object apply(Scheme interpreter, Object args) {
    try {
      if (isStatic) return method.invoke(null, toArray(args));
      else return method.invoke(first(args), toArray(rest(args)));
    }
    catch (IllegalAccessException e)
      { raiseJavaMethodError("Bad Java Method application:", e, args); }
    catch (IllegalArgumentException e)
      { raiseJavaMethodError("Bad Java Method application:", e, args); }
    catch (InvocationTargetException e)
      { raiseJavaMethodError("Bad Java Method application:", e, args); }
    catch (NullPointerException e)
      { raiseJavaMethodError("Bad Java Method application:", e, args); }
    catch (Exception e)
      { raiseJavaMethodError("Bad Java Method application:", e, args); }
    return null; /* unreached */
  }

  public static Class toClass(Object arg) throws ClassNotFoundException {
    if      (arg instanceof Class)  return (Class)arg;
    arg = stringify(arg, false);

    if (arg.equals("void"))    return java.lang.Void.TYPE;
    else if (arg.equals("boolean")) return java.lang.Boolean.TYPE;
    else if (arg.equals("char"))    return java.lang.Character.TYPE;
    else if (arg.equals("byte"))    return java.lang.Byte.TYPE;
    else if (arg.equals("short"))   return java.lang.Short.TYPE;
    else if (arg.equals("int"))     return java.lang.Integer.TYPE;
    else if (arg.equals("long"))    return java.lang.Long.TYPE;
    else if (arg.equals("float"))   return java.lang.Float.TYPE;
    else if (arg.equals("double"))  return java.lang.Double.TYPE;
    else return Class.forName((String)arg);
  }

  /** Convert a list of Objects into an array.  Peek at the argClasses
   * array to see what's expected.  That enables us to convert between
   * Double and Integer, something Java won't do automatically. **/
  public Object[] toArray(Object args) {
    int n = length(args);
    int diff = n - argClasses.length;
    if (diff != 0)
      error(Math.abs(diff) + " too " + ((diff>0) ? "many" : "few")
		   + " args to " + name);
    Object[] array = new Object[n];
    for(int i = 0; i < n && i < argClasses.length; i++) {
      if (argClasses[i] == java.lang.Integer.TYPE)
	array[i] = new Integer((int)num(first(args)));
      else
	array[i] = first(args);
      args = rest(args);
    }
    return array;
  }

  /** Convert a list of class names into an array of Classes. **/
  public static Class[] classArray(Object args) throws ClassNotFoundException {
    int n = length(args);
    Class[] array = new Class[n];
    for(int i = 0; i < n; i++) {
      array[i] = toClass(first(args));
      args = rest(args);
    }
    return array;
  }

}
