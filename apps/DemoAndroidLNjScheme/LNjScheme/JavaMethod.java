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

  /*** Backported ***/
  /*** The following functionality is inspired and pratially stolen
   * from the community version, which extented the original
   * jscheme.  I'd rather strip down jscheme for use as embedded
   * language (e.g., this use case does often not need quasiquote
   * and macro expansion) than use and digest the bloat of a jscheme
   * 7.2 or alike.
   *
   * However: I need constructors with arguments. ***/

  /** Each bucket in an method table contains a Class[] of
      parameterTypes and the corresponding method or constructor. **/
  private static final int BUCKET_SIZE = 2;
  private static Class[] getParameterTypes(Object m) {
    return (m instanceof Method) ? ((Method) m).getParameterTypes() :
      ((Constructor) m).getParameterTypes();
  }

  /** Returns Object[] of parameterType, method pairs. **/
  private static Object[] methodArray(Object[] v) {
    Object[] result = new Object[v.length*BUCKET_SIZE];
    for(int i = 0; i < v.length; i++) {
      result[i*BUCKET_SIZE] = getParameterTypes(v[i]);
      result[i*BUCKET_SIZE+1] = v[i];
    }
    return result;
  }
  /* */
  private static Object findMethod(Object[] methods, Object[] args) {
    int best = -1;
    /*
    System.err.println("Found " + (methods.length/2) + " constructors: " + methods);
    System.err.println("Checking against " + args.length + " args, these:");
    for(int i=0; i<args.length; i++)
        System.err.println("Arg" + i + ": " + args[i].getClass().getName() + "\n");
    */
    for(int m1 = 0; m1 < methods.length; m1 = m1 + BUCKET_SIZE) {
      Class[] p1 = ((Class[]) methods[m1]);
    /*
    System.err.println("Index " + m1 + " Arguments:");
    for(int i=0; i<p1.length; i++)
        System.err.println("Arg" + i + ": " + p1[i].getName() + "\n");
    */
      if(isApplicable(p1, args) &&
	 (best == -1 || !moreApplicable(((Class[]) methods[best]), p1)))
	best = m1;
    }
    // System.err.println("Index " + best + " Gave best match " + methods[best] + " returning " + methods[best+1]);
    if (best != -1) return methods[best+1];
    throw new RuntimeException("no applicable method found in " + methods + " for " + args);
    /*
    // print debugging info
    StringBuffer alts = new StringBuffer();
    for(int m1 = 0; m1 < methods.length; m1 = m1 + BUCKET_SIZE)
	if (methods[m1+1] instanceof Member)
           alts.append("   * "+methods[m1+1] +"\n");
	else {
            Class[] ts=(Class[]) methods[m1];
            alts.append("   * "+methods[m1+1]+" ( ");
	    for (int i=0;i<ts.length; i++)
		alts.append(ts[i]+" ");
            alts.append(")\n");
	}

    StringBuffer argtypes = new StringBuffer();
    for(int i=0; i<args.length; i++)
      if (args[i] == null) argtypes.append(" ? ");
      else argtypes.append(" "+args[i].getClass()+" ");
    return E.error("\n\nERROR: NO " +
                   ((methods[1] instanceof Member)?
                        ((methods[1] instanceof Method)? "METHOD":
			 "CONSTRUCTOR"): "PROCEDURE") +
                   " WITH NAME\n    "+
		   ((methods[1] instanceof Member)?
		    ((Member) methods[1]).getName() : "?") +
                  "\n and args\n     "+ U.vectorToList(args) +
                  "\n of types \n    "+argtypes.toString()+
                  "\n\n possible alternatives are :\n" + alts.toString() +
		   "\n\n");
    */
  }
  private static boolean isApplicable (Class[] types, Object[] args) {
    if (types.length == args.length) {
      for (int i = 0; i < args.length; i++)
	if (! isArgApplicable(types[i], args[i])) return false;
      return true;
    } else return false;
  }
  private static boolean isArgApplicable(Class p, Object a) {
    return (a == null  && Object.class.isAssignableFrom(p)) ||
	p.isInstance(a) ||
        p.isPrimitive() && (primitiveWrapperType(p)).isInstance(a);
  }
  /** Given a primitive type return its wrapper class. **/
  private static Class primitiveWrapperType(Class p) {
    return
      p == java.lang.Byte.TYPE ? java.lang.Byte.class :
      p == java.lang.Long.TYPE ? java.lang.Long.class :
      p == java.lang.Float.TYPE ? java.lang.Float.class :
      p == java.lang.Short.TYPE ? java.lang.Short.class :
      p == java.lang.Double.TYPE ? java.lang.Double.class :
      p == java.lang.Boolean.TYPE ? java.lang.Boolean.class :
      p == java.lang.Integer.TYPE ? java.lang.Integer.class :
      p == java.lang.Character.TYPE ? java.lang.Character.class :
      // (Class) E.error("unknow primitive type: ", p);
      null;
  }
  /** A method m1 is more specific than method m2 if all parameters of
    m1 are subclasses of the corresponding parameters of m2.  **/
  private static boolean moreApplicable(Class[] p1, Class[] p2) {
    for(int i = 0; i < p1.length; i++)
      if (!p2[i].isAssignableFrom(p1[i])) return false;
    return true;
  }

  private static String invokeError(String msg, Throwable e, Object[] args) {
    StringBuffer b = new StringBuffer();
    b.append(msg + " " + e + " Args:\n");
    for(int i=0; i<args.length; i++)
        b.append("Arg" + i + ": " + args[i].getClass().getName() + "\n");
    return b.toString();
  }

  private static Object invokeConstructor0(Object name, Object[] args) {
    Class cls = null;
    try{
        cls = toClass(name);
    } catch (ClassNotFoundException e) { throw new RuntimeException("\"new\": class not found: " + name); }
    java.lang.reflect.Constructor m[] = cls.getConstructors();
    // FIXME: the cast to (Object[]) is not nice.  Just how to get away?
    //
    // Should be possible to do this better:
    // https://stackoverflow.com/questions/234600/can-i-use-class-newinstance-with-constructor-arguments#234617
    //
    java.lang.reflect.Constructor c = (java.lang.reflect.Constructor)findMethod(methodArray(m), args);
    if(c == null) return null;
    else
        try {
            return c.newInstance(args);
        } catch (InstantiationException e) {
            throw new RuntimeException("InstantiationException " + e + " creating " + cls + " for " + args);
        } catch (IllegalAccessException e) {
            throw new RuntimeException("IllegalAccessException " + e + " creating " + cls + " for " + args);
        } catch (java.lang.reflect.InvocationTargetException e) {
            Throwable e1 = e.getTargetException();
            throw new RuntimeException(invokeError("InvocationTargetException: " + c, e1, args));
        }
  }

  public static Object invokeConstructor(Object name, Object args) {
      return invokeConstructor0(name, listToVector(args));
  }
}
