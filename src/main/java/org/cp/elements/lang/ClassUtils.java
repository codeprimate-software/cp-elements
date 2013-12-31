/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

/**
 * The ClassUtils class provides utility methods for working with Class objects.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Class
 * @see java.lang.Object
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ClassUtils {

  protected static final boolean DEFAULT_INITIALIZE_ON_LOAD_CLASS = true;

  public static final Class[] EMPTY_CLASS_ARRAY = new Class[0];

  /**
   * Determines whether a given Class type is assignable to a declared Class type.  A given Class type is assignable to
   * a declared Class type if it is in the same Class type hierarchy, i.e. the given Class type is a subclass,
   * or sub-interface of the declared Class type.  Null is also assignable to the declared (to) Class type.
   * <p/>
   * @param fromType the Class type evaluated for assignment compatibility with the declared Class type.
   * @param toType the declared Class type defining the type hierarchy, or bounds on the given Class type for
   * assignment compatibility.
   * @return a boolean value indicating given Class type is assignable to the declared Class type.
   * @see java.lang.Class#isAssignableFrom(Class)
   */
  @NullSafe
  public static boolean assignableTo(final Class<?> fromType, final Class<?> toType) {
    return (toType != null && (fromType == null || toType.isAssignableFrom(fromType)));
  }

  /**
   * Get the Class type of the specified Object.  Returns null if the Object reference is null.
   * <p/>
   * @param obj the Object who's Class type is being determined.
   * @return a Class object signifying the type of the specified Object.
   * @see java.lang.Object#getClass()
   */
  @NullSafe
  public static Class<?> getClass(final Object obj) {
    return (obj == null ? null : obj.getClass());
  }

  /**
   * Gets the fully-qualified name of the Class type for the specified Object.  Returns null if the Object reference
   * is null.
   * <p/>
   * @param obj the Object who's class name is determined.
   * @return a String value specifying the fully qualified class name of the Object.
   * @see java.lang.Class#getName()
   * @see java.lang.Object#getClass()
   */
  @NullSafe
  public static String getClassName(final Object obj) {
    return (obj == null ? null : obj.getClass().getName());
  }

  /**
   * Gets the unqualified, simple name of the Class type for the specified Object.  Returns null if the Object reference
   * is null.
   * <p/>
   * @param obj the Object who's simple class name is determined.
   * @return a String value indicating the simple class name of the Object.
   * @see java.lang.Class#getSimpleName()
   * @see java.lang.Object#getClass()
   */
  @NullSafe
  public static String getClassSimpleName(final Object obj) {
    return (obj == null ? null : obj.getClass().getSimpleName());
  }

  /**
   * Gets a Field object representing the named field on the specified Class.  This method will recursively search
   * up the class hierarchy of the specified class until the Object class is reached.  If the named field is found
   * then a Field object representing the field is returned otherwise a NoSuchFieldException is thrown.
   * <p/>
   * @param type the Class type to search for the specified field.
   * @param fieldName a String value specifying the name of the field on the Class type.
   * @return a Field objects representing the named field on the specified Class.
   * @throws NoSuchFieldException if the named field does not exist on the specified Class or a superclass
   * of the specified Class.
   * @see java.lang.Class#getDeclaredField(String)
   * @see java.lang.reflect.Field
   */
  public static Field getField(final Class type, final String fieldName) throws NoSuchFieldException {
    try {
      return type.getDeclaredField(fieldName);
    }
    catch (NoSuchFieldException e) {
      if (type.getSuperclass() != null) {
        return getField(type.getSuperclass(), fieldName);
      }
      throw e;
    }
  }

  /**
   * Gets a Method object representing the named method on the specified Class.  This method will recursively search
   * up the class hierarchy of the specified lass until the Object class is reached.  If the named field is found
   * then a Method object representing the method is returned otherwise a NoSuchMethodException is thrown.
   * <p/>
   * @param type the Class type to search for the specified method.
   * @param methodName a String value specifying the name of the method ofn the Class type.
   * @return a Method object representing the named method on the specified Class.
   * @throws NoSuchMethodException if the named method does not exist on the specified Class or a superclass
   * of the specified Class.
   * @see java.lang.Class#getDeclaredMethod(String, Class[])
   * @see java.lang.reflect.Method
   */
  public static Method getMethod(final Class type, final String methodName) throws NoSuchMethodException {
    try {
      return type.getDeclaredMethod(methodName);
    }
    catch (NoSuchMethodException e) {
      if (type.getSuperclass() != null) {
        return getMethod(type.getSuperclass(), methodName);
      }
      throw e;
    }
  }

  /**
   * Gets the fully-qualified name of the Class.
   * <p/>
   * @param type the Class type to return the fully-qualified name of.
   * @return a String value with the fully-qualified name of the Class.
   * @see java.lang.Class#getName()
   */
  @NullSafe
  public static String getName(final Class type) {
    return (type == null ? null : type.getName());
  }

  /**
   * Gets the simple name of the Class.
   * <p/>
   * @param type the Class type to return the simple name of.
   * @return a String value with the simple name of the Class.
   * @see java.lang.Class#getSimpleName()
   */
  @NullSafe
  public static String getSimpleName(final Class type) {
    return (type == null ? null : type.getSimpleName());
  }

  /**
   * Determines whether the given Object is an instance of the specified Class.  Note, an Object cannot be an
   * instance of null, so this method returns false if the Class type is null or the Object is null.
   * <p/>
   * @param obj the Object to test as an instance of the specified Class type.
   * @param type the Class type used in the instanceof operation.
   * @return a boolean value indicating whether the Object is an instance of the Class type.
   * @see java.lang.Class#isInstance(Object)
   */
  @NullSafe
  public static boolean instanceOf(final Object obj, final Class<?> type) {
    return (type != null && type.isInstance(obj));
  }

  /**
   * Determines whether the specified Class object represents an array type.
   * <p/>
   * @param type the Class object tested as an array type.
   * @return true iff the Class object is not null and represents an array type.
   * @see java.lang.Class#isArray()
   */
  @NullSafe
  public static boolean isArray(final Class type) {
    return (type != null && type.isArray());
  }

  /**
   * Determines whether the specified Class object represents an actual class, and not an Annotation, Array, Enum,
   * Interface or Primitive type.
   * <p/>
   * @param type the Class object tested as an actual class.
   * @return true iff the Class object is not null and represents an actual class.
   */
  @NullSafe
  public static boolean isClass(final Class type) {
    return (type != null && !(type.isAnnotation() || type.isArray() || type.isEnum() || type.isInterface()
      || type.isPrimitive()));
  }

  /**
   * Determines whether the specified Class object represents an enum type.
   * <p/>
   * @param type the Class object tested as an enum type.
   * @return true iff the Class object is not null and represents an enum type.
   * @see java.lang.Class#isEnum()
   */
  @NullSafe
  public static boolean isEnum(final Class type) {
    return (type != null && type.isEnum());
  }

  /**
   * Determines whether the specified Class object represents an interface.
   * <p/>
   * @param type the Class object tested as an interface.
   * @return true iff the Class object is not null and represents an interface.
   * @see java.lang.Class#isInterface()
   */
  @NullSafe
  public static boolean isInterface(final Class type) {
    return (type != null && type.isInterface());
  }

  /**
   * Determines whether the specified Class object represents a primitive type.
   * <p/>
   * @param type the Class object tested as a primitive type.
   * @return true iff the Class object is not null and represents a primitive type.
   * @see java.lang.Class#isPrimitive()
   */
  @NullSafe
  public static boolean isPrimitive(final Class type) {
    return (type != null && type.isPrimitive());
  }

  /**
   * Determines whether the specified class identified by name is available and present on the application classpath.
   * <p/>
   * @param className the fully qualified name of the class to determine the presence of.
   * @return a boolean value indicating whether the class identified by name is in the classpath.
   * @see #loadClass
   */
  public static boolean isPresent(final String className) {
    try {
      ClassUtils.loadClass(className);
      return true;
    }
    catch (TypeNotFoundException ignore) {
      return false;
    }
  }

  /**
   * Loads the Class object for the specified, fully qualified class name using the current Thread's context ClassLoader,
   * following by initializing the class.
   * <p/>
   * @param fullyQualifiedClassName a String value indicating the fully qualified class name of the Class to load.
   * @return a Class object for the specified, fully qualified class name.
   * @throws TypeNotFoundException if the Class identified by the fully qualified class name could not be found.
   * @see #loadClass(String)
   * @see java.lang.Class#forName(String, boolean, ClassLoader)
   */
  public static Class loadClass(final String fullyQualifiedClassName) {
    return loadClass(fullyQualifiedClassName, DEFAULT_INITIALIZE_ON_LOAD_CLASS,
      Thread.currentThread().getContextClassLoader());
  }

  /**
   * Loads the Class object for the specified, fully qualified class name using the provided ClassLoader and the option
   * to initialize the class (calling any static initializers) once loaded.
   * <p/>
   * @param fullyQualifiedClassName a String value indicating the fully qualified class name of the Class to load.
   * @param initialize a boolean value indicating whether to initialize the class after loading.
   * @param classLoader the ClassLoader used to load the class.
   * @return a Class object for the specified, fully qualified class name.
   * @throws TypeNotFoundException if the Class identified by the fully qualified class name could not be found.
   * @see java.lang.Class#forName(String, boolean, ClassLoader)
   */
  public static Class loadClass(final String fullyQualifiedClassName,
                                final boolean initialize,
                                final ClassLoader classLoader)
  {
    try {
      return Class.forName(fullyQualifiedClassName, initialize, classLoader);
    }
    catch (ClassNotFoundException e) {
      throw new TypeNotFoundException(String.format("Class (%1$s) was not found!", fullyQualifiedClassName), e);
    }
    catch (NoClassDefFoundError err) {
      throw new TypeNotFoundException(String.format("Class (%1$s) was not found!", fullyQualifiedClassName), err);
    }
  }

  /**
   * Determines whether the Object is an instance of any of the Class types and returns false if it is.
   * <p/>
   * @param obj the Object of the instanceof comparison.
   * @param types an array of Class types used in the instanceof comparison.
   * @return a true boolean value iff the Object is not an instance of any of the Class types.
   * @see #instanceOf(Object, Class)
   */
  @NullSafe
  public static boolean notInstanceOf(final Object obj, final Class... types) {
    boolean result = true;

    if (types != null) {
      for (int index = 0; result && index < types.length; index++) {
        result &= !instanceOf(obj, types[index]);
      }
    }

    return result;
  }

}
