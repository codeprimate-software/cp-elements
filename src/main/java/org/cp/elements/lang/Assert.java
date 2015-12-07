/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * 
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * 
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * 
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * 
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang;

import java.text.MessageFormat;
import java.util.Collection;
import java.util.Map;

/**
 * The Assert class is a more capable replacement for Java's assert facility, providing functionality to make assertions
 * about pre-conditions and state in order to ensure that an object's invariants are upheld and enforced.
 * 
 * @author John J. Blum
 * @see java.lang.String#format(String, Object...)
 * @see java.text.MessageFormat#format(String, Object...)
 * @see org.cp.elements.lang.AssertionFailedException
 * @see org.cp.elements.lang.LangExtensions#assertThat(Object)
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class Assert {

  /**
   * Asserts that an argument is valid.  The assertion holds if and only if valid is true.
   *
   * @param valid a Boolean value resulting from the evaluation of the criteria used by the caller
   * to determine the validity of the argument.
   * @param message a String specifying the message for the IllegalArgumentException thrown if the assertion fails.
   * @param arguments an array of Object arguments used placeholder values when formatting the message.
   * @throws java.lang.IllegalArgumentException if the argument is invalid.
   * @see #argument(Boolean, RuntimeException)
   */
  public static void argument(final Boolean valid, final String message, final Object... arguments) {
    argument(valid, new IllegalArgumentException(format(message, arguments)));
  }

  /**
   * Asserts that an argument is valid.  The assertion holds if and only if valid is true.
   *
   * @param valid a Boolean value resulting from the evaluation of the criteria used by the caller
   * to determine the validity of the argument.
   * @param e the RuntimeException thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the argument is invalid.
   * @see #argument(Boolean, String, Object...)
   */
  public static void argument(final Boolean valid, final RuntimeException e) {
    if (!Boolean.TRUE.equals(valid)) {
      throw e;
    }
  }

  /**
   * Asserts that the objects are comparable.  The assertion holds if and only if the Comparable objects
   * are equal in comparison.
   *
   * @param <T> the Comparable class type of the objects in the comparison.
   * @param obj1 the first Comparable object in the relational comparison.
   * @param obj2 the second Comparable object in the relational comparison.
   * @param message a String specifying the message for the ComparisonException thrown if the assertion fails.
   * @param arguments an array of Object arguments used as placeholder values when formatting the message.
   * @throws org.cp.elements.lang.ComparisonException if the assertion fails and the two objects are not comparable.
   * @see #comparable(Comparable, Comparable, RuntimeException)
   * @see java.lang.Comparable
   */
  public static <T extends Comparable<T>> void comparable(final T obj1, final T obj2, final String message, final Object... arguments) {
    comparable(obj1, obj2, new ComparisonException(format(message, arguments)));
  }

  /**
   * Asserts that the objects are comparable.  The assertion holds if and only if the Comparable objects
   * are equal in comparison.
   *
   * @param <T> the Comparable class type of the objects in the comparison.
   * @param obj1 the first Comparable object in the relational comparison.
   * @param obj2 the second Comparable object in the relational comparison.
   * @param e the RumtimeException thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the assertion fails and the two objects are not comparable.
   * @see #comparable(Comparable, Comparable, String, Object...)
   * @see java.lang.Comparable
   */
  public static <T extends Comparable<T>> void comparable(final T obj1, final T obj2, final RuntimeException e) {
    if (obj1 == null || obj2 == null || obj1.compareTo(obj2) != 0) {
      throw e;
    }
  }

  /**
   * Asserts that two object are equal as determined by Object.equals.  The assertion holds if and only if
   * both objects are not null and equal in value.
   * 
   * @param obj1 the left operand in the equality comparison.
   * @param obj2 the right operand in the equality comparison.
   * @param message a String specifying the message for the EqualityException thrown if the assertion fails.
   * @param arguments an array of Object arguments used as placeholder values when formatting the message.
   * @throws org.cp.elements.lang.EqualityException if the assertion fails and the two objects are not equal.
   * @see #equals(Object, Object, RuntimeException)
   * @see java.lang.Object#equals(Object)
   */
  public static void equals(final Object obj1, final Object obj2, final String message, final Object... arguments) {
    equals(obj1, obj2, new EqualityException(format(message, arguments)));
  }

  /**
   * Asserts that two object are equal as determined by Object.equals.  The assertion holds if and only if
   * both objects are not null and equal in value.
   *
   * @param obj1 the left operand in the equality comparison.
   * @param obj2 the right operand in the equality comparison.
   * @param e the RuntimeException thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the two objects are not equal.
   * @see #equals(Object, Object, String, Object...)
   * @see java.lang.Object#equals(Object)
   */
  public static void equals(final Object obj1, final Object obj2, final RuntimeException e) {
    if (obj1 == null || !obj1.equals(obj2)) {
      throw e;
    }
  }

  /**
   * Asserts that the current Thread holds the specified lock.  The assertion holds if and only if the lock is not null
   * and the current Thread is the holder of the lock.
   * 
   * @param lock the Object used as the lock and synchronization mutex/monitor.
   * @param message a String specifying the message for the IllegalMonitorStateException thrown if the assertion fails.
   * @param arguments an array of Object arguments used as placeholder values when formatting the message.
   * @throws java.lang.IllegalMonitorStateException if the current Thread does not hold the lock or the lock is null.
   * @see #holdsLock(Object, RuntimeException)
   * @see java.lang.Thread#holdsLock(Object)
   */
  public static void holdsLock(final Object lock, final String message, final Object... arguments) {
    holdsLock(lock, new IllegalMonitorStateException(format(message, arguments)));
  }

  /**
   * Asserts that the current Thread holds the specified lock.  The assertion holds if and only if the lock is not null
   * and the current Thread is the holder of the lock.
   *
   * @param lock the Object used as the lock and synchronization mutex/monitor.
   * @param e the RuntimeException thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the current Thread does not hold the lock or the lock is null.
   * @see #holdsLock(Object, String, Object...)
   * @see java.lang.Thread#holdsLock(Object)
   */
  public static void holdsLock(final Object lock, final RuntimeException e) {
    if (lock == null || !Thread.holdsLock(lock)) {
      throw e;
    }
  }

  /**
   * Asserts that the 'from' class type is assignable to the 'to' class type.  The assertion holds if and only if
   * the 'from' class type is the same as or a subclass of the 'to' class type.
   * 
   * @param from the class type being evaluated for assignment compatibility with the 'to' class type.
   * @param to the class type used to determine if the 'from' class type is assignment compatible.
   * @param message a String specifying the message for the ClassCastException thrown if the assertion fails.
   * @param arguments an array of Object arguments used as placeholder values when formatting the message.
   * @throws java.lang.ClassCastException if the 'from' class type is not assignment compatible with the 'to' class type.
   * @see #isAssignableTo(Class, Class, RuntimeException)
   * @see java.lang.Class#isAssignableFrom(Class)
   */
  public static void isAssignableTo(final Class<?> from, final Class<?> to, final String message, final Object... arguments) {
    isAssignableTo(from, to, new ClassCastException(format(message, arguments)));
  }

  /**
   * Asserts that the 'from' class type is assignable to the 'to' class type.  The assertion holds if and only if
   * the 'from' class type is the same as or a subclass of the 'to' class type.
   *
   * @param from the class type being evaluated for assignment compatibility with the 'to' class type.
   * @param to the class type used to determine if the 'from' class type is assignment compatible.
   * @param e the RuntimeException thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the 'from' class type is not assignment compatible with the 'to' class type.
   * @see #isAssignableTo(Class, Class, String, Object...)
   * @see java.lang.Class#isAssignableFrom(Class)
   */
  public static void isAssignableTo(final Class<?> from, final Class<?> to, final RuntimeException e) {
    if (to == null || (from != null && !to.isAssignableFrom(from))) {
      throw e;
    }
  }

  /**
   * Asserts that the condition is false.  The assertion holds if and only if the value is equal to false.
   * 
   * @param condition the Boolean value being evaluated as a false condition.
   * @param message a String specifying the message for the IllegalArgumentException thrown if the assertion fails.
   * @param arguments an array of Object arguments used as placeholder values when formatting the message.
   * @throws java.lang.IllegalArgumentException if the value is not false.
   * @see #isFalse(Boolean, RuntimeException)
   * @see java.lang.Boolean#FALSE
   */
  public static void isFalse(final Boolean condition, final String message, final Object... arguments) {
    isFalse(condition, new IllegalArgumentException(format(message, arguments)));
  }

  /**
   * Asserts that the condition is false.  The assertion holds if and only if the value is equal to false.
   *
   * @param condition the Boolean value being evaluated as a false condition.
   * @param e the RuntimeException thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the value is not false.
   * @see #isFalse(Boolean, String, Object...)
   * @see java.lang.Boolean#FALSE
   */
  public static void isFalse(final Boolean condition, final RuntimeException e) {
    if (!Boolean.FALSE.equals(condition)) {
      throw e;
    }
  }

  /**
   * Asserts that the given object is an instance of the specified class type.  The assertion holds if and only if
   * the object is not null and is an instance of the specified class type.  This assertion functions exactly
   * the same as the Java instanceof operator.
   *
   * @param obj the object evaluated as an instance of the class type.
   * @param type the class type used to evaluate the object in the instanceof operator.
   * @param message a String specifying the message for the IllegalArgumentException thrown  if the assertion fails.
   * @param arguments an array of Object arguments used as placeholder values when formatting the message.
   * @throws java.lang.IllegalArgumentException if the object is not an instance of the class type.
   * @see #isInstanceOf(Object, Class, RuntimeException)
   * @see java.lang.Class#isInstance(Object)
   */
  public static void isInstanceOf(final Object obj, final Class<?> type, final String message, final Object... arguments) {
    isInstanceOf(obj, type, new IllegalArgumentException(format(message, arguments)));
  }

  /**
   * Asserts that the given object is an instance of the specified class type.  The assertion holds if and only if
   * the object is not null and is an instance of the specified class type.  This assertion functions exactly
   * the same as the Java instanceof operator.
   *
   * @param obj the object evaluated as an instance of the class type.
   * @param type the class type used to evaluate the object in the instanceof operator.
   * @param e the RuntimeException thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the object is not an instance of class type.
   * @see #isInstanceOf(Object, Class, String, Object...)
   * @see java.lang.Class#isInstance(Object)
   */
  public static void isInstanceOf(final Object obj, final Class<?> type, final RuntimeException e) {
    if (!type.isInstance(obj)) {
      throw e;
    }
  }

  /**
   * Asserts that the condition is true.  The assertion holds if and only if the value is equal to true.
   *
   * @param condition the Boolean value being evaluated as a true condition.
   * @param message a String specifying the message for the IllegalArgumentException thrown  if the assertion fails.
   * @param arguments an array of Object arguments used as placeholder values when formatting the message.
   * @throws java.lang.IllegalArgumentException if the value is not true.
   * @see #isTrue(Boolean, RuntimeException)
   * @see java.lang.Boolean#TRUE
   */
  public static void isTrue(final Boolean condition, final String message, final Object... arguments) {
    isTrue(condition, new IllegalArgumentException(format(message, arguments)));
  }

  /**
   * Asserts that the condition is true.  The assertion holds if and only if the value is equal to true.
   *
   * @param condition the Boolean value being evaluated as a true condition.
   * @param e the RuntimeException thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the value is not true.
   * @see #isTrue(Boolean, String, Object...)
   * @see java.lang.Boolean#TRUE
   */
  public static void isTrue(final Boolean condition, final RuntimeException e) {
    if (!Boolean.TRUE.equals(condition)) {
      throw e;
    }
  }

  /**
   * Assert that the String is not blank.  The assertion holds if and only if the String is not null, empty
   * or contains only whitespace characters.
   * 
   * @param value the String being evaluated for blankness.
   * @param message a String specifying the message for the IllegalArgumentException thrown if the assertion fails.
   * @param arguments an array of Object arguments used as placeholder values when formatting the message.
   * @throws java.lang.IllegalArgumentException if the String is blank.
   * @see #notBlank(String, RuntimeException)
   * @see java.lang.String
   */
  public static void notBlank(final String value, final String message, final Object... arguments) {
    notBlank(value, new IllegalArgumentException(format(message, arguments)));
  }

  /**
   * Assert that the String is not blank.  The assertion holds if and only if the String is not null, empty
   * or contains only whitespace characters.
   *
   * @param value the String being evaluated for blankness.
   * @param e the RuntimeException thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the String is blank.
   * @see #notBlank(String, String, Object...)
   * @see java.lang.String
   */
  public static void notBlank(final String value, final RuntimeException e) {
    if (value == null || value.trim().isEmpty()) {
      throw e;
    }
  }

  /**
   * Asserts that the String is not empty.  The assertion holds if and only if the String is not the empty String.
   * 
   * @param value the String being evaluated for emptiness.
   * @param message a String specifying the message for the IllegalArgumentException thrown if the assertion fails.
   * @param arguments an array of Object arguments used as placeholder values when formatting the message.
   * @throws java.lang.IllegalArgumentException if the String is empty.
   * @see #notEmpty(String, RuntimeException)
   * @see java.lang.String
   */
  public static void notEmpty(final String value, final String message, final Object... arguments) {
    notEmpty(value, new IllegalArgumentException(format(message, arguments)));
  }

  /**
   * Asserts that the String is not empty.  The assertion holds if and only if the String is not the empty String.
   *
   * @param value the String being evaluated for emptiness.
   * @param e the RuntimeException thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the String is empty.
   * @see #notEmpty(String, String, Object...)
   * @see java.lang.String
   */
  public static void notEmpty(final String value, final RuntimeException e) {
    if ("".equals(value)) {
      throw e;
    }
  }

  /**
   * Asserts that the Object array is not empty.  The assertion holds if and only if the Object array is not null
   * and contains at least 1 element.
   * 
   * @param array the Object array to evaluate for emptiness.
   * @param message a String specifying the message for the IllegalArgumentException thrown if the assertion fails.
   * @param arguments an array of Object arguments used as placeholder values when formatting the message.
   * @throws java.lang.IllegalArgumentException if the Object array is null or empty.
   * @see #notEmpty(Object[], RuntimeException)
   * @see java.lang.Object[]
   */
  public static void notEmpty(final Object[] array, final String message, final Object... arguments) {
    notEmpty(array, new IllegalArgumentException(format(message, arguments)));
  }

  /**
   * Asserts that the Object array is not empty.  The assertion holds if and only if the Object array is not null
   * and contains at least 1 element.
   *
   * @param array the Object array to evaluate for emptiness.
   * @param e the RuntimeException thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the Object array is null or empty.
   * @see #notEmpty(Object[], String, Object...)
   * @see java.lang.Object[]
   */
  public static void notEmpty(final Object[] array, final RuntimeException e) {
    if (array == null || array.length == 0) {
      throw e;
    }
  }

  /**
   * Asserts that the Collection is not empty.  The assertion holds if and only if the Collection is not null
   * and contains at least 1 element.
   * 
   * @param collection the Collection to evaluate for emptiness.
   * @param message a String specifying the message for the IllegalArgumentException thrown if the assertion fails.
   * @param arguments an array of Object arguments used as placeholder values when formatting the message.
   * @throws java.lang.IllegalArgumentException if the Collection is null or empty.
   * @see #notEmpty(java.util.Collection, RuntimeException)
   * @see java.util.Collection#isEmpty()
   */
  public static void notEmpty(final Collection<?> collection, final String message, final Object... arguments) {
    notEmpty(collection, new IllegalArgumentException(format(message, arguments)));
  }

  /**
   * Asserts that the Collection is not empty.  The assertion holds if and only if the Collection is not null
   * and contains at least 1 element.
   *
   * @param collection the Collection to evaluate for emptiness.
   * @param e the RuntimeException thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the Collection is null or empty.
   * @see #notEmpty(java.util.Collection, String, Object...)
   * @see java.util.Collection#isEmpty()
   */
  public static void notEmpty(final Collection<?> collection, final RuntimeException e) {
    if (collection == null || collection.isEmpty()) {
      throw e;
    }
  }

  /**
   * Asserts that the Map is not empty.  The assertion holds if and only if the Map is not null
   * and contains at least 1 key/value mapping.
   * 
   * @param map the Map to evaluate for emptiness.
   * @param message a String specifying the message for the IllegalArgumentException thrown if the assertion fails.
   * @param arguments an array of Object arguments used as placeholder values when formatting the message.
   * @throws java.lang.IllegalArgumentException if the Map is null or empty.
   * @see #notEmpty(java.util.Map, RuntimeException)
   * @see java.util.Map#isEmpty()
   */
  public static void notEmpty(final Map<?, ?> map, final String message, final Object... arguments) {
    notEmpty(map, new IllegalArgumentException(format(message, arguments)));
  }

  /**
   * Asserts that the Map is not empty.  The assertion holds if and only if the Map is not null
   * and contains at least 1 key/value mapping.
   *
   * @param map the Map to evaluate for emptiness.
   * @param e the RuntimeException thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the Map is null or empty.
   * @see #notEmpty(java.util.Map, String, Object...)
   * @see java.util.Map#isEmpty()
   */
  public static void notEmpty(final Map<?, ?> map, final RuntimeException e) {
    if (map == null || map.isEmpty()) {
      throw e;
    }
  }

  /**
   * Asserts that the Object reference is not null.  The assertion holds if and only if the Object reference
   * is not null.
   * 
   * @param obj the Object reference being evaluated for null.
   * @param message a String specifying the message for the NullPointerException thrown if the assertion fails.
   * @param arguments an array of Object arguments used as placeholder values when formatting the message.
   * @throws java.lang.NullPointerException if the Object reference is null.
   * @see #notNull(Object, RuntimeException)
   * @see java.lang.Object
   */
  public static void notNull(final Object obj, final String message, final Object... arguments) {
    notNull(obj, new NullPointerException(format(message, arguments)));
  }

  /**
   * Asserts that the Object reference is not null.  The assertion holds if and only if the Object reference
   * is not null.
   *
   * @param obj the Object reference being evaluated for null.
   * @param e the RuntimeException thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the Object reference is null.
   * @see #notNull(Object, String, Object...)
   * @see java.lang.Object
   */
  public static void notNull(final Object obj, final RuntimeException e) {
    if (obj == null) {
      throw e;
    }
  }

  /**
   * Asserts that two objects are the same object as determined by the identity comparison.  The assertion holds
   * if and only if the two objects are the same object in memory.
   * 
   * @param obj1 the left operand in the identity comparison.
   * @param obj2 the right operand in the identity comparison.
   * @param message a String specifying the message for the IdentityException thrown if the assertion fails.
   * @param arguments an array of Object arguments used as placeholder values when formatting the message.
   * @throws org.cp.elements.lang.IdentityException if the two objects are not the same.
   * @see #same(Object, Object, RuntimeException)
   * @see java.lang.Object
   */
  public static void same(final Object obj1, final Object obj2, final String message, final Object... arguments) {
    same(obj1, obj2, new IdentityException(format(message, arguments)));
  }

  /**
   * Asserts that two objects are the same object as determined by the identity comparison.  The assertion holds
   * if and only if the two objects are the same object in memory.
   *
   * @param obj1 the left operand in the identity comparison.
   * @param obj2 the right operand in the identity comparison.
   * @param e the RuntimeException thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the objects are not the same.
   * @see #same(Object, Object, String, Object...)
   * @see java.lang.Object
   */
  public static void same(final Object obj1, final Object obj2, final RuntimeException e) {
    if (obj1 != obj2) {
      throw e;
    }
  }

  /**
   * Asserts that the state is valid.  The assertion holds if and only if valid is true.
   * 
   * @param valid a Boolean value indicating whether the state is valid.
   * @param message a String specifying the message for the IllegalStateException thrown if the assertion fails.
   * @param arguments an array of Object arguments used as placeholder values when formatting the message.
   * @throws java.lang.IllegalStateException if the state is invalid.
   * @see #state(Boolean, RuntimeException)
   */
  public static void state(final Boolean valid, final String message, final Object... arguments) {
    state(valid, new IllegalStateException(format(message, arguments)));
  }

  /**
   * Asserts that the state is valid.  The assertion holds if and only if valid is true.
   * 
   * @param valid a Boolean value indicating whether the state is valid.
   * @param e the RuntimeException thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the state is invalid.
   * @see #state(Boolean, String, Object...)
   */
  public static void state(final Boolean valid, final RuntimeException e) {
    if (!Boolean.TRUE.equals(valid)) {
      throw e;
    }
  }

  /**
   * Asserts that an operation is supported.  The assertion holds if and only if supported is true.
   *
   * @param supported a Boolean value resulting from the evaluation of the criteria used by the caller
   * to determine if the operation is supported.
   * @param message a String specifying the message of the UnsupportedOperationException thrown if the assertion fails.
   * @param arguments an array of Object arguments used as placeholder values when formatting the message.
   * @throws java.lang.UnsupportedOperationException if the operations is unsupported.
   * @see #supported(Boolean, RuntimeException)
   */
  public static void supported(final Boolean supported, final String message, final Object... arguments) {
    supported(supported, new UnsupportedOperationException(format(message, arguments)));
  }

  /**
   * Asserts that an operation is supported.  The assertion holds if and only if supported is true.
   *
   * @param supported a Boolean value resulting from the evaluation of the criteria used by the caller
   * to determine if the operation is supported.
   * @param e the RuntimeException thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the operation is unsupported.
   * @see #supported(Boolean, String, Object...)
   */
  public static void supported(final Boolean supported, final RuntimeException e) {
    if (!Boolean.TRUE.equals(supported)) {
      throw e;
    }
  }

  /**
   * Formats the specified message with the given arguments.
   *
   * @param message the String message to format.
   * @param arguments an array of Object values used when formatting the message.
   * @return the String message formatted with the arguments.
   * @see #messageFormat(String, Object...)
   * @see #stringFormat(String, Object...)
   */
  private static String format(final String message, final Object... arguments) {
    return stringFormat(messageFormat(message, arguments), arguments);
  }

  /**
   * Formats the specified message containing possible placeholders as defined by the java.text.MessageFormat class
   * in the Java API.
   *
   * @param message a String containing the message to format.
   * @param arguments an array of Object values used as placeholder values when formatting the message.
   * @return a String formatted with the arguments.
   * @see java.text.MessageFormat#format(String, Object...)
   */
  private static String messageFormat(final String message, final Object... arguments) {
    return (arguments == null ? message : MessageFormat.format(message, arguments));
  }

  /**
   * Formats the specified message containing possible placeholders as defined by the java.lang.String class
   * in the Java API.
   *
   * @param message a String containing the message to format.
   * @param arguments an array of Object values used as placeholder values when formatting the message.
   * @return a String formatted with the arguments.
   * @see java.lang.String#format(String, Object...)
   */
  private static String stringFormat(final String message, final Object... arguments) {
    return String.format(message, arguments);
  }

}
