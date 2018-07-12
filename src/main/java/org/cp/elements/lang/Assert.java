/*
 * Copyright 2016 Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.cp.elements.lang;

import java.text.MessageFormat;
import java.util.Collection;
import java.util.Map;
import java.util.function.Supplier;

import org.cp.elements.lang.annotation.NullSafe;

/**
 * The {@link Assert} class is a more capable replacement for Java's assert facility, providing functionality
 * to make assertions about pre-conditions and state in order to ensure that an object's invariants
 * are upheld and enforced.
 *
 * @author John J. Blum
 * @see java.lang.String#format(String, Object...)
 * @see java.text.MessageFormat#format(String, Object...)
 * @see java.util.function.Supplier
 * @see org.cp.elements.lang.AssertionException
 * @see org.cp.elements.lang.LangExtensions#assertThat(Object)
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class Assert {

  /**
   * Asserts that an argument is valid.
   *
   * The assertion holds if and only if {@code valid} is {@literal true}.
   *
   * For example, application code might assert that:
   *
   * <pre>
   *   <code>
   *     Assert.argument(age &gt;= 21, "Person must be 21 years of age to enter");
   *   </code>
   * </pre>
   *
   * @param valid {@link Boolean} value resulting from the evaluation of the criteria used by the application
   * to determine the validity of the argument.
   * @throws java.lang.IllegalArgumentException if the argument is invalid.
   * @see #argument(Boolean, String, Object...)
   */
  public static void argument(Boolean valid) {
    argument(valid, "argument is not valid");
  }

  /**
   * Asserts that an argument is valid.
   *
   * The assertion holds if and only if {@code valid} is {@literal true}.
   *
   * For example, application code might assert that:
   *
   * <pre>
   *   <code>
   *     Assert.argument(age &gt;= 21, "Person must be 21 years of age to enter");
   *   </code>
   * </pre>
   *
   * @param valid {@link Boolean} value resulting from the evaluation of the criteria used by the application
   * to determine the validity of the argument.
   * @param message {@link String} containing the message for the {@link IllegalArgumentException} thrown
   * if the assertion fails.
   * @param arguments array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalArgumentException if the argument is invalid.
   * @see #argument(Boolean, RuntimeException)
   */
  public static void argument(Boolean valid, String message, Object... arguments) {
    argument(valid, new IllegalArgumentException(format(message, arguments)));
  }

  /**
   * Asserts that an argument is valid.
   *
   * The assertion holds if and only if {@code valid} is {@literal true}.
   *
   * For example, application code might assert that:
   *
   * <pre>
   *   <code>
   *     Assert.argument(age &gt;= 21, "Person must be 21 years of age to enter");
   *   </code>
   * </pre>
   *
   * @param valid {@link Boolean} value resulting from the evaluation of the criteria used by the application
   * to determine the validity of the argument.
   * @param message {@link Supplier} containing the message for the {@link IllegalArgumentException} thrown
   * if the assertion fails.
   * @throws java.lang.IllegalArgumentException if the argument is invalid.
   * @see java.util.function.Supplier
   */
  public static void argument(Boolean valid, Supplier<String> message) {
    if (isNotValid(valid)) {
      throw new IllegalArgumentException(message.get());
    }
  }

  /**
   * Asserts that an argument is valid.
   *
   * The assertion holds if and only if {@code valid} is {@literal true}.
   *
   * For example, application code might assert that:
   *
   * <pre>
   *   <code>
   *     Assert.argument(age &gt;= 21, "Person must be 21 years of age to enter");
   *   </code>
   * </pre>
   *
   * @param valid {@link Boolean} value resulting from the evaluation of the criteria used by the application
   * to determine the validity of the argument.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the argument is invalid.
   */
  public static void argument(Boolean valid, RuntimeException cause) {
    if (isNotValid(valid)) {
      throw cause;
    }
  }

  /**
   * Determines whether the given {@link Boolean valid} argument evaluates to {@link Boolean#TRUE}.
   *
   * @param valid {@link Boolean} value to evaluate.
   * @return a boolean value to evaluate whether the given {@link Boolean valid} argument
   * evaluates to {@link Boolean#TRUE}.
   * @see java.lang.Boolean#TRUE
   */
  @NullSafe
  private static boolean isNotValid(Boolean valid) {
    return !Boolean.TRUE.equals(valid);
  }

  /**
   * Asserts the two {@link Object objects} are comparable.
   *
   * The assertion holds if and only if the {@link Comparable Comparable objects} are equal by comparison.
   *
   * @param <T> {@link Comparable Comparable class type} of the {@link Object objects} in the comparison.
   * @param obj1 first {@link Comparable Comparable object} in the relational comparison.
   * @param obj2 second {@link Comparable Comparable object} in the relational comparison.
   * @throws org.cp.elements.lang.ComparisonException if the assertion fails
   * and the two {@link Object objects} are not comparable.
   * @see #comparable(Comparable, Comparable, String, Object...)
   * @see java.lang.Comparable#compareTo(Object)
   */
  public static <T extends Comparable<T>> void comparable(T obj1, T obj2) {
    comparable(obj1, obj2, "[%1$s] is not comparable to [%2$s]", obj1, obj2);
  }

  /**
   * Asserts the two {@link Object objects} are comparable.
   *
   * The assertion holds if and only if the {@link Comparable Comparable objects} are equal by comparison.
   *
   * @param <T> {@link Comparable Comparable class type} of the {@link Object objects} in the comparison.
   * @param obj1 first {@link Comparable Comparable object} in the relational comparison.
   * @param obj2 second {@link Comparable Comparable object} in the relational comparison.
   * @param message {@link String} containing the message used in the {@link ComparisonException} thrown
   * if the assertion fails.
   * @param arguments array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws org.cp.elements.lang.ComparisonException if the assertion fails
   * and the two {@link Object objects} are not comparable.
   * @see #comparable(Comparable, Comparable, RuntimeException)
   * @see java.lang.Comparable#compareTo(Object)
   */
  public static <T extends Comparable<T>> void comparable(T obj1, T obj2, String message, Object... arguments) {
    comparable(obj1, obj2, new ComparisonException(format(message, arguments)));
  }

  /**
   * Asserts the two {@link Object objects} are comparable.
   *
   * The assertion holds if and only if the {@link Comparable Comparable objects} are equal by comparison.
   *
   * @param <T> {@link Comparable Comparable class type} of the {@link Object objects} in the comparison.
   * @param obj1 first {@link Comparable Comparable object} in the relational comparison.
   * @param obj2 second {@link Comparable Comparable object} in the relational comparison.
   * @param message {@link Supplier} containing the message used in the {@link ComparisonException} thrown
   * if the assertion fails.
   * @throws org.cp.elements.lang.ComparisonException if the assertion fails
   * and the two {@link Object objects} are not comparable.
   * @see java.lang.Comparable#compareTo(Object)
   * @see java.util.function.Supplier
   */
  public static <T extends Comparable<T>> void comparable(T obj1, T obj2, Supplier<String> message) {
    if (areNotComparable(obj1, obj2)) {
      throw new ComparisonException(message.get());
    }
  }

  /**
   * Asserts the two {@link Object objects} are comparable.
   *
   * The assertion holds if and only if the {@link Comparable Comparable objects} are equal by comparison.
   *
   * @param <T> {@link Comparable Comparable class type} of the {@link Object objects} in the comparison.
   * @param obj1 first {@link Comparable Comparable object} in the relational comparison.
   * @param obj2 second {@link Comparable Comparable object} in the relational comparison.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the assertion fails and the two {@link Object objects} are not comparable.
   * @see java.lang.Comparable#compareTo(Object)
   */
  public static <T extends Comparable<T>> void comparable(T obj1, T obj2, RuntimeException cause) {
    if (areNotComparable(obj1, obj2)) {
      throw cause;
    }
  }

  /**
   * Evaluates the given {@link Comparable Comparable objects} to determine whether they are equal by comparison.
   *
   * @param <T> {@link Comparable Comparable class type} of the given {@link Object objects}.
   * @param obj1 {@link Object} to compare for equality with {@link Object object 2}.
   * @param obj2 {@link Object} to compare for equality with {@link Object object 1}.
   * @return a boolean value indicating whether the given {@link Comparable Comparable objects}
   * to determine whether they are equal by comparison.
   * @see java.lang.Comparable#compareTo(Object)
   */
  @NullSafe
  private static <T extends Comparable<T>> boolean areNotComparable(T obj1, T obj2) {
    return obj1 == null || obj2 == null || obj1.compareTo(obj2) != 0;
  }

  /**
   * Asserts that two {@link Object objects} are equal as determined by {@link Object#equals(Object)}.
   *
   * The assertion holds if and only if both {@link Object objects} are not {@literal null}
   * and {@link Object#equals(Object) equal} in value.
   *
   * @param obj1 {@link Object left operand} in the equality comparison.
   * @param obj2 {@link Object right operand} in the equality comparison.
   * @throws org.cp.elements.lang.EqualityException if the assertion fails
   * and the two {@link Object objects} are not equal.
   * @see #equals(Object, Object, String, Object...)
   * @see java.lang.Object#equals(Object)
   */
  public static void equals(Object obj1, Object obj2) {
    equals(obj1, obj2, "[%1$s] is not equal to [%2$s]", obj1, obj2);
  }

  /**
   * Asserts that two {@link Object objects} are equal as determined by {@link Object#equals(Object)}.
   *
   * The assertion holds if and only if both {@link Object objects} are not {@literal null}
   * and {@link Object#equals(Object) equal} in value.
   *
   * @param obj1 {@link Object left operand} in the equality comparison.
   * @param obj2 {@link Object right operand} in the equality comparison.
   * @param message {@link String} containing the message used in the {@link EqualityException} thrown
   * if the assertion fails.
   * @param arguments array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws org.cp.elements.lang.EqualityException if the assertion fails
   * and the two {@link Object objects} are not equal.
   * @see #equals(Object, Object, RuntimeException)
   * @see java.lang.Object#equals(Object)
   */
  public static void equals(Object obj1, Object obj2, String message, Object... arguments) {
    equals(obj1, obj2, new EqualityException(format(message, arguments)));
  }

  /**
   * Asserts that two {@link Object objects} are equal as determined by {@link Object#equals(Object)}.
   *
   * The assertion holds if and only if both {@link Object objects} are not {@literal null}
   * and {@link Object#equals(Object) equal} in value.
   *
   * @param obj1 {@link Object left operand} in the equality comparison.
   * @param obj2 {@link Object right operand} in the equality comparison.
   * @param message {@link Supplier} containing the message used in the {@link EqualityException} thrown
   * if the assertion fails.
   * @throws org.cp.elements.lang.EqualityException if the assertion fails
   * and the two {@link Object objects} are not equal.
   * @see java.lang.Object#equals(Object)
   * @see java.util.function.Supplier
   */
  public static void equals(Object obj1, Object obj2, Supplier<String> message) {
    if (areNotEqual(obj1, obj2)) {
      throw new EqualityException(message.get());
    }
  }

  /**
   * Asserts that two {@link Object objects} are equal as determined by {@link Object#equals(Object)}.
   *
   * The assertion holds if and only if both {@link Object objects} are not {@literal null}
   * and {@link Object#equals(Object) equal} in value.
   *
   * @param obj1 {@link Object left operand} in the equality comparison.
   * @param obj2 {@link Object right operand} in the equality comparison.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the two {@link Object objects} are not equal.
   * @see java.lang.Object#equals(Object)
   */
  public static void equals(Object obj1, Object obj2, RuntimeException cause) {
    if (areNotEqual(obj1, obj2)) {
      throw cause;
    }
  }

  /**
   * Evaluate the given {@link Object objects} or equality.
   *
   * @param obj1 {@link Object} to evaluate for equality with {@link Object object 1}.
   * @param obj2 {@link Object} to evaluate for equality with {@link Object object 2}.
   * @return a boolean value indicating whether the given {@link Object objects} are equal.
   * @see java.lang.Object#equals(Object)
   */
  @NullSafe
  private static boolean areNotEqual(Object obj1, Object obj2) {
    return obj1 == null || !obj1.equals(obj2);
  }

  /**
   * Assert that the given {@link String} is not blank.
   *
   * The assertion holds if and only if the {@link String} is not {@literal null}, {@link String#isEmpty() empty}
   * or contains only {@link Character#isWhitespace(char) whitespace characters}.
   *
   * @param value {@link String} being evaluated.
   * @throws java.lang.IllegalArgumentException if the {@link String} is blank.
   * @see #hasText(String, String, Object...)
   * @see java.lang.String
   */
  public static void hasText(String value) {
    hasText(value, "argument is blank");
  }

  /**
   * Assert that the given {@link String} is not blank.
   *
   * The assertion holds if and only if the {@link String} is not {@literal null}, {@link String#isEmpty() empty}
   * or contains only {@link Character#isWhitespace(char) whitespace characters}.
   *
   * @param value {@link String} being evaluated.
   * @param message {@link String} containing the message used in the {@link IllegalArgumentException} thrown
   * if the assertion fails.
   * @param arguments array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalArgumentException if the {@link String} is blank.
   * @see #hasText(String, RuntimeException)
   * @see java.lang.String
   */
  public static void hasText(String value, String message, Object... arguments) {
    hasText(value, new IllegalArgumentException(format(message, arguments)));
  }

  /**
   * Assert that the given {@link String} is not blank.
   *
   * The assertion holds if and only if the {@link String} is not {@literal null}, {@link String#isEmpty() empty}
   * or contains only {@link Character#isWhitespace(char) whitespace characters}.
   *
   * @param value {@link String} being evaluated.
   * @param message {@link Supplier} containing the message used in the {@link IllegalArgumentException} thrown
   * if the assertion fails.
   * @throws java.lang.IllegalArgumentException if the {@link String} is blank.
   * @see java.util.function.Supplier
   * @see java.lang.String
   */
  public static void hasText(String value, Supplier<String> message) {
    if (isBlank(value)) {
      throw new IllegalArgumentException(message.get());
    }
  }

  /**
   * Assert that the given {@link String} is not blank.
   *
   * The assertion holds if and only if the {@link String} is not {@literal null}, {@link String#isEmpty() empty}
   * or contains only {@link Character#isWhitespace(char) whitespace characters}.
   *
   * @param value {@link String} being evaluated.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the {@link String} is blank.
   * @see java.lang.String
   */
  public static void hasText(String value, RuntimeException cause) {
    if (isBlank(value)) {
      throw cause;
    }
  }

  /**
   * Determines whether the given {@link String} is blank.
   *
   * @param value {@link String} to evaluate.
   * @return a boolean value indicating whether the given {@link String} is blank.
   * @see java.lang.String#isEmpty()
   */
  @NullSafe
  private static boolean isBlank(String value) {
    return value == null || value.trim().isEmpty();
  }

  /**
   * Asserts that the {@link Thread#currentThread() current Thread} holds the specified {@link Object lock}.
   *
   * The assertion holds if and only if the {@link Object lock} is not {@literal null}
   * and the {@link Thread#currentThread() current Thread} holds the given {@link Object lock}.
   *
   * @param lock {@link Object} used as the lock, monitor or mutex in the synchronization.
   * @throws java.lang.IllegalMonitorStateException if the {@link Thread#currentThread() current Thread}
   * does not hold the {@link Object lock} or the {@link Object lock} is {@literal null}.
   * @see #holdsLock(Object, String, Object...)
   * @see java.lang.Thread#holdsLock(Object)
   */
  public static void holdsLock(Object lock) {
    holdsLock(lock, "current thread [%1$s] does not hold lock [%2$s]", Thread.currentThread().getName(), lock);
  }

  /**
   * Asserts that the {@link Thread#currentThread() current Thread} holds the specified {@link Object lock}.
   *
   * The assertion holds if and only if the {@link Object lock} is not {@literal null}
   * and the {@link Thread#currentThread() current Thread} holds the given {@link Object lock}.
   *
   * @param lock {@link Object} used as the lock, monitor or mutex in the synchronization.
   * @param message {@link String} containing the message used in the {@link IllegalMonitorStateException} thrown
   * if the assertion fails.
   * @param arguments array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalMonitorStateException if the {@link Thread#currentThread() current Thread}
   * does not hold the {@link Object lock} or the {@link Object lock} is {@literal null}.
   * @see #holdsLock(Object, RuntimeException)
   * @see java.lang.Thread#holdsLock(Object)
   */
  public static void holdsLock(Object lock, String message, Object... arguments) {
    holdsLock(lock, new IllegalMonitorStateException(format(message, arguments)));
  }

  /**
   * Asserts that the {@link Thread#currentThread() current Thread} holds the specified {@link Object lock}.
   *
   * The assertion holds if and only if the {@link Object lock} is not {@literal null}
   * and the {@link Thread#currentThread() current Thread} holds the given {@link Object lock}.
   *
   * @param lock {@link Object} used as the lock, monitor or mutex in the synchronization.
   * @param message {@link Supplier} containing the message used in the {@link IllegalMonitorStateException} thrown
   * if the assertion fails.
   * @throws java.lang.IllegalMonitorStateException if the {@link Thread#currentThread() current Thread}
   * does not hold the {@link Object lock} or the {@link Object lock} is {@literal null}.
   * @see java.lang.Thread#holdsLock(Object)
   * @see java.util.function.Supplier
   */
  public static void holdsLock(Object lock, Supplier<String> message) {
    if (isNotLockHolder(lock)) {
      throw new IllegalMonitorStateException(message.get());
    }
  }

  /**
   * Asserts that the {@link Thread#currentThread() current Thread} holds the specified {@link Object lock}.
   *
   * The assertion holds if and only if the {@link Object lock} is not {@literal null}
   * and the {@link Thread#currentThread() current Thread} holds the given {@link Object lock}.
   *
   * @param lock {@link Object} used as the lock, monitor or mutex in the synchronization.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the {@link Thread#currentThread() current Thread}
   * does not hold the {@link Object lock} or the {@link Object lock} is {@literal null}.
   * @see java.lang.Thread#holdsLock(Object)
   */
  public static void holdsLock(Object lock, RuntimeException cause) {
    if (isNotLockHolder(lock)) {
      throw cause;
    }
  }

  /**
   * Determines whether the {@link Thread#currentThread() current Thread} is the holder
   * of the given {@link Object lock}.
   *
   * @param lock {@link Object} to evaluate as the lock.
   * @return a boolean value indicating whether the {@link Thread#currentThread() current Thread}
   * is the holder of the given {@link Object lock}.
   * @see java.lang.Thread#holdsLock(Object)
   */
  @NullSafe
  private static boolean isNotLockHolder(Object lock) {
    return lock == null || !Thread.holdsLock(lock);
  }

  /**
   * Asserts that the {@link Class 'from' class type} is assignable to the {@link Class 'to' class type}.
   *
   * The assertion holds if and only if the {@link Class 'from' class type} is the same as or a subclass
   * of the {@link Class 'to' class type}.
   *
   * @param from {@link Class class type} being evaluated for assignment compatibility
   * with the {@link Class 'to' class type}.
   * @param to {@link Class class type} used to determine if the {@link Class 'from' class type}
   * is assignment compatible.
   * @throws java.lang.ClassCastException if the {@link Class 'from' class type} is not assignment compatible
   * with the {@link Class 'to' class type}.
   * @see #isAssignableTo(Class, Class, String, Object...)
   * @see java.lang.Class#isAssignableFrom(Class)
   */
  public static void isAssignableTo(Class<?> from, Class<?> to) {
    isAssignableTo(from, to, "[%1$s] is not assignable to [%2$s]", from, to);
  }

  /**
   * Asserts that the {@link Class 'from' class type} is assignable to the {@link Class 'to' class type}.
   *
   * The assertion holds if and only if the {@link Class 'from' class type} is the same as or a subclass
   * of the {@link Class 'to' class type}.
   *
   * @param from {@link Class class type} being evaluated for assignment compatibility
   * with the {@link Class 'to' class type}.
   * @param to {@link Class class type} used to determine if the {@link Class 'from' class type}
   * is assignment compatible.
   * @param message {@link String} containing the message used in the {@link ClassCastException} thrown
   * if the assertion fails.
   * @param arguments array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.ClassCastException if the {@link Class 'from' class type} is not assignment compatible
   * with the {@link Class 'to' class type}.
   * @see #isAssignableTo(Class, Class, RuntimeException)
   * @see java.lang.Class#isAssignableFrom(Class)
   */
  public static void isAssignableTo(Class<?> from, Class<?> to, String message, Object... arguments) {
    isAssignableTo(from, to, new ClassCastException(format(message, arguments)));
  }

  /**
   * Asserts that the {@link Class 'from' class type} is assignable to the {@link Class 'to' class type}.
   *
   * The assertion holds if and only if the {@link Class 'from' class type} is the same as or a subclass
   * of the {@link Class 'to' class type}.
   *
   * @param from {@link Class class type} being evaluated for assignment compatibility
   * with the {@link Class 'to' class type}.
   * @param to {@link Class class type} used to determine if the {@link Class 'from' class type}
   * is assignment compatible.
   * @param message {@link String} containing the message used in the {@link ClassCastException} thrown
   * if the assertion fails.
   * @throws java.lang.ClassCastException if the {@link Class 'from' class type} is not assignment compatible
   * with the {@link Class 'to' class type}.
   * @see java.lang.Class#isAssignableFrom(Class)
   */
  public static void isAssignableTo(Class<?> from, Class<?> to, Supplier<String> message) {
    if (isNotAssignableTo(from, to)) {
      throw new ClassCastException(message.get());
    }
  }

  /**
   * Asserts that the {@link Class 'from' class type} is assignable to the {@link Class 'to' class type}.
   *
   * The assertion holds if and only if the {@link Class 'from' class type} is the same as or a subclass
   * of the {@link Class 'to' class type}.
   *
   * @param from {@link Class class type} being evaluated for assignment compatibility
   * with the {@link Class 'to' class type}.
   * @param to {@link Class class type} used to determine if the {@link Class 'from' class type}
   * is assignment compatible.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the {@link Class 'from' class type} is not assignment compatible
   * with the {@link Class 'to' class type}.
   * @see java.lang.Class#isAssignableFrom(Class)
   */
  public static void isAssignableTo(Class<?> from, Class<?> to, RuntimeException cause) {
    if (isNotAssignableTo(from, to)) {
      throw cause;
    }
  }

  /**
   * Determines whether the {@link Class 'from' class type} is assignable to the {@link Class 'to' class type}.
   *
   * @param from {@link Class class type} being evaluated for assignment compatibility
   * with the {@link Class 'to' class type}.
   * @param to {@link Class class type} used to determine if the {@link Class 'from' class type}
   * is assignment compatible.
   * @return a boolean value indicating whether the {@link Class 'from' class type} is assignable to
   * the {@link Class 'to' class type}.
   * @see java.lang.Class#isAssignableFrom(Class)
   */
  @NullSafe
  private static boolean isNotAssignableTo(Class<?> from, Class<?> to) {
    return to == null || (from != null && !to.isAssignableFrom(from));
  }

  /**
   * Asserts that the condition is {@literal false}.
   *
   * The assertion holds if and only if the value is equal to {@literal false}.
   *
   * @param condition {@link Boolean} value being evaluated.
   * @throws java.lang.IllegalArgumentException if the value is not {@literal false}.
   * @see #isFalse(Boolean, String, Object...)
   * @see java.lang.Boolean#FALSE
   */
  public static void isFalse(Boolean condition) {
    isFalse(condition, "condition [%1$s] is not false", condition);
  }

  /**
   * Asserts that the condition is {@literal false}.
   *
   * The assertion holds if and only if the value is equal to {@literal false}.
   *
   * @param condition {@link Boolean} value being evaluated.
   * @param message {@link String} containing the message used in the {@link IllegalArgumentException} thrown
   * if the assertion fails.
   * @param arguments array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalArgumentException if the value is not {@literal false}.
   * @see #isFalse(Boolean, RuntimeException)
   * @see java.lang.Boolean#FALSE
   */
  public static void isFalse(Boolean condition, String message, Object... arguments) {
    isFalse(condition, new IllegalArgumentException(format(message, arguments)));
  }

  /**
   * Asserts that the condition is {@literal false}.
   *
   * The assertion holds if and only if the value is equal to {@literal false}.
   *
   * @param condition {@link Boolean} value being evaluated.
   * @param message {@link Supplier} containing the message used in the {@link IllegalArgumentException} thrown
   * if the assertion fails.
   * @throws java.lang.IllegalArgumentException if the value is not {@literal false}.
   * @see java.lang.Boolean#FALSE
   */
  public static void isFalse(Boolean condition, Supplier<String> message) {
    if (isNotFalse(condition)) {
      throw new IllegalArgumentException(message.get());
    }
  }

  /**
   * Asserts that the condition is {@literal false}.
   *
   * The assertion holds if and only if the value is equal to {@literal false}.
   *
   * @param condition {@link Boolean} value being evaluated.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the value is not {@literal false}.
   * @see java.lang.Boolean#FALSE
   */
  public static void isFalse(Boolean condition, RuntimeException cause) {
    if (isNotFalse(condition)) {
      throw cause;
    }
  }

  /**
   * Determines whether the given condition is {@literal false}.
   *
   * @param condition {@link Boolean} value to evaluate.
   * @return a boolean indicating whether the given condition is {@literal false}.
   * @see java.lang.Boolean#FALSE
   */
  @NullSafe
  private static boolean isNotFalse(Boolean condition) {
    return !Boolean.FALSE.equals(condition);
  }

  /**
   * Asserts that the given {@link Object} is an instance of the specified {@link Class type}.
   *
   * The assertion holds if and only if the {@link Object} is not {@literal null} and is an instance of
   * the specified {@link Class type}.  This assertion functions exactly the same as
   * the Java {@literal instanceof} operator.
   *
   * @param obj {@link Object} evaluated as an instance of the {@link Class type}.
   * @param type {@link Class type} used to evaluate the {@link Object} in the {@literal instanceof} operator.
   * @throws org.cp.elements.lang.IllegalTypeException if the {@link Object} is not an instance of
   * the specified {@link Class type}.
   * @see #isInstanceOf(Object, Class, String, Object...)
   * @see java.lang.Class#isInstance(Object)
   */
  public static void isInstanceOf(Object obj, Class<?> type) {
    isInstanceOf(obj, type, "[%1$s] is not an instance of [%2$s]", obj, type);
  }

  /**
   * Asserts that the given {@link Object} is an instance of the specified {@link Class type}.
   *
   * The assertion holds if and only if the {@link Object} is not {@literal null} and is an instance of
   * the specified {@link Class type}.  This assertion functions exactly the same as
   * the Java {@literal instanceof} operator.
   *
   * @param obj {@link Object} evaluated as an instance of the {@link Class type}.
   * @param type {@link Class type} used to evaluate the {@link Object} in the {@literal instanceof} operator.
   * @param message {@link String} containing the message used in the {@link IllegalArgumentException} thrown
   * if the assertion fails.
   * @param arguments array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws org.cp.elements.lang.IllegalTypeException if the {@link Object} is not an instance of
   * the specified {@link Class type}.
   * @see #isInstanceOf(Object, Class, RuntimeException)
   * @see java.lang.Class#isInstance(Object)
   */
  public static void isInstanceOf(Object obj, Class<?> type, String message, Object... arguments) {
    isInstanceOf(obj, type, new IllegalTypeException(format(message, arguments)));
  }

  /**
   * Asserts that the given {@link Object} is an instance of the specified {@link Class type}.
   *
   * The assertion holds if and only if the {@link Object} is not {@literal null} and is an instance of
   * the specified {@link Class type}.  This assertion functions exactly the same as
   * the Java {@literal instanceof} operator.
   *
   * @param obj {@link Object} evaluated as an instance of the {@link Class type}.
   * @param type {@link Class type} used to evaluate the {@link Object} in the {@literal instanceof} operator.
   * @param message {@link Supplier} containing the message used in the {@link IllegalArgumentException} thrown
   * if the assertion fails.
   * @throws org.cp.elements.lang.IllegalTypeException if the {@link Object} is not an instance of
   * the specified {@link Class type}.
   * @see java.lang.Class#isInstance(Object)
   */
  public static void isInstanceOf(Object obj, Class<?> type, Supplier<String> message) {
    if (isNotInstanceOf(obj, type)) {
      throw new IllegalTypeException(message.get());
    }
  }

  /**
   * Asserts that the given {@link Object} is an instance of the specified {@link Class type}.
   *
   * The assertion holds if and only if the {@link Object} is not {@literal null} and is an instance of
   * the specified {@link Class type}.  This assertion functions exactly the same as
   * the Java {@literal instanceof} operator.
   *
   * @param obj {@link Object} evaluated as an instance of the {@link Class type}.
   * @param type {@link Class type} used to evaluate the {@link Object} in the {@literal instanceof} operator.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the {@link Object} is not an instance of {@link Class type}.
   * @see java.lang.Class#isInstance(Object)
   */
  public static void isInstanceOf(Object obj, Class<?> type, RuntimeException cause) {
    if (isNotInstanceOf(obj, type)) {
      throw cause;
    }
  }

  /**
   * Determines whether the given {@link Object} is an instance of {@link Class type}.
   *
   * @param obj {@link Object} to evaluate.
   * @param type {@link Class type} to use in the instance of operation.
   * @return a boolean value indicating whether the given {@link Object} is an instance of {@link Class type}.
   * @see java.lang.Class#isInstance(Object)
   * @see java.lang.Object
   */
  private static boolean isNotInstanceOf(Object obj, Class<?> type) {
    return !type.isInstance(obj);
  }

  /**
   * Asserts that the condition is {@literal true}.
   *
   * The assertion holds if and only if the value is equal to {@literal true}.
   *
   * @param condition {@link Boolean} value being evaluated as a {@literal true} condition.
   * @throws java.lang.IllegalArgumentException if the value is not {@literal true}.
   * @see #isTrue(Boolean, String, Object...)
   * @see java.lang.Boolean#TRUE
   */
  public static void isTrue(Boolean condition) {
    isTrue(condition, "condition [%1$s] is not true", condition);
  }

  /**
   * Asserts that the condition is {@literal true}.
   *
   * The assertion holds if and only if the value is equal to {@literal true}.
   *
   * @param condition {@link Boolean} value being evaluated as a {@literal true} condition.
   * @param message {@link String} containing the message used in the {@link IllegalArgumentException} thrown
   * if the assertion fails.
   * @param arguments array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalArgumentException if the value is not {@literal true}.
   * @see #isTrue(Boolean, RuntimeException)
   * @see java.lang.Boolean#TRUE
   */
  public static void isTrue(Boolean condition, String message, Object... arguments) {
    isTrue(condition, new IllegalArgumentException(format(message, arguments)));
  }

  /**
   * Asserts that the condition is {@literal true}.
   *
   * The assertion holds if and only if the value is equal to {@literal true}.
   *
   * @param condition {@link Boolean} value being evaluated as a {@literal true} condition.
   * @param message {@link String} containing the message used in the {@link IllegalArgumentException} thrown
   * if the assertion fails.
   * @throws java.lang.IllegalArgumentException if the value is not {@literal true}.
   * @see java.lang.Boolean#TRUE
   */
  public static void isTrue(Boolean condition, Supplier<String> message) {
    if (isNotTrue(condition)) {
      throw new IllegalArgumentException(message.get());
    }
  }

  /**
   * Asserts that the condition is {@literal true}.
   *
   * The assertion holds if and only if the value is equal to {@literal true}.
   *
   * @param condition {@link Boolean} value being evaluated as a {@literal true} condition.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the value is not {@literal true}.
   * @see java.lang.Boolean#TRUE
   */
  public static void isTrue(Boolean condition, RuntimeException cause) {
    if (isNotTrue(condition)) {
      throw cause;
    }
  }

  /**
   * Determines whether the {@link Boolean condition} is {@literal true}.
   *
   * @param condition {@link Boolean} value to evaluate.
   * @return a boolean value indicating whether the {@link Boolean condition} is {@literal true}.
   * @see java.lang.Boolean#TRUE
   */
  @NullSafe
  private static boolean isNotTrue(Boolean condition) {
    return !Boolean.TRUE.equals(condition);
  }

  /**
   * Asserts that the given {@link String} is not empty.
   *
   * The assertion holds if and only if the {@link String String} is not the {@link String#isEmpty() empty String}.
   *
   * @param value {@link String} to evaluate.
   * @throws java.lang.IllegalArgumentException if the {@link String} is empty.
   * @see #notEmpty(String, String, Object...)
   * @see java.lang.String
   */
  public static void notEmpty(String value) {
    notEmpty(value, "argument is empty");
  }

  /**
   * Asserts that the given {@link String} is not empty.
   *
   * The assertion holds if and only if the {@link String String} is not the {@link String#isEmpty() empty String}.
   *
   * @param value {@link String} to evaluate.
   * @param message {@link String} containing the message used in the {@link IllegalArgumentException} thrown
   * if the assertion fails.
   * @param arguments array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalArgumentException if the {@link String} is empty.
   * @see #notEmpty(String, RuntimeException)
   * @see java.lang.String
   */
  public static void notEmpty(String value, String message, Object... arguments) {
    notEmpty(value, new IllegalArgumentException(format(message, arguments)));
  }

  /**
   * Asserts that the given {@link String} is not empty.
   *
   * The assertion holds if and only if the {@link String String} is not the {@link String#isEmpty() empty String}.
   *
   * @param value {@link String} to evaluate.
   * @param message {@link Supplier} containing the message used in the {@link IllegalArgumentException} thrown
   * if the assertion fails.
   * @throws java.lang.IllegalArgumentException if the {@link String} is empty.
   * @see java.lang.String
   */
  public static void notEmpty(String value, Supplier<String> message) {
    if (isEmpty(value)) {
      throw new IllegalArgumentException(message.get());
    }
  }

  /**
   * Asserts that the given {@link String} is not empty.
   *
   * The assertion holds if and only if the {@link String String} is not the {@link String#isEmpty() empty String}.
   *
   * @param value {@link String} to evaluate.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the {@link String} is empty.
   * @see java.lang.String
   */
  public static void notEmpty(String value, RuntimeException cause) {
    if (isEmpty(value)) {
      throw cause;
    }
  }

  /**
   * Determines whether the given {@link String} is {@link String#isEmpty() empty}.
   *
   * @param value {@link String} to evaluate.
   * @return a boolean value indicating whether the given {@link String} is {@link String#isEmpty() empty}.
   * @see java.lang.String#isEmpty()
   */
  @NullSafe
  private static boolean isEmpty(String value) {
    return "".equals(value);
  }

  /**
   * Asserts that the {@link Object array} is not empty.
   *
   * The assertion holds if and only if the {@link Object array} is not {@literal null} and contains at least 1 element.
   *
   * @param array {@link Object array} to evaluate.
   * @throws java.lang.IllegalArgumentException if the {@link Object array} is {@literal null} or empty.
   * @see #notEmpty(Object[], String, Object...)
   * @see java.lang.Object[]
   */
  public static void notEmpty(Object[] array) {
    notEmpty(array, "array is empty");
  }

  /**
   * Asserts that the {@link Object array} is not empty.
   *
   * The assertion holds if and only if the {@link Object array} is not {@literal null} and contains at least 1 element.
   *
   * @param array {@link Object array} to evaluate.
   * @param message {@link String} containing the message using in the {@link IllegalArgumentException} thrown
   * if the assertion fails.
   * @param arguments array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalArgumentException if the {@link Object array} is {@literal null} or empty.
   * @see #notEmpty(Object[], RuntimeException)
   * @see java.lang.Object[]
   */
  public static void notEmpty(Object[] array, String message, Object... arguments) {
    notEmpty(array, new IllegalArgumentException(format(message, arguments)));
  }

  /**
   * Asserts that the {@link Object array} is not empty.
   *
   * The assertion holds if and only if the {@link Object array} is not {@literal null} and contains at least 1 element.
   *
   * @param array {@link Object array} to evaluate.
   * @param message {@link Supplier} containing the message using in the {@link IllegalArgumentException} thrown
   * if the assertion fails.
   * @throws java.lang.IllegalArgumentException if the {@link Object array} is {@literal null} or empty.
   * @see java.lang.Object[]
   */
  public static void notEmpty(Object[] array, Supplier<String> message) {
    if (isEmpty(array)) {
      throw new IllegalArgumentException(message.get());
    }
  }

  /**
   * Asserts that the {@link Object array} is not empty.
   *
   * The assertion holds if and only if the {@link Object array} is not {@literal null} and contains at least 1 element.
   *
   * @param array {@link Object array} to evaluate.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the {@link Object array} is {@literal null} or empty.
   * @see java.lang.Object[]
   */
  public static void notEmpty(Object[] array, RuntimeException cause) {
    if (isEmpty(array)) {
      throw cause;
    }
  }

  /**
   * Determines whether the given {@link Object array} is empty.
   *
   * @param array {@link Object array} to evaluate.
   * @return a boolean value indicating whether the given {@link Object array} is empty.
   */
  @NullSafe
  private static boolean isEmpty(Object[] array) {
    return array == null || array.length == 0;
  }

  /**
   * Asserts that the {@link Collection} is not empty.
   *
   * The assertion holds if and only if the {@link Collection} is not {@literal null} and contains at least 1 element.
   *
   * @param collection {@link Collection} to evaluate.
   * @throws java.lang.IllegalArgumentException if the {@link Collection} is {@literal null} or empty.
   * @see #notEmpty(Collection, String, Object...)
   * @see java.util.Collection#isEmpty()
   */
  public static void notEmpty(Collection<?> collection) {
    notEmpty(collection, "collection is empty");
  }

  /**
   * Asserts that the {@link Collection} is not empty.
   *
   * The assertion holds if and only if the {@link Collection} is not {@literal null} and contains at least 1 element.
   *
   * @param collection {@link Collection} to evaluate.
   * @param message {@link String} containing the message using in the {@link IllegalArgumentException} thrown
   * if the assertion fails.
   * @param arguments array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalArgumentException if the {@link Collection} is {@literal null} or empty.
   * @see #notEmpty(java.util.Collection, RuntimeException)
   * @see java.util.Collection#isEmpty()
   */
  public static void notEmpty(Collection<?> collection, String message, Object... arguments) {
    notEmpty(collection, new IllegalArgumentException(format(message, arguments)));
  }

  /**
   * Asserts that the {@link Collection} is not empty.
   *
   * The assertion holds if and only if the {@link Collection} is not {@literal null} and contains at least 1 element.
   *
   * @param collection {@link Collection} to evaluate.
   * @param message {@link Supplier} containing the message using in the {@link IllegalArgumentException} thrown
   * if the assertion fails.
   * @throws java.lang.IllegalArgumentException if the {@link Collection} is {@literal null} or empty.
   * @see java.util.Collection#isEmpty()
   */
  public static void notEmpty(Collection<?> collection, Supplier<String> message) {
    if (isEmpty(collection)) {
      throw new IllegalArgumentException(message.get());
    }
  }

  /**
   * Asserts that the {@link Collection} is not empty.
   *
   * The assertion holds if and only if the {@link Collection} is not {@literal null} and contains at least 1 element.
   *
   * @param collection {@link Collection} to evaluate.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the {@link Collection} is {@literal null} or empty.
   * @see java.util.Collection#isEmpty()
   */
  public static void notEmpty(Collection<?> collection, RuntimeException cause) {
    if (isEmpty(collection)) {
      throw cause;
    }
  }

  /**
   * Determines whether the given {@link Collection} is {@link Collection#isEmpty() empty}.
   *
   * @param collection {@link Collection} to evaluate.
   * @return a boolean value indicating whether the given {@link Collection} is {@link Collection#isEmpty() empty}.
   * @see java.util.Collection#isEmpty()
   */
  @NullSafe
  private static boolean isEmpty(Collection<?> collection) {
    return collection == null || collection.isEmpty();
  }

  /**
   * Asserts that the {@link Map} is not empty.
   *
   * The assertion holds if and only if the {@link Map} is not {@literal null}
   * and contains at least 1 key/value mapping.
   *
   * @param map {@link Map} to evaluate.
   * @throws java.lang.IllegalArgumentException if the {@link Map} is {@literal null} or empty.
   * @see #notEmpty(Map, String, Object...)
   * @see java.util.Map#isEmpty()
   */
  public static void notEmpty(Map<?, ?> map) {
    notEmpty(map, "map is empty");
  }

  /**
   * Asserts that the {@link Map} is not empty.
   *
   * The assertion holds if and only if the {@link Map} is not {@literal null}
   * and contains at least 1 key/value mapping.
   *
   * @param map {@link Map} to evaluate.
   * @param message {@link String} containing the message using in the {@link IllegalArgumentException} thrown
   * if the assertion fails.
   * @param arguments array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalArgumentException if the {@link Map} is {@literal null} or empty.
   * @see #notEmpty(java.util.Map, RuntimeException)
   * @see java.util.Map#isEmpty()
   */
  public static void notEmpty(Map<?, ?> map, String message, Object... arguments) {
    notEmpty(map, new IllegalArgumentException(format(message, arguments)));
  }

  /**
   * Asserts that the {@link Map} is not empty.
   *
   * The assertion holds if and only if the {@link Map} is not {@literal null}
   * and contains at least 1 key/value mapping.
   *
   * @param map {@link Map} to evaluate.
   * @param message {@link Supplier} containing the message using in the {@link IllegalArgumentException} thrown
   * if the assertion fails.
   * @throws java.lang.IllegalArgumentException if the {@link Map} is {@literal null} or empty.
   * @see java.util.Map#isEmpty()
   */
  public static void notEmpty(Map<?, ?> map, Supplier<String> message) {
    if (isEmpty(map)) {
      throw new IllegalArgumentException(message.get());
    }
  }

  /**
   * Asserts that the {@link Map} is not empty.
   *
   * The assertion holds if and only if the {@link Map} is not {@literal null}
   * and contains at least 1 key/value mapping.
   *
   * @param map {@link Map} to evaluate.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the {@link Map} is {@literal null} or empty.
   * @see java.util.Map#isEmpty()
   */
  public static void notEmpty(Map<?, ?> map, RuntimeException cause) {
    if (isEmpty(map)) {
      throw cause;
    }
  }

  /**
   * Determines whether the given {@link Map} is {@link Map#isEmpty() empty}.
   *
   * @param map {@link Map} to evaluate.
   * @return a boolean value indicating whether the given {@link Map} is {@link Map#isEmpty() empty}.
   * @see java.util.Map#isEmpty()
   */
  @NullSafe
  private static boolean isEmpty(Map<?, ?> map) {
    return map == null || map.isEmpty();
  }

  /**
   * Asserts that the {@link Object} reference is not {@literal null}.
   *
   * The assertion holds if and only if the {@link Object} reference is not {@literal null}.
   *
   * @param obj {@link Object} reference to evaluate.
   * @throws java.lang.IllegalArgumentException if the {@link Object} reference is {@literal null}.
   * @see #notNull(Object, String, Object...)
   * @see java.lang.Object
   */
  public static void notNull(Object obj) {
    notNull(obj, "argument is null");
  }

  /**
   * Asserts that the {@link Object} reference is not {@literal null}.
   *
   * The assertion holds if and only if the {@link Object} reference is not {@literal null}.
   *
   * @param obj {@link Object} reference to evaluate.
   * @param message {@link String} containing the message using in the {@link NullPointerException} thrown
   * if the assertion fails.
   * @param arguments array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalArgumentException if the {@link Object} reference is {@literal null}.
   * @see #notNull(Object, RuntimeException)
   * @see java.lang.Object
   */
  public static void notNull(Object obj, String message, Object... arguments) {
    notNull(obj, new IllegalArgumentException(format(message, arguments)));
  }

  /**
   * Asserts that the {@link Object} reference is not {@literal null}.
   *
   * The assertion holds if and only if the {@link Object} reference is not {@literal null}.
   *
   * @param obj {@link Object} reference to evaluate.
   * @param message {@link Supplier} containing the message using in the {@link NullPointerException} thrown
   * if the assertion fails.
   * @throws java.lang.IllegalArgumentException if the {@link Object} reference is {@literal null}.
   * @see java.lang.Object
   */
  public static void notNull(Object obj, Supplier<String> message) {
    if (isNull(obj)) {
      throw new IllegalArgumentException(message.get());
    }
  }

  /**
   * Asserts that the {@link Object} reference is not {@literal null}.
   *
   * The assertion holds if and only if the {@link Object} reference is not {@literal null}.
   *
   * @param obj {@link Object} reference to evaluate.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the {@link Object} reference is {@literal null}.
   * @see java.lang.Object
   */
  public static void notNull(Object obj, RuntimeException cause) {
    if (isNull(obj)) {
      throw cause;
    }
  }

  /**
   * Determines whether the given {@link Object} reference is {@literal null}.
   *
   * @param obj {@link Object} to evaluate.
   * @return a boolean value indicating whether the {@link Object} reference is {@literal null}.
   * @see java.lang.Object
   */
  @NullSafe
  private static boolean isNull(Object obj) {
    return obj == null;
  }

  /**
   * Asserts that two {@link Object objects} are the same {@link Object} as determined by the identity comparison.
   *
   * The assertion holds if and only if the two {@link Object objects} are the same {@link Object} in memory.
   *
   * @param obj1 {@link Object left operand} in the identity comparison.
   * @param obj2 {@link Object right operand} in the identity comparison.
   * @throws org.cp.elements.lang.IdentityException if the two {@link Object objects} are not the same.
   * @see #same(Object, Object, String, Object...)
   * @see java.lang.Object
   */
  public static void same(Object obj1, Object obj2) {
    same(obj1, obj2, "[%1$s] is not the same as [%2$s]", obj1, obj2);
  }

  /**
   * Asserts that two {@link Object objects} are the same {@link Object} as determined by the identity comparison.
   *
   * The assertion holds if and only if the two {@link Object objects} are the same {@link Object} in memory.
   *
   * @param obj1 {@link Object left operand} in the identity comparison.
   * @param obj2 {@link Object right operand} in the identity comparison.
   * @param message {@link String} containing the message used in the {@link IdentityException} thrown
   * if the assertion fails.
   * @param arguments array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws org.cp.elements.lang.IdentityException if the two {@link Object objects} are not the same.
   * @see #same(Object, Object, RuntimeException)
   * @see java.lang.Object
   */
  public static void same(Object obj1, Object obj2, String message, Object... arguments) {
    same(obj1, obj2, new IdentityException(format(message, arguments)));
  }

  /**
   * Asserts that two {@link Object objects} are the same {@link Object} as determined by the identity comparison.
   *
   * The assertion holds if and only if the two {@link Object objects} are the same {@link Object} in memory.
   *
   * @param obj1 {@link Object left operand} in the identity comparison.
   * @param obj2 {@link Object right operand} in the identity comparison.
   * @param message {@link Supplier} containing the message used in the {@link IdentityException} thrown
   * if the assertion fails.
   * @throws org.cp.elements.lang.IdentityException if the two {@link Object objects} are not the same.
   * @see java.lang.Object
   */
  public static void same(Object obj1, Object obj2, Supplier<String> message) {
    if (obj1 != obj2) {
      throw new IdentityException(message.get());
    }
  }

  /**
   * Asserts that two {@link Object objects} are the same {@link Object} as determined by the identity comparison.
   *
   * The assertion holds if and only if the two {@link Object objects} are the same {@link Object} in memory.
   *
   * @param obj1 {@link Object left operand} in the identity comparison.
   * @param obj2 {@link Object right operand} in the identity comparison.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the {@link Object objects} are not the same.
   * @see java.lang.Object
   */
  public static void same(Object obj1, Object obj2, RuntimeException cause) {
    if (obj1 != obj2) {
      throw cause;
    }
  }

  /**
   * Determines whether the given {@link Object objects} are the same {@link Object}.
   *
   * @param obj1 {@link Object left operand} in the identity comparison.
   * @param obj2 {@link Object right operand} in the identity comparison.
   * @return a boolean value indicating whether the given {@link Object objects} are the same {@link Object}.
   * @see java.lang.Object
   */
  @NullSafe
  private static boolean isNotSame(Object obj1, Object obj2) {
    return obj1 != obj2;
  }

  /**
   * Asserts that the state is valid.
   *
   * The assertion holds if and only if valid is {@literal true}.
   *
   * @param valid {@link Boolean} value indicating whether the state is valid.
   * @throws java.lang.IllegalStateException if the state is invalid.
   * @see #state(Boolean, String, Object...)
   */
  public static void state(Boolean valid) {
    state(valid, "state is invalid");
  }

  /**
   * Asserts that the state is valid.
   *
   * The assertion holds if and only if valid is {@literal true}.
   *
   * @param valid {@link Boolean} value indicating whether the state is valid.
   * @param message {@link String} containing the message used in the {@link IllegalStateException} thrown
   * if the assertion fails.
   * @param arguments array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalStateException if the state is invalid.
   * @see #state(Boolean, RuntimeException)
   */
  public static void state(Boolean valid, String message, Object... arguments) {
    state(valid, new IllegalStateException(format(message, arguments)));
  }

  /**
   * Asserts that the state is valid.
   *
   * The assertion holds if and only if valid is {@literal true}.
   *
   * @param valid {@link Boolean} value indicating whether the state is valid.
   * @param message {@link Supplier} containing the message used in the {@link IllegalStateException} thrown
   * if the assertion fails.
   * @throws java.lang.IllegalStateException if the state is invalid.
   */
  public static void state(Boolean valid, Supplier<String> message) {
    if (isNotValid(valid)) {
      throw new IllegalStateException(message.get());
    }
  }

  /**
   * Asserts that the state is valid.
   *
   * The assertion holds if and only if valid is {@literal true}.
   *
   * @param valid {@link Boolean} value indicating whether the state is valid.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the state is invalid.
   */
  public static void state(Boolean valid, RuntimeException cause) {
    if (isNotValid(valid)) {
      throw cause;
    }
  }

  /**
   * Asserts that an operation is supported.
   *
   * The assertion holds if and only if supported is {@literal true}.
   *
   * For example, the application code might assert that:
   *
   * <code>
   *   Assert.supported(dataAccessOperations.isCacheable(), "Data Access Operations (DAO) are not cacheable");
   * </code>
   *
   * @param supported {@link Boolean} value resulting from the evaluation of the criteria used by the application
   * to determine if the operation is supported.
   * @throws java.lang.UnsupportedOperationException if the operation is unsupported.
   * @see #supported(Boolean, String, Object...)
   */
  public static void supported(Boolean supported) {
    supported(supported, "operation not supported");
  }

  /**
   * Asserts that an operation is supported.
   *
   * The assertion holds if and only if supported is {@literal true}.
   *
   * For example, the application code might assert that:
   *
   * <code>
   *   Assert.supported(dataAccessOperations.isCacheable(), "Data Access Operations (DAO) are not cacheable");
   * </code>
   *
   * @param supported {@link Boolean} value resulting from the evaluation of the criteria used by the application
   * to determine if the operation is supported.
   * @param message {@link String} containing the message used in the {@link UnsupportedOperationException} thrown
   * if the assertion fails.
   * @param arguments array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.UnsupportedOperationException if the operations is unsupported.
   * @see #supported(Boolean, RuntimeException)
   */
  public static void supported(Boolean supported, String message, Object... arguments) {
    supported(supported, new UnsupportedOperationException(format(message, arguments)));
  }

  /**
   * Asserts that an operation is supported.
   *
   * The assertion holds if and only if supported is {@literal true}.
   *
   * For example, the application code might assert that:
   *
   * <code>
   *   Assert.supported(dataAccessOperations.isCacheable(), "Data Access Operations (DAO) are not cacheable");
   * </code>
   *
   * @param supported {@link Boolean} value resulting from the evaluation of the criteria used by the application
   * to determine if the operation is supported.
   * @param message {@link Supplier} containing the message used in the {@link UnsupportedOperationException} thrown
   * if the assertion fails.
   * @throws java.lang.UnsupportedOperationException if the operations is unsupported.
   * @see java.util.function.Supplier
   */
  public static void supported(Boolean supported, Supplier<String> message) {
    if (isNotSupported(supported)) {
      throw new UnsupportedOperationException(message.get());
    }
  }

  /**
   * Asserts that an operation is supported.
   *
   * The assertion holds if and only if supported is {@literal true}.
   *
   * For example, the application code might assert that:
   *
   * <code>
   *   Assert.supported(dataAccessOperations.isCacheable(), "Data Access Operations (DAO) are not cacheable");
   * </code>
   *
   * @param supported {@link Boolean} value resulting from the evaluation of the criteria used by the application
   * to determine if the operation is supported.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the operation is unsupported.
   */
  public static void supported(Boolean supported, RuntimeException cause) {
    if (isNotSupported(supported)) {
      throw cause;
    }
  }

  /**
   * Determines whether an application operation is supported.
   *
   * @param supported {@link Boolean} value resulting from the evaluation of the criteria used by the application
   * to determine if the operation is supported.
   * @return a boolean value indicating whether an application operation is supported.
   * @see java.lang.Boolean#TRUE
   */
  private static boolean isNotSupported(Boolean supported) {
    return !Boolean.TRUE.equals(supported);
  }

  /**
   * Formats the specified {@link String message} with the given {@link Object arguments}.
   *
   * @param message {@link String} containing the message to format.
   * @param arguments array of {@link Object arguments} used when formatting the {@link String message}.
   * @return the {@link String message} formatted with the {@link Object arguments}.
   * @see #messageFormat(String, Object...)
   * @see #stringFormat(String, Object...)
   */
  private static String format(String message, Object... arguments) {
    return stringFormat(messageFormat(message, arguments), arguments);
  }

  /**
   * Formats the specified {@link String message} containing possible placeholders as defined by {@link MessageFormat}.
   *
   * @param message {@link String} containing the message to format.
   * @param arguments array of {@link Object arguments} used when formatting the {@link String message}.
   * @return the {@link String message} formatted with the {@link Object arguments}.
   * @see java.text.MessageFormat#format(String, Object...)
   */
  private static String messageFormat(String message, Object... arguments) {
    return (arguments == null ? message : MessageFormat.format(message, arguments));
  }

  /**
   * Formats the specified {@link String message} containing possible placeholders as defined by {@link String}.
   *
   * @param message {@link String} containing the message to format.
   * @param arguments array of {@link Object arguments} used when formatting the {@link String message}.
   * @return the {@link String message} formatted with the {@link Object arguments}.
   * @see java.lang.String#format(String, Object...)
   */
  private static String stringFormat(String message, Object... arguments) {
    return String.format(message, arguments);
  }
}
