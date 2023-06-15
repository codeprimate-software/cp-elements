/*
 * Copyright 2011-Present Author or Authors.
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
import java.util.Iterator;
import java.util.Map;
import java.util.function.Predicate;
import java.util.function.Supplier;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * The {@link Assert} class is a more capable replacement for Java's {@literal assert} keyword, providing an API
 * to make assertions about pre-conditions and state in order to ensure an {@link Object Object's} invariants
 * are enforced and upheld.
 *
 * @author John J. Blum
 * @see java.util.function.Predicate
 * @see org.cp.elements.lang.AssertionException
 * @see org.cp.elements.lang.LangExtensions#assertThat(Object)
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class Assert {

  /**
   * Asserts that an {@literal argument} is valid.
   * <p>
   * The assertion holds if and only if (iff) the {@code argument} tested by the {@link Predicate}
   * evaluates to {@literal true}.
   * <p>
   * For example, application code might assert that:
   *
   * <pre>
   *   <code>
   *     Assert.argument(age, argument -&gt; argument &gt;= 21, "Person must be 21 years of age or older to enter");
   *   </code>
   * </pre>
   *
   * @param <T> {@link Class type} of the argument to assert.
   * @param argument {@link Object} value to test and validate.
   * @param argumentPredicate {@link Predicate} used to test and validate the argument;
   * must not be {@literal null}.
   * @throws java.lang.IllegalArgumentException if the argument is invalid.
   * @see #argument(Object, Predicate, String, Object...)
   * @see java.util.function.Predicate
   */
  public static <T> void argument(T argument, Predicate<T> argumentPredicate) {
    argument(argument, argumentPredicate, "Argument is not valid");
  }

  /**
   * Asserts that an {@literal argument} is valid.
   * <p>
   * The assertion holds if and only if (iff) the {@code argument} tested by the {@link Predicate}
   * evaluates to {@literal true}.
   * <p>
   * For example, application code might assert that:
   *
   * <pre>
   *   <code>
   *     Assert.argument(age, argument -&gt; argument &gt;= 21, "Person must be 21 years of age or older to enter");
   *   </code>
   * </pre>
   *
   * @param <T> {@link Class type} of the argument to assert.
   * @param argument {@link Object} value to test and validate.
   * @param argumentPredicate {@link Predicate} used to test and validate the argument;
   * must not be {@literal null}.
   * @param message {@link String} containing the description for the {@link IllegalArgumentException}
   * thrown if the assertion fails.
   * @param messagePlaceholderValues array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalArgumentException if the argument is invalid.
   * @see #argument(Object, Predicate, RuntimeException)
   * @see java.lang.IllegalArgumentException
   * @see java.util.function.Predicate
   */
  public static <T> void argument(T argument, Predicate<T> argumentPredicate, String message,
      Object... messagePlaceholderValues) {

    argument(argument, argumentPredicate, new IllegalArgumentException(format(message, messagePlaceholderValues)));
  }

  /**
   * Asserts that an {@literal argument} is valid.
   * <p>
   * The assertion holds if and only if (iff) the {@code argument} tested by the {@link Predicate}
   * evaluates to {@literal true}.
   * <p>
   * For example, application code might assert that:
   *
   * <pre>
   *   <code>
   *     Assert.argument(age, argument -&gt; argument &gt;= 21, "Person must be 21 years of age or older to enter");
   *   </code>
   * </pre>
   *
   * @param <T> {@link Class type} of the argument to assert.
   * @param argument {@link Object} value to test and validate.
   * @param argumentPredicate {@link Predicate} used to test and validate the argument;
   * must not be {@literal null}.
   * @param message {@link Supplier} containing the message for the {@link IllegalArgumentException}
   * thrown if the assertion fails.
   * @throws java.lang.IllegalArgumentException if the argument is invalid.
   * @see java.lang.IllegalArgumentException
   * @see java.util.function.Predicate
   * @see java.util.function.Supplier
   */
  public static <T> void argument(T argument, Predicate<T> argumentPredicate, Supplier<String> message) {
    if (isNotValid(argument, argumentPredicate)) {
      throw new IllegalArgumentException(message.get());
    }
  }

  /**
   * Asserts that an {@literal argument} is valid.
   * <p>
   * The assertion holds if and only if (iff) the {@code argument} tested by the {@link Predicate}
   * evaluates to {@literal true}.
   * <p>
   * For example, application code might assert that:
   *
   * <pre>
   *   <code>
   *     Assert.argument(age, argument -&gt; argument &gt;= 21, "Person must be 21 years of age or older to enter");
   *   </code>
   * </pre>
   *
   * @param <T> {@link Class type} of the argument to assert.
   * @param argument {@link Object} value to test and validate.
   * @param argumentPredicate {@link Predicate} used to test and validate the argument;
   * must not be {@literal null}.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the argument is invalid.
   * @see java.util.function.Predicate
   * @see java.lang.RuntimeException
   */
  public static <T> void argument(T argument, Predicate<T> argumentPredicate, RuntimeException cause) {
    if (isNotValid(argument, argumentPredicate)) {
      throw cause;
    }
  }

  /**
   * Null-safe utility method used to determine whether the given {@link Object argument},
   * tested by the given {@link Predicate argument predicate}, evaluates to {@literal true}.
   *
   * @param <T> {@link Class type} of the {@link Object} processed by the given {@link Predicate}.
   * @param argument {@link Object} value to test and validate.
   * @param argumentPredicate {@link Predicate} used to test and validate the argument;
   * must not be {@literal null}.
   * @return a boolean value indicating whether the given {@link Object argument}
   * tested by the given {@link Predicate} evaluates to {@literal true}.
   * @see #nullSafePredicate(Predicate)
   * @see java.util.function.Predicate
   * @see java.lang.Boolean#TRUE
   */
  @NullSafe
  private static <T> boolean isNotValid(@Nullable T argument, @Nullable Predicate<T> argumentPredicate) {
    return !Boolean.TRUE.equals(nullSafePredicate(argumentPredicate).test(argument));
  }

  /**
   * Utility method used to protect against a {@literal null} {@link Predicate}.
   *
   * @param <T> {@link Class type} of argument tested by the {@link Predicate}.
   * @param predicate {@link Predicate} to evaluate for a {@literal null} reference.
   * @return the given {@link Predicate} if not {@literal null}
   * or a {@literal new}, {@literal non-null} {@link Predicate} who's {@literal test}
   * evaluates to {@literal true}.
   * @see java.util.function.Predicate
   */
  private static @NotNull <T> Predicate<T> nullSafePredicate(@Nullable Predicate<T> predicate) {
    return predicate != null ? predicate : argument -> true;
  }

  /**
   * Asserts that two {@link Object objects} are comparable.
   * <p>
   * The assertion holds if and only if the (iff) {@link Comparable Comparable objects} are equal by comparison.
   *
   * @param <T> {@link Comparable Comparable class type} of the {@link Object objects} to compare.
   * @param obj1 first {@link Comparable Comparable object} in the relational comparison.
   * @param obj2 second {@link Comparable Comparable object} in the relational comparison.
   * @throws org.cp.elements.lang.ComparisonException if the assertion fails and the two {@link Object objects}
   * are not comparable.
   * @see #comparable(Comparable, Comparable, Supplier)
   * @see java.lang.Comparable#compareTo(Object)
   */
  public static <T extends Comparable<T>> void comparable(T obj1, T obj2) {
    comparable(obj1, obj2, () -> format("[%1$s] is not comparable to [%2$s]", obj1, obj2));
  }

  /**
   * Asserts that two {@link Object objects} are comparable.
   * <p>
   * The assertion holds if and only if the (iff) {@link Comparable Comparable objects} are equal by comparison.
   *
   * @param <T> {@link Comparable Comparable class type} of the {@link Object objects} to compare.
   * @param obj1 first {@link Comparable Comparable object} in the relational comparison.
   * @param obj2 second {@link Comparable Comparable object} in the relational comparison.
   * @param message {@link String} containing the description used in the {@link ComparisonException}
   * thrown if the assertion fails.
   * @param messagePlaceholderValues array of {@link Object arguments} used as placeholder values when formatting
   * the {@link String message}.
   * @throws org.cp.elements.lang.ComparisonException if the assertion fails and the two {@link Object objects}
   * are not comparable.
   * @see #comparable(Comparable, Comparable, RuntimeException)
   * @see java.lang.Comparable#compareTo(Object)
   */
  public static <T extends Comparable<T>> void comparable(T obj1, T obj2, String message,
      Object... messagePlaceholderValues) {

    comparable(obj1, obj2, new ComparisonException(format(message, messagePlaceholderValues)));
  }

  /**
   * Asserts that two {@link Object objects} are comparable.
   * <p>
   * The assertion holds if and only if the (iff) {@link Comparable Comparable objects} are equal by comparison.
   *
   * @param <T> {@link Comparable Comparable class type} of the {@link Object objects} to compare.
   * @param obj1 first {@link Comparable Comparable object} in the relational comparison.
   * @param obj2 second {@link Comparable Comparable object} in the relational comparison.
   * @param message {@link Supplier} containing the message used in the {@link ComparisonException}
   * thrown if the assertion fails.
   * @throws org.cp.elements.lang.ComparisonException if the assertion fails and the two {@link Object objects}
   * are not comparable.
   * @see java.lang.Comparable#compareTo(Object)
   * @see java.util.function.Supplier
   */
  public static <T extends Comparable<T>> void comparable(T obj1, T obj2, Supplier<String> message) {
    if (areNotComparable(obj1, obj2)) {
      throw new ComparisonException(message.get());
    }
  }

  /**
   * Asserts that two {@link Object objects} are comparable.
   * <p>
   * The assertion holds if and only if the (iff) {@link Comparable Comparable objects} are equal by comparison.
   *
   * @param <T> {@link Comparable Comparable class type} of the {@link Object objects} to compare.
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
   * Null-safe method used to evaluate the given {@link Comparable Comparable objects}
   * to determine whether the {@link Object objects} are equal by comparison.
   *
   * @param <T> {@link Comparable Comparable class type} of the given {@link Object objects}.
   * @param obj1 {@link Object} to compare for equality with {@link Object object 2}.
   * @param obj2 {@link Object} to compare for equality with {@link Object object 1}.
   * @return a boolean value indicating whether the given {@link Comparable Comparable objects}
   * are equal by comparison.
   * @see java.lang.Comparable#compareTo(Object)
   */
  @NullSafe
  private static <T extends Comparable<T>> boolean areNotComparable(@Nullable T obj1, @Nullable T obj2) {
    return obj1 == null || obj2 == null || obj1.compareTo(obj2) != 0;
  }

  /**
   * Asserts that two {@link Object objects} are equal as determined by {@link Object#equals(Object)}.
   * <p>
   * The assertion holds if and only if (iff) both {@link Object objects} are {@literal non-null}
   * and {@link Object#equals(Object) equal} in value.
   *
   * @param obj1 {@link Object left operand} in the equality comparison.
   * @param obj2 {@link Object right operand} in the equality comparison.
   * @throws org.cp.elements.lang.EqualityException if the assertion fails
   * and the two {@link Object objects} are not equal.
   * @see #equals(Object, Object, Supplier)
   * @see java.lang.Object#equals(Object)
   */
  public static void equals(Object obj1, Object obj2) {
    equals(obj1, obj2, () -> format("[%1$s] is not equal to [%2$s]", obj1, obj2));
  }

  /**
   * Asserts that two {@link Object objects} are equal as determined by {@link Object#equals(Object)}.
   * <p>
   * The assertion holds if and only if (iff) both {@link Object objects} are {@literal non-null}
   * and {@link Object#equals(Object) equal} in value.
   *
   * @param obj1 {@link Object left operand} in the equality comparison.
   * @param obj2 {@link Object right operand} in the equality comparison.
   * @param message {@link String} containing the description used in the {@link EqualityException}
   * thrown if the assertion fails.
   * @param messagePlaceholderValues array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws org.cp.elements.lang.EqualityException if the assertion fails
   * and the two {@link Object objects} are not equal.
   * @see #equals(Object, Object, RuntimeException)
   * @see java.lang.Object#equals(Object)
   */
  public static void equals(Object obj1, Object obj2, String message, Object... messagePlaceholderValues) {
    equals(obj1, obj2, new EqualityException(format(message, messagePlaceholderValues)));
  }

  /**
   * Asserts that two {@link Object objects} are equal as determined by {@link Object#equals(Object)}.
   * <p>
   * The assertion holds if and only if (iff) both {@link Object objects} are {@literal non-null}
   * and {@link Object#equals(Object) equal} in value.
   *
   * @param obj1 {@link Object left operand} in the equality comparison.
   * @param obj2 {@link Object right operand} in the equality comparison.
   * @param message {@link Supplier} containing the message used in the {@link EqualityException}
   * thrown if the assertion fails.
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
   * <p>
   * The assertion holds if and only if (iff) both {@link Object objects} are {@literal non-null}
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
   * Null-safe method used to evaluate the given {@link Object objects} for equality.
   *
   * @param obj1 {@link Object} to evaluate for equality with {@link Object object 2}.
   * @param obj2 {@link Object} to evaluate for equality with {@link Object object 1}.
   * @return a boolean value indicating whether the given {@link Object objects} are equal.
   * @see java.lang.Object#equals(Object)
   */
  @NullSafe
  private static boolean areNotEqual(@Nullable Object obj1, @Nullable Object obj2) {
    return obj1 == null || !obj1.equals(obj2);
  }

  /**
   * Assert that the given {@link String} is not {@literal blank}, {@literal empty} or {@literal null}.
   * <p>
   * The assertion holds if and only if (iff) the {@link String} is not {@literal null},
   * not {@link String#isEmpty() empty} and does not contain any
   * {@link Character#isWhitespace(char) whitespace characters}.
   *
   * @param value {@link String} being evaluated.
   * @throws java.lang.IllegalArgumentException if the {@link String} is {@literal blank}, {@literal empty}
   * or {@literal null}.
   * @see #hasText(String, Supplier)
   * @see java.lang.String
   */
  public static void hasText(String value) {
    hasText(value, () -> format("Argument [%s] is blank", value));
  }

  /**
   * Assert that the given {@link String} is not {@literal blank}, {@literal empty} or {@literal null}.
   * <p>
   * The assertion holds if and only if (iff) the {@link String} is not {@literal null},
   * not {@link String#isEmpty() empty} and does not contain any
   * {@link Character#isWhitespace(char) whitespace characters}.
   *
   * @param value {@link String} being evaluated.
   * @param message {@link String} containing the description used in the {@link IllegalArgumentException}
   * thrown if the assertion fails.
   * @param messagePlaceholderValues array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalArgumentException if the {@link String} is {@literal blank}, {@literal empty}
   * or {@literal null}.
   * @see #hasText(String, RuntimeException)
   * @see java.lang.String
   */
  public static void hasText(String value, String message, Object... messagePlaceholderValues) {
    hasText(value, new IllegalArgumentException(format(message, messagePlaceholderValues)));
  }

  /**
   * Assert that the given {@link String} is not {@literal blank}, {@literal empty} or {@literal null}.
   * <p>
   * The assertion holds if and only if (iff) the {@link String} is not {@literal null},
   * not {@link String#isEmpty() empty} and does not contain any
   * {@link Character#isWhitespace(char) whitespace characters}.
   *
   * @param value {@link String} being evaluated.
   * @param message {@link Supplier} containing the message used in the {@link IllegalArgumentException}
   * thrown if the assertion fails.
   * @throws java.lang.IllegalArgumentException if the {@link String} is {@literal blank}, {@literal empty}
   * or {@literal null}.
   * @see java.util.function.Supplier
   * @see java.lang.String
   */
  public static void hasText(String value, Supplier<String> message) {
    if (isBlank(value)) {
      throw new IllegalArgumentException(message.get());
    }
  }

  /**
   * Assert that the given {@link String} is not {@literal blank}, {@literal empty} or {@literal null}.
   * <p>
   * The assertion holds if and only if (iff) the {@link String} is not {@literal null},
   * not {@link String#isEmpty() empty} and does not contain any
   * {@link Character#isWhitespace(char) whitespace characters}.
   *
   * @param value {@link String} being evaluated.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the {@link String} is {@literal blank}, {@literal empty} or {@literal null}.
   * @see java.lang.String
   */
  public static void hasText(String value, RuntimeException cause) {
    if (isBlank(value)) {
      throw cause;
    }
  }

  /**
   * Null-safe method used to determine whether the given {@link String} is {@literal blank}, {@literal empty}
   * or {@literal null}.
   *
   * @param value {@link String} to evaluate.
   * @return a boolean value indicating whether the given {@link String} is {@literal blank}, {@literal empty}
   * or {@literal null}.
   * @see java.lang.String#isEmpty()
   */
  @NullSafe
  private static boolean isBlank(@Nullable String value) {
    return value == null || value.trim().isEmpty();
  }

  /**
   * Asserts that the {@link Thread#currentThread() current Thread} holds the specified {@link Object lock}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Object lock} is not {@literal null}
   * and the {@link Thread#currentThread() current Thread} holds the {@link Object lock}.
   *
   * @param lock {@link Object} used as the lock, object monitor or mutex in the synchronization.
   * @throws java.lang.IllegalMonitorStateException if the {@link Thread#currentThread() current Thread}
   * does not hold the {@link Object lock} or the {@link Object lock} is {@literal null}.
   * @see java.lang.Thread#holdsLock(Object)
   * @see #holdsLock(Object, Supplier)
   */
  public static void holdsLock(Object lock) {
    holdsLock(lock, () -> format("The current thread [%1$s] does not hold lock [%2$s]",
      Thread.currentThread().getName(), lock));
  }

  /**
   * Asserts that the {@link Thread#currentThread() current Thread} holds the specified {@link Object lock}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Object lock} is not {@literal null}
   * and the {@link Thread#currentThread() current Thread} holds the {@link Object lock}.
   *
   * @param lock {@link Object} used as the lock, object monitor or mutex in the synchronization.
   * @param message {@link String} containing the description used in the {@link IllegalMonitorStateException}
   * thrown if the assertion fails.
   * @param messagePlaceholderValues array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalMonitorStateException if the {@link Thread#currentThread() current Thread}
   * does not hold the {@link Object lock} or the {@link Object lock} is {@literal null}.
   * @see #holdsLock(Object, RuntimeException)
   * @see java.lang.Thread#holdsLock(Object)
   */
  public static void holdsLock(Object lock, String message, Object... messagePlaceholderValues) {
    holdsLock(lock, new IllegalMonitorStateException(format(message, messagePlaceholderValues)));
  }

  /**
   * Asserts that the {@link Thread#currentThread() current Thread} holds the specified {@link Object lock}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Object lock} is not {@literal null}
   * and the {@link Thread#currentThread() current Thread} holds the {@link Object lock}.
   *
   * @param lock {@link Object} used as the lock, object monitor or mutex in the synchronization.
   * @param message {@link Supplier} containing the message used in the {@link IllegalMonitorStateException}
   * thrown if the assertion fails.
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
   * <p>
   * The assertion holds if and only if (iff) the {@link Object lock} is not {@literal null}
   * and the {@link Thread#currentThread() current Thread} holds the {@link Object lock}.
   *
   * @param lock {@link Object} used as the lock, object monitor or mutex in the synchronization.
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
   * Null-safe method used to determine whether the {@link Thread#currentThread() current Thread}
   * is the holder of the given {@link Object lock}.
   *
   * @param lock {@link Object} to evaluate as the lock.
   * @return a boolean value indicating whether the {@link Thread#currentThread() current Thread}
   * holds the given {@link Object lock}.
   * @see java.lang.Thread#holdsLock(Object)
   */
  @NullSafe
  private static boolean isNotLockHolder(@Nullable Object lock) {
    return lock == null || !Thread.holdsLock(lock);
  }

  /**
   * Asserts that the {@link Class from class type} is assignable to the {@link Class to class type}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Class from class type} is the same as
   * or a subclass of the {@link Class to class type}.
   *
   * @param from {@link Class class type} being evaluated for assignment compatibility with
   * the {@link Class to class type}.
   * @param to {@link Class class type} used to determine if the {@link Class from class type}
   * is assignment compatible.
   * @throws java.lang.ClassCastException if the {@link Class from class type} is not assignment compatible with
   * the {@link Class to class type}.
   * @see #isAssignableTo(Class, Class, Supplier)
   * @see java.lang.Class#isAssignableFrom(Class)
   */
  public static void isAssignableTo(Class<?> from, Class<?> to) {
    isAssignableTo(from, to, () -> format("[%1$s] is not assignable to [%2$s]", from, to));
  }

  /**
   * Asserts that the {@link Class from class type} is assignable to the {@link Class to class type}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Class from class type} is the same as
   * or a subclass of the {@link Class to class type}.
   *
   * @param from {@link Class class type} being evaluated for assignment compatibility with
   * the {@link Class to class type}.
   * @param to {@link Class class type} used to determine if the {@link Class from class type}
   * is assignment compatible.
   * @param message {@link String} containing the description used in the {@link ClassCastException}
   * thrown if the assertion fails.
   * @param messagePlaceholderValues array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.ClassCastException if the {@link Class from class type} is not assignment compatible with
   * the {@link Class to class type}.
   * @see #isAssignableTo(Class, Class, RuntimeException)
   * @see java.lang.Class#isAssignableFrom(Class)
   */
  public static void isAssignableTo(Class<?> from, Class<?> to, String message, Object... messagePlaceholderValues) {
    isAssignableTo(from, to, new ClassCastException(format(message, messagePlaceholderValues)));
  }

  /**
   * Asserts that the {@link Class from class type} is assignable to the {@link Class to class type}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Class from class type} is the same as
   * or a subclass of the {@link Class to class type}.
   *
   * @param from {@link Class class type} being evaluated for assignment compatibility with
   * the {@link Class to class type}.
   * @param to {@link Class class type} used to determine if the {@link Class from class type}
   * is assignment compatible.
   * @param message {@link Supplier} containing the message used in the {@link ClassCastException}
   * thrown if the assertion fails.
   * @throws java.lang.ClassCastException if the {@link Class from class type} is not assignment compatible with
   * the {@link Class to class type}.
   * @see java.lang.Class#isAssignableFrom(Class)
   * @see java.util.function.Supplier
   */
  public static void isAssignableTo(Class<?> from, Class<?> to, Supplier<String> message) {
    if (isNotAssignableTo(from, to)) {
      throw new ClassCastException(message.get());
    }
  }

  /**
   * Asserts that the {@link Class from class type} is assignable to the {@link Class to class type}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Class from class type} is the same as
   * or a subclass of the {@link Class to class type}.
   *
   * @param from {@link Class class type} being evaluated for assignment compatibility with
   * the {@link Class to class type}.
   * @param to {@link Class class type} used to determine if the {@link Class from class type}
   * is assignment compatible.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the {@link Class from class type} is not assignment compatible with
   * the {@link Class to class type}.
   * @see java.lang.Class#isAssignableFrom(Class)
   */
  public static void isAssignableTo(Class<?> from, Class<?> to, RuntimeException cause) {
    if (isNotAssignableTo(from, to)) {
      throw cause;
    }
  }

  /**
   * Null-safe method used to determine whether the {@link Class from class type} is assignable to
   * the {@link Class to class type}.
   * <p>
   * {@literal null}, or the {@link Class from class type} when {@literal null}, can be assigned to
   * the {@link Class to class type}. However, you cannot assign something to {@literal null},
   * therefore the {@link Class to class type} can never be {@literal null}.
   *
   * @param from {@link Class class type} being evaluated for assignment compatibility with
   * the {@link Class to class type}.
   * @param to {@link Class class type} used to determine if the {@link Class from class type}
   * is assignment compatible.
   * @return a boolean value indicating whether the {@link Class from class type} is assignable to
   * the {@link Class to class type}.
   * @see java.lang.Class#isAssignableFrom(Class)
   */
  @NullSafe
  private static boolean isNotAssignableTo(@Nullable Class<?> from, @Nullable Class<?> to) {
    return to == null || (from != null && !to.isAssignableFrom(from));
  }

  /**
   * Asserts that the condition is {@literal false}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Boolean condition} is equal to {@literal false}.
   *
   * @param condition {@link Boolean value} being evaluated.
   * @throws java.lang.IllegalArgumentException if the {@link Boolean value} is not {@literal false}.
   * @see #isFalse(Boolean, Supplier)
   * @see java.lang.Boolean#FALSE
   */
  public static void isFalse(Boolean condition) {
    isFalse(condition, () -> format("Condition [%s] is not false", condition));
  }

  /**
   * Asserts that the condition is {@literal false}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Boolean condition} is equal to {@literal false}.
   *
   * @param condition {@link Boolean value} being evaluated.
   * @param message {@link String} containing the description used in the {@link IllegalArgumentException}
   * thrown if the assertion fails.
   * @param messagePlaceholderValues array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalArgumentException if the {@link Boolean value} is not {@literal false}.
   * @see #isFalse(Boolean, RuntimeException)
   * @see java.lang.Boolean#FALSE
   */
  public static void isFalse(Boolean condition, String message, Object... messagePlaceholderValues) {
    isFalse(condition, new IllegalArgumentException(format(message, messagePlaceholderValues)));
  }

  /**
   * Asserts that the condition is {@literal false}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Boolean condition} is equal to {@literal false}.
   *
   * @param condition {@link Boolean value} being evaluated.
   * @param message {@link Supplier} containing the message used in the {@link IllegalArgumentException}
   * thrown if the assertion fails.
   * @throws java.lang.IllegalArgumentException if the {@link Boolean value} is not {@literal false}.
   * @see java.util.function.Supplier
   * @see java.lang.Boolean#FALSE
   */
  public static void isFalse(Boolean condition, Supplier<String> message) {
    if (isNotFalse(condition)) {
      throw new IllegalArgumentException(message.get());
    }
  }

  /**
   * Asserts that the condition is {@literal false}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Boolean condition} is equal to {@literal false}.
   *
   * @param condition {@link Boolean value} being evaluated.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the {@link Boolean value} is not {@literal false}.
   * @see java.lang.Boolean#FALSE
   */
  public static void isFalse(Boolean condition, RuntimeException cause) {
    if (isNotFalse(condition)) {
      throw cause;
    }
  }

  /**
   * Null-safe method used to determine whether the given {@link Boolean condition} is {@literal false}.
   *
   * @param condition {@link Boolean value} to evaluate.
   * @return a boolean indicating whether the given {@link Boolean condition} is {@literal false}.
   * @see java.lang.Boolean#FALSE
   */
  @NullSafe
  private static boolean isNotFalse(@Nullable Boolean condition) {
    return !Boolean.FALSE.equals(condition);
  }

  /**
   * Asserts that the given {@link Object} is an instance of the specified {@link Class type}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Object} is not {@literal null}
   * and is an instance of the specified {@link Class type}.
   * <p>
   * This assertion functions exactly the same as the Java {@literal instanceof} operator.
   *
   * @param obj {@link Object} evaluated as an instance of {@link Class type}.
   * @param type {@link Class type} used to evaluate the {@link Object} in the {@literal instanceof} operator.
   * @throws org.cp.elements.lang.IllegalTypeException if the {@link Object} is not an instance of {@link Class type}.
   * @see #isInstanceOf(Object, Class, Supplier)
   * @see java.lang.Class#isInstance(Object)
   */
  public static void isInstanceOf(Object obj, Class<?> type) {
    isInstanceOf(obj, type, () -> format("[%1$s] is not an instance of [%2$s]", obj, type));
  }

  /**
   * Asserts that the given {@link Object} is an instance of the specified {@link Class type}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Object} is not {@literal null}
   * and is an instance of the specified {@link Class type}.
   * <p>
   * This assertion functions exactly the same as the Java {@literal instanceof} operator.
   *
   * @param obj {@link Object} evaluated as an instance of {@link Class type}.
   * @param type {@link Class type} used to evaluate the {@link Object} in the {@literal instanceof} operator.
   * @param message {@link String} containing the description used in the {@link IllegalTypeException}
   * thrown if the assertion fails.
   * @param messagePlaceholderValues array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws org.cp.elements.lang.IllegalTypeException if the {@link Object} is not an instance of {@link Class type}.
   * @see #isInstanceOf(Object, Class, RuntimeException)
   * @see java.lang.Class#isInstance(Object)
   */
  public static void isInstanceOf(Object obj, Class<?> type, String message, Object... messagePlaceholderValues) {
    isInstanceOf(obj, type, new IllegalTypeException(format(message, messagePlaceholderValues)));
  }

  /**
   * Asserts that the given {@link Object} is an instance of the specified {@link Class type}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Object} is not {@literal null}
   * and is an instance of the specified {@link Class type}.
   * <p>
   * This assertion functions exactly the same as the Java {@literal instanceof} operator.
   *
   * @param obj {@link Object} evaluated as an instance of {@link Class type}.
   * @param type {@link Class type} used to evaluate the {@link Object} in the {@literal instanceof} operator.
   * @param message {@link Supplier} containing the message used in the {@link IllegalTypeException}
   * thrown if the assertion fails.
   * @throws org.cp.elements.lang.IllegalTypeException if the {@link Object} is not an instance of {@link Class type}.
   * @see java.lang.Class#isInstance(Object)
   * @see java.util.function.Supplier
   */
  public static void isInstanceOf(Object obj, Class<?> type, Supplier<String> message) {
    if (isNotInstanceOf(obj, type)) {
      throw new IllegalTypeException(message.get());
    }
  }

  /**
   * Asserts that the given {@link Object} is an instance of the specified {@link Class type}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Object} is not {@literal null}
   * and is an instance of the specified {@link Class type}.
   * <p>
   * This assertion functions exactly the same as the Java {@literal instanceof} operator.
   *
   * @param obj {@link Object} evaluated as an instance of {@link Class type}.
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
   * Null-safe method used to determine whether the given {@link Object} is an instance of {@link Class type}.
   * <p>
   * Note that an {@link Object} cannot be an instance of {@literal null}.
   *
   * @param obj {@link Object} to evaluate.
   * @param type {@link Class type} used in the instance of operation.
   * @return a boolean value indicating whether the {@link Object} is an instance of {@link Class type}.
   * @see java.lang.Class#isInstance(Object)
   * @see java.lang.Object
   */
  @NullSafe
  private static boolean isNotInstanceOf(@Nullable Object obj, @Nullable Class<?> type) {
    return type == null || !type.isInstance(obj);
  }

  /**
   * Asserts that the {@link Boolean condition} is {@literal true}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Boolean condition} is equal to {@literal true}.
   *
   * @param condition {@link Boolean value} being evaluated.
   * @throws java.lang.IllegalArgumentException if the {@link Boolean condition} is not {@literal true}.
   * @see #isTrue(Boolean, Supplier)
   * @see java.lang.Boolean#TRUE
   */
  public static void isTrue(Boolean condition) {
    isTrue(condition, () -> format("Condition [%s] is not true", condition));
  }

  /**
   * Asserts that the {@link Boolean condition} is {@literal true}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Boolean condition} is equal to {@literal true}.
   *
   * @param condition {@link Boolean value} being evaluated.
   * @param message {@link String} containing the description used in the {@link IllegalArgumentException}
   * thrown if the assertion fails.
   * @param messagePlaceholderValues array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalArgumentException if the {@link Boolean condition} is not {@literal true}.
   * @see #isTrue(Boolean, RuntimeException)
   * @see java.lang.Boolean#TRUE
   */
  public static void isTrue(Boolean condition, String message, Object... messagePlaceholderValues) {
    isTrue(condition, new IllegalArgumentException(format(message, messagePlaceholderValues)));
  }

  /**
   * Asserts that the {@link Boolean condition} is {@literal true}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Boolean condition} is equal to {@literal true}.
   *
   * @param condition {@link Boolean value} being evaluated.
   * @param message {@link Supplier} containing the message used in the {@link IllegalArgumentException}
   * thrown if the assertion fails.
   * @throws java.lang.IllegalArgumentException if the {@link Boolean condition} is not {@literal true}.
   * @see java.util.function.Supplier
   * @see java.lang.Boolean#TRUE
   */
  public static void isTrue(Boolean condition, Supplier<String> message) {
    if (isNotTrue(condition)) {
      throw new IllegalArgumentException(message.get());
    }
  }

  /**
   * Asserts that the {@link Boolean condition} is {@literal true}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Boolean condition} is equal to {@literal true}.
   *
   * @param condition {@link Boolean value} being evaluated.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the {@link Boolean condition} is not {@literal true}.
   * @see java.lang.Boolean#TRUE
   */
  public static void isTrue(Boolean condition, RuntimeException cause) {
    if (isNotTrue(condition)) {
      throw cause;
    }
  }

  /**
   * Null-safe method used to determine whether the {@link Boolean condition} is {@literal true}.
   *
   * @param condition {@link Boolean value} to evaluate.
   * @return a boolean value indicating whether the {@link Boolean condition} is {@literal true}.
   * @see java.lang.Boolean#TRUE
   */
  @NullSafe
  private static boolean isNotTrue(@Nullable Boolean condition) {
    return !Boolean.TRUE.equals(condition);
  }

  /**
   * Asserts that the given {@link String} is not {@link String#isEmpty() empty}.
   * <p>
   * The assertion holds if and only if (iff) the {@link String String}
   * is not the {@link String#isEmpty() empty String}.
   *
   * @param value {@link String} to evaluate.
   * @throws java.lang.IllegalArgumentException if the {@link String} is {@link String#isEmpty() empty}.
   * @see #notEmpty(String, String, Object...)
   * @see java.lang.String
   */
  public static void notEmpty(String value) {
    notEmpty(value, "String value is empty");
  }

  /**
   * Asserts that the given {@link String} is not {@link String#isEmpty() empty}.
   * <p>
   * The assertion holds if and only if (iff) the {@link String String}
   * is not the {@link String#isEmpty() empty String}.
   *
   * @param value {@link String} to evaluate.
   * @param message {@link String} containing the description used in the {@link IllegalArgumentException}
   * thrown if the assertion fails.
   * @param messagePlaceholderValues array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalArgumentException if the {@link String} is {@link String#isEmpty() empty}.
   * @see #notEmpty(String, RuntimeException)
   * @see java.lang.String
   */
  public static void notEmpty(String value, String message, Object... messagePlaceholderValues) {
    notEmpty(value, new IllegalArgumentException(format(message, messagePlaceholderValues)));
  }

  /**
   * Asserts that the given {@link String} is not {@link String#isEmpty() empty}.
   * <p>
   * The assertion holds if and only if (iff) the {@link String String}
   * is not the {@link String#isEmpty() empty String}.
   *
   * @param value {@link String} to evaluate.
   * @param message {@link Supplier} containing the message used in the {@link IllegalArgumentException}
   * thrown if the assertion fails.
   * @throws java.lang.IllegalArgumentException if the {@link String} is {@link String#isEmpty() empty}.
   * @see java.util.function.Supplier
   * @see java.lang.String
   */
  public static void notEmpty(String value, Supplier<String> message) {
    if (isEmpty(value)) {
      throw new IllegalArgumentException(message.get());
    }
  }

  /**
   * Asserts that the given {@link String} is not {@link String#isEmpty() empty}.
   * <p>
   * The assertion holds if and only if (iff) the {@link String String}
   * is not the {@link String#isEmpty() empty String}.
   *
   * @param value {@link String} to evaluate.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the {@link String} is {@link String#isEmpty() empty}.
   * @see java.lang.String
   */
  public static void notEmpty(String value, RuntimeException cause) {
    if (isEmpty(value)) {
      throw cause;
    }
  }

  /**
   * Null-safe method used to determine whether the given {@link String} is {@link String#isEmpty() empty}.
   *
   * @param value {@link String} to evaluate.
   * @return a boolean value indicating whether the {@link String} is {@link String#isEmpty() empty}.
   * @see java.lang.String#isEmpty()
   */
  @NullSafe
  private static boolean isEmpty(@Nullable String value) {
    return value == null || value.isEmpty();
  }

  /**
   * Asserts that the {@link Object array} is not {@literal empty}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Object array} is not {@literal null}
   * and contains at least 1 element. Elements of the array are not evaluated.
   *
   * @param array {@link Object array} to evaluate.
   * @throws java.lang.IllegalArgumentException if the {@link Object array} is {@literal null} or {@literal empty}.
   * @see #notEmpty(Object[], String, Object...)
   */
  public static void notEmpty(Object[] array) {
    notEmpty(array, "Array is empty");
  }

  /**
   * Asserts that the {@link Object array} is not {@literal empty}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Object array} is not {@literal null}
   * and contains at least 1 element. Elements of the array are not evaluated.
   *
   * @param array {@link Object array} to evaluate.
   * @param message {@link String} containing the description used in the {@link IllegalArgumentException}
   * thrown if the assertion fails.
   * @param messagePlaceholderValues array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalArgumentException if the {@link Object array} is {@literal null} or {@literal empty}.
   * @see #notEmpty(Object[], RuntimeException)
   */
  public static void notEmpty(Object[] array, String message, Object... messagePlaceholderValues) {
    notEmpty(array, new IllegalArgumentException(format(message, messagePlaceholderValues)));
  }

  /**
   * Asserts that the {@link Object array} is not {@literal empty}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Object array} is not {@literal null}
   * and contains at least 1 element. Elements of the array are not evaluated.
   *
   * @param array {@link Object array} to evaluate.
   * @param message {@link Supplier} containing the message used in the {@link IllegalArgumentException}
   * thrown if the assertion fails.
   * @throws java.lang.IllegalArgumentException if the {@link Object array} is {@literal null} or {@literal empty}.
   * @see java.util.function.Supplier
   */
  public static void notEmpty(Object[] array, Supplier<String> message) {
    if (isEmpty(array)) {
      throw new IllegalArgumentException(message.get());
    }
  }

  /**
   * Asserts that the {@link Object array} is not {@literal empty}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Object array} is not {@literal null}
   * and contains at least 1 element. Elements of the array are not evaluated.
   *
   * @param array {@link Object array} to evaluate.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the {@link Object array} is {@literal null} or {@literal empty}.
   */
  public static void notEmpty(Object[] array, RuntimeException cause) {
    if (isEmpty(array)) {
      throw cause;
    }
  }

  /**
   * Null-safe method used to determine whether the given {@link Object array} is {@literal empty}.
   *
   * @param array {@link Object array} to evaluate.
   * @return a boolean value indicating whether the given {@link Object array} is {@literal empty}.
   */
  @NullSafe
  private static boolean isEmpty(@Nullable Object[] array) {
    return array == null || array.length == 0;
  }

  /**
   * Asserts that the {@link Collection} is not {@link Collection#isEmpty() empty}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Collection} is not {@literal null}
   * and contains at least 1 element. Elements of the {@link Collection} are not evaluated.
   *
   * @param collection {@link Collection} to evaluate.
   * @throws java.lang.IllegalArgumentException if the {@link Collection} is {@literal null}
   * or {@link Collection#isEmpty() empty}.
   * @see #notEmpty(Collection, String, Object...)
   * @see java.util.Collection#isEmpty()
   */
  public static void notEmpty(Collection<?> collection) {
    notEmpty(collection, "Collection is empty");
  }

  /**
   * Asserts that the {@link Collection} is not {@link Collection#isEmpty() empty}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Collection} is not {@literal null}
   * and contains at least 1 element. Elements of the {@link Collection} are not evaluated.
   *
   * @param collection {@link Collection} to evaluate.
   * @param message {@link String} containing the description used in the {@link IllegalArgumentException}
   * thrown if the assertion fails.
   * @param messagePlaceholderValues array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalArgumentException if the {@link Collection} is {@literal null}
   * or {@link Collection#isEmpty() empty}.
   * @see #notEmpty(java.util.Collection, RuntimeException)
   * @see java.util.Collection#isEmpty()
   */
  public static void notEmpty(Collection<?> collection, String message, Object... messagePlaceholderValues) {
    notEmpty(collection, new IllegalArgumentException(format(message, messagePlaceholderValues)));
  }

  /**
   * Asserts that the {@link Collection} is not {@link Collection#isEmpty() empty}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Collection} is not {@literal null}
   * and contains at least 1 element. Elements of the {@link Collection} are not evaluated.
   *
   * @param collection {@link Collection} to evaluate.
   * @param message {@link Supplier} containing the message used in the {@link IllegalArgumentException}
   * thrown if the assertion fails.
   * @throws java.lang.IllegalArgumentException if the {@link Collection} is {@literal null}
   * or {@link Collection#isEmpty() empty}.
   * @see java.util.Collection#isEmpty()
   * @see java.util.function.Supplier
   */
  public static void notEmpty(Collection<?> collection, Supplier<String> message) {
    if (isEmpty(collection)) {
      throw new IllegalArgumentException(message.get());
    }
  }

  /**
   * Asserts that the {@link Collection} is not {@link Collection#isEmpty() empty}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Collection} is not {@literal null}
   * and contains at least 1 element. Elements of the {@link Collection} are not evaluated.
   *
   * @param collection {@link Collection} to evaluate.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the {@link Collection} is {@literal null}
   * or {@link Collection#isEmpty() empty}.
   * @see java.util.Collection#isEmpty()
   */
  public static void notEmpty(Collection<?> collection, RuntimeException cause) {
    if (isEmpty(collection)) {
      throw cause;
    }
  }

  /**
   * Null-safe method used to determine whether the given {@link Collection} is {@literal null}
   * or {@link Collection#isEmpty() empty}.
   *
   * @param collection {@link Collection} to evaluate.
   * @return a boolean value indicating whether the {@link Collection} is {@literal null}
   * or {@link Collection#isEmpty() empty}.
   * @see java.util.Collection#isEmpty()
   */
  @NullSafe
  private static boolean isEmpty(@Nullable Collection<?> collection) {
    return collection == null || collection.isEmpty();
  }

  /**
   * Asserts that the {@link Iterable} is not empty.
   * <p>
   * The assertion holds if and only if (iff) the {@link Iterable} is not {@literal null}
   * and contains at least 1 element. Elements of the {@link Iterable} are not evaluated.
   *
   * @param iterable {@link Iterable} to evaluate.
   * @throws java.lang.IllegalArgumentException if the {@link Iterable} is {@literal null} or empty.
   * @see #notEmpty(Iterable, String, Object...)
   * @see java.lang.Iterable
   */
  public static void notEmpty(Iterable<?> iterable) {
    notEmpty(iterable, "Iterable is empty");
  }

  /**
   * Asserts that the {@link Iterable} is not empty.
   * <p>
   * The assertion holds if and only if (iff) the {@link Iterable} is not {@literal null}
   * and contains at least 1 element. Elements of the {@link Iterable} are not evaluated.
   *
   * @param iterable {@link Iterable} to evaluate.
   * @param message {@link String} containing the description used in the {@link IllegalArgumentException}
   * thrown if the assertion fails.
   * @param messagePlaceholderValues array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalArgumentException if the {@link Iterable} is {@literal null} or empty.
   * @see #notEmpty(Iterable, RuntimeException)
   * @see java.lang.Iterable
   */
  public static void notEmpty(Iterable<?> iterable, String message, Object... messagePlaceholderValues) {
    notEmpty(iterable, new IllegalArgumentException(format(message, messagePlaceholderValues)));
  }

  /**
   * Asserts that the {@link Iterable} is not empty.
   * <p>
   * The assertion holds if and only if (iff) the {@link Iterable} is not {@literal null}
   * and contains at least 1 element. Elements of the {@link Iterable} are not evaluated.
   *
   * @param iterable {@link Iterable} to evaluate.
   * @param message {@link Supplier} containing the message used in the {@link IllegalArgumentException}
   * thrown if the assertion fails.
   * @throws java.lang.IllegalArgumentException if the {@link Iterable} is {@literal null} or empty.
   * @see java.util.function.Supplier
   * @see java.lang.Iterable
   */
  public static void notEmpty(Iterable<?> iterable, Supplier<String> message) {
    if (isEmpty(iterable)) {
      throw new IllegalArgumentException(message.get());
    }
  }

  /**
   * Asserts that the {@link Iterable} is not empty.
   * <p>
   * The assertion holds if and only if (iff) the {@link Iterable} is not {@literal null}
   * and contains at least 1 element. Elements of the {@link Iterable} are not evaluated.
   *
   * @param iterable {@link Iterable} to evaluate.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the {@link Iterable} is {@literal null} or empty.
   * @see java.lang.Iterable
   */
  public static void notEmpty(Iterable<?> iterable, RuntimeException cause) {
    if (isEmpty(iterable)) {
      throw cause;
    }
  }

  /**
   * Null-safe method used to determine if the {@link Iterable} is {@literal null} or {@literal empty}.
   *
   * @param iterable {@link Iterable} to evaluate.
   * @return a boolean value indicating whether the {@link Iterable} is {@literal null} or {@literal empty}.
   * @see #isEmpty(Iterator)
   * @see java.lang.Iterable
   */
  @NullSafe
  private static boolean isEmpty(@Nullable Iterable<?> iterable) {
    return iterable == null || isEmpty(iterable.iterator());
  }

  /**
   * Null-safe method used to determine if the {@link Iterator} is {@literal null} or {@literal empty}.
   *
   * @param iterator {@link Iterator} to evaluate.
   * @return a boolean value indicating whether the {@link Iterator} is {@literal null} or {@literal empty}.
   * @see java.util.Iterator
   */
  @NullSafe
  private static boolean isEmpty(@Nullable Iterator<?> iterator) {
    return iterator == null || !iterator.hasNext();
  }

  /**
   * Asserts that the {@link Map} is not {@link Map#isEmpty() empty}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Map} is not {@literal null}
   * and contains at least 1 key/value mapping. Keys and values of the {@link Map} are not evaluated.
   *
   * @param map {@link Map} to evaluate.
   * @throws java.lang.IllegalArgumentException if the {@link Map} is {@literal null} or {@link Map#isEmpty() empty}.
   * @see #notEmpty(Map, String, Object...)
   * @see java.util.Map#isEmpty()
   */
  public static void notEmpty(Map<?, ?> map) {
    notEmpty(map, "Map is empty");
  }

  /**
   * Asserts that the {@link Map} is not {@link Map#isEmpty() empty}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Map} is not {@literal null}
   * and contains at least 1 key/value mapping. Keys and values of the {@link Map} are not evaluated.
   *
   * @param map {@link Map} to evaluate.
   * @param message {@link String} containing the description used in the {@link IllegalArgumentException}
   * thrown if the assertion fails.
   * @param messagePlaceholderValues array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalArgumentException if the {@link Map} is {@literal null} or {@link Map#isEmpty() empty}.
   * @see #notEmpty(java.util.Map, RuntimeException)
   * @see java.util.Map#isEmpty()
   */
  public static void notEmpty(Map<?, ?> map, String message, Object... messagePlaceholderValues) {
    notEmpty(map, new IllegalArgumentException(format(message, messagePlaceholderValues)));
  }

  /**
   * Asserts that the {@link Map} is not {@link Map#isEmpty() empty}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Map} is not {@literal null}
   * and contains at least 1 key/value mapping. Keys and values of the {@link Map} are not evaluated.
   *
   * @param map {@link Map} to evaluate.
   * @param message {@link Supplier} containing the message used in the {@link IllegalArgumentException}
   * thrown if the assertion fails.
   * @throws java.lang.IllegalArgumentException if the {@link Map} is {@literal null} or {@link Map#isEmpty() empty}.
   * @see java.util.function.Supplier
   * @see java.util.Map#isEmpty()
   */
  public static void notEmpty(Map<?, ?> map, Supplier<String> message) {
    if (isEmpty(map)) {
      throw new IllegalArgumentException(message.get());
    }
  }

  /**
   * Asserts that the {@link Map} is not {@link Map#isEmpty() empty}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Map} is not {@literal null}
   * and contains at least 1 key/value mapping. Keys and values of the {@link Map} are not evaluated.
   *
   * @param map {@link Map} to evaluate.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the {@link Map} is {@literal null} or {@link Map#isEmpty() empty}.
   * @see java.util.Map#isEmpty()
   */
  public static void notEmpty(Map<?, ?> map, RuntimeException cause) {
    if (isEmpty(map)) {
      throw cause;
    }
  }

  /**
   * Null-safe method used to determine whether the given {@link Map} is {@literal null} or {@link Map#isEmpty() empty}.
   *
   * @param map {@link Map} to evaluate.
   * @return a boolean value indicating whether the {@link Map} is {@literal null} or {@link Map#isEmpty() empty}.
   * @see java.util.Map#isEmpty()
   */
  @NullSafe
  private static boolean isEmpty(@Nullable Map<?, ?> map) {
    return map == null || map.isEmpty();
  }

  /**
   * Assert that the {@link Thread#currentThread() current Thread} has not been interrupted.
   *
   * @throws InterruptedException if the {@link Thread#currentThread() current Thread} was interrupted.
   * @see java.lang.Thread#currentThread()
   * @see java.lang.Thread#isInterrupted()
   * @see #notInterrupted(Supplier)
   */
  public static void notInterrupted() throws InterruptedException {
    notInterrupted(()-> format("Thread [%s] was interrupted", Thread.currentThread().getName()));
  }

  /**
   * Assert that the {@link Thread#currentThread() current Thread} has not been interrupted.
   *
   * @param message {@link String} containing the description used in the {@link InterruptedException}
   * thrown if the assertion fails.
   * @param messagePlaceholderValues array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws InterruptedException if the {@link Thread#currentThread() current Thread} was interrupted.
   * @see java.lang.Thread#currentThread()
   * @see java.lang.Thread#isInterrupted()
   * @see #notInterrupted(Supplier)
   */
  public static void notInterrupted(String message, Object... messagePlaceholderValues) throws InterruptedException {
    notInterrupted(() -> format(message, messagePlaceholderValues));
  }

  /**
   * Assert that the {@link Thread#currentThread() current Thread} has not been interrupted.
   *
   * @param message {@link Supplier} containing the message used in the {@link InterruptedException}
   * thrown if the assertion fails.
   * @throws InterruptedException if the {@link Thread#currentThread() current Thread} was interrupted.
   * @see java.lang.Thread#currentThread()
   * @see java.lang.Thread#isInterrupted()
   * @see #notInterrupted(Supplier)
   */
  public static void notInterrupted(Supplier<String> message) throws InterruptedException {

    if (Thread.currentThread().isInterrupted()) {
      throw new InterruptedException(message.get());
    }
  }

  /**
   * Assert that the {@link Thread#currentThread() current Thread} has not been interrupted.
   *
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws RuntimeException if the {@link Thread#currentThread() current Thread} was interrupted.
   * @see java.lang.Thread#currentThread()
   * @see java.lang.Thread#isInterrupted()
   * @see #notInterrupted(Supplier)
   */
  public static void notInterrupted(RuntimeException cause) {

    if (Thread.currentThread().isInterrupted()) {
      throw cause;
    }
  }

  /**
   * Asserts that the {@link Object} reference is not {@literal null}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Object} reference is not {@literal null}.
   *
   * @param obj {@link Object} reference to evaluate.
   * @throws java.lang.IllegalArgumentException if the {@link Object} reference is {@literal null}.
   * @see #notNull(Object, String, Object...)
   * @see java.lang.Object
   */
  public static void notNull(Object obj) {
    notNull(obj, "Argument is null");
  }

  /**
   * Asserts that the {@link Object} reference is not {@literal null}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Object} reference is not {@literal null}.
   *
   * @param obj {@link Object} reference to evaluate.
   * @param message {@link String} containing the description used in the {@link IllegalArgumentException}
   * thrown if the assertion fails.
   * @param messagePlaceholderValues array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalArgumentException if the {@link Object} reference is {@literal null}.
   * @see #notNull(Object, RuntimeException)
   * @see java.lang.Object
   */
  public static void notNull(Object obj, String message, Object... messagePlaceholderValues) {
    notNull(obj, new IllegalArgumentException(format(message, messagePlaceholderValues)));
  }

  /**
   * Asserts that the {@link Object} reference is not {@literal null}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Object} reference is not {@literal null}.
   *
   * @param obj {@link Object} reference to evaluate.
   * @param message {@link Supplier} containing the message used in the {@link NullPointerException}
   * thrown if the assertion fails.
   * @throws java.lang.IllegalArgumentException if the {@link Object} reference is {@literal null}.
   * @see java.util.function.Supplier
   * @see java.lang.Object
   */
  public static void notNull(Object obj, Supplier<String> message) {
    if (isNull(obj)) {
      throw new IllegalArgumentException(message.get());
    }
  }

  /**
   * Asserts that the {@link Object} reference is not {@literal null}.
   * <p>
   * The assertion holds if and only if (iff) the {@link Object} reference is not {@literal null}.
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
   * Null-safe method used to determine whether the {@link Object} reference is {@literal null}.
   *
   * @param obj {@link Object} reference to evaluate.
   * @return a boolean value indicating whether the {@link Object} reference is {@literal null}.
   * @see java.lang.Object
   */
  @NullSafe
  private static boolean isNull(@Nullable Object obj) {
    return obj == null;
  }

  /**
   * Assert that two {@link Object Objects} are not the same {@link Object} as determined by the identity comparison.
   * <p>
   * The assertion holds if and only if (iff) the two {@link Object objects} are not the same {@link Object} in memory.
   *
   * @param obj1 {@link Object left operand} in the identity comparison.
   * @param obj2 {@link Object right operand} in the identity comparison.
   * @throws org.cp.elements.lang.IdentityException if the two {@link Object objects} are the same.
   * @see #notSame(Object, Object, String, Object...)
   * @see java.lang.Object
   */
  public static void notSame(Object obj1, Object obj2) {
    notSame(obj1, obj2, "[%1$s] is the same as [%2$s]", obj1, obj2);
  }

  /**
   * Assert that two {@link Object Objects} are not the same {@link Object} as determined by the identity comparison.
   * <p>
   * The assertion holds if and only if (iff) the two {@link Object objects} are not the same {@link Object} in memory.
   *
   * @param obj1 {@link Object left operand} in the identity comparison.
   * @param obj2 {@link Object right operand} in the identity comparison.
   * @param message {@link String} containing the description used in the {@link IdentityException}
   * thrown if the assertion fails.
   * @param messagePlaceholderValues array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws org.cp.elements.lang.IdentityException if the two {@link Object objects} are the same.
   * @see #notSame(Object, Object, RuntimeException)
   * @see java.lang.Object
   */
  public static void notSame(Object obj1, Object obj2, String message, Object... messagePlaceholderValues) {
    notSame(obj1, obj2, new IdentityException(format(message, messagePlaceholderValues)));
  }

  /**
   * Assert that two {@link Object Objects} are not the same {@link Object} as determined by the identity comparison.
   * <p>
   * The assertion holds if and only if (iff) the two {@link Object objects} are not the same {@link Object} in memory.
   *
   * @param obj1 {@link Object left operand} in the identity comparison.
   * @param obj2 {@link Object right operand} in the identity comparison.
   * @param message {@link Supplier} containing the message used in the {@link IdentityException}
   * thrown if the assertion fails.
   * @throws org.cp.elements.lang.IdentityException if the two {@link Object objects} are the same.
   * @see #notSame(Object, Object, String, Object...)
   * @see java.lang.Object
   */
  public static void notSame(Object obj1, Object obj2, Supplier<String> message) {
    if (isSame(obj1, obj2)) {
      throw new IdentityException(message.get());
    }
  }

  /**
   * Assert that two {@link Object Objects} are not the same {@link Object} as determined by the identity comparison.
   * <p>
   * The assertion holds if and only if (iff) the two {@link Object objects} are not the same {@link Object} in memory.
   *
   * @param obj1 {@link Object left operand} in the identity comparison.
   * @param obj2 {@link Object right operand} in the identity comparison.
   * @param cause {@link RuntimeException} thrown if the two {@link Object objects} are the same.
   * @throws RuntimeException the two {@link Object objects} are the same.
   * @see java.lang.Object
   */
  public static void notSame(Object obj1, Object obj2, RuntimeException cause) {
    if (isSame(obj1, obj2)) {
      throw cause;
    }
  }

  /**
   * Null-safe method used to determine whether two {@link Object Objects} are the same {@link Object}.
   *
   * @param obj1 {@link Object left operand} in the identity comparison.
   * @param obj2 {@link Object right operand} in the identity comparison.
   * @return a boolean value indicating whether two {@link Object Objects} are the same {@link Object}.
   * @see java.lang.Object
   */
  @NullSafe
  private static boolean isSame(@Nullable Object obj1, @Nullable Object obj2) {
    return obj1 == obj2;
  }

  /**
   * Asserts that two {@link Object Objects} are the same {@link Object} as determined by the identity comparison.
   * <p>
   * The assertion holds if and only if (iff) the two {@link Object Objects} are the same {@link Object} in memory.
   *
   * @param obj1 {@link Object left operand} in the identity comparison.
   * @param obj2 {@link Object right operand} in the identity comparison.
   * @throws org.cp.elements.lang.IdentityException if the two {@link Object objects} are not the same.
   * @see #same(Object, Object, Supplier)
   * @see java.lang.Object
   */
  public static void same(Object obj1, Object obj2) {
    same(obj1, obj2, () -> format("[%1$s] is not the same as [%2$s]", obj1, obj2));
  }

  /**
   * Asserts that two {@link Object Objects} are the same {@link Object} as determined by the identity comparison.
   * <p>
   * The assertion holds if and only if (iff) the two {@link Object Objects} are the same {@link Object} in memory.
   *
   * @param obj1 {@link Object left operand} in the identity comparison.
   * @param obj2 {@link Object right operand} in the identity comparison.
   * @param message {@link String} containing the description used in the {@link IdentityException}
   * thrown if the assertion fails.
   * @param messagePlaceholderValues array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws org.cp.elements.lang.IdentityException if the two {@link Object objects} are not the same.
   * @see #same(Object, Object, RuntimeException)
   * @see java.lang.Object
   */
  public static void same(Object obj1, Object obj2, String message, Object... messagePlaceholderValues) {
    same(obj1, obj2, new IdentityException(format(message, messagePlaceholderValues)));
  }

  /**
   * Asserts that two {@link Object Objects} are the same {@link Object} as determined by the identity comparison.
   * <p>
   * The assertion holds if and only if (iff) the two {@link Object Objects} are the same {@link Object} in memory.
   *
   * @param obj1 {@link Object left operand} in the identity comparison.
   * @param obj2 {@link Object right operand} in the identity comparison.
   * @param message {@link Supplier} containing the message used in the {@link IdentityException}
   * thrown if the assertion fails.
   * @throws org.cp.elements.lang.IdentityException if the two {@link Object objects} are not the same.
   * @see java.util.function.Supplier
   * @see java.lang.Object
   */
  public static void same(Object obj1, Object obj2, Supplier<String> message) {
    if (isNotSame(obj1, obj2)) {
      throw new IdentityException(message.get());
    }
  }

  /**
   * Asserts that two {@link Object Objects} are the same {@link Object} as determined by the identity comparison.
   * <p>
   * The assertion holds if and only if (iff) the two {@link Object Objects} are the same {@link Object} in memory.
   *
   * @param obj1 {@link Object left operand} in the identity comparison.
   * @param obj2 {@link Object right operand} in the identity comparison.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the {@link Object Objects} are not the same.
   * @see java.lang.Object
   */
  public static void same(Object obj1, Object obj2, RuntimeException cause) {
    if (isNotSame(obj1, obj2)) {
      throw cause;
    }
  }

  /**
   * Null-safe method used to determine whether the given {@link Object Objects} are the same {@link Object}.
   *
   * @param obj1 {@link Object left operand} in the identity comparison.
   * @param obj2 {@link Object right operand} in the identity comparison.
   * @return a boolean value indicating whether the given {@link Object Objects} are the same {@link Object}.
   * @see java.lang.Object
   */
  @NullSafe
  private static boolean isNotSame(@Nullable Object obj1, @Nullable Object obj2) {
    return obj1 != obj2;
  }

  /**
   * Asserts that the state is valid.
   * <p>
   * The assertion holds if and only if (iff) valid is {@literal true}.
   *
   * @param state {@link Boolean value} indicating whether the state is valid.
   * @throws java.lang.IllegalStateException if the state is invalid.
   * @see #state(Boolean, String, Object...)
   */
  public static void state(Boolean state) {
    state(state, "State is invalid");
  }

  /**
   * Asserts that the state is valid.
   * <p>
   * The assertion holds if and only if (iff) valid is {@literal true}.
   *
   * @param state {@link Boolean value} indicating whether the state is valid.
   * @param message {@link String} containing the message used in the {@link IllegalStateException}
   * thrown if the assertion fails.
   * @param messagePlaceholderValues array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.IllegalStateException if the state is invalid.
   * @see #state(Boolean, RuntimeException)
   */
  public static void state(Boolean state, String message, Object... messagePlaceholderValues) {
    state(state, new IllegalStateException(format(message, messagePlaceholderValues)));
  }

  /**
   * Asserts that the state is valid.
   * <p>
   * The assertion holds if and only if (iff) valid is {@literal true}.
   *
   * @param state {@link Boolean value} indicating whether the state is valid.
   * @param message {@link Supplier} containing the message used in the {@link IllegalStateException}
   * thrown if the assertion fails.
   * @throws java.lang.IllegalStateException if the state is invalid.
   * @see java.util.function.Supplier
   */
  public static void state(Boolean state, Supplier<String> message) {
    if (isNotValid(state, argument -> Boolean.TRUE.equals(state))) {
      throw new IllegalStateException(message.get());
    }
  }

  /**
   * Asserts that the state is valid.
   * <p>
   * The assertion holds if and only if (iff) valid is {@literal true}.
   *
   * @param state {@link Boolean value} indicating whether the state is valid.
   * @param cause {@link RuntimeException} thrown if the assertion fails.
   * @throws java.lang.RuntimeException if the state is invalid.
   */
  public static void state(Boolean state, RuntimeException cause) {
    if (isNotValid(state, argument -> Boolean.TRUE.equals(state))) {
      throw cause;
    }
  }

  /**
   * Asserts that an operation is supported.
   * <p>
   * The assertion holds if and only if (iff) supported is {@literal true}.
   * <p>
   * For example, the application code might assert that:
   * <p>
   * <code>
   *   Assert.supported(dataAccessOperations.isCacheable(), "Data Access Operations (DAO) are not cacheable");
   * </code>
   *
   * @param supported {@link Boolean value} resulting from the evaluation of criteria used by the application
   * to determine if the operation is supported.
   * @throws java.lang.UnsupportedOperationException if the operation is unsupported.
   * @see #supported(Boolean, String, Object...)
   */
  public static void supported(Boolean supported) {
    supported(supported, "Operation not supported");
  }

  /**
   * Asserts that an operation is supported.
   * <p>
   * The assertion holds if and only if (iff) supported is {@literal true}.
   * <p>
   * For example, the application code might assert that:
   * <p>
   * <code>
   *   Assert.supported(dataAccessOperations.isCacheable(), "Data Access Operations (DAO) are not cacheable");
   * </code>
   *
   * @param supported {@link Boolean value} resulting from the evaluation of criteria used by the application
   * to determine if the operation is supported.
   * @param message {@link String} containing the description used in the {@link UnsupportedOperationException}
   * thrown if the assertion fails.
   * @param messagePlaceholderValues array of {@link Object arguments} used as placeholder values
   * when formatting the {@link String message}.
   * @throws java.lang.UnsupportedOperationException if the operation is unsupported.
   * @see #supported(Boolean, RuntimeException)
   */
  public static void supported(Boolean supported, String message, Object... messagePlaceholderValues) {
    supported(supported, new UnsupportedOperationException(format(message, messagePlaceholderValues)));
  }

  /**
   * Asserts that an operation is supported.
   * <p>
   * The assertion holds if and only if (iff) supported is {@literal true}.
   * <p>
   * For example, the application code might assert that:
   * <p>
   * <code>
   *   Assert.supported(dataAccessOperations.isCacheable(), "Data Access Operations (DAO) are not cacheable");
   * </code>
   *
   * @param supported {@link Boolean value} resulting from the evaluation of criteria used by the application
   * to determine if the operation is supported.
   * @param message {@link Supplier} containing the message used in the {@link UnsupportedOperationException}
   * thrown if the assertion fails.
   * @throws java.lang.UnsupportedOperationException if the operation is unsupported.
   * @see java.util.function.Supplier
   */
  public static void supported(Boolean supported, Supplier<String> message) {
    if (isNotSupported(supported)) {
      throw new UnsupportedOperationException(message.get());
    }
  }

  /**
   * Asserts that an operation is supported.
   * <p>
   * The assertion holds if and only if (iff) supported is {@literal true}.
   * <p>
   * For example, the application code might assert that:
   * <p>
   * <code>
   *   Assert.supported(dataAccessOperations.isCacheable(), "Data Access Operations (DAO) are not cacheable");
   * </code>
   *
   * @param supported {@link Boolean value} resulting from the evaluation of criteria used by the application
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
   * Null-safe method used to determine whether an application operation is supported.
   *
   * @param supported {@link Boolean value} resulting from the evaluation of criteria used by the application
   * to determine if the operation is supported.
   * @return a boolean value indicating whether an application operation is supported.
   * @see java.lang.Boolean#TRUE
   */
  @NullSafe
  private static boolean isNotSupported(@Nullable Boolean supported) {
    return !Boolean.TRUE.equals(supported);
  }

  /**
   * Null-safe utility method used to measure the length of the {@link Object} array. The length of the array
   * is measured by the number of elements in the array.
   *
   * @param array {@link Object} array to measure its length.
   * @return the length of the array or return {@literal 0} if the array is {@literal null}.
   */
  @NullSafe
  private static int arrayLength(Object... array) {
    return array != null ? array.length : 0;
  }

  /**
   * Formats the given {@link String message} with the array of {@link Object arguments}.
   *
   * @param message {@link String} containing the message to format.
   * @param arguments array of {@link Object arguments} used to format the {@link String message}.
   * @return the {@link String message} formatted with the array of {@link Object arguments}.
   * @see #messageFormat(String, Object...)
   * @see #stringFormat(String, Object...)
   */
  @NullSafe
  private static @Nullable String format(@Nullable String message, Object... arguments) {
    return stringFormat(messageFormat(message, arguments), arguments);
  }

  /**
   * Formats the given {@link String message} containing placeholders as defined by
   * the Java {@link MessageFormat} class.
   *
   * @param message {@link String} containing the message to format.
   * @param arguments array of {@link Object arguments} used to format the {@link String message}.
   * @return the {@link String message} formatted with the array of {@link Object arguments}.
   * @see java.text.MessageFormat#format(String, Object...)
   */
  @NullSafe
  private static @Nullable String messageFormat(@Nullable String message, Object... arguments) {
    return arrayLength(arguments) > 0 ? MessageFormat.format(String.valueOf(message), arguments) : message;
  }

  /**
   * Formats the given {@link String message} containing placeholders as defined by
   * the Java {@link String} class.
   *
   * @param message {@link String} containing the message to format.
   * @param arguments array of {@link Object arguments} used to format the {@link String message}.
   * @return the {@link String message} formatted with the array of {@link Object arguments}.
   * @see java.lang.String#format(String, Object...)
   */
  @NullSafe
  private static @Nullable String stringFormat(@Nullable String message, Object... arguments) {
    return arrayLength(arguments) > 0 ? String.format(String.valueOf(message), arguments) : message;
  }
}
