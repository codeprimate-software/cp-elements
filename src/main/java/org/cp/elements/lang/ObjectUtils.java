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

import static org.cp.elements.lang.CheckedExceptionsFactory.newCloneNotSupportedException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalStateException;
import static org.cp.elements.util.ArrayUtils.nullSafeArray;

import java.lang.reflect.Constructor;
import java.util.Optional;
import java.util.function.Supplier;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.reflect.ConstructorNotFoundException;
import org.cp.elements.lang.reflect.ReflectionUtils;

/**
 * Abstract utility class used to perform operations on {@link Object Objects}.
 *
 * @author John J. Blum
 * @see java.lang.reflect.Constructor
 * @see java.lang.Object
 * @see java.util.Optional
 * @see org.cp.elements.lang.reflect.ReflectionUtils
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ObjectUtils extends ReflectionUtils {

  public static final Object NULL_OBJECT_REFERENCE = null;

  public static final Object[] EMPTY_OBJECT_ARRAY = new Object[0];

  /**
   * Null-safe method whether the array is {@literal null}, {@literal empty} or all values in the array
   * are {@literal null}.
   *
   * @param values an array of values being evaluated for null.
   * @return {@literal true} if and only if the array is {@literal null}, {@literal empty}
   * or all elements in the array are {@literal null}.
   * @see #areAnyNull(Object...)
   */
  @NullSafe
  @SuppressWarnings("all")
  public static boolean areAllNull(Object... values) {

    for (Object value : nullSafeArray(values)) {
      if (value != null) {
        return false;
      }
    }

    return true;
  }

  /**
   * Null-safe method to determine if any of the values in the array are null.
   *
   * @param values an array of values being evaluated for null
   * @return true if and only if the array is not null, not empty and contains 1 null element.
   * @see #areAllNull(Object...)
   */
  @NullSafe
  public static boolean areAnyNull(Object... values) {

    for (Object value : nullSafeArray(values)) {
      if (value == null) {
        return true;
      }
    }

    return false;
  }

  /**
   * Null-safe clone operation for {@link Object Objects} that implement the {@link Cloneable} interface
   * or implement a copy constructor.
   *
   * @param <T> {@link Cloneable} {@link Class} type of the object.
   * @param obj {@link Object} to clone.
   * @return a "clone" of a given {@link Object} if {@link Cloneable} or the given {@link Object Object's}
   * {@link Class} type contains a copy constructor.
   * @see java.lang.Cloneable
   */
  @NullSafe
  @SuppressWarnings("unchecked")
  public static @NotNull <T> T clone(@Nullable T obj) {

    if (obj instanceof Cloneable) {
      return (T) invoke(obj, CLONE_METHOD_NAME, obj.getClass());
    }
    else if (obj != null) {
      try {

        Class<T> objectType = (Class<T>) obj.getClass();

        Constructor<T> copyConstructor = resolveConstructor(objectType, new Class<?>[] { objectType }, obj);

        return copyConstructor.newInstance(obj);
      }
      catch (ConstructorNotFoundException ignore) {
        // copy constructor was not found in the Object's class type
      }
      catch (Exception cause) {
        throw new CloneException("[clone] using [copy constructor] was unsuccessful", cause);
      }
    }

    throw new UnsupportedOperationException(newCloneNotSupportedException(
      "[clone] is not supported for object of type [%s]", getClassSimpleName(obj)));
  }

  /**
   * Safely executes the given {@link ThrowableOperation} handling any {@link Throwable}
   * thrown during the normal execution of the operation by rethrowing an {@link IllegalStateException}.
   *
   * @param <T> {@link Class type} of the {@link Object return value}.
   * @param operation {@link ThrowableOperation} to execute.
   * @return the {@link Object result} of the {@link ThrowableOperation}.
   * @see #doOperationSafely(ThrowableOperation, Object)
   * @see org.cp.elements.lang.ThrowableOperation
   */
  public static @Nullable <T> T doOperationSafely(@NotNull ThrowableOperation<T> operation) {
    return doOperationSafely(operation, (T) null);
  }

  /**
   * Safely executes the given {@link ThrowableOperation} handling any {@link Throwable}
   * thrown during the normal execution of the operation by returning the given {@link Object default value}
   * or rethrowing an {@link IllegalStateException} if the {@link Object default value} is {@literal null}.
   *
   * @param <T> {@link Class type} of the {@link Object return value}.
   * @param operation {@link ThrowableOperation} to execute.
   * @param defaultValue {@link Object} to return if the {@link ThrowableOperation} throws a {@link Throwable}.
   * @return the {@link Object result} of the {@link ThrowableOperation} or {@link Object default value}
   * if the {@link ThrowableOperation} throws a {@link Throwable}.
   * @see #doOperationSafely(ThrowableOperation, Supplier)
   * @see #returnValueOrThrowIfNull(Object, RuntimeException)
   * @see org.cp.elements.lang.ThrowableOperation
   */
  public static @Nullable <T> T doOperationSafely(@NotNull ThrowableOperation<T> operation, @NotNull T defaultValue) {

    Supplier<T> valueSupplier = () -> defaultValue;

    return doOperationSafely(operation, valueSupplier);
  }

  /**
   * Safely executes the given {@link ThrowableOperation} handling any {@link Throwable}
   * thrown during the normal execution of the operation by returning the given {@link Object default value}
   * or rethrowing an {@link IllegalStateException} if the {@link Object default value} is {@literal null}.
   *
   * @param <T> {@link Class type} of the {@link Object return value}.
   * @param operation {@link ThrowableOperation} to execute.
   * @param valueSupplier {@link Supplier} used to supply a {@link Object value}
   * if the {@link ThrowableOperation} throws a {@link Throwable}.
   * @return the {@link Object result} of the {@link ThrowableOperation} or a {@link Object value} supplied by
   * the {@link Supplier} if the {@link ThrowableOperation} throws a {@link Throwable}.
   * @throws IllegalStateException if the {@link Supplier} supplies a {@literal null} {@link Object value}.
   * @see #returnValueOrThrowIfNull(Object, RuntimeException)
   * @see org.cp.elements.lang.ThrowableOperation
   */
  public static @Nullable <T> T doOperationSafely(@NotNull ThrowableOperation<T> operation,
      @NotNull Supplier<T> valueSupplier) {

    try {
      return operation.run(EMPTY_OBJECT_ARRAY);
    }
    catch (Throwable cause) {
      return returnValueOrThrowIfNull(valueSupplier.get(),
        newIllegalStateException(cause, "Failed to execute operation [%s]", operation));
    }
  }

  /**
   * Gets the first {@literal non-null} {@link Object value} in the array of values.
   *
   * @param <T> {@link Class type} of the {@link Object values} in the array.
   * @param values array of {@link Object values} from which the first {@literal non-null} {@link Object value}
   * will be returned; should not be {@literal null}.
   * @return the first {@literal non-null} {@link Object value} in the array of values.
   */
  @NullSafe
  @SuppressWarnings({ "unchecked", "varargs" })
  public static @Nullable <T> T returnFirstNonNullValue(T... values) {

    for (T value : nullSafeArray(values)) {
      if (value != null) {
        return value;
      }
    }

    return null;
  }

  /**
   * Returns the given {@code value} if not {@literal null} or returns the {@code defaultValue}.
   *
   * @param <T> {@link Class} type of the {@code value} and {@code defaultValue}.
   * @param value {@link Object} to evaluate for {@literal null}.
   * @param defaultValue {@link Object} value to return if {@code value} is {@literal null}.
   * @return the given {@code value} if not {@literal null} or returns the {@code defaultValue}.
   * @see #returnValueOrDefaultIfNull(Object, Supplier)
   */
  public static @Nullable <T> T returnValueOrDefaultIfNull(@Nullable T value, @Nullable T defaultValue) {
    return returnValueOrDefaultIfNull(value, () -> defaultValue);
  }

  /**
   * Returns the given {@code value} if not {@literal null} or call the given {@link Supplier} to supply a value.
   *
   * @param <T> {@link Class} type of the {@code value}.
   * @param value {@link Object} to evaluate for {@literal null}.
   * @param valueSupplier {@link Supplier} used to supply a value if {@code value} is {@literal null}.
   * @return the given {@code value} if not {@literal null} or call the given {@link Supplier} to supply a value.
   * @see java.util.function.Supplier
   */
  public static @Nullable <T> T returnValueOrDefaultIfNull(@Nullable T value, @NotNull Supplier<T> valueSupplier) {
    return Optional.ofNullable(value).orElseGet(valueSupplier);
  }

  /**
   * Returns the given {@code value} if not {@literal null} or throws an {@link IllegalArgumentException}.
   *
   * @param <T> {@link Class} type of the {@code value}.
   * @param value {@link Object} value to evaluate.
   * @return the given {@code value} if not {@literal null} or throws an {@link IllegalArgumentException}.
   * @throws IllegalArgumentException if {@code value} is {@literal null}.
   * @see #returnValueOrThrowIfNull(Object, RuntimeException)
   */
  @NullSafe
  public static @NotNull <T> T returnValueOrThrowIfNull(@Nullable T value) {
    return returnValueOrThrowIfNull(value, newIllegalArgumentException("Value must not be null"));
  }

  /**
   * Returns the given {@code value} if not {@literal null} or throws the given {@link RuntimeException}.
   *
   * @param <T> {@link Class} type of the {@code value}.
   * @param value {@link Object} value to evaluate.
   * @param exception {@link RuntimeException} to throw if {@code value} is {@literal null}.
   * @return the given {@code value} if not {@literal null} or throws the given {@link RuntimeException}.
   * @throws IllegalArgumentException if {@code exception} is {@literal null}.
   * @throws RuntimeException if {@code value} is {@literal null}.
   */
  public static @NotNull <T> T returnValueOrThrowIfNull(@Nullable T value, @NotNull RuntimeException exception) {

    Assert.notNull(exception, "RuntimeException must not be null");

    return Optional.ofNullable(value).orElseThrow(() -> exception);
  }

  /**
   * Safely returns the value supplied by the given {@link Supplier}.  If an {@link Exception} or {@link Error} occurs,
   * then {@literal null} is returned.
   *
   * @param <T> {@link Class} type of the value to get.
   * @param valueSupplier {@link Supplier} of the value.
   * @return a value from the given {@link Supplier} in an error safe manner.
   * @see java.util.function.Supplier
   * @see #safeGetValue(Supplier, Object)
   */
  public static @Nullable <T> T safeGetValue(@NotNull Supplier<T> valueSupplier) {
    return safeGetValue(valueSupplier, null);
  }

  /**
   * Safely returns the value supplied by the given {@link Supplier}.  If an {@link Exception} or {@link Error} occurs
   * then the {@code defaultValue} will be returned.
   *
   * @param <T> {@link Class} type of the value to get.
   * @param valueSupplier {@link Supplier} of the value.
   * @param defaultValue value to return if the {@link Supplier} is unable to supply the value.
   * @return a value from the given {@link Supplier} in an error safe manner.  If an {@link Exception} or {@link Error}
   * occurs then the {@code defaultValue} will be returned.
   * @see java.util.function.Supplier
   */
  public static @Nullable <T> T safeGetValue(@NotNull Supplier<T> valueSupplier, @Nullable T defaultValue) {

    try {
      return valueSupplier.get();
    }
    catch (Throwable ignore) {
      return defaultValue;
    }
  }

  /**
   * Determines whether two objects are equal in value as determined by the Object.equals method in addition to
   * guarding against null values.  Both objects are considered equal if and only if both are non-null
   * and obj1.equals(obj2).
   *
   * @param obj1 the first Object in the equality comparison.
   * @param obj2 the second Object in the equality comparison.
   * @return a boolean value indicating whether the two Objects are equal in value.
   * @see java.lang.Object#equals(Object)
   */
  @NullSafe
  public static boolean equals(@Nullable Object obj1, @Nullable Object obj2) {
    return obj1 != null && obj1.equals(obj2);
  }

  /**
   * Determines whether two objects are equal in value as determined by the Object.equals method.  Both objects are
   * considered equal if and only if both are null or obj1.equals(obj2).
   *
   * @param obj1 the first Object in the equality comparison.
   * @param obj2 the second Object in the equality comparison.
   * @return a boolean value indicating whether the two Objects are equal in value where two null Object references
   * are considered equal as well.
   * @see java.lang.Object#equals(Object)
   */
  @NullSafe
  public static boolean equalsIgnoreNull(@Nullable Object obj1, @Nullable Object obj2) {
    return obj1 == null ? obj2 == null : obj1.equals(obj2);
  }

  /**
   * Determines whether {@code obj1} is {@literal null} or equal to {@code obj2}.
   *
   * @param obj1 {@link Object} being evaluated in the equality comparison.
   * @param obj2 {@link Object} to compare for equality with {@code obj1} if {@code obj1} is not {@literal null}.
   * @return a boolean value indicating whether {@code obj1} is {@literal null} or equal to {@code obj2}.
   * @see java.lang.Object#equals(Object)
   */
  @NullSafe
  public static boolean isNullOrEqualTo(@Nullable Object obj1, @Nullable Object obj2) {
    return obj1 == null || obj1.equals(obj2);
  }

  /**
   * Calculates the hash code of an object by invoking Object.hashCode for non-null objects and returning 0 if the
   * object is null.
   *
   * @param obj the Object who's hash code is computed and returned.
   * @return an integer value with the calculated hash code of the object.
   * @see java.lang.Object#hashCode()
   */
  @NullSafe
  public static int hashCode(@Nullable Object obj) {
    return obj != null ? obj.hashCode() : 0;
  }

  /**
   * Transforms the object into a String by invoking Object.toString for non-null objects and returning null for
   * null object references.
   *
   * @param obj the Object who's toString method will be called.
   * @return a String representation of the object.
   * @see java.lang.Object#toString()
   */
  @NullSafe
  public static @Nullable String toString(@Nullable Object obj) {
    return obj != null ? obj.toString() : null;
  }
}
