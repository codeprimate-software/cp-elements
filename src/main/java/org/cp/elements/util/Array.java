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
package org.cp.elements.util;

import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalStateException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newUnsupportedOperationException;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.TypeResolver;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.util.stream.StreamUtils;
import org.cp.elements.util.stream.Streamable;

/**
 * Abstract Data Type (ADT) modeling and wrapping any Java array.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object elements} in the array.
 * @see org.cp.elements.util.stream.Streamable
 * @see java.lang.FunctionalInterface
 * @see java.lang.Iterable
 * @since 2.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface Array<T> extends Iterable<T>, Streamable<T> {

  /**
   * Factory method used to construct a new {@link Array} with no elements.
   *
   * @param <T> {@link Class type} of the {@link Object elements} in the array.
   * @return a new, empty {@link Array}.
   */
  static <T> Array<T> empty() {
    return Collections::emptyIterator;
  }

  /**
   * Factory method used to construct a new, immutable {@link Array} from the given array of {@link T elements}.
   *
   * @param <T> {@link Class type} of {@link Object elements} in the array.
   * @param array Java array from which to construct a new, immutable {@link Array}.
   * @return a new, immutable {@link Array} from the given Java array.
   * @throws IllegalArgumentException if {@link T array} is {@literal null}.
   * @see #of(Object[])
   */
  @SafeVarargs
  static <T> Array<T> immutableOf(T... array) {

    return new AbstractArray<>(array) {

      @Override
      public Array<T> set(int index, T value) {
        throw newIllegalStateException("Array is immutable");
      }
    };
  }

  /**
   * Factory method used to construct a new {@link Array} from the given array of {@link T elements}.
   *
   * @param <T> {@link Class type} of {@link Object elements} in the array.
   * @param array Java array from which to construct a new {@link Array}.
   * @return a new {@link Array} from the given Java array.
   * @throws IllegalArgumentException if {@link T array} is {@literal null}.
   */
  @SafeVarargs
  static <T> Array<T> of(T... array) {
    return AbstractArray.of(array);
  }

  /**
   * Factory method used to construct a new {@link Array} containing a single element.
   *
   * @param <T> {@link Class type} of {@link Object elements} in the array.
   * @param element {@link T object} to add to the array.
   * @return a new, single element {@link Array}.
   * @throws IllegalArgumentException if {@link T element} is {@literal null}.
   * @see #of(Object[])
   */
  static <T> Array<T> singleton(T element) {
    Assert.notNull(element, "Element is required");
    return of(element);
  }

  /**
   * Determines whether this {@link Array} is empty.
   *
   * @return a boolean value indicating whether this {@link Array} is empty.
   * @see #isNotEmpty()
   * @see #length()
   */
  default boolean isEmpty() {
    return length() == 0;
  }

  /**
   * Determines whether this {@link Array} is empty.
   *
   * @return a boolean value indicating whether this {@link Array} is empty.
   * @see #isEmpty()
   */
  default boolean isNotEmpty() {
    return !isEmpty();
  }

  /**
   * Finds all {@link T elements} in this {@link Array} matching the given {@link Predicate} as an {@link Array}.
   *
   * @param predicate {@link Predicate} used to match {@link T elements} from this {@link Array}.
   * @return all elements in this {@link Array} matching the given {@link Predicate} as an {@link Array}.
   * @see java.util.function.Predicate
   * @see #findOne(Predicate)
   */
  @SuppressWarnings("unchecked")
  default Array<T> findBy(@NotNull Predicate<T> predicate) {

    Assert.notNull(predicate, "Predicate is required");

    List<T> list = stream().filter(predicate).toList();
    Class<T> componentType = (Class<T>) TypeResolver.getInstance().resolveType(this).getComponentType();
    T[] array = ArrayUtils.asArray(list, componentType);

    return Array.of(array);
  }

  /**
   * Finds an {@link Optional} single {@link T element} from this {@link Array} matching the given {@link Predicate}.
   *
   * @param predicate {@link Predicate} used to match the single {@link T element}.
   * @return an {@link Optional} single {@link T element} from this {@link Array} matching the given {@link Predicate}.
   * @throws IllegalArgumentException if {@link Predicate} is {@literal null}.
   * @see #findBy(Predicate)
   * @see java.util.Optional
   */
  default Optional<T> findOne(Predicate<T> predicate) {
    Assert.notNull(predicate, "Predicate is required");
    return stream().filter(predicate).findFirst();
  }

  /**
   * Gets the {@link T element} at the given {@link Integer index} in this {@link Array}.
   *
   * @param index {@link Integer} referring to the index of the element to return.
   * @return the {@link T element} at the given {@link Integer index} in this {@link Array}.
   * @see #set(int, Object)
   */
  default T get(int index) {
    return toArray()[index];
  }

  /**
   * Sets the element at {@link Integer index} in this {@link Array} to the given {@link T value}.
   *
   * @param index {@link Integer} referring to the element to set.
   * @param value {@link T value} to set the element to.
   * @return a new {@link Array}.
   * @see #get(int)
   */
  default Array<T> set(int index, T value) {
    T[] array = toArray();
    array[index] = value;
    return Array.of(array);
  }

  /**
   * Returns the {@link Integer number of elements} contained in this {@link Array}.
   *
   * @return the {@link Integer number of elements} contained in this {@link Array}.
   */
  default int length() {
    return Long.valueOf(stream().count()).intValue();
  }

  /**
   * Requires that a single {@link T element} matching the given {@link Predicate} be found in this {@link Array}.
   *
   * @param predicate {@link Predicate} used to match an {@link T element}; required.
   * @return a single {@link T element} from this {@link Array} matching the {@link Predicate}.
   * @throws IllegalArgumentException if {@link Predicate} is {@literal null}.
   * @throws NoSuchElementException if a single {@link T element} cannot be found matching the {@link Predicate}.
   * @see java.util.function.Predicate
   * @see #findOne(Predicate)
   */
  default T requireOne(@NotNull Predicate<T> predicate) {
    return findOne(predicate).orElseThrow(() -> new NoSuchElementException("Element not found"));
  }

  /**
   * Returns a {@link Stream} over the {@link T elements} in this {@link Array}.
   *
   * @return a {@link Stream} over the {@link T elements} in this {@link Array}.
   * @see java.util.stream.Stream
   */
  @Override
  default Stream<T> stream() {
    return StreamUtils.stream(this);
  }

  /**
   * Transforms the {@link T elements} in this {@link Array} into {@link Object Objects} of type {@link V}.
   *
   * @param <V> {@link Class transformed type} of the {@link T element} in this {@link Array}.
   * @param function {@link Function} used to transform the {@link T elements} in this {@link Array}.
   * @return a new {@link Array} with {@link T elements} from this {@link Array} transformed into {@link Object Objects}
   * of type {@link V}.
   * @see java.util.function.Function
   */
  @SuppressWarnings("unchecked")
  default <V> Array<V> transform(@NotNull Function<T, V> function) {
    Assert.notNull(function, "Function is required");
    V[] array = (V[]) stream().map(function).toArray();
    return Array.of(array);
  }

  /**
   * Returns this {@link Array} as a Java array containing elements of type {@link T}.
   * <p>
   * This operation simply makes a shallow copy of this {@link Array}.
   *
   * @return this {@link Array} as a Java array containing elements of type {@link T}.
   * @throws UnsupportedOperationException by default.
   */
  default T[] toArray() {
    throw newUnsupportedOperationException("Getting a Java array from Array is not supported");
  }

  /**
   * Abstract base class for all {@link Array} implementations.
   *
   * @param <T> {@link Class type} of {@link Object elements} in the array.
   */
  abstract class AbstractArray<T> implements Array<T> {

    /**
     * Factory method used to construct a new {@link AbstractArray} from the given Java array.
     *
     * @param <T> {@link Class type} of {@link Object elements} in the array.
     * @param array Java array used to construct and initialize the new {@link Array}; required.
     * @return a new {@link AbstractArray} constructed and initalized from the given Java array.
     * @throws IllegalArgumentException if {@link T array} is {@literal null}.
     */
    @SafeVarargs
    static <T> AbstractArray<T> of(T... array) {
      Assert.notNull(array, "Array is required");
      return new AbstractArray<T>(array) { };
    }

    private final T[] array;

    /**
     * Construct a new {@link AbstractArray} from the given Java array.
     *
     * @param array Java array used to contruct an initialize a new {@link Array}.
     * @throws IllegalArgumentException if {@link T array} is {@literal null}.
     */
    protected AbstractArray(T[] array) {
      Assert.notNull(array, "Array is required");
      this.array = ArrayUtils.asTypedArray(array, resolveComponentType(array));
    }

    @SuppressWarnings("unchecked")
    private Class<T> resolveComponentType(T[] array) {

      Class<?> componentType = TypeResolver.getInstance().resolveType(array);

      Class<?> resolvedComponentType = Object.class.equals(componentType)
        ? array.length > 0 ? array[0].getClass() : componentType
        : componentType;

      return (Class<T>) resolvedComponentType;
    }

    @Override
    public T get(int index) {
      return this.array[index];
    }

    @Override
    @SuppressWarnings("all")
    public Iterator<T> iterator() {
      return CollectionUtils.unmodifiableIterator(ArrayUtils.asIterator(this.array));
    }

    @Override
    public int length() {
      return this.array.length;
    }

    @Override
    public T[] toArray() {
      return ArrayUtils.shallowCopy(this.array);
    }
  }
}
