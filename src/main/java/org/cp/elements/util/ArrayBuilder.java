/*
 * Copyright 2017-Present Author or Authors.
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

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Builder;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.TypeResolver;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Elements {@link Builder} used to build an mutable array from an given array or {@link Iterable}.
 *
 * @param <T> {@link Class type} of {@link Object elements} in the array.
 * @see org.cp.elements.lang.Builder
 */
public class ArrayBuilder<T> implements Builder<T[]> {

  public static final String LAMBDA = "$$Lambda";

  /**
   * Factory method used to construct a new {@link ArrayBuilder} with an empty array.
   *
   * @param <T> {@link Class type} of {@link Object elements} in the array.
   * @return a new {@link ArrayBuilder} with an empty array.
   */
  public static <T> ArrayBuilder<T> empty() {
    return new ArrayBuilder<>(new ArrayList<>());
  }

  /**
   * Factory method used to construct a new {@link ArrayBuilder} initialized from the given array.
   *
   * @param <T> {@link Class type} of {@link Object elements} in the array.
   * @param elements array of elements used to initialize the builder.
   * @return a new {@link ArrayBuilder} initialized from the given array.
   * @see #from(Iterable)
   */
  @SafeVarargs
  public static <T> ArrayBuilder<T> from(T... elements) {
    return from(Arrays.asList(ArrayUtils.nullSafeArray(elements)));
  }

  /**
   * Factory method used to construct a new {@link ArrayBuilder} initialized from the given {@link Iterable}.
   *
   * @param <T> {@link Class type} of {@link Object elements} in the {@link Iterable}.
   * @param elements {@link Iterable} used to initialize the array.
   * @return a new {@link ArrayBuilder} initialized form the given {@link Iterable}.
   * @throws IllegalArgumentException if {@link Iterable} is {@literal null}.
   * @see Iterable
   */
  public static <T> ArrayBuilder<T> from(Iterable<T> elements) {
    Assert.notNull(elements, "An Iterable collection of elements is required");
    return new ArrayBuilder<>(new ArrayList<>(CollectionUtils.asList(elements)));
  }

  private final List<T> list;

  /**
   * Constructs a new {@link ArrayBuilder} initialized form the given {@link List}.
   *
   * @param list {@link List} used to model the elements of the array.
   * @see List
   */
  protected ArrayBuilder(List<T> list) {
    this.list = list;
  }

  /**
   * Determines whether the array has any elements.
   *
   * @return a boolean value indicating whether the array has any elements.
   */
  public boolean isEmpty() {
    return this.list.isEmpty();
  }

  /**
   * Adds the given {@link T element} to the array.
   *
   * @param element {@link T element} to add to the array.
   * @return this {@link ArrayBuilder}.
   * @throws IllegalArgumentException if {@link T element} is {@literal null}.
   */
  public ArrayBuilder<T> add(@NotNull T element) {
    Assert.notNull(element, "Element is required");
    this.list.add(element);
    return this;
  }

  /**
   * Removes the last element from the array.
   *
   * @return the removed element.
   * @throws IllegalArgumentException if the array is empty.
   */
  public T remove() {
    int index = toIndex(size());
    Assert.argument(index, idx -> idx > -1, new ArrayIndexOutOfBoundsException("Array is empty"));
    return this.list.remove(index);
  }

  /**
   * Returns the {@link Integer number} of elements in the array.
   *
   * @return the {@link Integer number} of elements in the array.
   */
  public int size() {
    return this.list.size();
  }

  /**
   * Converts a {@link Integer number} into an {@literal index} of an indexed data structure such as an array or list.
   *
   * @param number {@link Integer number} to convert to an index.
   * @return the {@link Integer index} for the given {@link Integer number}.
   */
  protected int toIndex(int number) {
    return number - 1;
  }

  @SuppressWarnings("unchecked")
  private Class<T> resolveElementType() {

    Class<T> elementType = (Class<T>) TypeResolver.getInstance().resolveType(this.list);
    String elementTypeName = elementType.getName();
    int index = elementTypeName.indexOf(LAMBDA);

    return index > -1
      ? ClassUtils.loadClass(elementTypeName.substring(0, index))
      : elementType;
  }

  @SuppressWarnings("unchecked")
  private T[] newArray(int length) {
    Class<T> componentType = resolveElementType();
    return (T[]) Array.newInstance(componentType, length);
  }

  @Override
  public T[] build() {
    return this.list.toArray(newArray(size()));
  }
}
