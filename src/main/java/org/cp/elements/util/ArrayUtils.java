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

import static org.cp.elements.lang.LangExtensions.assertThat;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Random;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.FilteringTransformer;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Transformer;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract utility class providing methods and functionality for processing arrays.
 *
 * @author John J. Blum
 * @see java.lang.Iterable
 * @see java.lang.reflect.Array
 * @see java.util.ArrayList
 * @see java.util.Arrays
 * @see java.util.Collections
 * @see java.util.Comparator
 * @see java.util.Enumeration
 * @see java.util.Iterator
 * @see java.util.List
 * @see java.util.function.Function
 * @see java.util.function.Predicate
 * @see java.util.stream.Collectors
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ArrayUtils {

  @SuppressWarnings("all")
  public static final Object[] EMPTY_ARRAY = new Object[0];

  private static final Random random = new Random();

  /**
   * Adds (inserts) the element to the end of the array.
   *
   * @param <T> Class type of the elements stored in the array.
   * @param element element to insert at the end of the array.
   * @param array array in which to insert the element.
   * @return a new array with the element inserted at the end.
   * @throws IllegalArgumentException if the array is null.
   * @see #insert(Object, Object[], int)
   * @see #count(Object[])
   */
  public static @NotNull <T> T[] append(@Nullable T element, @NotNull T[] array) {
    return insert(element, array, count(array));
  }

  /**
   * Converts the varargs into an actual array reference.
   *
   * @param <T> {@link Class type} of elements stored in the array.
   * @param elements vararg elements to convert into an array.
   * @return the given array.
   */
  @SafeVarargs
  public static <T> T[] asArray(T... elements) {
    return elements;
  }

  /**
   * Converts the {@link Enumeration} into an array.
   *
   * @param <T> {@link Class type} of elements in the array.
   * @param enumeration {@link Enumeration} to convert into an array.
   * @param componentType {@link Class type} of elements in the {@link Enumeration}.
   * @return an array containing all the elements from the {@link Enumeration}.
   * Returns an empty array if the {@link Enumeration} is {@literal null} or {@literal empty}.
   * @see #asArray(Iterable, Class)
   * @see java.util.Enumeration
   */
  @NullSafe
  public static @NotNull <T> T[] asArray(@Nullable Enumeration<T> enumeration, @Nullable Class<T> componentType) {
    return asArray(CollectionUtils.asIterable(enumeration), componentType);
  }

  /**
   * Converts the {@link Iterable} into an array.
   *
   * @param <T> {@link Class type} of elements in the array.
   * @param iterable {@link Iterable} to convert into an array.
   * @param componentType {@link Class type} of elements in the {@link Iterable}.
   * @return an array containing all the elements from the {@link Iterable}.
   * Returns an empty array if the {@link Iterable} is {@literal null} or {@literal empty}.
   * @see java.lang.Iterable
   */
  @NullSafe
  @SuppressWarnings("unchecked")
  public static @NotNull <T> T[] asArray(@Nullable Iterable<T> iterable, @Nullable Class<T> componentType) {

    List<T> arrayList = new ArrayList<>();

    for (T element : CollectionUtils.nullSafeIterable(iterable)) {
      arrayList.add(element);
    }

    Class<?> resolvedComponentType = ObjectUtils.returnFirstNonNullValue(componentType, Object.class);

    T[] array = (T[]) Array.newInstance(resolvedComponentType, arrayList.size());

    return arrayList.toArray(array);
  }

  /**
   * Converts the {@link Iterator} into an array.
   *
   * @param <T> {@link Class type} of elements in the array.
   * @param iterator {@link Iterator} to convert into an array.
   * @param componentType {@link Class type} of elements in the {@link Iterator}.
   * @return an array containing all the elements from the {@link Iterator}.
   * Returns an empty array if the {@link Iterator} is {@literal null} or {@literal empty}.
   * @see #asArray(Iterable, Class)
   * @see java.util.Iterator
   */
  @NullSafe
  public static @NotNull <T> T[] asArray(@Nullable Iterator<T> iterator, @Nullable Class<T> componentType) {
    return asArray(CollectionUtils.asIterable(iterator), componentType);
  }

  /**
   * Returns an {@link Enumeration} enumerating over the elements in the array.
   *
   * @param <T> Class type of the elements in the array.
   * @param array array to enumerate.
   * @return an {@link Enumeration} over the elements in the array or an empty {@link Enumeration}
   * if the array is null or empty.
   * @see java.util.Enumeration
   */
  @NullSafe
  @SafeVarargs
  public static @NotNull <T> Enumeration<T> asEnumeration(T... array) {

    return array == null ? Collections.emptyEnumeration() : new Enumeration<>() {

      private int index;

      @Override
      public boolean hasMoreElements() {
        return this.index < array.length;
      }

      @Override
      public T nextElement() {

        Assert.isTrue(hasMoreElements(), new NoSuchElementException("No more elements"));

        return array[this.index++];
      }
    };
  }

  /**
   * Returns an {@link Iterable} over the elements in the array.
   *
   * @param <T> Class type of the elements in the array.
   * @param array array to iterate.
   * @return an {@link Iterable} over the elements in the array
   * or an empty {@link Iterable} if the array is null or empty.
   * @see java.lang.Iterable
   * @see #asIterator(Object[])
   */
  @NullSafe
  @SafeVarargs
  public static @NotNull <T> Iterable<T> asIterable(T... array) {
    return () -> asIterator(array);
  }

  /**
   * Returns an {@link Iterator} iterating over the elements in the array.
   *
   * @param <T> Class type of the elements in the array.
   * @param array array to iterate.
   * @return an {@link Iterator} to iterate over the elements in the array
   * or an empty {@link Iterator} if the array is null or empty.
   * @see java.util.Iterator
   */
  @NullSafe
  @SafeVarargs
  public static @NotNull <T> Iterator<T> asIterator(T... array) {

    return array == null ? Collections.emptyIterator() : new Iterator<>() {

      private int index;

      @Override
      public boolean hasNext() {
        return this.index < array.length;
      }

      @Override
      public T next() {

        Assert.isTrue(hasNext(), new NoSuchElementException("No more elements"));

        return array[this.index++];
      }
    };
  }

  /**
   * Counts the number of elements in the array.
   *
   * @param <T> Class type of the elements in the array.
   * @param array array to evaluate.
   * @return an integer value indicating the number of elements in the array (a.k.a. size).
   */
  @NullSafe
  public static <T> int count(@Nullable T[] array) {
    return array != null ? array.length : 0;
  }

  /**
   * Counts the number of elements in the array matching the criteria (rules) defined by the {@link Predicate}.
   *
   * @param <T> {@link Class type} of the elements in the array.
   * @param array array to search.
   * @param predicate {@link Predicate} used to match elements in the array and tally the count.
   * @return an integer value indicating the number of elements in the array matching the criteria (rules)
   * defined by the {@link Predicate}.
   * @throws IllegalArgumentException if {@link Predicate} is {@literal null}.
   * @see java.util.function.Predicate
   */
  public static <T> long count(@Nullable T[] array, @NotNull Predicate<T> predicate) {

    Assert.notNull(predicate, "Predicate is required");

    return Arrays.stream(nullSafeArray(array))
      .filter(predicate)
      .count();
  }

  /**
   * Deeply copies the given array into a new array of the same {@link Class type}.
   *
   * This function performs a deep copy by {@literal cloning} each element from the original array.
   *
   * @param <T> {@link Class type} of the array elements.
   * @param array array to copy.
   * @return a deep copy of the given array.
   * @throws IllegalArgumentException if the given array is {@literal null}, or an individual element
   * from the original array is not {@link Cloneable}.
   * @see #deepCopy(Object[], Function)
   * @see #shallowCopy(Object[])
   */
  @SuppressWarnings("all")
  public static @NotNull <T> T[] deepCopy(@NotNull T[] array) {

    Assert.notNull(array, "Array is required");

    return deepCopy(array, element -> element != null ? ObjectUtils.clone(element) : null);
  }

  /**
   * Deeply copies the given array into a new array of the same {@link Class type}.
   * <p>
   * This function performs a deep copy by applying the provided {@link Function}
   * used to copy each element from the original array.
   *
   * @param <T> {@link Class type} of the array elements.
   * @param array array to copy.
   * @param copyFunction {@link Function} used to copy, or clone an element from the original array.
   * @return a deep copy of the given array.
   * @throws IllegalArgumentException if the given array or {@link Function copy function} is {@literal null}.
   * @see #deepCopy(Object[])
   * @see #shallowCopy(Object[])
   */
  @SuppressWarnings("unchecked")
  public static @NotNull <T> T[] deepCopy(@NotNull T[] array, @NotNull Function<T, T> copyFunction) {

    Assert.notNull(array, "Array is required");
    Assert.notNull(copyFunction, "Copy Function is required");

    T[] arrayCopy = (T[]) Array.newInstance(array.getClass().getComponentType(), array.length);

    for (int index = 0, size = array.length; index < size; index++) {
      arrayCopy[index] = copyFunction.apply(array[index]);
    }

    return arrayCopy;
  }

  /**
   * Returns the given {@code array} if not {@literal null} or empty, otherwise returns the {@code defaultArray}.
   *
   * @param <T> {@link Class} type of the elements in the array.
   * @param array array to evaluate.
   * @param defaultArray array to return if the given {@code array} is {@literal null} or empty.
   * @return the given {@code array} if not {@literal null} or empty otherwise return the {@code defaultArray}.
   */
  public static @Nullable <T> T[] defaultIfEmpty(@Nullable T[] array, @Nullable T[] defaultArray) {
    return isNotEmpty(array) ? array : defaultArray;
  }

  /**
   * Returns an empty array.
   *
   * @return an empty array with no capacity for elements.
   * @see #EMPTY_ARRAY
   */
  @NullSafe
  public static @NotNull Object[] emptyArray() {
    return EMPTY_ARRAY.clone();
  }

  /**
   * Filters the elements in the array using the given {@link Predicate}.
   *
   * @param <T> Class type of the elements in the array.
   * @param array array to predicate.
   * @param predicate {@link Predicate} used to filter the array elements.
   * @return a new array of the array class component type containing only elements from the given array
   * accepted by the {@link Predicate}.
   * @throws IllegalArgumentException if either the array or {@link Predicate} are {@literal null}.
   * @see #filterAndTransform(Object[], FilteringTransformer)
   * @see java.util.function.Predicate
   */
  @SuppressWarnings("unchecked")
  public static @NotNull <T> T[] filter(@NotNull T[] array, @NotNull Predicate<T> predicate) {

    Assert.notNull(array, "Array is required");
    Assert.notNull(predicate, "Predicate is required");

    List<T> arrayList = Arrays.stream(array)
      .filter(predicate)
      .toList();

    T[] newArray = (T[]) Array.newInstance(array.getClass().getComponentType(), arrayList.size());

    return arrayList.toArray(newArray);
  }

  /**
   * Filters and transforms the elements in the array.
   *
   * @param <T> Class type of the elements in the array.
   * @param array array to filter and transform.
   * @param filteringTransformer {@link FilteringTransformer} used to filter and transform the array elements.
   * @return a new array of the array class component type containing elements from the given array filtered
   * (accepted) and transformed by the {@link FilteringTransformer}.
   * @throws IllegalArgumentException if either the array or {@link FilteringTransformer} are null.
   * @see org.cp.elements.lang.FilteringTransformer
   * @see #transform(Object[], Transformer)
   * @see #filter(Object[], Predicate)
   */
  @SuppressWarnings("all")
  public static @NotNull <T> T[] filterAndTransform(@NotNull T[] array,
      @NotNull FilteringTransformer<T> filteringTransformer) {

    return transform(filter(array, filteringTransformer), filteringTransformer);
  }

  /**
   * Searches the array returning all elements accepted by the {@link Predicate}.
   *
   * @param <T> Class type of the elements in the array.
   * @param array array to search.
   * @param predicate {@link Predicate} used to find all elements in the array accepted by the {@link Predicate}.
   * @return a {@link List} containing all the elements from the array accepted by the {@link Predicate}
   * or an empty {@link List} if no elements were found.
   * @throws IllegalArgumentException if {@link Predicate} is {@literal null}.
   * @see java.util.function.Predicate
   * @see #findOne(Object[], Predicate)
   */
  @SuppressWarnings("all")
  public static @NotNull <T> List<T> findAll(@Nullable T[] array, @NotNull Predicate<T> predicate) {

    Assert.notNull(predicate, "Predicate is required");

    return Arrays.stream(nullSafeArray(array))
      .filter(predicate)
      .collect(Collectors.toList());
  }

  /**
   * Searches the array for the first element accepted by the {@link Predicate}.
   *
   * @param <T> {@link Class type} of the elements in the array.
   * @param array array to search.
   * @param predicate {@link Predicate} used to find the first element in the array accepted by the {@link Predicate}.
   * @return the first element from the array accepted by the {@link Predicate} or {@literal null} if no such element
   * is found.
   * @throws IllegalArgumentException if {@link Predicate} is {@literal null}.
   * @see java.util.function.Predicate
   * @see #findAll(Object[], Predicate)
   */
  public static @NotNull <T> T findOne(@Nullable T[] array, @NotNull Predicate<T> predicate) {

    Assert.notNull(predicate, "Predicate is required");

    return Arrays.stream(nullSafeArray(array))
      .filter(predicate)
      .findFirst()
      .orElse(null);
  }

  /**
   * Returns the element at the given index in the {@code array}.
   *
   * @param <T> {@link Class} type of elements in the array.
   * @param array array from which to extract the given element at index.
   * @param index integer indicating the index of the element in the array to return.
   * @return the element at the given index in the array.
   * @see #getElementAt(Object[], int, Object)
   */
  @NullSafe
  public static @Nullable <T> T getElementAt(@Nullable T[] array, int index) {
    return getElementAt(array, index, null);
  }

  /**
   * Returns the element at the given index in the {@code array}.
   *
   * @param <T> {@link Class} type of elements in the array.
   * @param array array from which to extract the given element at index.
   * @param index integer indicating the index of the element in the array to return.
   * @param defaultValue default value to return if the array is {@literal null}, empty
   * or does not have an element at the given index.
   * @return the element at the given index in the array, or returns the default value
   * if the array is {@literal null}, empty or does not contain an element at the given index.
   * @see #getElementAt(Object[], int, Object)
   */
  @NullSafe
  public static @Nullable <T> T getElementAt(@Nullable T[] array, int index, @Nullable T defaultValue) {
    return nullSafeLength(array) > index ? array[index] : defaultValue;
  }

  /**
   * Returns the first element (at index 0) in the {@code array}.
   *
   * @param <T> {@link Class} type of elements in the array.
   * @param array array from which to extract the first element.
   * @return the first element in the array or {@literal null}
   * if the {@code array} is {@literal null} or empty.
   * @see #getFirstElement(Object[], Object)
   */
  @NullSafe
  @SafeVarargs
  public static @Nullable <T> T getFirstElement(T... array) {
    return getFirstElement(array, null);
  }

  /**
   * Returns the first element (at index 0) in the {@code array}.
   *
   * @param <T> {@link Class} type of elements in the array.
   * @param array array from which to extract the first element.
   * @param defaultValue default value to return if the given array is {@literal null} or empty.
   * @return the first element in the array or return the {@code defaultValue} if the {@code array}
   * is {@literal null} or empty.
   * @see #getElementAt(Object[], int, Object)
   */
  @NullSafe
  public static @Nullable <T> T getFirstElement(@Nullable T[] array, @Nullable T defaultValue) {
    return getElementAt(array, 0, defaultValue);
  }

  /**
   * Returns the last element (at index array.length - 1) in the {@code array}.
   *
   * @param <T> {@link Class} type of elements in the array.
   * @param array array from which to extract the last element.
   * @return the last element in the array or {@literal null} if the {@code array} is {@literal null} or empty.
   * @see #getLastElement(Object[], Object)
   */
  @NullSafe
  @SafeVarargs
  public static @Nullable <T> T getLastElement(T... array) {
    return getLastElement(array, null);
  }

  /**
   * Returns the last element (at index 0) in the {@code array}.
   *
   * @param <T> {@link Class} type of elements in the array.
   * @param array array from which to extract the last element.
   * @param defaultValue default value to return if the given array is {@literal null} or empty.
   * @return the last element in the array or return the {@code defaultValue} if the {@code array}
   * is {@literal null} or empty.
   * @see #getElementAt(Object[], int, Object)
   */
  @NullSafe
  public static @Nullable <T> T getLastElement(@Nullable T[] array, @Nullable T defaultValue) {
    return getElementAt(array, Math.max(0, nullSafeLength(array) - 1), defaultValue);
  }

  /**
   * Null-safe method to find the index of the given {@code element} in the given {@code array}.
   *
   * @param <T> {@link Class} type of elements in the array.
   * @param array array used to search for {@code element}.
   * @param element {@link Object} element to search for in the given {@code array}.
   * @return the index of the given {@code element} in the given {@code array} or return {@literal -1}
   * if {@code element} could not be found.
   */
  @NullSafe
  public static <T> int indexOf(@Nullable T[] array, @Nullable T element) {

    for (int index = 0, length = nullSafeLength(array); index < length; index++) {
      if (ObjectUtils.equalsIgnoreNull(array[index], element)) {
        return index;
      }
    }

    return -1;
  }

  /**
   * Inserts element into the array at index.
   *
   * @param <T> Class type of the elements in the array.
   * @param element element to insert into the array.
   * @param array array in which the element is inserted.
   * @param index an integer indicating the index into the array at which the element will be inserted.
   * @return a new array with element inserted at index.
   * @throws IllegalArgumentException if array is null.
   * @throws ArrayIndexOutOfBoundsException if given index is not a valid index in the array.
   * @see #prepend(Object, Object[])
   * @see #append(Object, Object[])
   */
  @SuppressWarnings("unchecked")
  public static @NotNull <T> T[] insert(@Nullable T element, @NotNull T[] array, int index) {

    Assert.notNull(array, "Array is required");

    assertThat(index).throwing(new ArrayIndexOutOfBoundsException(
      String.format("[%1$d] is not a valid index [0, %2$d] in the array", index, array.length)))
        .isGreaterThanEqualToAndLessThanEqualTo(0, array.length);

    Class<?> componentType = array.getClass().getComponentType();

    componentType = ObjectUtils.returnFirstNonNullValue(componentType, ObjectUtils.getClass(element));
    componentType = ObjectUtils.returnFirstNonNullValue(componentType, Object.class);

    T[] newArray = (T[]) Array.newInstance(componentType, array.length + 1);

    if (index > 0) {
      System.arraycopy(array, 0, newArray, 0, index);
    }

    newArray[index] = element;

    if (index < array.length) {
      System.arraycopy(array, index, newArray, index + 1, array.length - index);
    }

    return newArray;
  }

  /**
   * Determines whether the array is empty.  An array is empty if it is null or contains no elements.
   *
   * @param array array to evaluate.
   * @return a boolean value indicating whether the array is empty.
   * @see #isNotEmpty(Object[])
   * @see #count(Object[])
   */
  @NullSafe
  public static boolean isEmpty(@Nullable Object[] array) {
    return count(array) == 0;
  }

  /**
   * Determines whether the array is non-empty.  An array is non-empty if it is not null and contains
   * at least 1 element.
   *
   * @param array array to evaluate.
   * @return a boolean value indicating whether the array is non-empty.
   * @see #isEmpty(Object[])
   */
  @NullSafe
  public static boolean isNotEmpty(@Nullable Object[] array) {
    return !isEmpty(array);
  }

  /**
   * Null-safe operation to determine whether the size (length) of the array matches the given size.
   *
   * @param array array to evaluate.
   * @param size {@link Integer} indicating the length of the array to match.
   * @return a boolean value indicating whether the given array matches the given size (length).
   * @see #nullSafeLength(Object[])
   */
  @NullSafe
  public static boolean isSize(@Nullable Object[] array, int size) {
    return nullSafeLength(array) == size;
  }

  /**
   * Null-safe operation to determine whether the given array has a length of 1.
   *
   * @param array array to evaluate.
   * @return a boolean value indicating whether the length of the array is 1.
   * @see #nullSafeLength(Object[])
   */
  @NullSafe
  public static boolean isSizeOne(Object... array) {
    return nullSafeLength(array) == 1;
  }

  /**
   * Determines whether the array is not {@literal null} and contains no {@literal null} elements.
   *
   * @param array array to evaluate.
   * @return a boolean value indicating whether the array is not {@literal null}
   * and contains no {@literal null} elements.
   */
  @NullSafe
  public static boolean noNullElements(Object... array) {

    return array != null && Arrays.stream(array)
      .filter(Objects::nonNull)
      .count() == array.length;
  }

  /**
   * Null-safe method returning the array if not null otherwise returns an empty array.
   *
   * @param <T> Class type of the elements in the array.
   * @param array array to evaluate.
   * @return the array if not null otherwise an empty array.
   * @see #nullSafeArray(Object[], Class)
   * @see #componentType(Object[])
   */
  @NullSafe
  public static @NotNull <T> T[] nullSafeArray(@Nullable T[] array) {
    return nullSafeArray(array, componentType(array));
  }

  /**
   * Null-safe method returning the array if not null otherwise returns an empty array.
   *
   * @param <T> Class type of the elements in the array.
   * @param array array to evaluate.
   * @param componentType {@link Class} type of the elements in the array.  Defaults to {@link Object} if null.
   * @return the array if not null otherwise an empty array.
   * @see java.lang.reflect.Array#newInstance(Class, int)
   * @see #nullSafeArray(Object[])
   */
  @NullSafe
  @SuppressWarnings("unchecked")
  public static @NotNull <T> T[] nullSafeArray(@Nullable T[] array, @Nullable Class<?> componentType) {
    return array != null ? array
      : (T[]) Array.newInstance(ObjectUtils.returnFirstNonNullValue(componentType, Object.class), 0);
  }

  @NullSafe
  static @NotNull <T> Class<?> componentType(@Nullable T[] array) {
    return array != null ? array.getClass().getComponentType() : Object.class;
  }

  /**
   * Determines the length of the array.  This method is a null-safe operation and handles null arrays
   * by returning 0.
   *
   * @param <T> Class type of the elements in the array.
   * @param array array to evaluate.
   * @return the length of the array or 0 if the array is null or empty.
   * @see #count(Object[])
   */
  @NullSafe
  public static <T> int nullSafeLength(@Nullable T[] array) {
    return count(array);
  }

  /**
   * Prepends (inserts) the element at the beginning of the array.
   *
   * @param <T> Class type of the elements in the array.
   * @param element element to insert.
   * @param array array in which to insert the element.
   * @return a new array with the element inserted at the beginning.
   * @throws IllegalArgumentException if the array is null.
   * @see #insert(Object, Object[], int)
   */
  public static @NotNull <T> T[] prepend(@Nullable T element, @NotNull T[] array) {
    return insert(element, array, 0);
  }

  /**
   * Removes the element at index from the given array.
   *
   * @param <T> {@link Class} type of the elements in the array.
   * @param array array from which to remove the element at index.
   * @param index index of the element to remove from the array.
   * @return a new array with the element at index removed.
   * @throws IllegalArgumentException if the given array is {@literal null}.
   * @throws ArrayIndexOutOfBoundsException if the {@code index} is not valid.
   */
  @SuppressWarnings("unchecked")
  public static @NotNull <T> T[] remove(@NotNull T[] array, int index) {

    Assert.notNull(array, "Array is required");

    assertThat(index).throwing(new ArrayIndexOutOfBoundsException(
      String.format("[%1$d] is not a valid index [0, %2$d] in the array", index, array.length)))
        .isGreaterThanEqualToAndLessThan(0, array.length);

    Class<?> componentType = ObjectUtils.returnFirstNonNullValue(array.getClass().getComponentType(), Object.class);

    T[] newArray = (T[]) Array.newInstance(componentType, array.length - 1);

    if (index > 0) {
      System.arraycopy(array, 0, newArray, 0, index);
    }

    if (index + 1 < array.length) {
      System.arraycopy(array, index + 1, newArray, index, (array.length - index - 1));
    }

    return newArray;
  }

  /**
   * Shallowly copies the given array into a new array of the same {@link Class type}.
   *
   * @param <T> {@link Class type} of the array elements.
   * @param array array to copy.
   * @return a shallow copy of the original array.
   * @throws IllegalArgumentException if the array is {@literal null}.
   * @see #deepCopy(Object[])
   */
  @SuppressWarnings("unchecked")
  public static @NotNull <T> T[] shallowCopy(@NotNull T[] array) {

    Assert.notNull(array, "Array is required");

    T[] arrayCopy = (T[]) Array.newInstance(array.getClass().getComponentType(), array.length);

    System.arraycopy(array, 0, arrayCopy, 0, array.length);

    return arrayCopy;
  }

  /**
   * Shuffles the elements in the array.  This method guarantees a random, uniform shuffling of the array elements
   * with an operational efficiency of O(n).
   *
   * @param <T> Class type of the elements in the array.
   * @param array array of elements to shuffle.
   * @return the array of elements shuffled.
   * @see #isNotEmpty(Object[])
   */
  public static @NotNull <T> T[] shuffle(@NotNull T[] array) {

    if (isNotEmpty(array)) {

      random.setSeed(System.currentTimeMillis());

      for (int index = 0, lengthMinusOne = array.length - 1; index < lengthMinusOne; index++) {

        int randomIndex = random.nextInt(lengthMinusOne - index) + 1;

        swap(array, index, index + randomIndex);
      }
    }

    return array;
  }

  /**
   * Sorts the {@link Comparable} elements in the given array.
   *
   * @param <T> {@link Class} type of the elements in the array.
   * @param array array of {@link Comparable} elements to sort.
   * @return the given array.
   * @see #sort(Object[], Comparator)
   * @see java.lang.Comparable
   */
  public static @NotNull <T extends Comparable<T>> T[] sort(@NotNull T[] array) {
    return sort(array, Comparable::compareTo);
  }

  /**
   * Sorts the elements in the given array.
   *
   * @param <T> {@link Class} type of the elements in the array.
   * @param array array of elements to sort.
   * @param comparator {@link Comparator} used to sort (order) the elements in the array.
   * @return the given array sorted.
   * @see java.util.Comparator
   */
  public static @NotNull <T> T[] sort(@NotNull T[] array, @NotNull Comparator<T> comparator) {

    Arrays.sort(array, comparator);

    return array;
  }

  /**
   * Create a sub-array containing elements at the specified indices in the given, required {@link T array}.
   *
   * @param <T> {@link Class type} of elements contained in the array.
   * @param array source array; must not be {@literal null}.
   * @param indices array of indexes referring to elements contained in the array to include in the sub-array;
   * must not be {@literal null}.
   * @return a sub-array containing elements from the given array at the specified indices.
   * @throws ArrayIndexOutOfBoundsException if the indices are not valid indexes in the given array.
   * @throws NullPointerException if either the array or indices are {@literal null}.
   */
  @SuppressWarnings("unchecked")
  public static @NotNull <T> T[] subArray(@NotNull T[] array, int... indices) {

    List<T> subArrayList = new ArrayList<>(indices.length);

    for (int index : indices) {
      subArrayList.add(array[index]);
    }

    return subArrayList.toArray((T[]) Array.newInstance(array.getClass().getComponentType(), subArrayList.size()));
  }

  /**
   * Create a sub-array containing elements from the given, required {@link T array}.
   *
   * @param <T> {@link Class type} of elements contained in the array.
   * @param array source array; must not be {@literal null}.
   * @param offset {@link Integer} specifying the index in the array from which to start extracting elements.
   * @param length {@link Integer} specifying the number of elements to extract from the array.
   * @return a sub-array containing elements from the given array between {@code offset}
   * and {@code offset} + {@code length}
   * @throws ArrayIndexOutOfBoundsException if the {@code offset} or {@code length} are not valid.
   * @throws NullPointerException if the array is {@literal null}.
   * @see #subArray(Object[], int...)
   */
  public static @NotNull <T> T[] subArray(@NotNull T[] array, int offset, int length) {

    int[] indices = new int[length];

    for (int index = 0; index < length; index++) {
      indices[index] = offset + index;
    }

    return subArray(array, indices);
  }

  /**
   * Swaps elements in the array at indexOne and indexTwo.
   *
   * @param <T> Class type of the elements in the array.
   * @param array array with the elements to swap.
   * @param indexOne index of the first element to swap.
   * @param indexTwo index of the second element to swap.
   * @return the array with the specified elements at indexOne and indexTwo swapped.
   * @throws ArrayIndexOutOfBoundsException if the indexes are not valid indexes in the array.
   * @throws NullPointerException if the array is null.
   */
  public static @NotNull <T> T[] swap(@NotNull T[] array, int indexOne, int indexTwo) {

    T elementAtIndexOne = array[indexOne];

    array[indexOne] = array[indexTwo];
    array[indexTwo] = elementAtIndexOne;

    return array;
  }

  /**
   * Converts the given {@link Object} array into a {@link String} array.
   *
   * @param array array of {@link Object Objects} to convert into a {@link String} array.
   * @return a {@link String} array for the given {@link Object} array.
   */
  @NullSafe
  public static @NotNull String[] toStringArray(Object... array) {

    String[] stringArray = new String[nullSafeLength(array)];

    int index = 0;

    for (Object element : nullSafeArray(array)) {
      stringArray[index++] = String.valueOf(element);
    }

    return stringArray;
  }

  /**
   * Transforms the elements in the array using the provided {@link Transformer}.
   *
   * @param <T> Class type of the elements in the array.
   * @param array array of elements to transform.
   * @param transformer {@link Transformer} used to transform the array elements.
   * @return the array of elements transformed.
   * @throws IllegalArgumentException if either the array or {@link Transformer} are null.
   * @see org.cp.elements.lang.Transformer
   */
  public static @NotNull <T> T[] transform(@NotNull T[] array, @NotNull Transformer<T> transformer) {

    Assert.notNull(array, "Array is required");
    Assert.notNull(transformer, "Transformer is required");

    for (int index = 0; index < array.length; index++) {
      array[index] = transformer.transform(array[index]);
    }

    return array;
  }
}
