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

package org.cp.elements.util;

import static java.util.Arrays.stream;
import static org.cp.elements.lang.LangExtensions.assertThat;
import static org.cp.elements.lang.ObjectUtils.defaultIfNull;
import static org.cp.elements.lang.ObjectUtils.equalsIgnoreNull;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Random;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.FilteringTransformer;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Transformer;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * {@link ArrayUtils} encapsulates utility methods and functionality for interacting with {@link Object} arrays.
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
 * @see java.util.stream.Collectors
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.FilteringTransformer
 * @see org.cp.elements.lang.Transformer
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ArrayUtils {

  @SuppressWarnings("all")
  private static final Object[] EMPTY_ARRAY = new Object[0];

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
  public static <T> T[] append(T element, T[] array) {
    return insert(element, array, count(array));
  }

  /**
   * Converts the varargs into an actual array reference.
   *
   * @param <T> Class type of the elements stored in the array.
   * @param elements elements (varargs) to convert into an array.
   * @return the given array.
   */
  @SafeVarargs
  public static <T> T[] asArray(T... elements) {
    return elements;
  }

  /**
   * Converts the {@link Enumeration} into an array.
   *
   * @param <T> Class type of the elements in the {@link Enumeration}.
   * @param enumeration {@link Enumeration} to convert into an array.
   * @param componentType {@link Class} type of the elements in the {@link Enumeration}.
   * @return an array containing all the elements from the {@link Enumeration}.  Returns an empty array
   * if the {@link Enumeration} is null or empty.
   * @see java.util.Enumeration
   * @see #asArray(Iterable, Class)
   */
  @NullSafe
  public static <T> T[] asArray(Enumeration<T> enumeration, Class<T> componentType) {
    return asArray(CollectionUtils.asIterable(enumeration), componentType);
  }

  /**
   * Converts the {@link Iterable} into an array.
   *
   * @param <T> Class type of the elements in the {@link Iterable}.
   * @param iterable {@link Iterable} collection to convert into an array.
   * @param componentType {@link Class} type of the elements in the {@link Iterable}.
   * @return an array containing all the elements from the {@link Iterable} collection.  Returns an empty array
   * if the {@link Iterable} is null or empty.
   * @see java.lang.Class
   * @see java.lang.Iterable
   */
  @NullSafe
  @SuppressWarnings("unchecked")
  public static <T> T[] asArray(Iterable<T> iterable, Class<T> componentType) {

    List<T> arrayList = new ArrayList<>();

    for (T element : CollectionUtils.nullSafeIterable(iterable)) {
      arrayList.add(element);
    }

    return arrayList.toArray((T[]) Array.newInstance(defaultIfNull(componentType, Object.class), arrayList.size()));
  }

  /**
   * Converts the {@link Iterator} into an array.
   *
   * @param <T> Class type of the elements in the {@link Iterator}.
   * @param iterator {@link Iterator} to convert into an array.
   * @param componentType {@link Class} type of the elements in the {@link Iterator}.
   * @return an array containing all the elements from the {@link Iterator}.  Returns an empty array
   * if the {@link Iterator} is null or empty.
   * @see java.util.Iterator
   * @see #asArray(Iterable, Class)
   */
  @NullSafe
  public static <T> T[] asArray(Iterator<T> iterator, Class<T> componentType) {
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
  public static <T> Enumeration<T> asEnumeration(T... array) {

    return (array == null ? Collections.emptyEnumeration() : new Enumeration<T>() {

      private int index = 0;

      @Override
      public boolean hasMoreElements() {
        return (index < array.length);
      }

      @Override
      public T nextElement() {

        Assert.isTrue(hasMoreElements(), new NoSuchElementException("No more elements"));

        return array[index++];
      }
    });
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
  public static <T> Iterable<T> asIterable(T... array) {
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
  public static <T> Iterator<T> asIterator(T... array) {

    return (array == null ? Collections.emptyIterator() : new Iterator<T>() {

      private int index = 0;

      @Override
      public boolean hasNext() {
        return (index < array.length);
      }

      @Override
      public T next() {

        Assert.isTrue(hasNext(), new NoSuchElementException("No more elements"));

        return array[index++];
      }
    });
  }

  /**
   * Counts the number of elements in the array.
   *
   * @param <T> Class type of the elements in the array.
   * @param array array to evaluate.
   * @return an integer value indicating the number of elements in the array (a.k.a. size).
   */
  @NullSafe
  public static <T> int count(T[] array) {
    return array != null ? array.length : 0;
  }

  /**
   * Counts the number of elements in the array matching the criteria (rules) defined by the {@link Filter}.
   *
   * @param <T> Class type of the elements in the array.
   * @param array array to search.
   * @param filter {@link Filter} used to match elements in the array and tally the count.
   * @return an integer value indicating the number of elements in the array matching the criteria (rules)
   * defined by the {@link Filter}.
   * @throws IllegalArgumentException if {@link Filter} is null.
   * @see org.cp.elements.lang.Filter
   */
  public static <T> long count(T[] array, Filter<T> filter) {

    Assert.notNull(filter, "Filter is required");

    return stream(nullSafeArray(array)).filter(filter::accept).count();
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
  @SuppressWarnings("unchecked")
  public static <T> T[] deepCopy(T[] array) {

    Assert.notNull(array, "Array is required");

    return deepCopy(array, element -> element != null ? ObjectUtils.clone(element) : null);
  }

  /**
   * Deeply copies the given array into a new array of the same {@link Class type}.
   *
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
  public static <T> T[] deepCopy(T[] array, Function<T, T> copyFunction) {

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
  public static <T> T[] defaultIfEmpty(T[] array, T[] defaultArray) {
    return isNotEmpty(array) ? array : defaultArray;
  }

  /**
   * Returns an empty array.
   *
   * @return an empty array with no capacity for elements.
   * @see #EMPTY_ARRAY
   */
  @NullSafe
  public static Object[] emptyArray() {
    return EMPTY_ARRAY.clone();
  }

  /**
   * Filters the elements in the array.
   *
   * @param <T> Class type of the elements in the array.
   * @param array array to filter.
   * @param filter {@link Filter} used to filter the array elements.
   * @return a new array of the array class component type containing only elements from the given array
   * accepted by the {@link Filter}.
   * @throws IllegalArgumentException if either the array or {@link Filter} are null.
   * @see #filterAndTransform(Object[], FilteringTransformer)
   * @see org.cp.elements.lang.Filter
   */
  @SuppressWarnings("unchecked")
  public static <T> T[] filter(T[] array, Filter<T> filter) {

    Assert.notNull(array, "Array is required");
    Assert.notNull(filter, "Filter is required");

    List<T> arrayList = stream(array).filter(filter::accept).collect(Collectors.toList());

    return arrayList.toArray((T[]) Array.newInstance(array.getClass().getComponentType(), arrayList.size()));
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
   * @see #filter(Object[], Filter)
   * @see #transform(Object[], Transformer)
   */
  @SuppressWarnings("unchecked")
  public static <T> T[] filterAndTransform(T[] array, FilteringTransformer<T> filteringTransformer) {
    return transform(filter(array, filteringTransformer), filteringTransformer);
  }

  /**
   * Searches the array returning all elements accepted by the {@link Filter}.
   *
   * @param <T> Class type of the elements in the array.
   * @param array array to search.
   * @param filter {@link Filter} used to find all elements in the array accepted by the {@link Filter}.
   * @return a {@link List} containing all the elements from the array accepted by the {@link Filter}
   * or an empty {@link List} if no elements were found.
   * @throws IllegalArgumentException if {@link Filter} is null.
   * @see org.cp.elements.lang.Filter
   * @see #findOne(Object[], Filter)
   */
  @SuppressWarnings("unchecked")
  public static <T> List<T> findAll(T[] array, Filter<T> filter) {

    Assert.notNull(filter, "Filter is required");

    return stream(nullSafeArray(array)).filter(filter::accept).collect(Collectors.toList());
  }

  /**
   * Searches the array for the first element accepted by the {@link Filter}.
   *
   * @param <T> Class type of the elements in the array.
   * @param array array to search.
   * @param filter {@link Filter} used to find the first element in the array accepted by the {@link Filter}.
   * @return the first element from the array accepted by the {@link Filter} or null if no such element is found.
   * @throws IllegalArgumentException if {@link Filter} is null.
   * @see org.cp.elements.lang.Filter
   * @see #findAll(Object[], Filter)
   */
  public static <T> T findOne(T[] array, Filter<T> filter) {

    Assert.notNull(filter, "Filter is required");

    return stream(nullSafeArray(array)).filter(filter::accept).findFirst().orElse(null);
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
  public static <T> T getElementAt(T[] array, int index) {
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
  public static <T> T getElementAt(T[] array, int index, T defaultValue) {
    return nullSafeLength(array) > index ? array[index] : defaultValue;
  }

  /**
   * Returns the first element (at index 0) in the {@code array}.
   *
   * @param <T> {@link Class} type of elements in the array.
   * @param array array from which to extract the first element.
   * @return the first element in the array or {@literal null}
   * if the {@code array} is {@literal null} or empty.
   * @see #getFirst(Object[], Object)
   */
  @NullSafe
  @SafeVarargs
  public static <T> T getFirst(T... array) {
    return getFirst(array, null);
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
  public static <T> T getFirst(T[] array, T defaultValue) {
    return getElementAt(array, 0, defaultValue);
  }

  /**
   * Returns the last element (at index array.length - 1) in the {@code array}.
   *
   * @param <T> {@link Class} type of elements in the array.
   * @param array array from which to extract the last element.
   * @return the last element in the array or {@literal null} if the {@code array} is {@literal null} or empty.
   * @see #getLast(Object[], Object)
   */
  @NullSafe
  @SafeVarargs
  public static <T> T getLast(T... array) {
    return getLast(array, null);
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
  public static <T> T getLast(T[] array, T defaultValue) {
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
  public static <T> int indexOf(T[] array, T element) {

    for (int index = 0, length = nullSafeLength(array); index < length; index++) {
      if (equalsIgnoreNull(array[index], element)) {
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
  public static <T> T[] insert(T element, T[] array, int index) {

    Assert.notNull(array, "Array is required");

    assertThat(index).throwing(new ArrayIndexOutOfBoundsException(
      String.format("[%1$d] is not a valid index [0, %2$d] in the array", index, array.length)))
        .isGreaterThanEqualToAndLessThanEqualTo(0, array.length);

    Class<?> componentType = array.getClass().getComponentType();

    componentType = defaultIfNull(componentType, ObjectUtils.getClass(element));
    componentType = defaultIfNull(componentType, Object.class);

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
  public static boolean isEmpty(Object[] array) {
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
  public static boolean isNotEmpty(Object[] array) {
    return !isEmpty(array);
  }

  /**
   * Null-safe operation to determine whether the given array has a length of 1.
   *
   * @param array array to evaluate.
   * @return a boolean value indicating whether the length of the array is 1.
   */
  @NullSafe
  public static boolean isSizeOne(Object... array) {
    return array != null && array.length == 1;
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
  public static <T> T[] nullSafeArray(T[] array) {
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
  public static <T> T[] nullSafeArray(T[] array, Class<?> componentType) {
    return array != null ? array : (T[]) Array.newInstance(defaultIfNull(componentType, Object.class), 0);
  }

  /* non-Javadoc */
  @NullSafe
  @SuppressWarnings("unchecked")
  static <T> Class<?> componentType(T[] array) {
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
  public static <T> int nullSafeLength(T[] array) {
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
  public static <T> T[] prepend(T element, T[] array) {
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
  public static <T> T[] remove(T[] array, int index) {

    Assert.notNull(array, "Array is required");

    assertThat(index).throwing(new ArrayIndexOutOfBoundsException(
      String.format("[%1$d] is not a valid index [0, %2$d] in the array", index, array.length)))
        .isGreaterThanEqualToAndLessThan(0, array.length);

    Class<?> componentType = defaultIfNull(array.getClass().getComponentType(), Object.class);

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
  public static <T> T[] shallowCopy(T[] array) {

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
  @NullSafe
  public static <T> T[] shuffle(T[] array) {

    if (isNotEmpty(array)) {

      Random random = new Random(System.currentTimeMillis());

      for (int index = 0, lengthMinusOne = array.length - 1; index < lengthMinusOne; index++) {

        int randomIndex = (random.nextInt(lengthMinusOne - index) + 1);

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
  public static <T extends Comparable<T>> T[] sort(T[] array) {
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
  public static <T> T[] sort(T[] array, Comparator<T> comparator) {

    Arrays.sort(array, comparator);

    return array;
  }

  /**
   * Creates a sub-array from the given array with elements at the specified indices in the given array.
   *
   * @param <T> the Class type of the elements in the array.
   * @param array the source array.
   * @param indices an array of indexes to elements in the given array to include in the sub-array.
   * @return a sub-array from the given array with elements at the specified indices in the given array.
   * @throws ArrayIndexOutOfBoundsException if the indices are not valid indexes in the array.
   * @throws NullPointerException if either the array or indices are null.
   */
  @SuppressWarnings("unchecked")
  public static <T> T[] subArray(T[] array, int... indices) {

    List<T> subArrayList = new ArrayList<>(indices.length);

    for (int index : indices) {
      subArrayList.add(array[index]);
    }

    return subArrayList.toArray((T[]) Array.newInstance(array.getClass().getComponentType(), subArrayList.size()));
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
  public static <T> T[] swap(T[] array, int indexOne, int indexTwo) {

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
  public static String[] toStringArray(Object... array) {

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
  public static <T> T[] transform(T[] array, Transformer<T> transformer) {

    Assert.notNull(array, "Array is required");
    Assert.notNull(transformer, "Transformer is required");

    for (int index = 0; index < array.length; index++) {
      array[index] = transformer.transform(array[index]);
    }

    return array;
  }
}
