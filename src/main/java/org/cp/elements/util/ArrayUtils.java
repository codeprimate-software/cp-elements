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

package org.cp.elements.util;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.FilteringTransformer;
import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.RelationalOperator;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.Transformer;

/**
 * The ArrayUtils class encapsulates utility methods for interacting with Object arrays.
 * 
 * @author John J. Blum
 * @see java.lang.Iterable
 * @see java.lang.reflect.Array
 * @see java.util.ArrayList
 * @see java.util.Enumeration
 * @see java.util.Iterator
 * @see java.util.List
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.FilteringTransformer
 * @see org.cp.elements.lang.Transformer
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ArrayUtils {

  private static final Object[] EMPTY_ARRAY = new Object[0];

  /**
   * Adds (inserts) the element at the end of the array.
   * 
   * @param <T> the type of elements stored in the array.
   * @param element the element to insert into the end of the array.
   * @param array the array in which to insert the element.
   * @return a new array with the element inserted at the end.
   * @see #insert(Object, Object[], int)
   */
  public static <T> T[] append(final T element, final T[] array) {
    return insert(element, array, length(array));
  }

  /**
   * Convenience method for specifying an array.
   *
   * @param <T> the type of elements stored in the array.
   * @param array the array.
   * @return the array as itself.
   */
  public static <T> T[] asArray(T... array) {
    return array;
  }

  /**
   * Converts the Iterable object into an array.
   *
   * @param <T> the type of elements in the Iterable as well as the resulting array.
   * @param iterable the Iterable object to convert into an array.
   * @return an array containing the elements from the Iterable object.  Returns an empty array
   * if the Iterable object reference is null.
   * @see java.lang.Iterable
   */
  @NullSafe
  @SuppressWarnings("unchecked")
  public static <T> T[] asArray(final Iterable<T> iterable, final Class<T> componentType) {
    List<T> arrayList = new ArrayList<T>();

    if (iterable != null) {
      for (T element : iterable) {
        arrayList.add(element);
      }
    }

    return arrayList.toArray((T[]) Array.newInstance(ObjectUtils.defaultIfNull(componentType, Object.class),
      arrayList.size()));
  }

  /**
   * Counts the number of elements from the Object array matching the criteria defined by the Filter.
   * 
   * @param <T> the Class type of the elements in the Object array.
   * @param array the Object array of elements to search and count.
   * @param filter the Filter used to find matching elements from the Object array and tally the count.
   * @return an integer value of the number of elements from the Object array matching the criteria defined
   * by the Filter.
   * @throws NullPointerException if either the Object array or Filter are null.
   * @see #findAll(Object[], org.cp.elements.lang.Filter)
   * @see org.cp.elements.lang.Filter
   */
  public static <T> int count(final T[] array, final Filter<T> filter) {
    return findAll(array, filter).length;
  }

  /**
   * Gets an empty Object array (an Object array containing no elements nor buckets for elements).
   * 
   * @return an empty Object array with no capacity for elements.
   */
  public static Object[] emptyArray() {
    return EMPTY_ARRAY.clone();
  }

  /**
   * Gets the Object array if not null otherwise returns an empty Object array.
   * 
   * @param array the Object array reference being tested with a null check.
   * @return the Object array if not null otherwise returns an empty Object array.
   * @see #emptyArray()
   */
  public static Object[] emptyArray(final Object... array) {
    return (array != null ? array : emptyArray());
  }

  /**
   * Gets an Enumeration to enumerate over the elements in the specified Object array.
   * 
   * @param array the Object array backing the Enumeration implementation.
   * @param <T> the Class type of the elements in the Object array.
   * @return an Enumeration over the elements in the Object array.
   * @throws NullPointerException if the reference to the Object array is null.
   * @see java.util.Enumeration
   */
  public static <T> Enumeration<T> enumeration(final T... array) {
    Assert.notNull(array, "The array of elements cannot be null!");

    return new Enumeration<T>() {
      private int index = 0;

      public boolean hasMoreElements() {
        return (index < array.length);
      }

      public T nextElement() {
        Assert.isTrue(hasMoreElements(), new NoSuchElementException(
          "The Enumeration has reached the end of the array!"));
        return array[index++];
      }
    };
  }

  /**
   * Filters the elements of the given Object array using the specified Filter.  Any element not accepted by the Filter
   * is set to null at index in the array.
   *
   * @param <T> the Class type of the array elements.
   * @param array the Object array to filter using the specified Filter.
   * @param filter the Filter used to filter the array elements.
   * @return the Object array with the elements filtered.
   * @throws java.lang.NullPointerException if the Object array or Filter references are null.
   * @see org.cp.elements.lang.Filter
   */
  public static <T> T[] filter(final T[] array, final Filter<T> filter) {
    Assert.notNull(array, "The Object array to filter cannot be null!");
    Assert.notNull(filter, "The Filter used to filter the array cannot be null!");

    int index = 0;

    for (T element : array) {
      array[index] = (filter.accept(element) ? array[index] : null);
      index++;
    }

    return array;
  }

  /**
   * Filters and transforms the elements of the given array using the FilteringTransformer.
   *
   * @param <T> the Class type of the array elements.
   * @param array the Object array to filter and transform.
   * @param filteringTransformer the FilteringTransformer used to filter and transform elements in the array.
   * @return the array of elements filtered and transformed.
   * @throws java.lang.NullPointerException if the Object array or FilteringTransformer references are null.
   * @see org.cp.elements.lang.FilteringTransformer
   */
  public static <T> T[] filterAndTransform(final T[] array, final FilteringTransformer<T> filteringTransformer) {
    Assert.notNull(array, "The Object array to filter and then transform the elements of cannot be null!");
    Assert.notNull(filteringTransformer, "The FilteringTransformer used to filter and transform the array elements cannot be null!");

    int index = 0;

    for (T element : array) {
      array[index] = (filteringTransformer.accept(element) ? filteringTransformer.transform(element) : null);
      index++;
    }

    return array;
  }

  /**
   * Searches the Object array for the first element matching the criteria defined by the Filter.
   * 
   * @param <T> the Class type of the elements in the Object array.
   * @param array the Object array of elements to search.
   * @param filter the Filter defining the criteria used to find the first matching element from the Object array.
   * @return the first element from the Object array matching the criteria defined by the Filter or null
   * if no such element is found.
   * @throws NullPointerException if either the Object array or Filter is null.
   * @see java.lang.Iterable
   * @see org.cp.elements.lang.Filter
   */
  public static <T> T find(final T[] array, final Filter<T> filter) {
    Assert.notNull(array, "The Object array from which to find the first element accepted by the Filter cannot be null!");
    Assert.notNull(filter, "The Filter used to find a matching element from the Object array cannot be null!");

    T matchingElement = null;

    for (T element : array) {
      if (filter.accept(element)) {
        matchingElement = element;
        break;
      }
    }

    return matchingElement;
  }

  /**
   * Searches the Object array for elements matching the criteria defined by the Filter.
   * 
   * @param <T> the Class type of the elements in the Object array.
   * @param array the Object array of elements to search.
   * @param filter the Filter defining the criteria used to find and match elements from the Object array.
   * @return an Object array of the same type consisting of elements from the originating Object array matching
   * the criteria defined by the Filter.
   * @throws NullPointerException if either the Object array or Filter is null.
   * @see org.cp.elements.lang.Filter
   */
  @SuppressWarnings("unchecked")
  public static <T> T[] findAll(final T[] array, final Filter<T> filter) {
    Assert.notNull(array, "The Object array from which to find elements accepted by the Filter cannot be null!");
    Assert.notNull(filter, "The Filter used to find elements from the Object array cannot be null!");

    List<T> arrayList = new ArrayList<T>(array.length);

    for (T element : array) {
      if (filter.accept(element)) {
        arrayList.add(element);
      }
    }

    ((ArrayList) arrayList).trimToSize();

    return arrayList.toArray((T[]) Array.newInstance(array.getClass().getComponentType(), arrayList.size()));
  }

  /**
   * Gets the first element in the array (at position 0).
   *
   * @param <T> the Class type of the elements in the array.
   * @param array the array from which to extract the first element.
   * @return the first element of the array or null if the array is null or empty.
   * @see #isEmpty(Object[])
   */
  public static <T> T getFirst(final T... array) {
    return (isEmpty(array) ? null : array[0]);
  }

  /**
   * Inserts element into the array at index.
   * 
   * @param <T> the type of elements stored in the array.
   * @param element the element to insert into the array.
   * @param array the array used to insert the element.
   * @param index the integer index at which the element will be inserted into the array.
   * @return a new array with the element inserted at index.
   */
  @SuppressWarnings("unchecked")
  public static <T> T[] insert(final T element, final T[] array, final int index) {
    Assert.notNull(array, "The array to insert the element into cannot be null!");

    Assert.isTrue(RelationalOperator.greaterThanEqualToAndLessThan(0, array.length + 1).evaluate(index),
      new ArrayIndexOutOfBoundsException(String.format("The index (%1$s) is not valid array index [0, %2$s]!",
        index, array.length)));

    Class<?> componentType = array.getClass().getComponentType();

    componentType = (componentType != null ? componentType : element.getClass());

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
   * Determines whether the specified Object array is empty.  An array of Objects is empty if the Object array
   * reference is null, or Object array contains no elements (length of zero).
   * 
   * @param array the Object array being tested as empty.
   * @return a boolean value indicating whether the specified Object array is empty.
   * @see #length(Object[])
   */
  public static boolean isEmpty(final Object[] array) {
    return (length(array) == 0);
  }

  /**
   * Gets an Iterable over the elements in the specified Object array.
   * 
   * @param <T> the Class type of element in the Object array.
   * @param array the Object array backing the Iterable implementation.
   * @return an Iterable object over the elements in the array.
   * @throws NullPointerException if the array is null.
   * @see #iterator(Object[])
   * @see java.lang.Iterable
   */
  public static <T> Iterable<T> iterable(final T... array) {
    Assert.notNull(array, "The array of elements cannot be null!");

    return new Iterable<T>() {
      @Override public Iterator<T> iterator() {
        return ArrayUtils.iterator(array);
      }
    };
  }

  /**
   * Gets an Iterator to iterate over the elements in the specified Object array.
   * 
   * @param array the Object array backing the Iterator implementation.
   * @param <T> the Class type of the elements in the Object array.
   * @return an Iterator over the elements in the Object array.
   * @throws NullPointerException if the reference to the Object array is null.
   * @see java.util.Iterator
   */
  public static <T> Iterator<T> iterator(final T... array) {
    Assert.notNull(array, "The array of elements cannot be null!");

    return new Iterator<T>() {
      private int index = 0;

      public boolean hasNext() {
        return (index < array.length);
      }

      public T next() {
        Assert.isTrue(hasNext(), new NoSuchElementException("The Iterator has reached the end of the array!"));
        return array[index++];
      }

      public void remove() {
        throw new UnsupportedOperationException(StringUtils.NOT_IMPLEMENTED);
      }
    };
  }

  /**
   * Determines the length, or size of the Object array.  The size of the Object array is determined by it's length
   * if non-null, otherwise 0 is returned for null Object arrays.
   * 
   * @param array the Object array who's length, or size is being determined.
   * @return an integer value specifying the length, or size of the Object array.
   */
  public static int length(final Object[] array) {
    return (array == null ? 0 : array.length);
  }

  /**
   * Prepends (insert) element at the beginning of the array.
   * 
   * @param <T> the type of elements stored in the array.
   * @param element the element to insert into the array.
   * @param array the array used to insert the element.
   * @return a new array with the element inserted at the beginning.
   * @see #insert(Object, Object[], int)
   */
  public static <T> T[] prepend(final T element, T[] array) {
    return insert(element, array, 0);
  }

  /**
   * Creates a sub-array from the given array with the values at the specified indices in the given array.
   *
   * @param <T> the Class type of the array elements.
   * @param array the original Object array from which to create the sub-array.
   * @param indices the indices of values from the original array to include in the sub-array.
   * @return a sub-array from the given array with the values at the specified indices in the given array.
   */
  @SuppressWarnings("unchecked")
  public static <T> T[] subArray(final T[] array, int... indices) {
    List<T> subArray = new ArrayList<T>(indices.length);

    for (int index : indices) {
      subArray.add(array[index]);
    }

    return subArray.toArray((T[]) Array.newInstance(getComponentType(array), indices.length));
  }

  /*
   * non-Javadoc
   */
  @SuppressWarnings("unchecked")
  private static Class<?> getComponentType(final Object[] array) {
    Class<?> type = (!isEmpty(array) ? ClassUtils.getClass(array[0]) : null);
    return ObjectUtils.defaultIfNull(type, Object.class);
  }

  /**
   * Transforms the elements in the given Object array using the specified Transformer.
   *
   * @param <T> the Class type of the array elements.
   * @param array the Object array who's elements will be transformed by the specified Transformer.
   * @param transformer the Transformer used to transform the elements in the array.
   * @return the Object array with it's elements transformed.
   * @throws java.lang.NullPointerException if the Object array or Transformer references are null.
   * @see org.cp.elements.lang.Transformer
   */
  public static <T> T[] transform(final T[] array, final Transformer<T> transformer) {
    Assert.notNull(array, "The Object array who's elements will be transformed cannot be null!");
    Assert.notNull(transformer, "The Transformer used to transform the array elements cannot be null!");

    int index = 0;

    for (T element : array) {
      array[index++] = transformer.transform(element);
    }

    return array;
  }

}
