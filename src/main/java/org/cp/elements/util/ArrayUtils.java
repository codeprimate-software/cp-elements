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

package org.cp.elements.util;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.RelationalOperator;
import org.cp.elements.lang.StringUtils;

/**
 * The ArrayUtils class provides utility methods for working with Object arrays.
 * <p/>
 * @author John J. Blum
 * @since 1.0.0
 * @see java.lang.reflect.Array
 * @see java.util.ArrayList
 * @see java.util.Enumeration
 * @see java.util.Iterator
 * @see java.util.List
 * @see org.cp.elements.lang.Filter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ArrayUtils {

  private static final Object[] EMPTY_ARRAY = new Object[0];

  /**
   * Adds (insert) element at the end of the array.
   * <p/>
   * @param <T> the type of elements stored in the array.
   * @param element the element to insert into the array.
   * @param array the array used to insert the element.
   * @return a new array with the element inserted at the end.
   * @see #insert(Object, Object[], int)
   */
  public static <T> T[] add(final T element, final T[] array) {
    return insert(element, array, length(array));
  }

  /**
   * Convenience method for specifying an array.
   * <p/>
   * @param <T> the type of elements stored in the array.
   * @param array the array.
   * @return the array as itself.
   */
  public static <T> T[] asArray(T... array) {
    return array;
  }

  /**
   * Counts the number of elements from the Object array matching the criteria defined by the Filter.
   * <p/>
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
   * <p/>
   * @return an empty Object array with no capacity for elements.
   */
  public static Object[] emptyArray() {
    return EMPTY_ARRAY.clone();
  }

  /**
   * Gets the Object array if not null otherwise returns an empty Object array.
   * <p/>
   * @param array the Object array reference being tested with a null check.
   * @return the Object array if not null otherwise returns an empty Object array.
   * @see #emptyArray()
   */
  public static Object[] emptyArray(final Object... array) {
    return (array != null ? array : emptyArray());
  }

  /**
   * Gets an Enumeration to enumerate over the elements in the specified Object array.
   * <p/>
   * @param array the Object array backing the Enumeration implementation.
   * @param <T> the Class type of the elements in the Object array.
   * @return an Enumeration over the elements in the Object array.
   * @throws NullPointerException if the reference to the Object array is null.
   * @see java.util.Enumeration
   */
  public static <T>Enumeration<T> enumeration(final T... array) {
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
   * Searches the Object array for the first element matching the criteria defined by the Filter.
   * <p/>
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

    for (final T element : array) {
      if (filter.accept(element)) {
        matchingElement = element;
        break;
      }
    }

    return matchingElement;
  }

  /**
   * Searches the Object array for elements matching the criteria defined by the Filter.
   * <p/>
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

    final List<T> arrayList = new ArrayList<T>(array.length);

    for (final T element : array) {
      if (filter.accept(element)) {
        arrayList.add(element);
      }
    }

    ((ArrayList) arrayList).trimToSize();

    return arrayList.toArray((T[]) Array.newInstance(array.getClass().getComponentType(), arrayList.size()));
  }

  /**
   * Inserts element into the array at index.
   * <p/>
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

    final T[] newArray = (T[]) Array.newInstance(componentType, array.length + 1);

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
   * <p/>
   * @param array the Object array being tested as empty.
   * @return a boolean value indicating whether the specified Object array is empty.
   * @see #length(Object[])
   */
  public static boolean isEmpty(final Object[] array) {
    return (length(array) == 0);
  }

  /**
   * Gets an Iterable over the elements in the specified Object array.
   * <p/>
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
   * <p/>
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
   * <p/>
   * @param array the Object array who's length, or size is being determined.
   * @return an integer value specifying the length, or size of the Object array.
   */
  public static int length(final Object[] array) {
    return (array == null ? 0 : array.length);
  }

  /**
   * Prepends (insert) element at the beginning of the array.
   * <p/>
   * @param <T> the type of elements stored in the array.
   * @param element the element to insert into the array.
   * @param array the array used to insert the element.
   * @return a new array with the element inserted at the beginning.
   * @see #insert(Object, Object[], int)
   */
  public static <T> T[] prepend(final T element, T[] array) {
    return insert(element, array, 0);
  }

}
