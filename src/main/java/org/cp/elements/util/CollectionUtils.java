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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.Renderer;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.support.ToStringRenderer;

/**
 * The CollectionUtils class provides utility methods for working with the Java Collections Framework
 * and specifically the Collection classes.
 * <p/>
 * @author John J. Blum
 * @since 1.0.0
 * @see java.lang.Iterable
 * @see java.util.Collection
 * @see java.util.Collections
 * @see java.util.Enumeration
 * @see java.util.Iterator
 * @see java.util.List
 * @see java.util.Set
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.Renderer
 * @see org.cp.elements.lang.support.ToStringRenderer
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class CollectionUtils {

  /**
   * Counts the number of elements from the Iterable collection matching the criteria defined by the Filter.
   * <p/>
   * @param <T> the Class type of the elements in the Iterable collection.
   * @param collection the Iterable collection of elements to search and count.
   * @param filter the Filter used to find matching elements from the Iterable collection and tally the count.
   * @return an integer value of the number of elements from the Iterable collection matching the criteria
   * defined by the Filter.
   * @throws NullPointerException if either the Iterable collection or Filter are null.
   * @see #findAll(Iterable, org.cp.elements.lang.Filter)
   * @see java.lang.Iterable
   * @see org.cp.elements.lang.Filter
   */
  public static <T> int count(final Iterable<T> collection, final Filter<T> filter) {
    return findAll(collection, filter).size();
  }

  /**
   * Gets the List if not null or returns an empty List.
   * <p/>
   * @param list the List reference being tested with a null check.
   * @param <T> the Class type of the List elements.
   * @return the List if not null otherwise return an empty List.
   * @see java.util.Collections#emptyList()
   * @see java.util.List
   */
  public static <T> List<T> emptyList(final List<T> list) {
    return (list != null ? list : Collections.<T>emptyList());
  }

  /**
   * Gets the Set if not null or returns an empty Set.
   * <p/>
   * @param set the Set reference being tested with a null check.
   * @param <T> the Class type of the Set elements.
   * @return the Set if not null otherwise return an empty Set.
   * @see java.util.Collections#emptySet()
   * @see java.util.Set
   */
  public static <T> Set<T> emptySet(final Set<T> set) {
    return (set != null ? set : Collections.<T>emptySet());
  }

  /**
   * Adapts the specified Iterator into an instance of the Enumeration interface.
   * <p/>
   * @param it the Iterator to adapt into an Enumeration.
   * @param <T> the Class type of the elements to enumerate.
   * @return an Enumeration implementation backed by the specified Iterator.
   * @throws NullPointerException if the Iterator is null.
   * @see java.util.Enumeration
   * @see java.util.Iterator
   */
  public static <T> Enumeration<T> enumeration(final Iterator<T> it) {
    Assert.notNull(it, "The Iterator backing the Enumeration cannot be null!");

    return new Enumeration<T>() {
      public boolean hasMoreElements() {
        return it.hasNext();
      }

      public T nextElement() {
        Assert.isTrue(hasMoreElements(), new NoSuchElementException(
          "The Enumeration has reached the end of the iteration!"));
        return it.next();
      }
    };
  }

  /**
   * Filters the Collection of elements retaining only those elements matching the criteria defined by the Filter.
   * <p/>
   * @param <T> the Class type of the elements in the Collection.
   * @param collection the Collection of elements to filter.
   * @param filter the Filter used to filter the Collection of elements.
   * @return the original Collection with only those elements retained that match the criteria defined by the Filter.
   * @throws NullPointerException if either the Collection of Filter are null.
   * @see #findAll(Iterable, org.cp.elements.lang.Filter)
   * @see java.util.Collection#retainAll(java.util.Collection)
   * @see org.cp.elements.lang.Filter
   */
  public static <T> Collection<T> filter(final Collection<T> collection, final Filter<T> filter) {
    Assert.notNull(collection, "The Collection of elements to filter cannot be null!");
    Assert.notNull(filter, "The Filter used to filter the Collection of elements cannot be null!");

    collection.retainAll(findAll(collection, filter));

    return collection;
  }

  /**
   * Searches the Iterable collection for the first element matching the criteria defined by the Filter.
   * <p/>
   * @param <T> the Class type of the elements in the Iterable collection.
   * @param collection the Iterable collection of elements to search.
   * @param filter the Filter defining the criteria used to find the first matching element from the Iterable collection.
   * @return the first element from the Iterable collection matching the criteria defined by the Filter or null
   * if no such element is found.
   * @throws NullPointerException if either the Iterable collection or the Filter is null.
   * @see java.lang.Iterable
   * @see org.cp.elements.lang.Filter
   */
  public static <T> T find(final Iterable<T> collection, final Filter<T> filter) {
    Assert.notNull(collection, "The Iterable collection from which to find the first matching element cannot be null!");
    Assert.notNull(filter, "The Filter used to find one matching element from the Iterable collection cannot be null!");

    T matchingElement = null;

    for (final T element : collection) {
      if (filter.accept(element)) {
        matchingElement = element;
        break;
      }
    }

    return matchingElement;
  }

  /**
   * Searches the Iterable collection for elements matching the criteria defined by the Filter.
   * <p/>
   * @param <T> the Class type of the elements in the Iterable collection.
   * @param collection the Iterable collection of elements to search.
   * @param filter the Filter defining the criteria used to find matching elements from the Iterable collection.
   * @return a List consisting of elements from the original Iterable collection matching the criteria defined
   * by the Filter.  Elements are returned in the order they are found.
   * @throws NullPointerException if either the Iterable collection or Filter is null.
   * @see java.lang.Iterable
   * @see org.cp.elements.lang.Filter
   */
  public static <T> List<T> findAll(final Iterable<T> collection, final Filter<T> filter) {
    Assert.notNull(collection, "The Iterable collection from which to find elements cannot be null!");
    Assert.notNull(filter, "The Filter used to find elements from the Iterable collection cannot be null!");

    final List<T> matchingElements = new ArrayList<T>();

    for (final T element : collection) {
      if (filter.accept(element)) {
        matchingElements.add(element);
      }
    }

    return matchingElements;
  }

  /**
   * Determines whether the specified Collection is empty.  A Collection is empty if it contains no elements
   * or the specified Collection object reference is null.
   * <p/>
   * @param collection the Collection being tested as empty.
   * @return a boolean value indicating whether the specified Collection is empty.
   * @see java.util.Collection#isEmpty()
   */
  public static boolean isEmpty(final Collection collection) {
    return (collection == null || collection.isEmpty());
  }

  /**
   * Adapts the specified Iterator into an instance of the Iterable interface.
   * <p/>
   * @param iterator the Iterator backing the Iterable implementation.
   * @param <T> the Class type of elements iterated by the Iterator.
   * @return an Iterable implementation backed by the Iterator.
   * @throws NullPointerException if the Iterator is null.
   * @see java.lang.Iterable
   * @see java.util.Iterator
   */
  public static <T> Iterable<T> iterable(final Iterator<T> iterator) {
    Assert.notNull(iterator, "The Iterator backing the Iterable implementation cannot be null!");

    return new Iterable<T>() {
      public Iterator<T> iterator() {
        return iterator;
      }
    };
  }

  /**
   * Adapts the specified Enumeration into an instance of the Iterator interface.
   * <p/>
   * @param enumeration the Enumeration backing the Iterator implementation.
   * @param <T> the Class type of the elements to iterate.
   * @return an Iterator implementation backed by the specified Enumeration.
   * @throws NullPointerException if the Enumeration is null.
   * @see java.util.Enumeration
   * @see java.util.Iterator
   */
  public static <T> Iterator<T> iterator(final Enumeration<T> enumeration) {
    Assert.notNull(enumeration, "The Enumeration backing the Iterator cannot be null!");

    return new Iterator<T>() {
      public boolean hasNext() {
        return enumeration.hasMoreElements();
      }

      public T next() {
        Assert.isTrue(hasNext(), new NoSuchElementException(
          "The Iterator has reached the end of the enumeration!"));
        return enumeration.nextElement();
      }

      public void remove() {
        throw new UnsupportedOperationException(StringUtils.NOT_IMPLEMENTED);
      }
    };
  }

  /**
   * Determines the size of the specified Collection.  If the Collection is null or contains no elements, then the
   * size of the Collection is 0, otherwise the size of the Collection is determined by it's size() method.
   * <p/>
   * @param collection the Collection who's size is being determined.
   * @return an integer value specifying the size of the Collection.
   * @see java.util.Collection#size()
   */
  public static int size(final Collection collection) {
    return (collection == null ? 0 : collection.size());
  }

  /**
   * Gets a String representation of the Iterable collection.
   * <p/>
   * @param <T> the class type of elements in the Iterable collection.
   * @param collection the Iterable collection of elements to render as a String.
   * @return a String representation of the Iterable collection.
   * @see java.lang.Iterable
   */
  public static <T> String toString(final Iterable<T> collection) {
    return toString(collection, new ToStringRenderer<T>());
  }

  /**
   * Gets a String representation of the Iterable collection using the specified Renderer to render the elements.
   * <p/>
   * @param <T> the class type of elements in the Iterable collection.
   * @param collection the Iterable collection of elements to render as a String.
   * @param renderer the Renderer used to render the elements in the Iterable collection as String.
   * @return a String representation of the Iterable collection.
   * @see java.lang.Iterable
   * @see org.cp.elements.lang.Renderer
   */
  public static <T> String toString(final Iterable<T> collection, final Renderer<T> renderer) {
    final StringBuilder buffer = new StringBuilder("[");
    int count = 0;

    for (final T element : collection) {
      buffer.append(count++ > 0 ? ", " : StringUtils.EMPTY_STRING).append(renderer.render(element));
    }

    buffer.append("]");

    return buffer.toString();
  }

  /**
   * Gets an anonymous, read-only Iterator implementation wrapping the specified Iterator to prevent modifications
   * through invocations of the Iterator's remove method.
   * <p/>
   * @param iterator the Iterator to wrap in a read-only implementation of the Iterator interface.
   * @param <T> the Class type of the objects for which the Iterator iterates.
   * @return am anonymous, read-only Iterator implementation wrapping the specified Iterator.
   * @throws NullPointerException if the Iterator is null.
   * @see java.util.Iterator
   */
  public static <T> Iterator<T> unmodifiableIterator(final Iterator<T> iterator) {
    Assert.notNull(iterator, "The Iterator to guard from modification cannot be null!");

    return new Iterator<T>() {
      public boolean hasNext() {
        return iterator.hasNext();
      }

      public T next() {
        return iterator.next();
      }

      public void remove() {
        throw new UnsupportedOperationException(StringUtils.NOT_IMPLEMENTED);
      }
    };
  }

}
