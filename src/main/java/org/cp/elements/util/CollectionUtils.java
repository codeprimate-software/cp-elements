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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Random;
import java.util.Set;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.FilteringTransformer;
import org.cp.elements.lang.Renderer;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.Transformer;
import org.cp.elements.lang.support.ToStringRenderer;

/**
 * The CollectionUtils class provides utility methods for working with the Java Collections Framework
 * and specifically the Collection classes.
 * 
 * @author John J. Blum
 * @see java.lang.Iterable
 * @see java.util.Collection
 * @see java.util.Collections
 * @see java.util.Enumeration
 * @see java.util.Iterator
 * @see java.util.List
 * @see java.util.Set
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.FilteringTransformer
 * @see org.cp.elements.lang.Renderer
 * @see org.cp.elements.lang.Transformer
 * @see org.cp.elements.lang.support.ToStringRenderer
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class CollectionUtils {

  /**
   * Counts the number of elements from the Iterable collection matching the criteria defined by the Filter.
   * 
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
   * 
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
   * 
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
   * 
   * @param <T> the Class type of the elements to enumerate.
   * @param it the Iterator to adapt into an Enumeration.
   * @return an Enumeration implementation backed by the specified Iterator.
   * @throws NullPointerException if the Iterator is null.
   * @see #iterator(java.util.Enumeration)
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
   * 
   * @param <T> the Class type of the elements in the Collection.
   * @param <C> the Class type of the Collection.
   * @param collection the Collection of elements to filter.
   * @param filter the Filter used to filter the Collection of elements.
   * @return the original Collection with only those elements retained that match the criteria defined by the Filter.
   * @throws NullPointerException if either the Collection of Filter are null.
   * @see #findAll(Iterable, org.cp.elements.lang.Filter)
   * @see java.util.Collection#retainAll(java.util.Collection)
   * @see org.cp.elements.lang.Filter
   */
  public static <T, C extends Collection<T>> C filter(final C collection, final Filter<T> filter) {
    Assert.notNull(collection, "The Collection of elements to filter cannot be null!");
    Assert.notNull(filter, "The Filter used to filter the Collection of elements cannot be null!");

    collection.retainAll(findAll(collection, filter));

    return collection;
  }

  /**
   * Filters and then transforms the retained, filtered elements in the given Collection using the FilteringTransformer.
   *
   * @param <T> the Class type of the Collection elements.
   * @param <C> the Class type of the Collection.
   * @param collection the Collection to filter and transform.
   * @param filteringTransformer the FilteringTransformer used to filter and transform elements of the given Collection.
   * @return the given Collection with the elements filtered and transformed according to the FilteringTransformer.
   * @throws java.lang.NullPointerException if the Collection or FilteringTransformer references are null.
   * @see #filter(java.util.Collection, org.cp.elements.lang.Filter)
   * @see #transform(java.util.Collection, org.cp.elements.lang.Transformer)
   * @see org.cp.elements.lang.FilteringTransformer
   * @see java.util.Collection
   */
  public static <T, C extends Collection<T>> C filterAndTransform(final C collection,
                                                                  final FilteringTransformer<T> filteringTransformer)
  {
    return transform(filter(collection, filteringTransformer), filteringTransformer);
  }

  /**
   * Searches the Iterable collection for the first element matching the criteria defined by the Filter.
   * 
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

    for (T element : collection) {
      if (filter.accept(element)) {
        matchingElement = element;
        break;
      }
    }

    return matchingElement;
  }

  /**
   * Searches the Iterable collection for elements matching the criteria defined by the Filter.
   * 
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

    List<T> matchingElements = new ArrayList<T>();

    for (T element : collection) {
      if (filter.accept(element)) {
        matchingElements.add(element);
      }
    }

    return matchingElements;
  }

  /**
   * Determines whether the specified Collection is empty.  A Collection is empty if it contains no elements
   * or the specified Collection object reference is null.
   * 
   * @param collection the Collection being tested as empty.
   * @return a boolean value indicating whether the specified Collection is empty.
   * @see java.util.Collection#isEmpty()
   */
  public static boolean isEmpty(final Collection collection) {
    return (collection == null || collection.isEmpty());
  }

  /**
   * Adapts the specified Collection into an instance of the Iterable interface.
   *
   * @param <T> the Class type of elements contained in the Collection.
   * @param collection the Collection backing the Iterable implementation.
   * @return an Iterable implementation backed by the Collection.
   * @throws NullPointerException if the Collection is null.
   * @see #iterable(java.util.Iterator)
   * @see java.lang.Iterable
   * @see java.util.Collection#iterator()
   */
  public static <T> Iterable<T> iterable(final Collection<T> collection) {
    Assert.notNull(collection, "The Collection backing the Iterable implementation cannot be null!");
    return iterable(collection.iterator());
  }

  /**
   * Adapts the specified Enumeration into an instance of the Iterable interface.
   * 
   * @param <T> the Class type of elements enumerated by the Enumeration.
   * @param enumeration the Enumeration backing the Iterable implementation.
   * @return an Iterable implementation backed by the Enumeration.
   * @throws NullPointerException if the Enumeration is null.
   * @see #iterable(java.util.Iterator)
   * @see #iterator(java.util.Enumeration)
   * @see java.lang.Iterable
   * @see java.util.Enumeration
   */
  public static <T> Iterable<T> iterable(final Enumeration<T> enumeration) {
    Assert.notNull(enumeration, "The Enumeration back the Iterable implementation cannot be null!");
    return iterable(iterator(enumeration));
  }

  /**
   * Adapts the specified Iterator into an instance of the Iterable interface.
   * 
   * @param <T> the Class type of elements iterated by the Iterator.
   * @param iterator the Iterator backing the Iterable implementation.
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
   * 
   * @param <T> the Class type of the elements to iterate.
   * @param enumeration the Enumeration backing the Iterator implementation.
   * @return an Iterator implementation backed by the specified Enumeration.
   * @throws NullPointerException if the Enumeration is null.
   * @see #enumeration(java.util.Iterator)
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
   * Shuffles the elements in the given List.  This method guarantees a random, uniform shuffling of the elements
   * in the List, and has an operational efficiency of O(n).
   *
   * @param <T> the class type of the elements in the list.
   * @param list the List of elements to shuffle.
   * @return the shuffled List of elements.
   * @throws java.lang.NullPointerException if the List references is null.
   * @see java.util.List
   */
  public static <T> List<T> shuffle(final List<T> list) {
    Assert.notNull(list, "The List to shuffle must not be null!");

    if (!isEmpty(list)) {
      Random random = new Random(System.currentTimeMillis());

      for (int index = 0, adjustedSize = size(list) - 1; index < adjustedSize; index++) {
        T elementAtIndex = list.get(index);
        int randomIndex = (random.nextInt(adjustedSize - index) + 1);
        list.set(index, list.get(randomIndex));
        list.set(randomIndex, elementAtIndex);
      }
    }

    return list;
  }

  /**
   * Determines the size of the specified Collection.  If the Collection is null or contains no elements, then the
   * size of the Collection is 0, otherwise the size of the Collection is determined by it's size() method.
   * 
   * @param collection the Collection who's size is being determined.
   * @return an integer value specifying the size of the Collection.
   * @see java.util.Collection#size()
   */
  public static int size(final Collection collection) {
    return (collection == null ? 0 : collection.size());
  }

  /**
   * Creates a sub-list from the given list with the values at the specified indices in the given list.
   *
   * @param <T> the Class type of the list elements.
   * @param list the original List from which to create the sub-List.
   * @param indices the indices of values from the original list to include in the sub-list.
   * @return a sub-list from the given list with the values at the specified indices in the given list.
   * @see java.util.List
   */
  public static <T> List<T> subList(final List<T> list, int... indices) {
    List<T> subList = new ArrayList<T>(indices.length);

    for (int index : indices) {
      subList.add(list.get(index));
    }

    return subList;
  }

  /**
   * Gets a String representation of the Iterable collection.
   * 
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
   * 
   * @param <T> the class type of elements in the Iterable collection.
   * @param collection the Iterable collection of elements to render as a String.
   * @param renderer the Renderer used to render the elements in the Iterable collection as String.
   * @return a String representation of the Iterable collection.
   * @see java.lang.Iterable
   * @see org.cp.elements.lang.Renderer
   */
  public static <T> String toString(final Iterable<T> collection, final Renderer<T> renderer) {
    StringBuilder buffer = new StringBuilder("[");
    int count = 0;

    for (T element : collection) {
      buffer.append(count++ > 0 ? ", " : StringUtils.EMPTY_STRING).append(renderer.render(element));
    }

    buffer.append("]");

    return buffer.toString();
  }

  /**
   * Transforms the elements of the given Collection using the specified Transformer.
   *
   * @param <T> the Class type of the Collection elements.
   * @param <C> the Class type of the Collection.
   * @param collection the Collection of elements to transform.
   * @param transformer the Transformer used to transform the elements of the Collection.
   * @return the Collection of elements transformed by the specified Transformer.
   * @throws java.lang.NullPointerException if the Collection or Transformer references are null.
   * @see org.cp.elements.lang.Transformer
   * @see java.util.Collection
   */
  public static <T, C extends Collection<T>> C transform(final C collection, final Transformer<T> transformer) {
    Assert.notNull(collection, "The Collection of elements to transform cannot be null!");
    Assert.notNull(transformer, "The Transformer used to transform the elements of the Collection cannot be null!");

    List<T> transformedElements = new ArrayList<T>(collection.size());

    for (T element : collection) {
      transformedElements.add(transformer.transform(element));
    }

    collection.clear();
    collection.addAll(transformedElements);

    return collection;
  }

  /**
   * Gets an anonymous, read-only Iterator implementation wrapping the specified Iterator to prevent modifications
   * through invocations of the Iterator's remove method.
   * 
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
