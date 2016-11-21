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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.stream.Collectors;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.FilteringTransformer;
import org.cp.elements.lang.NullSafe;
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
 * @see java.util.stream.Collectors
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
   * Null-safe method to convert the array of elements into a {@link Set}.
   *
   * @param <T> {@link Class} type of the elements in the array.
   * @param elements array of elements to convert into a {@link Set}.
   * @return the array of elements as a {@link Set}.
   * @see java.util.Set
   */
  @NullSafe
  @SafeVarargs
  public static <T> Set<T> asSet(T... elements) {
    Set<T> set = new HashSet<>(ArrayUtils.nullSafeLength(elements));
    Collections.addAll(set, ArrayUtils.nullSafeArray(elements));
    return set;
  }

  /**
   * Counts the number of elements in the {@link Iterable} collection.  If {@link Iterable} is null or contains
   * no elements, then count will be 0.  If the {@link Iterable} is a {@link Collection}, then {@link Collection#size()}
   * is returned, otherwise the elements of the {@link Iterable} are iterated over, counting the number of elements
   * in the iteration in order to determine it's size.
   *
   * @param iterable {@link Iterable} collection of elements being evaluated.
   * @return an integer value indicating the number of elements in the {@link Iterable} collection (i.e. size).
   * @see java.lang.Iterable
   * @see java.util.Collection#size()
   * @see #count(Iterable, Filter)
   */
  @NullSafe
  public static int count(Iterable<?> iterable) {
    return (iterable instanceof Collection ? ((Collection) iterable).size() : count(iterable, (element) -> true));
  }

  /**
   * Counts the number of elements in the {@link Iterable} collection accepted by the {@link Filter}.
   * 
   * @param <T> Class type of the elements in the {@link Iterable} collection.
   * @param iterable {@link Iterable} collection of elements being evaluated.
   * @param filter {@link Filter} used to determine the count of elements in the {@link Iterable} collection
   * accepted by the {@link Filter}.
   * @return an integer value indicating the number of elements in the {@link Iterable} collection accepted
   * by the {@link Filter}.
   * @throws IllegalArgumentException if {@link Filter} is null.
   * @see java.lang.Iterable
   * @see org.cp.elements.lang.Filter
   * @see #nullSafeIterable(Iterable)
   */
  public static <T> int count(Iterable<T> iterable, Filter<T> filter) {
    Assert.notNull(filter, "Filter cannot be null");

    int count = 0;

    for (T element : nullSafeIterable(iterable)) {
      if (filter.accept(element)) {
        count++;
      }
    }

    return count;
  }

  /**
   * Returns an empty {@link Iterable} with no elements.
   *
   * @param <T> Class type of the elements in the {@link Iterable}.
   * @return an empty {@link Iterable}.
   * @see java.util.Collections#emptyIterator()
   * @see java.lang.Iterable
   */
  @NullSafe
  public static <T> Iterable<T> emptyIterable() {
    return Collections::<T>emptyIterator;
  }

  /**
   * Adapts the {@link Iterator} into an instance of the {@link Enumeration} interface.
   * Returns an empty {@link Enumeration} if the {@link Iterator} is null.
   * 
   * @param <T> Class type of the elements in the {@link Iterator}.
   * @param iterator {@link Iterator} to adapt into an {@link Enumeration}.
   * @return an {@link Enumeration} implementation enumerating over the elements in the {@link Iterator}.
   * @see java.util.Collections#emptyEnumeration()
   * @see #iterator(java.util.Enumeration)
   * @see java.util.Enumeration
   * @see java.util.Iterator
   */
  @NullSafe
  public static <T> Enumeration<T> enumeration(Iterator<T> iterator) {
    return (iterator == null ? Collections.emptyEnumeration() : new Enumeration<T>() {
      @Override
      public boolean hasMoreElements() {
        return iterator.hasNext();
      }

      @Override
      public T nextElement() {
        return iterator.next();
      }
    });
  }

  /**
   * Returns a filtered {@link Collection} containing only the elements from the given {@link Collection} accepted by
   * the {@link Filter}.
   * 
   * @param <T> Class type of the elements in the {@link Collection}.
   * @param collection {@link Collection} to filter.
   * @param filter {@link Filter} used to filter the {@link Collection}.
   * @return a filtered {@link Collection} of elements accepted by the {@link Filter}.
   * @throws IllegalArgumentException if either the {@link Collection} or {@link Filter} are null.
   * @see org.cp.elements.lang.Filter
   */
  @SuppressWarnings("unchecked")
  public static <T> Collection<T> filter(Collection<T>  collection, Filter<T> filter) {
    Assert.notNull(collection, "Collection cannot be null");
    Assert.notNull(filter, "Filter cannot be null");

    return collection.stream().filter(filter::accept).collect(Collectors.toList());
  }

  /**
   * Filters and transforms the {@link Collection} of elements using the specified {@link FilteringTransformer}.
   *
   * @param <T> Class type of the elements in the {@link Collection}.
   * @param collection {@link Collection} to filter and transform.
   * @param filteringTransformer {@link FilteringTransformer} used to filter and transform the {@link Collection}.
   * @return a filtered, transformed {@link Collection} of elements accepted and modified
   * by the {@link FilteringTransformer}.
   * @throws IllegalArgumentException if either the {@link Collection} or {@link FilteringTransformer} are null.
   * @see org.cp.elements.lang.FilteringTransformer
   * @see java.util.Collection
   */
  @SuppressWarnings("unchecked")
  public static <T> Collection<T> filterAndTransform(Collection<T> collection,
      FilteringTransformer<T> filteringTransformer) {

    Assert.notNull(collection, "Collection cannot be null");
    Assert.notNull(filteringTransformer, "FilteringTransformer cannot be null");

    return collection.stream().filter(filteringTransformer::accept).map(filteringTransformer::transform)
      .collect(Collectors.toList());
  }

  /**
   * Searches the {@link Iterable} for the first element accepted by the {@link Filter}.
   * 
   * @param <T> Class type of the elements in the {@link Iterable}.
   * @param iterable {@link Iterable} collection of elements to search.
   * @param filter {@link Filter} used to find the first element from the {@link Iterable} accepted
   * by the {@link Filter}.
   * @return the first element from the {@link Iterable} accepted by the {@link Filter}, or {@literal null}
   * if no such element is found.
   * @throws IllegalArgumentException if {@link Filter} is null.
   * @see #findAll(Iterable, Filter)
   * @see #nullSafeIterable(Iterable)
   * @see org.cp.elements.lang.Filter
   * @see java.lang.Iterable
   */
  public static <T> T find(Iterable<T> iterable, Filter<T> filter) {
    Assert.notNull(filter, "Filter cannot be null");

    for (T element : nullSafeIterable(iterable)) {
      if (filter.accept(element)) {
        return element;
      }
    }

    return null;
  }

  /**
   * Searches the {@link Iterable} for all elements accepted by the {@link Filter}.
   * 
   * @param <T> Class type of the elements in the {@link Iterable}.
   * @param iterable {@link Iterable} collection of elements to search.
   * @param filter {@link Filter} used to find elements from the {@link Iterable} collection accepted
   * by the {@link Filter}.
   * @return a {@link List} containing elements from the {@link Iterable} collection accepted by the {@link Filter}.
   * @throws IllegalArgumentException if {@link Filter} is null.
   * @see #find(Iterable, Filter)
   * @see #nullSafeIterable(Iterable)
   * @see org.cp.elements.lang.Filter
   * @see java.lang.Iterable
   */
  public static <T> List<T> findAll(Iterable<T> iterable, Filter<T> filter) {
    Assert.notNull(filter, "Filter cannot be null");

    List<T> acceptedElements = new ArrayList<>();

    for (T element : nullSafeIterable(iterable)) {
      if (filter.accept(element)) {
        acceptedElements.add(element);
      }
    }

    return acceptedElements;
  }

  /**
   * Determines if the {@link Collection} is empty.  The {@link Collection} is empty if it contains no elements
   * or the {@link Collection} object reference is null.
   * 
   * @param collection {@link Collection} to evaluate.
   * @return a boolean value indicating if the {@link Collection} is empty.
   * @see java.util.Collection#isEmpty()
   * @see #isNotEmpty(Collection)
   */
  @NullSafe
  public static boolean isEmpty(Collection<?> collection) {
    return (collection == null || collection.isEmpty());
  }

  /**
   * Determines if the {@link Collection} is not empty.  The {@link Collection} is not empty if it is not null
   * and contains at least 1 element.
   *
   * @param collection {@link Collection} to evaluate.
   * @return a boolean value indicating if the {@link Collection} is not empty.
   * @see #isEmpty(Collection)
   */
  @NullSafe
  public static boolean isNotEmpty(Collection<?> collection) {
    return !isEmpty(collection);
  }

  /**
   * Adapts the {@link Enumeration} into an instance of the {@link Iterable} interface.
   * 
   * @param <T> Class type of the elements in the {@link Enumeration}.
   * @param enumeration {@link Enumeration} to adapt into an {@link Iterable}.
   * @return an {@link Iterable} implementation backed by the {@link Enumeration}.
   * @see #iterator(java.util.Enumeration)
   * @see java.util.Enumeration
   * @see java.lang.Iterable
   */
  @NullSafe
  public static <T> Iterable<T> iterable(Enumeration<T> enumeration) {
    return () -> iterator(enumeration);
  }

  /**
   * Adapts the {@link Iterator} into an instance of the {@link Iterable} interface.
   * 
   * @param <T> Class type of the elements in the {@link Iterator}.
   * @param iterator {@link Iterator} to adapt into an {@link Iterable}.
   * @return an {@link Iterable} implementation backed by the {@link Iterator}.
   * @see #nullSafeIterator(Iterator)
   * @see java.util.Iterator
   * @see java.lang.Iterable
   */
  @NullSafe
  public static <T> Iterable<T> iterable(Iterator<T> iterator) {
    return () -> nullSafeIterator(iterator);
  }

  /**
   * Adapts the {@link Enumeration} into an instance of the {@link Iterator} interface.
   *
   * @param <T> Class type of the elements in the {@link Enumeration}.
   * @param enumeration {@link Enumeration} to adapt into an {@link Iterator}.
   * @return an {@link Iterator} implementation iterating over the elements in the {@link Enumeration}.
   * @see java.util.Collections#emptyIterator()
   * @see #enumeration(java.util.Iterator)
   * @see java.util.Enumeration
   * @see java.util.Iterator
   */
  @NullSafe
  public static <T> Iterator<T> iterator(Enumeration<T> enumeration) {
    return (enumeration == null ? Collections.emptyIterator() : new Iterator<T>() {
      @Override
      public boolean hasNext() {
        return enumeration.hasMoreElements();
      }

      @Override
      public T next() {
        return enumeration.nextElement();
      }
    });
  }

  /**
   * Null-safe method returning the given {@link Enumeration} or an empty {@link Enumeration} if null.
   *
   * @param <T> Class type of the elements in the {@link Enumeration}.
   * @param enumeration {@link Enumeration} to evaluate.
   * @return the given {@link Enumeration} or an empty {@link Enumeration} if null.
   * @see java.util.Collections#emptyEnumeration()
   * @see java.util.Enumeration
   */
  @NullSafe
  public static <T> Enumeration<T> nullSafeEnumeration(Enumeration<T> enumeration) {
    return (enumeration != null ? enumeration : Collections.emptyEnumeration());
  }

  /**
   * Null-safe method returning the given {@link Iterable} or an empty {@link Iterable} if null.
   *
   * @param <T> Class type of the elements in the {@link Iterable}.
   * @param iterable {@link Iterable} to evaluate.
   * @return the given {@link Iterable} or an empty {@link Iterable} if null.
   * @see #emptyIterable()
   * @see java.lang.Iterable
   */
  @NullSafe
  public static <T> Iterable<T> nullSafeIterable(Iterable<T> iterable) {
    return (iterable != null ? iterable : emptyIterable());
  }

  /**
   * Null-safe method returning the given {@link Iterator} or an empty {@link Iterator} if null.
   *
   * @param <T> Class type of the elements in the {@link Iterator}.
   * @param iterator {@link Iterator} to evaluate.
   * @return the given {@link Iterator} or an empty {@link Iterator} if null.
   * @see java.util.Collections#emptyIterator()
   * @see java.util.Iterator
   */
  @NullSafe
  public static <T> Iterator<T> nullSafeIterator(Iterator<T> iterator) {
    return (iterator != null ? iterator : Collections.emptyIterator());
  }

  /**
   * Null-safe method returning the given {@link List} or an empty {@link List} if null.
   *
   * @param <T> Class type of the elements in the {@link List}.
   * @param list {@link List} to evaluate.
   * @return the given {@link List} or an empty {@link List} if null.
   * @see java.util.Collections#emptyList()
   * @see java.util.List
   */
  @NullSafe
  public static <T> List<T> nullSafeList(List<T> list) {
    return (list != null ? list : Collections.emptyList());
  }

  /**
   * Null-safe method returning the given {@link Set} or an empty {@link Set} if null.
   *
   * @param <T> Class type of the elements in the {@link Set}.
   * @param set {@link Set} to evaluate.
   * @return the given {@link Set} or an empty {@link Set} if null.
   * @see java.util.Collections#emptySet()
   * @see java.util.Set
   */
  @NullSafe
  public static <T> Set<T> nullSafeSet(Set<T> set) {
    return (set != null ? set : Collections.emptySet());
  }

  /**
   * Determines the size of the Collection.
   *
   * @param collection {@link Collection} to evaluate.
   * @return the size of, or number of elements in the {@link Collection}.
   * @see java.util.Collection#size()
   */
  @NullSafe
  public static int nullSafeSize(Collection<?> collection) {
    return (collection != null ? collection.size() : 0);
  }

  /**
   * Shuffles the elements in the {@link List}.  This method guarantees a random, uniform shuffling of the elements
   * in the {@link List} with an operational efficiency of O(n).
   *
   * @param <T> Class type of the elements in the {@link List}.
   * @param list {@link List} of elements to shuffle.
   * @return the {@link List} of elements shuffled.
   * @see java.util.Collections#swap(List, int, int)
   * @see #isNotEmpty(Collection)
   * @see java.util.List
   */
  @NullSafe
  public static <T> List<T> shuffle(List<T> list) {
    if (isNotEmpty(list)) {
      Random random = new Random(System.currentTimeMillis());

      for (int index = 0, sizeMinusOne = count(list) - 1; index < sizeMinusOne; index++) {
        int randomIndex = (random.nextInt(sizeMinusOne - index) + 1);
        Collections.swap(list, index, index + randomIndex);
      }
    }

    return list;
  }

  /**
   * Creates a sub-list from the given {@link List} with values at the specified indices.
   *
   * @param <T> Class type of the elements in the {@link List}.
   * @param list {@link List} of elements from which to create the sub-list.
   * @param indices array of indices referring to values in the {@link List} to include in the sub-list.
   * @return a sub-list containing values at the given indices in the {@link List}.
   * @throws IllegalArgumentException if either the {@link List} or array of indices are null.
   * @throws IndexOutOfBoundsException if the indices are not valid indexes in the {@link List}.
   * @see java.util.List
   */
  public static <T> List<T> subList(List<T> list, int... indices) {
    Assert.notNull(list, "List cannot be null");
    Assert.notNull(indices, "Indices cannot be null");

    List<T> subList = new ArrayList<>(indices.length);

    for (int index : indices) {
      subList.add(list.get(index));
    }

    return subList;
  }

  /**
   * Null-safe method to convert the given {@link Iterable} collection of elements into a {@link List}.
   *
   * @param <T> {@link Class} type of the elements in the {@link Iterable}.
   * @param iterable {@link Iterable} to convert into a {@link List}.
   * @return a {@link List} of the elements from the {@link Iterable} object.
   * @see #nullSafeIterable(Iterable)
   * @see java.lang.Iterable
   * @see java.util.List
   */
  @NullSafe
  public static <T> List<T> toList(Iterable<T> iterable) {
    if (iterable instanceof Collection) {
      return new ArrayList<>((Collection<T>) iterable);
    }
    else {
      List<T> list = new ArrayList<>();

      for (T element : nullSafeIterable(iterable)) {
        list.add(element);
      }

      return list;
    }
  }

  /**
   * Null-safe method to convert the given {@link Iterable} collection of elements into a {@link Set}.
   *
   * @param <T> {@link Class} type of the elements in the {@link Iterable}.
   * @param iterable {@link Iterable} to convert into a {@link Set}.
   * @return a {@link Set} of the elements from the {@link Iterable} object.
   * @see #nullSafeIterable(Iterable)
   * @see java.lang.Iterable
   * @see java.util.Set
   */
  @NullSafe
  public static <T> Set<T> toSet(Iterable<T> iterable) {
    if (iterable instanceof Collection) {
      return new HashSet<>((Collection<T>) iterable);
    }
    else {
      Set<T> set = new HashSet<>();

      for (T element : nullSafeIterable(iterable)) {
        set.add(element);
      }

      return set;
    }
  }

  /**
   * Returns a {@link String} representation of the {@link Iterable}.
   * 
   * @param iterable {@link Iterable} to render as a {@link String}.
   * @return a String representation of the {@link Iterable}.
   * @see #toString(Iterable, Renderer)
   * @see java.lang.Iterable
   */
  @NullSafe
  public static String toString(Iterable<?> iterable) {
    return toString(iterable, new ToStringRenderer<>());
  }

  /**
   * Returns a {@link String} representation of the {@link Iterable} rendered with the {@link Renderer}.
   *
   * @param <T> Class type of the elements in the {@link Iterable}.
   * @param iterable {@link Iterable} to render as a {@link String}.
   * @param renderer {@link Renderer} used to render the elements in the {@link Iterable} as {@link String}s.
   * @return a {@link String} representation of the {@link Iterable}.
   * @see org.cp.elements.lang.Renderer
   * @see java.lang.Iterable
   */
  @NullSafe
  public static <T> String toString(Iterable<T> iterable, Renderer<T> renderer) {
    StringBuilder buffer = new StringBuilder("[");
    int count = 0;

    for (T element : nullSafeIterable(iterable)) {
      buffer.append(count++ > 0 ? StringUtils.COMMA_SPACE_DELIMITER : StringUtils.EMPTY_STRING)
        .append(renderer.render(element));
    }

    buffer.append("]");

    return buffer.toString();
  }

  /**
   * Transforms the elements in the {@link Collection} using the {@link Transformer}.
   *
   * @param <T> Class type of the elements in the {@link Collection}.
   * @param collection {@link Collection} of elements to transform.
   * @param transformer {@link Transformer} used to transform the elements in the {@link Collection}.
   * @return the {@link Collection} of elements transformed by the given {@link Transformer}.
   * @throws IllegalArgumentException if either the {@link Collection} or {@link Transformer} are null.
   * @see org.cp.elements.lang.Transformer
   * @see java.util.Collection
   */
  @SuppressWarnings("unchecked")
  public static <T> Collection<T> transform(Collection<T> collection, Transformer<T> transformer) {
    Assert.notNull(collection, "Collection cannot be null");
    Assert.notNull(transformer, "Transformer cannot be null");

    return collection.stream().map(transformer::transform).collect(Collectors.toList());
  }

  /**
   * Returns an immutable {@link Iterator} implementation wrapping the given {@link Iterator} to prevent modifications
   * through invocations of the {@link Iterator#remove()} method.
   * 
   * @param <T> Class type of the elements in the {@link Iterator}.
   * @param iterator {@link Iterator} to make immutable.
   * @return an immutable {@link Iterator} implementation wrapping the given {@link Iterator}.
   * @throws IllegalArgumentException if {@link Iterator} is null.
   * @see java.util.Iterator
   */
  public static <T> Iterator<T> unmodifiableIterator(Iterator<T> iterator) {
    Assert.notNull(iterator, "Iterator cannot be null");

    return new Iterator<T>() {
      @Override
      public boolean hasNext() {
        return iterator.hasNext();
      }

      @Override
      public T next() {
        return iterator.next();
      }

      @Override
      public void remove() {
        throw new UnsupportedOperationException("Iterator is immutable");
      }
    };
  }
}
