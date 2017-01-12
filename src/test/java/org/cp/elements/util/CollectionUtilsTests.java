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

import static org.cp.elements.util.ArrayUtils.asArray;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.FilteringTransformer;
import org.cp.elements.lang.NumberUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for {@link CollectionUtils}.
 *
 * @author John J. Blum
 * @see java.lang.Iterable
 * @see java.util.Collection
 * @see java.util.Collections
 * @see java.util.Enumeration
 * @see java.util.Iterator
 * @see java.util.List
 * @see java.util.Set
 * @see org.junit.Test
 * @see org.cp.elements.util.CollectionUtils
 * @since 1.0.0
 */
public class CollectionUtilsTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @SafeVarargs
  protected final <T> void assertElements(Collection<T> collection, T... elements) {
    assertThat(collection, is(notNullValue(Collection.class)));
    assertThat(collection.size(), is(equalTo(elements.length)));
    assertThat(collection.containsAll(asCollection(elements)), is(true));
  }

  protected <T> void assertShuffled(Iterable<T> source, Iterable<T> target) {
    assertTrue("'source' must not be null and must have elements", source != null && source.iterator().hasNext());
    assertTrue("'target' must not be null and must have elements", target != null && target.iterator().hasNext());

    Iterator<T> targetIterator = target.iterator();

    boolean shuffled = false;

    for (T sourceElement : source) {
      shuffled &= targetIterator.hasNext();
      shuffled |= !sourceElement.equals(targetIterator.next());
    }

    assertTrue(String.format("Target [%1$s] was not shuffled", target), shuffled);
  }

  @SafeVarargs
  protected final <T> Collection<T> asCollection(T... elements) {
    return Arrays.asList(elements);
  }

  @SafeVarargs
  protected final <T> Enumeration<T> asEnumeration(T... elements) {
    return new Enumeration<T>() {

      int index = 0;

      @Override
      public boolean hasMoreElements() {
        return (index < elements.length);
      }

      @Override
      public T nextElement() {
        Assert.isTrue(hasMoreElements(), new NoSuchElementException("No more elements"));
        return elements[index++];
      }
    };
  }

  @SafeVarargs
  protected final <T> Iterable<T> asIterable(T... elements) {
    return () -> asIterator(elements);
  }

  @SafeVarargs
  protected final <T> Iterator<T> asIterator(T... elements) {
    return new Iterator<T>() {

      int index = 0;

      @Override
      public boolean hasNext() {
        return (index < elements.length);
      }

      @Override
      public T next() {
        Assert.isTrue(hasNext(), new NoSuchElementException("No more elements"));
        return elements[index++];
      }
    };
  }

  @Test
  public void addAllIterableElementsToList() {
    List<Integer> numbers = new ArrayList<>(Arrays.asList(1, 2, 3));
    List<Integer> newNumbers = CollectionUtils.addAll(numbers, asIterable(3, 4, 5));

    assertThat(newNumbers, is(sameInstance(numbers)));
    assertThat(newNumbers.size(), is(equalTo(6)));
    assertThat(newNumbers, contains(1, 2, 3, 3, 4, 5));
  }

  @Test
  public void addAllIterableElementsToSet() {
    Set<Integer> numbers = new HashSet<>(Arrays.asList(1, 2, 3));
    Set<Integer> newNumbers = CollectionUtils.addAll(numbers, asIterable(3, 4, 5));

    assertThat(newNumbers, is(sameInstance(numbers)));
    assertThat(newNumbers.size(), is(equalTo(5)));
    assertThat(newNumbers, contains(1, 2, 3, 4, 5));
  }

  @Test
  public void addEmptyIterableToCollection() {
    Set<Integer> numbers = new HashSet<>(Arrays.asList(1, 2, 3));
    Set<Integer> newNumers = CollectionUtils.addAll(numbers, asIterable());

    assertThat(newNumers, is(sameInstance(numbers)));
    assertThat(newNumers.size(), is(equalTo(3)));
    assertThat(newNumers, contains(1, 2, 3));
  }

  @Test
  public void addNullIterableToCollection() {
    Set<Integer> numbers = new HashSet<>(Arrays.asList(1, 2, 3));
    Set<Integer> newNumers = CollectionUtils.addAll(numbers, null);

    assertThat(newNumers, is(sameInstance(numbers)));
    assertThat(newNumers.size(), is(equalTo(3)));
    assertThat(newNumers, contains(1, 2, 3));
  }

  @Test
  public void addAllToNullCollection() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Collection must not be null");

    CollectionUtils.addAll(null, asIterable(1));
  }

  @Test
  public void asEnumerationForIterator() {
    String[] elements = { "test", "testing", "tested"};
    Enumeration<String> enumeration = CollectionUtils.asEnumeration(asIterator(elements));

    assertThat(enumeration, is(notNullValue(Enumeration.class)));

    for (String element : elements) {
      assertThat(enumeration.hasMoreElements(), is(true));
      assertThat(enumeration.nextElement(), is(equalTo(element)));
    }

    assertThat(enumeration.hasMoreElements(), is(false));
  }

  @Test
  public void asEnumerationForEmptyIterator() {
    Enumeration<?> enumeration = CollectionUtils.asEnumeration(Collections.emptyIterator());

    assertThat(enumeration, is(notNullValue(Enumeration.class)));
    assertThat(enumeration.hasMoreElements(), is(false));
  }

  @Test
  public void asEnumerationForNullIterator() {
    Enumeration<?> enumeration = CollectionUtils.asEnumeration(null);

    assertThat(enumeration, is(notNullValue(Enumeration.class)));
    assertThat(enumeration.hasMoreElements(), is(false));
  }

  @Test
  public void asEnumerationForSingleElementIterator() {
    Enumeration<String> enumeration = CollectionUtils.asEnumeration(asIterator("test"));

    assertThat(enumeration, is(notNullValue(Enumeration.class)));
    assertThat(enumeration.hasMoreElements(), is(true));
    assertThat(enumeration.nextElement(), is(equalTo("test")));
    assertThat(enumeration.hasMoreElements(), is(false));
  }

  @Test
  public void asIterableWithEnumeration() {
    Integer[] elements = { 0, 1, 2 };
    Iterable<Integer> iterable = CollectionUtils.asIterable(asEnumeration(elements));

    assertThat(iterable, is(notNullValue(Iterable.class)));

    int index = 0;

    for (Integer element : iterable) {
      assertThat(element, is(equalTo(elements[index++])));
    }

    assertThat(index, is(equalTo(elements.length)));
  }

  @Test
  public void asIterableWithEmptyEnumeration() {
    Iterable<?> iterable = CollectionUtils.asIterable(Collections.emptyEnumeration());

    assertThat(iterable, is(notNullValue(Iterable.class)));
    assertThat(iterable.iterator(), is(notNullValue()));
    assertThat(iterable.iterator().hasNext(), is(false));
  }

  @Test
  public void asIterableWithNullEnumeration() {
    Iterable<?> iterable = CollectionUtils.asIterable((Enumeration<?>) null);

    assertThat(iterable, is(notNullValue(Iterable.class)));
    assertThat(iterable.iterator(), is(notNullValue(Iterator.class)));
    assertThat(iterable.iterator().hasNext(), is(false));
  }

  @Test
  public void asIterableWithIterator() {
    Integer[] elements = { 0, 1, 2 };
    Iterable<Integer> iterable = CollectionUtils.asIterable(asIterator(elements));

    assertThat(iterable, is(notNullValue(Iterable.class)));

    int index = 0;

    for (Integer element : iterable) {
      assertThat(element, is(equalTo(elements[index++])));
    }

    assertThat(index, is(equalTo(elements.length)));
  }

  @Test
  public void asIterableWithEmptyIterator() {
    Iterable<?> iterable = CollectionUtils.asIterable(Collections.emptyIterator());

    assertThat(iterable, is(notNullValue(Iterable.class)));
    assertThat(iterable.iterator(), is(notNullValue(Iterator.class)));
    assertThat(iterable.iterator().hasNext(), is(false));
  }

  @Test
  public void asIterableWithNullIterator() {
    Iterable<?> iterable = CollectionUtils.asIterable((Iterator<?>) null);

    assertThat(iterable, is(notNullValue(Iterable.class)));
    assertThat(iterable.iterator(), is(notNullValue(Iterator.class)));
    assertThat(iterable.iterator().hasNext(), is(false));
  }

  @Test
  public void asIteratorForEnumeration() {
    Integer[] elements = { 0, 1, 2 };
    Iterator<Integer> iterator = CollectionUtils.asIterator(asEnumeration(elements));

    assertThat(iterator, is(notNullValue(Iterator.class)));

    for (Integer element : elements) {
      assertThat(iterator.hasNext(), is(true));
      assertThat(iterator.next(), is(equalTo(element)));
    }

    assertThat(iterator.hasNext(), is(false));
  }

  @Test
  public void asIteratorForEnumerationIsUnmodifiable() {
    Iterator<String> iterator = CollectionUtils.asIterator(asEnumeration("test"));

    assertThat(iterator, is(notNullValue(Iterator.class)));
    assertThat(iterator.hasNext(), is(true));

    try {
      exception.expect(UnsupportedOperationException.class);
      iterator.remove();
    }
    finally {
      assertThat(iterator.hasNext(), is(true));
      assertThat(iterator.next(), is(equalTo("test")));
      assertThat(iterator.hasNext(), is(false));
    }
  }

  @Test
  public void asIteratorForEmptyEnumeration() {
    Iterator<?> iterator = CollectionUtils.asIterator(Collections.emptyEnumeration());

    assertThat(iterator, is(notNullValue(Iterator.class)));
    assertThat(iterator.hasNext(), is(false));
  }

  @Test
  public void asIteratorForNullEnumeration() {
    Iterator<?> iterator = CollectionUtils.asIterator(null);

    assertThat(iterator, is(notNullValue(Iterator.class)));
    assertThat(iterator.hasNext(), is(false));
  }

  @Test
  public void asIteratorForSingleElementEnumeration() {
    Iterator<?> iterator = CollectionUtils.asIterator(asEnumeration("test"));

    assertThat(iterator, is(notNullValue(Iterator.class)));
    assertThat(iterator.hasNext(), is(true));
    assertThat(iterator.next(), is(equalTo("test")));
    assertThat(iterator.hasNext(), is(false));
  }

  @Test
  public void asListWithCollection() {
    Collection<String> collection = asCollection("test", "testing", "tested");
    List<String> list = CollectionUtils.asList(collection);

    assertThat(list, is(notNullValue(List.class)));
    assertThat(list, is(not(sameInstance(collection))));
    assertThat(list, is(equalTo(collection)));
  }

  @Test
  public void asListWithEmptyIterable() {
    List<?> list = CollectionUtils.asList(asIterable());

    assertThat(list, is(notNullValue(List.class)));
    assertThat(list.isEmpty(), is(true));
  }

  @Test
  public void asListWithIterable() {
    Iterable<String> iterable = asIterable("test", "testing", "tested");
    List<String> list = CollectionUtils.asList(iterable);

    assertThat(list, is(notNullValue(List.class)));
    assertThat(list.size(), is(equalTo(3)));
    assertThat(list.containsAll(asCollection("test", "testing", "tested")), is(true));
  }

  @Test
  public void asListWithNullIterable() {
    List<?> list = CollectionUtils.asList(null);

    assertThat(list, is(notNullValue(List.class)));
    assertThat(list.isEmpty(), is(true));
  }

  @Test
  public void asSetWithArray() {
    Set<Integer> set = CollectionUtils.asSet(1, 2, 4, 8);

    assertThat(set, is(notNullValue(Set.class)));
    assertThat(set.size(), is(equalTo(4)));
    assertThat(set.containsAll(asCollection(1, 2, 4, 8)), is(true));
  }

  @Test
  public void asSetWitEmptyArray() {
    Set<Integer> set = CollectionUtils.asSet();

    assertThat(set, is(notNullValue(Set.class)));
    assertThat(set.isEmpty(), is(true));
  }

  @Test
  public void asSetWitNullArray() {
    Set<String> set = CollectionUtils.asSet((String[]) null);

    assertThat(set, is(notNullValue(Set.class)));
    assertThat(set.isEmpty(), is(true));
  }

  @Test
  public void asSetWithCollection() {
    Collection<String> collection = asCollection("test", "testing", "tested");
    Set<String> set = CollectionUtils.asSet(collection);

    assertThat(set, is(notNullValue(Set.class)));
    assertThat(set.size(), is(equalTo(collection.size())));
    assertThat(set.containsAll(collection), is(true));
  }

  @Test
  public void asSetWithEmptyIterable() {
    Set<?> set = CollectionUtils.asSet(asIterable());

    assertThat(set, is(notNullValue(Set.class)));
    assertThat(set.isEmpty(), is(true));
  }

  @Test
  public void asSetWithIterable() {
    Set<Integer> set = CollectionUtils.asSet(asIterable(1, 2, 4, 8));

    assertThat(set, is(notNullValue(Set.class)));
    assertThat(set.size(), is(equalTo(4)));
    assertThat(set.containsAll(asCollection(1, 2, 4, 8)), is(true));
  }

  @Test
  public void asSetWithIterableHavingDuplicates() {
    Set<Integer> set = CollectionUtils.asSet(asIterable(1, 2, 4, 1, 8, 2));

    assertThat(set, is(notNullValue(Set.class)));
    assertThat(set.size(), is(equalTo(4)));
    assertThat(set.containsAll(asCollection(1, 2, 4, 8)), is(true));
  }

  @Test
  public void asSetWithNullIterable() {
    Set<?> set = CollectionUtils.asSet((Iterable<?>) null);

    assertThat(set, is(notNullValue(Set.class)));
    assertThat(set.isEmpty(), is(true));
  }

  @Test
  public void containsWithCollectionAndArrayElementsReturnsTrue() {
    Collection<?> collection = asCollection(1, 2, 3);

    assertThat(CollectionUtils.containsAny(collection, 1), is(true));
    assertThat(CollectionUtils.containsAny(collection, 1, 2), is(true));
    assertThat(CollectionUtils.containsAny(collection, 1, 2, 3), is(true));
  }

  @Test
  public void containsWithCollectionAndArrayElementsReturnsFalse() {
    Collection<?> collection = asCollection(1, 2, 3);

    assertThat(CollectionUtils.containsAny(collection, -1), is(false));
    assertThat(CollectionUtils.containsAny(collection, 1.5d, 2.01d), is(false));
    assertThat(CollectionUtils.containsAny(collection, 4, 5, 6), is(false));
  }

  @Test
  public void containsWithCollectionAndNullArrayReturnsFalse() {
    assertThat(CollectionUtils.containsAny(asCollection(1, 2, 3), (Object[]) null), is(false));
  }

  @Test
  @SuppressWarnings("all")
  public void containsWithNullCollectionAndArrayReturnsFalse() {
    assertThat(CollectionUtils.containsAny(null, asArray(1, 2, 3)), is(false));
  }

  @Test
  public void containsWithNullCollectionAndNullArrayIsFalse() {
    assertThat(CollectionUtils.containsAny(null, (Object[]) null), is(false));
  }

  @Test
  public void countCollectionReturnsTen() {
    Collection<?> collection = asCollection(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);

    assertThat(CollectionUtils.count(collection), is(equalTo(10)));
  }

  @Test
  public void countCollectionWithInitialCapacityReturnsZero() {
    assertThat(CollectionUtils.count(new ArrayList<>(10)), is(equalTo(0)));
  }

  @Test
  public void countEmptyCollectionReturnsZero() {
    assertThat(CollectionUtils.count(Collections.emptyList()), is(equalTo(0)));
  }

  @Test
  public void countSingleElementCollectionReturnsOne() {
    assertThat(CollectionUtils.count(Collections.singleton(1)), is(equalTo(1)));
  }

  @Test
  public void countIterableReturnsTen() {
    Iterable<?> iterable = asIterable(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);

    assertThat(CollectionUtils.count(iterable), is(equalTo(10)));
  }

  @Test
  public void countEmptyIterableReturnsZero() {
    assertThat(CollectionUtils.count(asIterable()), is(equalTo(0)));
  }

  @Test
  public void countSingleElementIterableReturnsOne() {
    assertThat(CollectionUtils.count(asIterable(1)), is(equalTo(1)));
  }

  @Test
  public void countNullReturnsZero() {
    assertThat(CollectionUtils.count(null), is(equalTo(0)));
  }

  @Test
  public void countCollectionWithFilter() {
    Collection<Integer> numbers = asCollection(1, 2, 3, 4, 5, 6, 7, 8, 9);

    Filter<Integer> evenNumbers = NumberUtils::isEven;
    Filter<Integer> oddNumbers = NumberUtils::isOdd;

    assertThat(CollectionUtils.count(numbers, evenNumbers), is(equalTo(4)));
    assertThat(CollectionUtils.count(numbers, oddNumbers), is(equalTo(5)));
  }

  @Test
  public void countEmptyWithFilterReturnsZero() {
    assertThat(CollectionUtils.count(Collections.emptyList(), (element) -> true), is(equalTo(0)));
  }

  @Test
  public void countIterableWithFilter() {
    Iterable<Integer> iterable = asIterable(0, 1, 2);

    Filter<Integer> evenNumbers = NumberUtils::isEven;
    Filter<Integer> oddNumbers = NumberUtils::isOdd;

    assertThat(CollectionUtils.count(iterable, evenNumbers), is(equalTo(2)));
    assertThat(CollectionUtils.count(iterable, oddNumbers), is(equalTo(1)));
  }

  @Test
  public void countNullWithFilterReturnsZero() {
    assertThat(CollectionUtils.count(null, (element) -> true), is(equalTo(0)));
  }

  @Test
  public void countWithFilterAcceptsAll() {
    assertThat(CollectionUtils.count(asIterable(0, 1, 2), (element) -> true), is(equalTo(3)));
  }

  @Test
  public void countWithFilterRejectsAll() {
    assertThat(CollectionUtils.count(asIterable(0, 1, 2), (element) -> false), is(equalTo(0)));
  }

  @Test
  public void countWithNullFilter() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Filter cannot be null");

    CollectionUtils.count(Collections.emptyList(), null);
  }

  @Test
  public void defaultIfEmptyWithNonNullNonEmptyIterableReturnsIterable() {
    Iterable<Number> iterable = asIterable(1);
    Iterable<Number> defaultIterable = asIterable(2);

    assertThat(CollectionUtils.defaultIfEmpty(iterable, defaultIterable), is(sameInstance(iterable)));
  }

  @Test
  public void defaultIfNullWithEmptyIterableReturnsDefault() {
    Iterable<Number> defaultIterable = asIterable(2);

    assertThat(CollectionUtils.defaultIfEmpty(asIterable(), defaultIterable), is(sameInstance(defaultIterable)));
  }

  @Test
  public void defaultIfNullWithNullIterableReturnsDefault() {
    Iterable<Number> defaultIterable = asIterable(2);

    assertThat(CollectionUtils.defaultIfEmpty(null, defaultIterable), is(sameInstance(defaultIterable)));
  }

  @Test
  public void defaultIfNullWithNullIterableAndNullDefaultReturnsNull() {
    assertThat(CollectionUtils.defaultIfEmpty(null, null), is(nullValue(Iterable.class)));
  }

  @Test
  public void emptyIterableIsNonNullEmptyIterable() {
    Iterable<?> emptyIterable = CollectionUtils.emptyIterable();

    assertThat(emptyIterable, is(notNullValue(Iterable.class)));
    assertThat(emptyIterable.iterator(), is(notNullValue(Iterator.class)));
    assertThat(emptyIterable.iterator().hasNext(), is(false));
  }

  @Test
  public void filterCollection() {
    Filter<Integer> evenNumberFilter = NumberUtils::isEven;
    Filter<Integer> oddNumberFilter = NumberUtils::isOdd;

    Collection<Integer> numbers = asCollection(1, 2, 3, 4, 5, 6, 7, 8, 9);
    Collection<Integer> evenNumbers = CollectionUtils.filter(numbers, evenNumberFilter);
    Collection<Integer> oddNumbers = CollectionUtils.filter(numbers, oddNumberFilter);

    assertThat(evenNumbers, is(notNullValue(Collection.class)));
    assertThat(evenNumbers, is(not(sameInstance(numbers))));
    assertThat(oddNumbers, is(notNullValue(Collection.class)));
    assertThat(oddNumbers, is(not(sameInstance(numbers))));

    assertElements(evenNumbers, 2, 4, 6, 8);
    assertElements(oddNumbers, 1, 3, 5, 7, 9);
  }

  @Test
  public void filterCollectionAcceptsAll() {
    assertElements(CollectionUtils.filter(asCollection(0, 1, 2), (element) -> true), 0, 1, 2);
  }

  @Test
  public void filterCollectionRejectsAll() {
    assertElements(CollectionUtils.filter(asCollection(0, 1, 2), (element) -> false));
  }

  @Test
  public void filterEmptyCollection() {
    assertElements(CollectionUtils.filter(Collections.emptyList(), (element) -> true));
  }

  @Test
  public void filterNullCollection() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Collection cannot be null");

    CollectionUtils.filter(null, (element) -> true);
  }

  @Test
  public void filterWithNullFilter() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Filter cannot be null");

    CollectionUtils.filter(Collections.emptyList(), null);
  }

  @Test
  public void filterAndTransformCollection() {
    Collection<String> strings = asCollection("test", null, "testing", "", "tested", "  ");

    Collection<String> upperCaseStrings = CollectionUtils.filterAndTransform(strings,
      new FilteringTransformer<String>() {
        @Override
        public boolean accept(String value) {
          return StringUtils.hasText(value);
        }

        @Override
        public String transform(String value) {
          return value.toUpperCase();
        }
      }
    );

    assertThat(upperCaseStrings, is(notNullValue(Collection.class)));
    assertThat(upperCaseStrings, is(not(sameInstance(strings))));
    assertElements(upperCaseStrings, "TEST", "TESTING", "TESTED");
  }

  @Test
  public void filterAndTransformCollectionAcceptsAll() {
    Collection<Integer> numbers = asCollection(0, 1, 2, 4, 8);

    Collection<Integer> negativeNumbers = CollectionUtils.filterAndTransform(numbers,
      new FilteringTransformer<Integer>() {
        @Override
        public boolean accept(Integer number) {
          return (number != null);
        }

        @Override
        public Integer transform(Integer number) {
          return (number * -1);
        }
      });

    assertThat(negativeNumbers, is(notNullValue(Collection.class)));
    assertThat(negativeNumbers, is(not(sameInstance(numbers))));
    assertElements(negativeNumbers, 0, -1, -2, -4, -8);
    assertElements(numbers, 0, 1, 2, 4, 8);
  }

  @Test
  public void filterAndTransformCollectionRejectsAll() {
    Collection<Integer> numbers = asCollection(0, 1, 2, 4, 8);

    Collection<Integer> noNumbers = CollectionUtils.filterAndTransform(numbers,
      new FilteringTransformer<Integer>() {
        @Override
        public boolean accept(Integer number) {
          return false;
        }

        @Override
        public Integer transform(Integer number) {
          return (number * -1);
        }
      });

    assertThat(noNumbers, is(notNullValue(Collection.class)));
    assertThat(noNumbers, is(not(sameInstance(numbers))));
    assertThat(noNumbers.isEmpty(), is(true));
    assertElements(numbers, 0, 1, 2, 4, 8);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void filterAndTransformEmptyCollection() {
    FilteringTransformer<Object> mockFilteringTransformer = mock(FilteringTransformer.class);

    when(mockFilteringTransformer.accept(anyObject())).thenReturn(true);
    when(mockFilteringTransformer.transform(anyObject())).thenReturn(null);

    Collection<Object> emptyCollection = Collections.emptyList();
    Collection<Object> filteredTransformedCollection = CollectionUtils.filterAndTransform(
      emptyCollection, mockFilteringTransformer);

    assertThat(filteredTransformedCollection, is(notNullValue(Collection.class)));
    assertThat(filteredTransformedCollection, is(not(sameInstance(emptyCollection))));
    assertThat(filteredTransformedCollection.isEmpty(), is(true));

    verifyZeroInteractions(mockFilteringTransformer);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void filterAndTransformNullCollection() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Collection cannot be null");

    FilteringTransformer mockFilteringTransformer = mock(FilteringTransformer.class);

    try {
      CollectionUtils.filterAndTransform(null, mockFilteringTransformer);
    }
    finally {
      verifyZeroInteractions(mockFilteringTransformer);
    }
  }

  @Test
  public void filterAndTransformWithNullFilter() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("FilteringTransformer cannot be null");

    CollectionUtils.filterAndTransform(Collections.emptyList(), null);
  }

  @Test
  public void find() {
    Person jackBlack = Person.newPerson("Jack", "Black");
    Person cookieDoe = Person.newPerson("Cookie", "Doe");
    Person froDoe = Person.newPerson("Fro", "Doe");
    Person hoeDoe = Person.newPerson("Hoe", "Doe");
    Person janeDoe = Person.newPerson("Jane", "Doe");
    Person joeDoe = Person.newPerson("Joe", "Doe");
    Person jonDoe = Person.newPerson("Jon", "Doe");
    Person pieDoe = Person.newPerson("Pie", "Doe");
    Person playDoe = Person.newPerson("Play", "Doe");
    Person sourDoe = Person.newPerson("Sour", "Doe");
    Person joeDirt = Person.newPerson("Joe", "Dirt");
    Person jackHandy = Person.newPerson("Jack", "Handy");
    Person sandyHandy = Person.newPerson("Sandy", "Handy");

    List<Person> people = Arrays.asList(jackHandy, jonDoe, janeDoe, sandyHandy, sourDoe, pieDoe, cookieDoe, joeDirt,
      playDoe, jackBlack, joeDoe, froDoe, hoeDoe);

    Person foundPerson = CollectionUtils.find(people, (person) -> "Joe".equalsIgnoreCase(person.getFirstName()));

    assertThat(foundPerson, is(equalTo(joeDirt)));

    foundPerson = CollectionUtils.find(people, (person) -> "Jack".equalsIgnoreCase(person.getFirstName()));

    assertThat(foundPerson, is(equalTo(jackHandy)));
  }

  @Test
  public void findWithNonMatchingFilter() {
    Person jackBlack = Person.newPerson("Jack", "Black");
    Person cookieDoe = Person.newPerson("Cookie", "Doe");
    Person froDoe = Person.newPerson("Fro", "Doe");
    Person hoeDoe = Person.newPerson("Hoe", "Doe");
    Person janeDoe = Person.newPerson("Jane", "Doe");
    Person joeDoe = Person.newPerson("Joe", "Doe");
    Person jonDoe = Person.newPerson("Jon", "Doe");
    Person pieDoe = Person.newPerson("Pie", "Doe");
    Person playDoe = Person.newPerson("Play", "Doe");
    Person sourDoe = Person.newPerson("Sour", "Doe");
    Person joeDirt = Person.newPerson("Joe", "Dirt");
    Person jackHandy = Person.newPerson("Jack", "Handy");
    Person sandyHandy = Person.newPerson("Sandy", "Handy");

    List<Person> people = Arrays.asList(jackHandy, jonDoe, janeDoe, sandyHandy, sourDoe, pieDoe, cookieDoe, joeDirt,
      playDoe, jackBlack, joeDoe, froDoe, hoeDoe);

    Person foundPerson = CollectionUtils.find(people, (person) -> ("Play".equalsIgnoreCase(person.getFirstName())
          && "Toe".equalsIgnoreCase(person.getLastName())));

    assertThat(foundPerson, is(nullValue(Person.class)));
  }

  @Test
  public void findWithEmptyIterable() {
    assertThat(CollectionUtils.find(asIterable(), (element) -> true), is(nullValue()));
  }

  @Test
  public void findWithNullIterable() {
    assertThat(CollectionUtils.find(null, (element) -> true), is(nullValue()));
  }

  @Test
  public void findWithNullFilter() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Filter cannot be null");

    CollectionUtils.find(asIterable(), null);
  }

  @Test
  public void findAll() {
    Person jackBlack = Person.newPerson("Jack", "Black");
    Person cookieDoe = Person.newPerson("Cookie", "Doe");
    Person froDoe = Person.newPerson("Fro", "Doe");
    Person hoeDoe = Person.newPerson("Hoe", "Doe");
    Person janeDoe = Person.newPerson("Jane", "Doe");
    Person joeDoe = Person.newPerson("Joe", "Doe");
    Person jonDoe = Person.newPerson("Jon", "Doe");
    Person pieDoe = Person.newPerson("Pie", "Doe");
    Person playDoe = Person.newPerson("Play", "Doe");
    Person sourDoe = Person.newPerson("Sour", "Doe");
    Person joeDirt = Person.newPerson("Joe", "Dirt");
    Person jackHandy = Person.newPerson("Jack", "Handy");
    Person sandyHandy = Person.newPerson("Sandy", "Handy");

    List<Person> people = Arrays.asList(jackHandy, jonDoe, janeDoe, sandyHandy, sourDoe, pieDoe, cookieDoe, joeDirt,
      playDoe, jackBlack, joeDoe, froDoe, hoeDoe);

    List<Person> doeFamily = CollectionUtils.findAll(people, (person) -> "doe".equalsIgnoreCase(person.getLastName()));

    assertThat(doeFamily, is(notNullValue(List.class)));
    assertThat(doeFamily.size(), is(equalTo(9)));
    assertElements(doeFamily, cookieDoe, froDoe, hoeDoe, janeDoe, joeDoe, jonDoe, pieDoe, playDoe, sourDoe);
  }

  @Test
  public void findAllWithFilterMatchingAll() {
    Person jackBlack = Person.newPerson("Jack", "Black");
    Person cookieDoe = Person.newPerson("Cookie", "Doe");
    Person froDoe = Person.newPerson("Fro", "Doe");
    Person hoeDoe = Person.newPerson("Hoe", "Doe");
    Person janeDoe = Person.newPerson("Jane", "Doe");
    Person joeDoe = Person.newPerson("Joe", "Doe");
    Person jonDoe = Person.newPerson("Jon", "Doe");
    Person pieDoe = Person.newPerson("Pie", "Doe");
    Person playDoe = Person.newPerson("Play", "Doe");
    Person sourDoe = Person.newPerson("Sour", "Doe");
    Person joeDirt = Person.newPerson("Joe", "Dirt");
    Person jackHandy = Person.newPerson("Jack", "Handy");
    Person sandyHandy = Person.newPerson("Sandy", "Handy");

    List<Person> people = Arrays.asList(jackHandy, jonDoe, janeDoe, sandyHandy, sourDoe, pieDoe, cookieDoe, joeDirt,
      playDoe, jackBlack, joeDoe, froDoe, hoeDoe);

    List<Person> allPeople = CollectionUtils.findAll(people, (person) -> true);

    assertThat(allPeople, is(notNullValue(List.class)));
    assertThat(allPeople.size(), is(equalTo(people.size())));
    assertElements(allPeople, people.toArray(new Person[people.size()]));
  }

  @Test
  public void findAllWithFilterMatchingNone() {
    Person jackBlack = Person.newPerson("Jack", "Black");
    Person cookieDoe = Person.newPerson("Cookie", "Doe");
    Person froDoe = Person.newPerson("Fro", "Doe");
    Person hoeDoe = Person.newPerson("Hoe", "Doe");
    Person janeDoe = Person.newPerson("Jane", "Doe");
    Person joeDoe = Person.newPerson("Joe", "Doe");
    Person jonDoe = Person.newPerson("Jon", "Doe");
    Person pieDoe = Person.newPerson("Pie", "Doe");
    Person playDoe = Person.newPerson("Play", "Doe");
    Person sourDoe = Person.newPerson("Sour", "Doe");
    Person joeDirt = Person.newPerson("Joe", "Dirt");
    Person jackHandy = Person.newPerson("Jack", "Handy");
    Person sandyHandy = Person.newPerson("Sandy", "Handy");

    List<Person> people = Arrays.asList(jackHandy, jonDoe, janeDoe, sandyHandy, sourDoe, pieDoe, cookieDoe, joeDirt,
      playDoe, jackBlack, joeDoe, froDoe, hoeDoe);

    List<Person> noPeople = CollectionUtils.findAll(people, (person) -> false);

    assertThat(noPeople, is(notNullValue(List.class)));
    assertThat(noPeople.isEmpty(), is(true));
  }

  @Test
  public void findAllWitEmptyIterable() {
    List<?> matches = CollectionUtils.findAll(asIterable(), (element) -> true);

    assertThat(matches, is(notNullValue(List.class)));
    assertThat(matches.isEmpty(), is(true));
  }

  @Test
  public void findAllWitNullIterable() {
    List<?> matches = CollectionUtils.findAll(null, (element) -> true);

    assertThat(matches, is(notNullValue(List.class)));
    assertThat(matches.isEmpty(), is(true));
  }

  @Test
  public void findAllWithNullFilter() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Filter cannot be null");

    CollectionUtils.findAll(Collections.emptyList(), null);
  }

  @Test
  public void isEmptyWithEmptyCollectionIsTrue() {
    assertThat(CollectionUtils.isEmpty(null), is(true));
    assertThat(CollectionUtils.isEmpty(Collections.emptyList()), is(true));
    assertThat(CollectionUtils.isEmpty(new ArrayList<>(10)), is(true));
  }

  @Test
  public void isEmptyWithNonEmptyCollectionIsFalse() {
    assertThat(CollectionUtils.isEmpty(Collections.singleton(0)), is(false));
    assertThat(CollectionUtils.isEmpty(asCollection(0, 1, 2)), is(false));
    assertThat(CollectionUtils.isEmpty(Collections.singleton("null")), is(false));
    assertThat(CollectionUtils.isEmpty(asCollection("test", "testing", "tested")), is(false));
  }

  @Test
  public void isNotEmptyWithEmptyCollectionIsFalse() {
    assertThat(CollectionUtils.isNotEmpty(null), is(false));
    assertThat(CollectionUtils.isNotEmpty(Collections.emptyList()), is(false));
    assertThat(CollectionUtils.isNotEmpty(new ArrayList<>(10)), is(false));
  }

  @Test
  public void isNotEmptyWithNonEmptyCollectionIsTrue() {
    assertThat(CollectionUtils.isNotEmpty(Collections.singleton(0)), is(true));
    assertThat(CollectionUtils.isNotEmpty(asCollection(0, 1, 2)), is(true));
    assertThat(CollectionUtils.isNotEmpty(Collections.singleton("null")), is(true));
    assertThat(CollectionUtils.isNotEmpty(asCollection("test", "testing", "tested")), is(true));
  }

  @Test
  public void nullSafeCollectionWithCollection() {
    Collection<?> collection = asCollection(1, 2, 3);

    assertThat(CollectionUtils.nullSafeCollection(collection), is(sameInstance(collection)));
  }

  @Test
  public void nullSafeCollectionWithEmptyCollection() {
    Collection<?> collection = asCollection();

    assertThat(CollectionUtils.nullSafeCollection(collection), is(sameInstance(collection)));
  }

  @Test
  public void nullSafeCollectionWithNullCollection() {
    Collection<?> collection = CollectionUtils.nullSafeCollection(null);

    assertThat(collection, is(notNullValue(Collection.class)));
    assertThat(collection.isEmpty(), is(true));
  }

  @Test
  public void nullSafeEnumerationWithEnumeration() {
    Enumeration<?> enumeration = asEnumeration("test");

    assertThat(CollectionUtils.nullSafeEnumeration(enumeration), is(sameInstance(enumeration)));
  }

  @Test
  public void nullSafeEnumerationWithEmptyEnumeration() {
    Enumeration<?> enumeration = Collections.emptyEnumeration();

    assertThat(CollectionUtils.nullSafeEnumeration(enumeration), is(sameInstance(enumeration)));
  }

  @Test
  public void nullSafeEnumerationWithNullEnumeration() {
    Enumeration<?> enumeration = CollectionUtils.nullSafeEnumeration(null);

    assertThat(enumeration, is(notNullValue(Enumeration.class)));
    assertThat(enumeration.hasMoreElements(), is(false));
  }

  @Test
  public void nullSafeIterableWithIterable() {
    Iterable<?> iterable = asIterable("test");

    assertThat(CollectionUtils.nullSafeIterable(iterable), is(sameInstance(iterable)));
  }

  @Test
  public void nullSafeIterableWithEmptyIterable() {
    Iterable<?> iterable = asIterable();

    assertThat(CollectionUtils.nullSafeIterable(iterable), is(sameInstance(iterable)));
  }

  @Test
  public void nullSafeIterableWithNullIterable() {
    Iterable<?> iterable = CollectionUtils.nullSafeIterable(null);

    assertThat(iterable, is(notNullValue(Iterable.class)));
    assertThat(iterable.iterator(), is(notNullValue(Iterator.class)));
    assertThat(iterable.iterator().hasNext(), is(false));
  }

  @Test
  public void nullSafeIteratorWithIterator() {
    Iterator<?> iterator = asIterator("test");

    assertThat(CollectionUtils.nullSafeIterator(iterator), is(sameInstance(iterator)));
  }

  @Test
  public void nullSafeIteratorWithEmptyIterator() {
    Iterator<?> iterator = Collections.emptyIterator();

    assertThat(CollectionUtils.nullSafeIterator(iterator), is(sameInstance(iterator)));
  }

  @Test
  public void nullSafeIteratorWithNullIterator() {
    Iterator<?> iterator = CollectionUtils.nullSafeIterator(null);

    assertThat(iterator, is(notNullValue(Iterator.class)));
    assertThat(iterator.hasNext(), is(false));
  }

  @Test
  public void nullSafeListWitList() {
    List<?> list = Collections.singletonList("test");

    assertThat(CollectionUtils.nullSafeList(list), is(sameInstance(list)));
  }

  @Test
  public void nullSafeListWitEmptyList() {
    List<?> list = Collections.emptyList();

    assertThat(CollectionUtils.nullSafeList(list), is(sameInstance(list)));
  }

  @Test
  public void nullSafeListWithNullList() {
    List<?> list = CollectionUtils.nullSafeList(null);

    assertThat(list, is(notNullValue(List.class)));
    assertThat(list.isEmpty(), is(true));
  }

  @Test
  public void nullSafeSetWithSet() {
    Set<?> set = Collections.singleton("test");

    assertThat(CollectionUtils.nullSafeSet(set), is(sameInstance(set)));
  }

  @Test
  public void nullSafeSetWithEmptySet() {
    Set<?> set = Collections.emptySet();

    assertThat(CollectionUtils.nullSafeSet(set), is(sameInstance(set)));
  }

  @Test
  public void nullSafeSetWithNullSet() {
    Set<?> set = CollectionUtils.nullSafeSet(null);

    assertThat(set, is(notNullValue(Set.class)));
    assertThat(set.isEmpty(), is(true));
  }

  @Test
  public void nullSafeSizeForCollection() {
    Collection<?> collection = Collections.singleton("test");

    assertThat(CollectionUtils.nullSafeSize(collection), is(equalTo(collection.size())));
  }

  @Test
  public void nullSafeSizeForEmptyCollection() {
    assertThat(CollectionUtils.nullSafeSize(asCollection()), is(equalTo(0)));
  }

  @Test
  public void nullSafeSizeForNullCollection() {
    assertThat(CollectionUtils.nullSafeSize(null), is(equalTo(0)));
  }

  @Test
  public void shuffle() {
    List<Integer> numbers = Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
    List<Integer> shuffledNumbers = CollectionUtils.shuffle(new ArrayList<>(numbers));

    assertThat(shuffledNumbers, is(notNullValue(List.class)));
    assertThat(shuffledNumbers, is(not(equalTo(numbers))));
    assertShuffled(numbers, shuffledNumbers);

    List<Integer> shuffledNumbersAgain = CollectionUtils.shuffle(new ArrayList<>(shuffledNumbers));

    assertThat(shuffledNumbersAgain, is(notNullValue(List.class)));
    assertThat(shuffledNumbersAgain, is(not(equalTo(shuffledNumbers))));
    assertThat(shuffledNumbersAgain, is(not(equalTo(numbers))));
    assertShuffled(shuffledNumbers, shuffledNumbersAgain);
    assertShuffled(numbers, shuffledNumbersAgain);
  }

  @Test
  public void shuffleEmptyList() {
    List<?> emptyList = Collections.emptyList();
    List<?> shuffledEmptyList = CollectionUtils.shuffle(emptyList);

    assertThat(shuffledEmptyList, is(sameInstance(emptyList)));
    assertThat(shuffledEmptyList.isEmpty(), is(true));
  }

  @Test
  public void shuffleNullList() {
    assertThat(CollectionUtils.shuffle(null), is(nullValue(List.class)));
  }

  @Test
  public void shuffleSingleElementList() {
    List<String> singleElementList = Collections.singletonList("test");
    List<String> shuffledSingleElementList = CollectionUtils.shuffle(singleElementList);

    assertThat(shuffledSingleElementList, is(sameInstance(singleElementList)));
    assertThat(shuffledSingleElementList.size(), is(equalTo(1)));
    assertThat(shuffledSingleElementList.get(0), is(equalTo("test")));
  }

  @Test
  public void subList() {
    List<Integer> subList = CollectionUtils.subList(Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 1, 2, 4, 8);

    assertThat(subList, is(notNullValue(List.class)));
    assertElements(subList, 1, 2, 4, 8);
  }

  @Test
  public void subListWithEmptyList() {
    List<Object> list = Collections.emptyList();
    List<?> subList = CollectionUtils.subList(list);

    assertThat(subList, is(not(sameInstance(list))));
    assertThat(subList.isEmpty(), is(true));
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void subListWithEmptyListAndIndices() {
    CollectionUtils.subList(Collections.emptyList(), 0, 1, 2);
  }

  @Test
  public void subListWithListAndNoIndices() {
    List<String> subList = CollectionUtils.subList(Arrays.asList("test", "testing", "tested"));

    assertThat(subList, is(notNullValue(List.class)));
    assertThat(subList.isEmpty(), is(true));
  }

  @Test
  public void subListWithNullList() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("List cannot be null");

    CollectionUtils.subList(null, 0, 1, 2);
  }

  @Test
  public void subListWithNullIndices() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Indices cannot be null");

    CollectionUtils.subList(Collections.emptyList(), (int[]) null);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void subListWithOverflowIndex() {
    CollectionUtils.subList(Collections.singletonList("test"), 1);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void subListWithUnderFlowIndex() {
    CollectionUtils.subList(Collections.singletonList("test"), -1);
  }

  @Test
  public void toStringFromIterable() {
    assertThat(CollectionUtils.toString(asIterable("test", "testing", "tested")),
      is(equalTo("[test, testing, tested]")));
  }

  @Test
  public void toStringFromIterableUsingRenderer() {
    assertThat(CollectionUtils.toString(asIterable("test"), String::toUpperCase), is(equalTo("[TEST]")));
  }

  @Test
  public void toStringFromEmptyIterable() {
    assertThat(CollectionUtils.toString(asIterable()), is(equalTo("[]")));
  }

  @Test
  public void toStringFromNullIterable() {
    assertThat(CollectionUtils.toString(null), is(equalTo("[]")));
  }

  @Test
  public void transformCollection() {
    Collection<String> collection = asCollection("test", "testing", "tested");
    Collection<String> transformedCollection = CollectionUtils.transform(collection, StringUtils::toUpperCase);

    assertThat(transformedCollection, is(not(sameInstance(collection))));
    assertElements(transformedCollection, "TEST", "TESTING", "TESTED");
  }

  @Test
  public void transformEmptyCollection() {
    Collection<Object> collection = Collections.emptySet();
    Collection<Object> transformedCollection = CollectionUtils.transform(collection, (value) -> "test");

    assertThat(transformedCollection, is(notNullValue(Collection.class)));
    assertThat(transformedCollection, is(not(sameInstance(collection))));
    assertThat(transformedCollection.isEmpty(), is(true));
  }

  @Test
  public void transformNullCollection() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Collection cannot be null");

    CollectionUtils.transform(null, (value) -> "test");
  }

  @Test
  public void transformWithNullTransformer() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Transformer cannot be null");

    CollectionUtils.transform(Collections.emptyList(), null);
  }

  @Test
  public void unmodifiableIterator() {
    String[] elements = { "test", "testing", "tested" };
    Iterator<String> unmodifiableIterator = CollectionUtils.unmodifiableIterator(asIterator(elements));

    assertThat(unmodifiableIterator, is(notNullValue(Iterator.class)));

    for (String element : elements) {
      assertThat(unmodifiableIterator.hasNext(), is(true));
      assertThat(unmodifiableIterator.next(), is(equalTo(element)));
    }

    assertThat(unmodifiableIterator.hasNext(), is(false));
  }

  @Test
  public void unmodifiableIteratorIsImmutable() {
    Iterator<String> iterator = CollectionUtils.unmodifiableIterator(asIterator("test"));

    assertThat(iterator, is(notNullValue(Iterator.class)));
    assertThat(iterator.hasNext(), is(true));

    try {
      exception.expect(UnsupportedOperationException.class);
      exception.expectCause(is(nullValue(Throwable.class)));
      exception.expectMessage("Iterator is immutable");

      iterator.remove();
    }
    finally {
      assertThat(iterator.hasNext(), is(true));
      assertThat(iterator.next(), is(equalTo("test")));
      assertThat(iterator.hasNext(), is(false));
    }
  }

  @Test
  public void unmodifiableIteratorWithNullIterator() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Iterator cannot be null");

    CollectionUtils.unmodifiableIterator(null);
  }

  static class Person {

    private final String firstName;
    private final String lastName;

    public static Person newPerson(String firstName, String lastName) {
      return new Person(firstName, lastName);
    }

    Person(String firstName, String lastName) {
      Assert.hasText(firstName, "'firstName' must be specified");
      Assert.hasText(lastName, "'lastName' must be specified");

      this.firstName = firstName;
      this.lastName = lastName;
    }

    public String getFirstName() {
      return firstName;
    }

    public String getLastName() {
      return lastName;
    }

    @Override
    public boolean equals(final Object obj) {
      if (obj == this) {
        return true;
      }

      if (!(obj instanceof Person)) {
        return false;
      }

      Person that = (Person) obj;

      return ObjectUtils.equals(this.getFirstName(), that.getFirstName())
        && ObjectUtils.equals(this.getLastName(), that.getLastName());
    }

    @Override
    public int hashCode() {
      int hashValue = 17;
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getFirstName());
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getLastName());
      return hashValue;
    }

    @Override
    public String toString() {
      return getFirstName().concat(" ").concat(getLastName());
    }
  }
}
