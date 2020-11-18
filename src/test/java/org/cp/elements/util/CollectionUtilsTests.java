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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
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
import org.junit.Test;

/**
 * Unit Tests for {@link CollectionUtils}.
 *
 * @author John J. Blum
 * @see java.lang.Iterable
 * @see java.util.Arrays
 * @see java.util.Collection
 * @see java.util.Collections
 * @see java.util.Enumeration
 * @see java.util.Iterator
 * @see java.util.List
 * @see java.util.Set
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.cp.elements.util.CollectionUtils
 * @since 1.0.0
 */
public class CollectionUtilsTests {

  @SafeVarargs
  private final <T> void assertElements(Collection<T> collection, T... elements) {

    assertThat(collection).isNotNull();
    assertThat(collection.size()).isEqualTo(elements.length);
    assertThat(collection.containsAll(asCollection(elements))).isTrue();
  }

  private <T> void assertShuffled(Iterable<T> source, Iterable<T> target) {

    assertThat(source != null && source.iterator().hasNext()).as("'source' must not be null and must have elements")
      .isTrue();
    assertThat(target != null && target.iterator().hasNext()).as("'target' must not be null and must have elements")
      .isTrue();

    Iterator<T> targetIterator = target.iterator();

    boolean shuffled = false;

    for (T sourceElement : source) {
      shuffled &= targetIterator.hasNext();
      shuffled |= !sourceElement.equals(targetIterator.next());
    }

    assertThat(shuffled).as(String.format("Target [%1$s] was not shuffled", target)).isTrue();
  }

  @SafeVarargs
  private final <T> Collection<T> asCollection(T... elements) {
    return Arrays.asList(elements);
  }

  @SafeVarargs
  private final <T> Enumeration<T> asEnumeration(T... elements) {

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
  private final <T> Iterable<T> asIterable(T... elements) {
    return () -> asIterator(elements);
  }

  @SafeVarargs
  private final <T> Iterator<T> asIterator(T... elements) {

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
  public void addAllArrayElementsToList() {

    List<Integer> numbers = new ArrayList<>(Arrays.asList(1, 2, 3));

    List<Integer> newNumbers = CollectionUtils.addAll(numbers, 3, 4, 5);

    assertThat(newNumbers).isNotNull();
    assertThat(newNumbers).hasSize(6);
    assertThat(newNumbers).containsExactly(1, 2, 3, 3, 4, 5);
  }

  @Test
  public void addAllArrayElementsToSet() {

    Set<Integer> numbers = new HashSet<>(Arrays.asList(1, 2, 3));

    Set<Integer> newNumbers = CollectionUtils.addAll(numbers, 3, 4, 5);

    assertThat(newNumbers).isNotNull();
    assertThat(newNumbers).hasSize(5);
    assertThat(newNumbers).containsExactly(1, 2, 3, 4, 5);
  }

  @Test
  public void addEmptyArrayToCollection() {

    Collection<Integer> numbers = new ArrayList<>(Arrays.asList(1, 2, 3));
    Collection<Integer> newNumbers = CollectionUtils.addAll(numbers);

    assertThat(newNumbers).isNotNull();
    assertThat(newNumbers).hasSize(3);
    assertThat(newNumbers).containsExactly(1, 2, 3);

  }

  @Test
  public void addNullArrayToCollection() {

    Collection<Integer> numbers = new ArrayList<>(Arrays.asList(1, 2, 3));
    Collection<Integer> newNumbers = CollectionUtils.addAll(numbers, (Integer[]) null);

    assertThat(newNumbers).isNotNull();
    assertThat(newNumbers).hasSize(3);
    assertThat(newNumbers).containsExactly(1, 2, 3);
  }

  @Test(expected = IllegalArgumentException.class)
  public void addArrayElementsToNullCollection() {

    try {
      CollectionUtils.addAll(null, 1, 2, 3);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Collection is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void addAllIterableElementsToList() {

    List<Integer> numbers = new ArrayList<>(Arrays.asList(1, 2, 3));
    List<Integer> newNumbers = CollectionUtils.addAll(numbers, asIterable(3, 4, 5));

    assertThat(newNumbers).isSameAs(numbers);
    assertThat(newNumbers).hasSize(6);
    assertThat(newNumbers).containsExactly(1, 2, 3, 3, 4, 5);
  }

  @Test
  public void addAllIterableElementsToSet() {

    Set<Integer> numbers = new HashSet<>(Arrays.asList(1, 2, 3));
    Set<Integer> newNumbers = CollectionUtils.addAll(numbers, asIterable(3, 4, 5));

    assertThat(newNumbers).isSameAs(numbers);
    assertThat(newNumbers).hasSize(5);
    assertThat(newNumbers).containsExactly(1, 2, 3, 4, 5);
  }

  @Test
  public void addEmptyIterableToCollection() {

    Collection<Integer> numbers = new HashSet<>(Arrays.asList(1, 2, 3));
    Collection<Integer> newNumbers = CollectionUtils.addAll(numbers, asIterable());

    assertThat(newNumbers).isSameAs(numbers);
    assertThat(newNumbers).hasSize(3);
    assertThat(newNumbers).containsExactly(1, 2, 3);
  }

  @Test
  public void addNullIterableToCollection() {

    Collection<Integer> numbers = new HashSet<>(Arrays.asList(1, 2, 3));
    Collection<Integer> newNumbers = CollectionUtils.addAll(numbers, (Iterable<Integer>) null);

    assertThat(newNumbers).isSameAs(numbers);
    assertThat(newNumbers).hasSize(3);
    assertThat(newNumbers).containsExactly(1, 2, 3);
  }

  @Test(expected = IllegalArgumentException.class)
  public void addIterableElementsToNullCollection() {

    try {
      CollectionUtils.addAll(null, asIterable(1));
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Collection is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void asEnumerationForIterator() {

    String[] elements = { "test", "testing", "tested"};

    Enumeration<String> enumeration = CollectionUtils.asEnumeration(asIterator(elements));

    assertThat(enumeration).isNotNull();

    for (String element : elements) {
      assertThat(enumeration.hasMoreElements()).isTrue();
      assertThat(enumeration.nextElement()).isEqualTo(element);
    }

    assertThat(enumeration.hasMoreElements()).isFalse();
  }

  @Test
  public void asEnumerationForEmptyIterator() {

    Enumeration<?> enumeration = CollectionUtils.asEnumeration(Collections.emptyIterator());

    assertThat(enumeration).isNotNull();
    assertThat(enumeration.hasMoreElements()).isFalse();
  }

  @Test
  public void asEnumerationForNullIterator() {

    Enumeration<?> enumeration = CollectionUtils.asEnumeration(null);

    assertThat(enumeration).isNotNull();
    assertThat(enumeration.hasMoreElements()).isFalse();
  }

  @Test
  public void asEnumerationForSingleElementIterator() {

    Enumeration<String> enumeration = CollectionUtils.asEnumeration(asIterator("test"));

    assertThat(enumeration).isNotNull();
    assertThat(enumeration.hasMoreElements()).isTrue();
    assertThat(enumeration.nextElement()).isEqualTo("test");
    assertThat(enumeration.hasMoreElements()).isFalse();
  }

  @Test
  public void asIterableWithEnumeration() {

    Integer[] elements = { 0, 1, 2 };

    Iterable<Integer> iterable = CollectionUtils.asIterable(asEnumeration(elements));

    assertThat(iterable).isNotNull();

    int index = 0;

    for (Integer element : iterable) {
      assertThat(element).isEqualTo(elements[index++]);
    }

    assertThat(index).isEqualTo(elements.length);
  }

  @Test
  public void asIterableWithEmptyEnumeration() {

    Iterable<?> iterable = CollectionUtils.asIterable(Collections.emptyEnumeration());

    assertThat(iterable).isNotNull();
    assertThat(iterable.iterator()).isNotNull();
    assertThat(iterable.iterator().hasNext()).isFalse();
  }

  @Test
  public void asIterableWithNullEnumeration() {

    Iterable<?> iterable = CollectionUtils.asIterable((Enumeration<?>) null);

    assertThat(iterable).isNotNull();
    assertThat(iterable.iterator()).isNotNull();
    assertThat(iterable.iterator().hasNext()).isFalse();
  }

  @Test
  public void asIterableWithIterator() {

    Integer[] elements = { 0, 1, 2 };

    Iterable<Integer> iterable = CollectionUtils.asIterable(asIterator(elements));

    assertThat(iterable).isNotNull();

    int index = 0;

    for (Integer element : iterable) {
      assertThat(element).isEqualTo(elements[index++]);
    }

    assertThat(index).isEqualTo(elements.length);
  }

  @Test
  public void asIterableWithEmptyIterator() {

    Iterable<?> iterable = CollectionUtils.asIterable(Collections.emptyIterator());

    assertThat(iterable).isNotNull();
    assertThat(iterable.iterator()).isNotNull();
    assertThat(iterable.iterator().hasNext()).isFalse();
  }

  @Test
  public void asIterableWithNullIterator() {

    Iterable<?> iterable = CollectionUtils.asIterable((Iterator<?>) null);

    assertThat(iterable).isNotNull();
    assertThat(iterable.iterator()).isNotNull();
    assertThat(iterable.iterator().hasNext()).isFalse();
  }

  @Test
  public void asIteratorForEnumeration() {

    Integer[] elements = { 0, 1, 2 };

    Iterator<Integer> iterator = CollectionUtils.asIterator(asEnumeration(elements));

    assertThat(iterator).isNotNull();

    for (Integer element : elements) {
      assertThat(iterator.hasNext()).isTrue();
      assertThat(iterator.next()).isEqualTo(element);
    }

    assertThat(iterator.hasNext()).isFalse();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void asIteratorForEnumerationIsUnmodifiable() {

    Iterator<String> iterator = CollectionUtils.asIterator(asEnumeration("test"));

    assertThat(iterator).isNotNull();
    assertThat(iterator.hasNext()).isTrue();

    try {
      iterator.remove();
    }
    finally {
      assertThat(iterator.hasNext()).isTrue();
      assertThat(iterator.next()).isEqualTo("test");
      assertThat(iterator.hasNext()).isFalse();
    }
  }

  @Test
  public void asIteratorForEmptyEnumeration() {

    Iterator<?> iterator = CollectionUtils.asIterator(Collections.emptyEnumeration());

    assertThat(iterator).isNotNull();
    assertThat(iterator.hasNext()).isFalse();
  }

  @Test
  public void asIteratorForNullEnumeration() {

    Iterator<?> iterator = CollectionUtils.asIterator(null);

    assertThat(iterator).isNotNull();
    assertThat(iterator.hasNext()).isFalse();
  }

  @Test
  public void asIteratorForSingleElementEnumeration() {

    Iterator<?> iterator = CollectionUtils.asIterator(asEnumeration("test"));

    assertThat(iterator).isNotNull();
    assertThat(iterator.hasNext()).isTrue();
    assertThat(iterator.next()).isEqualTo("test");
    assertThat(iterator.hasNext()).isFalse();
  }

  @Test
  public void asListWithArray() {

    List<Object> list = CollectionUtils.asList("test", "testing", "tested");

    assertThat(list).isNotNull();
    assertThat(list.size()).isEqualTo(3);
    assertThat(list.containsAll(asCollection("test", "testing", "tested"))).isTrue();

    for (int index = 0, size = list.size(); index < size; index++) {
      list.set(index, list.get(index).toString().toUpperCase());
    }

    assertThat(list.containsAll(asCollection("TEST", "TESTING", "TESTED"))).isTrue();
  }

  @Test
  public void asListWithEmptyArray() {

    List<Object> list = CollectionUtils.asList();

    assertThat(list).isNotNull();
    assertThat(list.isEmpty()).isTrue();
  }

  @Test
  public void asListWithNullArray() {

    List<Object> list = CollectionUtils.asList((Object[]) null);

    assertThat(list).isNotNull();
    assertThat(list.isEmpty()).isTrue();
  }

  @Test
  public void asListWithCollection() {

    Collection<String> collection = asCollection("test", "testing", "tested");

    List<String> list = CollectionUtils.asList(collection);

    assertThat(list).isNotNull();
    assertThat(list).isNotSameAs(collection);
    assertThat(list).isEqualTo(collection);
  }

  @Test
  public void asListWithEmptyIterable() {

    List<?> list = CollectionUtils.asList(asIterable());

    assertThat(list).isNotNull();
    assertThat(list.isEmpty()).isTrue();
  }

  @Test
  public void asListWithIterable() {

    Iterable<String> iterable = asIterable("test", "testing", "tested");

    List<String> list = CollectionUtils.asList(iterable);

    assertThat(list).isNotNull();
    assertThat(list.size()).isEqualTo(3);
    assertThat(list.containsAll(asCollection("test", "testing", "tested"))).isTrue();
  }

  @Test
  public void asListWithNullIterable() {

    List<?> list = CollectionUtils.asList((Iterable<?>) null);

    assertThat(list).isNotNull();
    assertThat(list.isEmpty()).isTrue();
  }

  @Test
  public void asSetWithArray() {

    Set<Integer> set = CollectionUtils.asSet(1, 2, 4, 8);

    assertThat(set).isNotNull();
    assertThat(set.size()).isEqualTo(4);
    assertThat(set.containsAll(asCollection(1, 2, 4, 8))).isTrue();
  }

  @Test
  public void asSetWitEmptyArray() {

    Set<Integer> set = CollectionUtils.asSet();

    assertThat(set).isNotNull();
    assertThat(set.isEmpty()).isTrue();
  }

  @Test
  public void asSetWitNullArray() {

    Set<String> set = CollectionUtils.asSet((String[]) null);

    assertThat(set).isNotNull();
    assertThat(set.isEmpty()).isTrue();
  }

  @Test
  public void asSetWithCollection() {

    Collection<String> collection = asCollection("test", "testing", "tested");

    Set<String> set = CollectionUtils.asSet(collection);

    assertThat(set).isNotNull();
    assertThat(set.size()).isEqualTo(collection.size());
    assertThat(set.containsAll(collection)).isTrue();
  }

  @Test
  public void asSetWithEmptyIterable() {

    Set<?> set = CollectionUtils.asSet(asIterable());

    assertThat(set).isNotNull();
    assertThat(set.isEmpty()).isTrue();
  }

  @Test
  public void asSetWithIterable() {

    Set<Integer> set = CollectionUtils.asSet(asIterable(1, 2, 4, 8));

    assertThat(set).isNotNull();
    assertThat(set.size()).isEqualTo(4);
    assertThat(set.containsAll(asCollection(1, 2, 4, 8))).isTrue();
  }

  @Test
  public void asSetWithIterableHavingDuplicates() {

    Set<Integer> set = CollectionUtils.asSet(asIterable(1, 2, 4, 1, 8, 2));

    assertThat(set).isNotNull();
    assertThat(set.size()).isEqualTo(4);
    assertThat(set.containsAll(asCollection(1, 2, 4, 8))).isTrue();
  }

  @Test
  public void asSetWithNullIterable() {

    Set<?> set = CollectionUtils.asSet((Iterable<?>) null);

    assertThat(set).isNotNull();
    assertThat(set.isEmpty()).isTrue();
  }

  @Test
  public void containsWithCollectionAndArrayElementsReturnsTrue() {

    Collection<?> collection = asCollection(1, 2, 3);

    assertThat(CollectionUtils.containsAny(collection, 1)).isTrue();
    assertThat(CollectionUtils.containsAny(collection, 1, 2)).isTrue();
    assertThat(CollectionUtils.containsAny(collection, 1, 2, 3)).isTrue();
  }

  @Test
  public void containsWithCollectionAndArrayElementsReturnsFalse() {

    Collection<?> collection = asCollection(1, 2, 3);

    assertThat(CollectionUtils.containsAny(collection, -1)).isFalse();
    assertThat(CollectionUtils.containsAny(collection, 1.5d, 2.01d)).isFalse();
    assertThat(CollectionUtils.containsAny(collection, 4, 5, 6)).isFalse();
  }

  @Test
  public void containsWithCollectionAndNullArrayReturnsFalse() {
    assertThat(CollectionUtils.containsAny(asCollection(1, 2, 3), (Object[]) null)).isFalse();
  }

  @Test
  public void containsWithNullCollectionAndArrayReturnsFalse() {
    assertThat(CollectionUtils.containsAny(null, 1, 2, 3)).isFalse();
  }

  @Test
  public void containsWithNullCollectionAndNullArrayIsFalse() {
    assertThat(CollectionUtils.containsAny(null, (Object[]) null)).isFalse();
  }

  @Test
  public void countCollectionReturnsTen() {

    Collection<?> collection = asCollection(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);

    assertThat(CollectionUtils.count(collection)).isEqualTo(10L);
  }

  @Test
  public void countCollectionWithInitialCapacityReturnsZero() {
    assertThat(CollectionUtils.count(new ArrayList<>(10))).isEqualTo(0L);
  }

  @Test
  public void countEmptyCollectionReturnsZero() {
    assertThat(CollectionUtils.count(Collections.emptyList())).isEqualTo(0L);
  }

  @Test
  public void countSingleElementCollectionReturnsOne() {
    assertThat(CollectionUtils.count(Collections.singleton(1))).isEqualTo(1L);
  }

  @Test
  public void countIterableReturnsTen() {

    Iterable<?> iterable = asIterable(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);

    assertThat(CollectionUtils.count(iterable)).isEqualTo(10L);
  }

  @Test
  public void countEmptyIterableReturnsZero() {
    assertThat(CollectionUtils.count(asIterable())).isEqualTo(0L);
  }

  @Test
  public void countSingleElementIterableReturnsOne() {
    assertThat(CollectionUtils.count(asIterable(1))).isEqualTo(1L);
  }

  @Test
  public void countNullReturnsZero() {
    assertThat(CollectionUtils.count(null)).isEqualTo(0L);
  }

  @Test
  public void countCollectionWithFilter() {

    Collection<Integer> numbers = asCollection(1, 2, 3, 4, 5, 6, 7, 8, 9);

    Filter<Integer> evenNumbers = NumberUtils::isEven;
    Filter<Integer> oddNumbers = NumberUtils::isOdd;

    assertThat(CollectionUtils.count(numbers, evenNumbers)).isEqualTo(4L);
    assertThat(CollectionUtils.count(numbers, oddNumbers)).isEqualTo(5L);
  }

  @Test
  public void countEmptyWithFilterReturnsZero() {
    assertThat(CollectionUtils.count(Collections.emptyList(), (element) -> true)).isEqualTo(0L);
  }

  @Test
  public void countIterableWithFilter() {

    Iterable<Integer> iterable = asIterable(0, 1, 2);

    Filter<Integer> evenNumbers = NumberUtils::isEven;
    Filter<Integer> oddNumbers = NumberUtils::isOdd;

    assertThat(CollectionUtils.count(iterable, evenNumbers)).isEqualTo(2L);
    assertThat(CollectionUtils.count(iterable, oddNumbers)).isEqualTo(1L);
  }

  @Test
  public void countNullWithFilterReturnsZero() {
    assertThat(CollectionUtils.count(null, (element) -> true)).isEqualTo(0L);
  }

  @Test
  public void countWithFilterAcceptsAll() {
    assertThat(CollectionUtils.count(asIterable(0, 1, 2), (element) -> true)).isEqualTo(3L);
  }

  @Test
  public void countWithFilterRejectsAll() {
    assertThat(CollectionUtils.count(asIterable(0, 1, 2), (element) -> false)).isEqualTo(0L);
  }

  @Test(expected = IllegalArgumentException.class)
  public void countWithNullFilter() {

    try {
      CollectionUtils.count(Collections.emptyList(), null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Filter is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void defaultIfEmptyWithNonNullNonEmptyIterableReturnsIterable() {

    Iterable<Number> iterable = asIterable(1);
    Iterable<Number> defaultIterable = asIterable(2);

    assertThat(CollectionUtils.defaultIfEmpty(iterable, defaultIterable)).isSameAs(iterable);
  }

  @Test
  public void defaultIfNullWithEmptyIterableReturnsDefault() {

    Iterable<Number> defaultIterable = asIterable(2);

    assertThat(CollectionUtils.defaultIfEmpty(asIterable(), defaultIterable)).isSameAs(defaultIterable);
  }

  @Test
  public void defaultIfNullWithNullIterableReturnsDefault() {

    Iterable<Number> defaultIterable = asIterable(2);

    assertThat(CollectionUtils.defaultIfEmpty(null, defaultIterable)).isSameAs(defaultIterable);
  }

  @Test
  public void defaultIfNullWithNullIterableAndNullDefaultReturnsNull() {
    assertThat(CollectionUtils.<Object, Iterable<Object>>defaultIfEmpty(null, null)).isNull();
  }

  @Test
  public void emptyIterableIsNonNullEmptyIterable() {

    Iterable<?> emptyIterable = CollectionUtils.emptyIterable();

    assertThat(emptyIterable).isNotNull();
    assertThat(emptyIterable.iterator()).isNotNull();
    assertThat(emptyIterable.iterator().hasNext()).isFalse();
  }

  @Test
  public void filterCollection() {

    Filter<Integer> evenNumberFilter = NumberUtils::isEven;
    Filter<Integer> oddNumberFilter = NumberUtils::isOdd;

    Collection<Integer> numbers = asCollection(1, 2, 3, 4, 5, 6, 7, 8, 9);
    Collection<Integer> evenNumbers = CollectionUtils.filter(numbers, evenNumberFilter);
    Collection<Integer> oddNumbers = CollectionUtils.filter(numbers, oddNumberFilter);

    assertThat(evenNumbers).isNotNull();
    assertThat(evenNumbers).isNotSameAs(numbers);
    assertThat(oddNumbers).isNotNull();
    assertThat(oddNumbers).isNotSameAs(numbers);

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

  @Test(expected = IllegalArgumentException.class)
  public void filterNullCollection() {

    try {
      CollectionUtils.filter(null, (element) -> true);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Collection is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void filterWithNullFilter() {

    try {
      CollectionUtils.filter(Collections.emptyList(), null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Filter is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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

    assertThat(upperCaseStrings).isNotNull();
    assertThat(upperCaseStrings).isNotSameAs(strings);
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

    assertThat(negativeNumbers).isNotNull();
    assertThat(negativeNumbers).isNotSameAs(numbers);
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

    assertThat(noNumbers).isNotNull();
    assertThat(noNumbers).isNotSameAs(numbers);
    assertThat(noNumbers.isEmpty()).isTrue();
    assertElements(numbers, 0, 1, 2, 4, 8);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void filterAndTransformEmptyCollection() {

    FilteringTransformer<Object> mockFilteringTransformer = mock(FilteringTransformer.class);

    when(mockFilteringTransformer.accept(any())).thenReturn(true);
    when(mockFilteringTransformer.transform(any())).thenReturn(null);

    Collection<Object> emptyCollection = Collections.emptyList();
    Collection<Object> filteredTransformedCollection = CollectionUtils.filterAndTransform(
      emptyCollection, mockFilteringTransformer);

    assertThat(filteredTransformedCollection).isNotNull();
    assertThat(filteredTransformedCollection).isNotSameAs(emptyCollection);
    assertThat(filteredTransformedCollection.isEmpty()).isTrue();

    verifyZeroInteractions(mockFilteringTransformer);
  }

  @SuppressWarnings({ "rawtypes", "unchecked" })
  @Test(expected = IllegalArgumentException.class)
  public void filterAndTransformNullCollection() {

    FilteringTransformer mockFilteringTransformer = mock(FilteringTransformer.class);

    try {
      CollectionUtils.filterAndTransform(null, mockFilteringTransformer);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Collection is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyZeroInteractions(mockFilteringTransformer);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void filterAndTransformWithNullFilter() {

    try {
      CollectionUtils.filterAndTransform(Collections.emptyList(), null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("FilteringTransformer is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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

    Person foundPerson = CollectionUtils.findOne(people, (person) -> "Joe".equalsIgnoreCase(person.getFirstName()));

    assertThat(foundPerson).isEqualTo(joeDirt);

    foundPerson = CollectionUtils.findOne(people, (person) -> "Jack".equalsIgnoreCase(person.getFirstName()));

    assertThat(foundPerson).isEqualTo(jackHandy);
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

    Person foundPerson = CollectionUtils.findOne(people, (person) -> ("Play".equalsIgnoreCase(person.getFirstName())
          && "Toe".equalsIgnoreCase(person.getLastName())));

    assertThat(foundPerson).isNull();
  }

  @Test
  public void findWithEmptyIterable() {
    assertThat(CollectionUtils.<Object>findOne(asIterable(), (element) -> true)).isNull();
  }

  @Test
  public void findWithNullIterable() {
    assertThat(CollectionUtils.<Object>findOne(null, (element) -> true)).isNull();
  }

  @Test(expected = IllegalArgumentException.class)
  public void findWithNullFilter() {

    try {
      CollectionUtils.findOne(asIterable(), null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Filter is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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

    assertThat(doeFamily).isNotNull();
    assertThat(doeFamily.size()).isEqualTo(9);
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

    assertThat(allPeople).isNotNull();
    assertThat(allPeople.size()).isEqualTo(people.size());
    assertElements(allPeople, people.toArray(new Person[0]));
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

    assertThat(noPeople).isNotNull();
    assertThat(noPeople.isEmpty()).isTrue();
  }

  @Test
  public void findAllWitEmptyIterable() {

    List<?> matches = CollectionUtils.findAll(asIterable(), (element) -> true);

    assertThat(matches).isNotNull();
    assertThat(matches.isEmpty()).isTrue();
  }

  @Test
  public void findAllWitNullIterable() {

    List<?> matches = CollectionUtils.findAll(null, (element) -> true);

    assertThat(matches).isNotNull();
    assertThat(matches.isEmpty()).isTrue();
  }

  @Test(expected = IllegalArgumentException.class)
  public void findAllWithNullFilter() {

    try {
      CollectionUtils.findAll(Collections.emptyList(), null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Filter is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void isEmptyWithEmptyCollectionIsTrue() {

    assertThat(CollectionUtils.isEmpty(null)).isTrue();
    assertThat(CollectionUtils.isEmpty(Collections.emptyList())).isTrue();
    assertThat(CollectionUtils.isEmpty(new ArrayList<>(10))).isTrue();
  }

  @Test
  public void isEmptyWithNonEmptyCollectionIsFalse() {

    assertThat(CollectionUtils.isEmpty(Collections.singleton(0))).isFalse();
    assertThat(CollectionUtils.isEmpty(asCollection(0, 1, 2))).isFalse();
    assertThat(CollectionUtils.isEmpty(Collections.singleton("null"))).isFalse();
    assertThat(CollectionUtils.isEmpty(asCollection("test", "testing", "tested"))).isFalse();
  }

  @Test
  public void isNotEmptyWithEmptyCollectionIsFalse() {

    assertThat(CollectionUtils.isNotEmpty(null)).isFalse();
    assertThat(CollectionUtils.isNotEmpty(Collections.emptyList())).isFalse();
    assertThat(CollectionUtils.isNotEmpty(new ArrayList<>(10))).isFalse();
  }

  @Test
  public void isNotEmptyWithNonEmptyCollectionIsTrue() {

    assertThat(CollectionUtils.isNotEmpty(Collections.singleton(0))).isTrue();
    assertThat(CollectionUtils.isNotEmpty(asCollection(0, 1, 2))).isTrue();
    assertThat(CollectionUtils.isNotEmpty(Collections.singleton("null"))).isTrue();
    assertThat(CollectionUtils.isNotEmpty(asCollection("test", "testing", "tested"))).isTrue();
  }

  @Test
  public void isSizeOneWithCollectionHavingOneElementIsTrue() {
    assertThat(CollectionUtils.isSizeOne(Collections.singletonList(1))).isTrue();
  }

  @Test
  public void isSizeOneWithCollectionHavingTwoElementsIsFalse() {
    assertThat(CollectionUtils.isSizeOne(Arrays.asList(1, 2))).isFalse();
  }

  @Test
  public void isSizeOneWithEmptyCollectionIsFalse() {
    assertThat(CollectionUtils.isSizeOne(Collections.emptyList())).isFalse();
  }

  @Test
  public void isSizeOneWithNullCollectionIsFalse() {
    assertThat(CollectionUtils.isSizeOne(null)).isFalse();
  }

  @Test
  public void isSizeXWithCollectionHavingSizeXIsTrue() {

    assertThat(CollectionUtils.isSize(Arrays.asList(1, 2, 3), 3)).isTrue();
    assertThat(CollectionUtils.isSize(Collections.singletonList(1), 1)).isTrue();
  }

  @Test
  public void isSizeXWithCollectionHavingSizeYIsFalse() {

    assertThat(CollectionUtils.isSize(Arrays.asList(1, 2, 3), 2)).isFalse();
    assertThat(CollectionUtils.isSize(Arrays.asList(1, 2, 3), 4)).isFalse();
    assertThat(CollectionUtils.isSize(Collections.singletonList(1), 2)).isFalse();
    assertThat(CollectionUtils.isSize(Collections.emptyList(), 1)).isFalse();
  }

  @Test
  public void isSizeXWithEmptyCollectionIsFalse() {
    assertThat(CollectionUtils.isSize(Collections.emptyList(), 1)).isFalse();
  }

  @Test
  public void isSizeZeroWithEmptyCollectionIsTrue() {
    assertThat(CollectionUtils.isSize(Collections.emptyList(), 0)).isTrue();
  }

  @Test
  public void isSizeXWithNullCollectionIsFalse() {
    assertThat(CollectionUtils.isSize(null, 1)).isFalse();
  }

  @Test
  public void isSizeZeroWithNullCollectionIsTrue() {
    assertThat(CollectionUtils.isSize(null, 0)).isTrue();
  }

  @Test
  public void nullSafeCollectionWithCollection() {

    Collection<?> collection = asCollection(1, 2, 3);

    assertThat(CollectionUtils.nullSafeCollection(collection)).isSameAs(collection);
  }

  @Test
  public void nullSafeCollectionWithEmptyCollection() {

    Collection<?> collection = asCollection();

    assertThat(CollectionUtils.nullSafeCollection(collection)).isSameAs(collection);
  }

  @Test
  public void nullSafeCollectionWithNullCollection() {

    Collection<?> collection = CollectionUtils.nullSafeCollection(null);

    assertThat(collection).isNotNull();
    assertThat(collection.isEmpty()).isTrue();
  }

  @Test
  public void nullSafeEnumerationWithEnumeration() {

    Enumeration<?> enumeration = asEnumeration("test");

    assertThat(CollectionUtils.nullSafeEnumeration(enumeration)).isSameAs(enumeration);
  }

  @Test
  public void nullSafeEnumerationWithEmptyEnumeration() {

    Enumeration<?> enumeration = Collections.emptyEnumeration();

    assertThat(CollectionUtils.nullSafeEnumeration(enumeration)).isSameAs(enumeration);
  }

  @Test
  public void nullSafeEnumerationWithNullEnumeration() {

    Enumeration<?> enumeration = CollectionUtils.nullSafeEnumeration(null);

    assertThat(enumeration).isNotNull();
    assertThat(enumeration.hasMoreElements()).isFalse();
  }

  @Test
  public void nullSafeIterableWithIterable() {

    Iterable<?> iterable = asIterable("test");

    assertThat(CollectionUtils.nullSafeIterable(iterable)).isSameAs(iterable);
  }

  @Test
  public void nullSafeIterableWithEmptyIterable() {

    Iterable<?> iterable = asIterable();

    assertThat(CollectionUtils.nullSafeIterable(iterable)).isSameAs(iterable);
  }

  @Test
  public void nullSafeIterableWithNullIterable() {

    Iterable<?> iterable = CollectionUtils.nullSafeIterable(null);

    assertThat(iterable).isNotNull();
    assertThat(iterable.iterator()).isNotNull();
    assertThat(iterable.iterator().hasNext()).isFalse();
  }

  @Test
  public void nullSafeIteratorWithIterator() {

    Iterator<?> iterator = asIterator("test");

    assertThat(CollectionUtils.nullSafeIterator(iterator)).isSameAs(iterator);
  }

  @Test
  public void nullSafeIteratorWithEmptyIterator() {

    Iterator<?> iterator = Collections.emptyIterator();

    assertThat(CollectionUtils.nullSafeIterator(iterator)).isSameAs(iterator);
  }

  @Test
  public void nullSafeIteratorWithNullIterator() {

    Iterator<?> iterator = CollectionUtils.nullSafeIterator(null);

    assertThat(iterator).isNotNull();
    assertThat(iterator.hasNext()).isFalse();
  }

  @Test
  public void nullSafeListWitList() {

    List<?> list = Collections.singletonList("test");

    assertThat(CollectionUtils.nullSafeList(list)).isSameAs(list);
  }

  @Test
  public void nullSafeListWitEmptyList() {

    List<?> list = Collections.emptyList();

    assertThat(CollectionUtils.nullSafeList(list)).isSameAs(list);
  }

  @Test
  public void nullSafeListWithNullList() {

    List<?> list = CollectionUtils.nullSafeList(null);

    assertThat(list).isNotNull();
    assertThat(list.isEmpty()).isTrue();
  }

  @Test
  public void nullSafeSetWithSet() {

    Set<?> set = Collections.singleton("test");

    assertThat(CollectionUtils.nullSafeSet(set)).isSameAs(set);
  }

  @Test
  public void nullSafeSetWithEmptySet() {

    Set<?> set = Collections.emptySet();

    assertThat(CollectionUtils.nullSafeSet(set)).isSameAs(set);
  }

  @Test
  public void nullSafeSetWithNullSet() {

    Set<?> set = CollectionUtils.nullSafeSet(null);

    assertThat(set).isNotNull();
    assertThat(set.isEmpty()).isTrue();
  }

  @Test
  public void nullSafeSizeForCollection() {

    Collection<?> collection = Collections.singleton("test");

    assertThat(CollectionUtils.nullSafeSize(collection)).isEqualTo(collection.size());
  }

  @Test
  public void nullSafeSizeForEmptyCollection() {
    assertThat(CollectionUtils.nullSafeSize(asCollection())).isEqualTo(0);
  }

  @Test
  public void nullSafeSizeForNullCollection() {
    assertThat(CollectionUtils.nullSafeSize(null)).isEqualTo(0);
  }

  @Test
  public void shuffle() {

    List<Integer> numbers = Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
    List<Integer> shuffledNumbers = CollectionUtils.shuffle(new ArrayList<>(numbers));

    assertThat(shuffledNumbers).isNotNull();
    assertThat(shuffledNumbers).isNotEqualTo(numbers);
    assertShuffled(numbers, shuffledNumbers);

    List<Integer> shuffledNumbersAgain = CollectionUtils.shuffle(new ArrayList<>(shuffledNumbers));

    assertThat(shuffledNumbersAgain).isNotNull();
    assertThat(shuffledNumbersAgain).isNotEqualTo(shuffledNumbers);
    assertThat(shuffledNumbersAgain).isNotEqualTo(numbers);
    assertShuffled(shuffledNumbers, shuffledNumbersAgain);
    assertShuffled(numbers, shuffledNumbersAgain);
  }

  @Test
  @SuppressWarnings("all")
  public void shuffleEmptyList() {

    List<?> emptyList = Collections.emptyList();
    List<?> shuffledEmptyList = CollectionUtils.shuffle(emptyList);

    assertThat(shuffledEmptyList).isSameAs(emptyList);
    assertThat(shuffledEmptyList.isEmpty()).isTrue();
  }

  @Test
  public void shuffleNullList() {
    assertThat(CollectionUtils.shuffle(null)).isNull();
  }

  @Test
  public void shuffleSingleElementList() {

    List<String> singleElementList = Collections.singletonList("test");
    List<String> shuffledSingleElementList = CollectionUtils.shuffle(singleElementList);

    assertThat(shuffledSingleElementList).isSameAs(singleElementList);
    assertThat(shuffledSingleElementList.size()).isEqualTo(1);
    assertThat(shuffledSingleElementList.get(0)).isEqualTo("test");
  }

  @Test
  public void subList() {

    List<Integer> subList = CollectionUtils.subList(Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 1, 2, 4, 8);

    assertThat(subList).isNotNull();
    assertElements(subList, 1, 2, 4, 8);
  }

  @Test
  public void subListWithEmptyList() {

    List<Object> list = Collections.emptyList();
    List<?> subList = CollectionUtils.subList(list);

    assertThat(subList).isNotSameAs(list);
    assertThat(subList.isEmpty()).isTrue();
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void subListWithEmptyListAndIndices() {
    CollectionUtils.subList(Collections.emptyList(), 0, 1, 2);
  }

  @Test
  public void subListWithListAndNoIndices() {

    List<String> subList = CollectionUtils.subList(Arrays.asList("test", "testing", "tested"));

    assertThat(subList).isNotNull();
    assertThat(subList.isEmpty()).isTrue();
  }

  @Test(expected = IllegalArgumentException.class)
  public void subListWithNullList() {

    try {
      CollectionUtils.subList(null, 0, 1, 2);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("List is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void subListWithNullIndices() {

    try {
      CollectionUtils.subList(Collections.emptyList(), (int[]) null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Indices are required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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
  public void toArrayWithCollectionOfNumbers() {

    Collection<Integer> collection = Arrays.asList(1, 2, 3);

    Integer[] array = CollectionUtils.toArray(collection, Integer.class);

    assertThat(array).isNotNull();
    assertThat(array.getClass().isArray()).isTrue();
    assertThat(array.getClass().getComponentType()).isEqualTo(Integer.class);
    assertThat(Arrays.asList(array)).containsExactly(1, 2, 3);
  }

  @Test
  public void toArrayWithCollectionOfStrings() {

    Collection<String> collection = Arrays.asList("one", "two", "three");

    String[] array = CollectionUtils.toArray(collection, String.class);

    assertThat(array).isNotNull();
    assertThat(array.getClass().isArray()).isTrue();
    assertThat(array.getClass().getComponentType()).isEqualTo(String.class);
    assertThat(Arrays.asList(array)).containsExactly("one", "two", "three");
  }

  @Test
  public void toArrayWithEmptyCollection() {

    String[] array = CollectionUtils.toArray(Collections.emptySet(), String.class);

    assertThat(array).isNotNull();
    assertThat(array.getClass().isArray()).isTrue();
    assertThat(array.getClass().getComponentType()).isEqualTo(String.class);
    assertThat(array).isEmpty();
  }

  @Test
  public void toArrayWithNullCollection() {

    Integer[] array = CollectionUtils.toArray(null, Integer.class);

    assertThat(array).isNotNull();
    assertThat(array.getClass().isArray()).isTrue();
    assertThat(array.getClass().getComponentType()).isEqualTo(Integer.class);
    assertThat(array).isEmpty();
  }

  @Test
  public void toStringFromIterable() {
    assertThat(CollectionUtils.toString(asIterable("test", "testing", "tested"))).isEqualTo("[test, testing, tested]");
  }

  @Test
  public void toStringFromIterableUsingRenderer() {
    assertThat(CollectionUtils.toString(asIterable("test"), String::toUpperCase)).isEqualTo("[TEST]");
  }

  @Test
  public void toStringFromEmptyIterable() {
    assertThat(CollectionUtils.toString(asIterable())).isEqualTo("[]");
  }

  @Test
  public void toStringFromNullIterable() {
    assertThat(CollectionUtils.toString(null)).isEqualTo("[]");
  }

  @Test
  public void transformCollection() {

    Collection<String> collection = asCollection("test", "testing", "tested");
    Collection<String> transformedCollection = CollectionUtils.transform(collection, StringUtils::toUpperCase);

    assertThat(transformedCollection).isNotSameAs(collection);
    assertElements(transformedCollection, "TEST", "TESTING", "TESTED");
  }

  @Test
  public void transformEmptyCollection() {

    Collection<Object> collection = Collections.emptySet();
    Collection<Object> transformedCollection = CollectionUtils.transform(collection, (value) -> "test");

    assertThat(transformedCollection).isNotNull();
    assertThat(transformedCollection).isNotSameAs(collection);
    assertThat(transformedCollection.isEmpty()).isTrue();
  }

  @Test(expected = IllegalArgumentException.class)
  public void transformNullCollection() {

    try {
      CollectionUtils.transform(null, (value) -> "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Collection is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void transformWithNullTransformer() {

    try {
      CollectionUtils.transform(Collections.emptyList(), null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Transformer is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void unmodifiableIterator() {

    String[] elements = { "test", "testing", "tested" };

    Iterator<String> unmodifiableIterator = CollectionUtils.unmodifiableIterator(asIterator(elements));

    assertThat(unmodifiableIterator).isNotNull();

    for (String element : elements) {
      assertThat(unmodifiableIterator.hasNext()).isTrue();
      assertThat(unmodifiableIterator.next()).isEqualTo(element);
    }

    assertThat(unmodifiableIterator.hasNext()).isFalse();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void unmodifiableIteratorIsImmutable() {

    Iterator<String> iterator = CollectionUtils.unmodifiableIterator(asIterator("test"));

    assertThat(iterator).isNotNull();
    assertThat(iterator.hasNext()).isTrue();

    try {
      iterator.remove();
    }
    catch (UnsupportedOperationException expected) {

      assertThat(expected).hasMessage("Iterator is immutable");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(iterator.hasNext()).isTrue();
      assertThat(iterator.next()).isEqualTo("test");
      assertThat(iterator.hasNext()).isFalse();
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void unmodifiableIteratorWithNullIterator() {

    try {
      CollectionUtils.unmodifiableIterator(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Iterator is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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

      if (this == obj) {
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
