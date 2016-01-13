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

import static org.cp.elements.util.CollectionExtensions.from;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

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
import java.util.Vector;

import org.cp.elements.lang.Filter;
import org.cp.elements.lang.FilteringTransformer;
import org.cp.elements.lang.NumberUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.Transformer;
import org.cp.elements.lang.support.DefaultFilter;
import org.junit.Test;

/**
 * The CollectionUtilsTest class is a test suite of test cases testing the contract and functionality 
 * of the CollectionUtils class.
 * 
 * @author John J. Blum
 * @see java.util.Collection
 * @see java.util.Collections
 * @see org.junit.Test
 * @see org.cp.elements.util.CollectionUtils
 * @since 1.0.0
 */
public class CollectionUtilsTest {

  protected <T> void assertShuffled(final Iterable<T> source, final Iterable<T> target) {
    assertTrue("'source' must not be null and must have elements!", source != null && source.iterator().hasNext());
    assertTrue("'target' must not be null and must have elements!", target != null && target.iterator().hasNext());

    Iterator<T> targetIterator = target.iterator();
    boolean shuffled = false;

    for (T sourceElement : source) {
      shuffled &= targetIterator.hasNext();
      shuffled |= !sourceElement.equals(targetIterator.next());
    }

    assertTrue(String.format("Target (%1$s) was not shuffled!", target), shuffled);
  }

  @Test
  public void count() {
    Collection<String> collection = Arrays.asList("test", "fixture", "testing", "mock", "tested");

    assertNotNull(collection);
    assertEquals(5, collection.size());
    assertEquals(3, CollectionUtils.count(collection, (value) -> StringUtils.contains(value, "test")));
  }

  @Test
  public void countReturnsSize() {
    Collection<String> collection = Arrays.asList("test", "testing", "tested");

    assertNotNull(collection);
    assertEquals(collection.size(), CollectionUtils.count(collection, new DefaultFilter<>(true)));
  }

  @Test
  public void countReturnsZero() {
    Collection<String> collection = Arrays.asList("test", "testing", "tested");

    assertNotNull(collection);
    assertEquals(0, CollectionUtils.count(collection, new DefaultFilter<>(false)));
  }

  @Test(expected = NullPointerException.class)
  public void countWithNullCollection() {
    CollectionUtils.count(null, new DefaultFilter<>(true));
  }

  @Test(expected = NullPointerException.class)
  public void countWithNullFilter() {
    CollectionUtils.count(Collections.emptyList(), null);
  }

  @Test
  public void nullSafeListWithNonNullNonEmptyList() {
    List<?> expectedList = Arrays.asList("test", "testing", "tested");
    List<?> actualList = CollectionUtils.nullSafeList(expectedList);

    assertThat(actualList, is(sameInstance(actualList)));
  }

  @Test
  public void nullSafeListWithEmptyList() {
    List<?> expectedList = new ArrayList<>(0);
    List<?> actualList = CollectionUtils.nullSafeList(expectedList);

    assertThat(actualList, is(sameInstance(expectedList)));
  }

  @Test
  public void nullSafeListWithNullList() {
    List<?> actualList = CollectionUtils.nullSafeList(null);

    assertThat(actualList, is(notNullValue()));
    assertThat(actualList.isEmpty(), is(true));
  }

  @Test
  public void nullSafeSetWithNonNullNonEmptySet() {
    Set<?> expectedSet = from("test", "testing", "tested").toSet();
    Set<?> actualSet = CollectionUtils.nullSafeSet(expectedSet);

    assertThat(actualSet, is(sameInstance(expectedSet)));
  }

  @Test
  public void nullSafeSetWithEmptySet() {
    Set<?> expectedSet = new HashSet<>(0);
    Set<?> actualSet = CollectionUtils.nullSafeSet(expectedSet);

    assertThat(actualSet, is(sameInstance(expectedSet)));
  }

  @Test
  public void nullSafeSetWithNullSet() {
    Set<?> actualSet = CollectionUtils.nullSafeSet(null);

    assertThat(actualSet, is(notNullValue()));
    assertThat(actualSet.isEmpty(), is(true));
  }

  @Test
  public void enumeration() {
    List<?> expectedList = Arrays.asList("test", "testing", "tested");
    Enumeration<?> expectedListEnumeration = CollectionUtils.enumeration(expectedList.iterator());

    assertNotNull(expectedListEnumeration);

    int index = 0;

    while (expectedListEnumeration.hasMoreElements()) {
      assertEquals(expectedList.get(index++), expectedListEnumeration.nextElement());
    }

    assertEquals(expectedList.size(), index);
  }

  @Test(expected = NoSuchElementException.class)
  public void enumerationWithExhaustedIterator() {
    Enumeration<?> enumeration = CollectionUtils.enumeration(Collections.singletonList("test").iterator());

    assertNotNull(enumeration);
    assertTrue(enumeration.hasMoreElements());
    assertEquals("test", enumeration.nextElement());
    assertFalse(enumeration.hasMoreElements());

    enumeration.nextElement();
  }

  @Test
  public void enumerationWithNoElementIterator() {
    Enumeration<?> noElementEnumeration = CollectionUtils.enumeration(Collections.emptyList().iterator());

    assertNotNull(noElementEnumeration);
    assertFalse(noElementEnumeration.hasMoreElements());
  }

  @Test(expected = NullPointerException.class)
  public void enumerationWithNullIterator() {
    CollectionUtils.enumeration(null);
  }

  @Test
  public void enumerationWithSingleElementIterator() {
    Enumeration<?> singleElementEnumeration = CollectionUtils.enumeration(Collections.singletonList("test").iterator());

    assertNotNull(singleElementEnumeration);
    assertTrue(singleElementEnumeration.hasMoreElements());
    assertEquals("test", singleElementEnumeration.nextElement());
    assertFalse(singleElementEnumeration.hasMoreElements());
  }

  @Test
  public void filter() {
    Collection<String> collection = new ArrayList<>(Arrays.asList("test", "testing", "tested"));

    assertNotNull(collection);
    assertFalse(collection.isEmpty());
    assertEquals(3, collection.size());

    Collection<String> filteredCollection = CollectionUtils.filter(collection, "tested"::equalsIgnoreCase);

    assertSame(collection, filteredCollection);
    assertFalse(collection.isEmpty());
    assertEquals(1, collection.size());
    assertTrue(collection.contains("tested"));
  }

  @Test
  public void filterEmptiesCollection() {
    Collection<String> collection = new ArrayList<>(Arrays.asList("test", "testing", "tested"));

    assertNotNull(collection);
    assertFalse(collection.isEmpty());
    assertEquals(3, collection.size());

    Collection<String> filteredCollection = CollectionUtils.filter(collection, new DefaultFilter<>(false));

    assertSame(collection, filteredCollection);
    assertTrue(collection.isEmpty());
  }

  @Test
  public void filterRetainsCollection() {
    Collection<String> collection = Arrays.asList("test", "testing", "tested");

    assertNotNull(collection);
    assertFalse(collection.isEmpty());
    assertEquals(3, collection.size());

    Collection<String> filteredCollection = CollectionUtils.filter(collection, new DefaultFilter<>(true));

    assertSame(collection, filteredCollection);
    assertFalse(collection.isEmpty());
    assertEquals(3, collection.size());
  }

  @Test(expected = NullPointerException.class)
  public void filterNullCollection() {
    CollectionUtils.filter(null, new DefaultFilter<>(true));
  }

  @Test(expected = NullPointerException.class)
  public void filterWithNullFilter() {
    CollectionUtils.filter(Collections.emptyList(), null);
  }

  @Test
  public void filterAndTransform() {
    List<String> collection = new ArrayList<>(Arrays.asList("  ", "test", null, "testing", "", "tested"));

    FilteringTransformer<String> filteringTransformer = new FilteringTransformer<String>() {
      @Override public boolean accept(final String value) {
        return (StringUtils.length(value) > 4);
      }

      @Override public String transform(final String value) {
        return value.toUpperCase();
      }
    };

    List<String> actualCollection = CollectionUtils.filterAndTransform(collection, filteringTransformer);

    assertSame(collection, actualCollection);
    assertFalse(collection.isEmpty());
    assertEquals(2, collection.size());
    assertEquals("TESTING", collection.get(0));
    assertEquals("TESTED", collection.get(1));
  }

  @Test
  public void filterAndTransformEmptyCollection() {
    Collection<String> collection = new ArrayList<>(Arrays.asList("test", "testing", "tested"));

    FilteringTransformer<String> filteringTransformer = new FilteringTransformer<String>() {
      @Override public boolean accept(final String obj) {
        return false;
      }

      @Override public String transform(final String value) {
        return null;
      }
    };

    Collection<String> actualCollection = CollectionUtils.filterAndTransform(collection, filteringTransformer);

    assertSame(collection, actualCollection);
    assertTrue(collection.isEmpty());
  }

  @Test
  public void find() {
    Person cookieDoe = new Person("Cookie", "Doe");
    Person janeDoe = new Person("Jane", "Doe");
    Person jonDoe = new Person("Jon", "Doe");
    Person pieDoe = new Person("Pie", "Doe");
    Person jackHandy = new Person("Jack", "Handy");
    Person sandyHandy = new Person("Sandy", "Handy");

    List<Person> people = Arrays.asList(jackHandy, jonDoe, janeDoe, sandyHandy, pieDoe, cookieDoe);

    Filter<Person> doeFilter = (person) -> "Doe".equalsIgnoreCase(person.getLastName());

    Person actualPerson = CollectionUtils.find(people, doeFilter);

    assertNotNull(actualPerson);
    assertEquals(jonDoe, actualPerson);
  }

  @Test
  public void findWithNonMatchingFilter() {
    Person cookieDoe = new Person("Cookie", "Doe");
    Person janeDoe = new Person("Jane", "Doe");
    Person jonDoe = new Person("Jon", "Doe");
    Person pieDoe = new Person("Pie", "Doe");
    Person jackHandy = new Person("Jack", "Handy");
    Person sandyHandy = new Person("Sandy", "Handy");

    List<Person> people = Arrays.asList(jackHandy, jonDoe, janeDoe, sandyHandy, pieDoe, cookieDoe);

    Filter<Person> doeFilter = (person) -> ("Play".equalsIgnoreCase(person.getFirstName())
      && "Doe".equalsIgnoreCase(person.getLastName()));

    Person actualPerson = CollectionUtils.find(people, doeFilter);

    assertNull(actualPerson);
  }

  @Test(expected = NullPointerException.class)
  public void findWithNullFilter() {
    CollectionUtils.find(Collections.emptyList(), null);
  }

  @Test(expected = NullPointerException.class)
  public void findWithNullIterable() {
    CollectionUtils.find(null, new DefaultFilter<>(true));
  }

  @Test
  public void findAll() {
    List<Integer> numbers = Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);

    Filter<Integer> oddNumberFilter = NumberUtils::isOdd;

    List<Integer> actualNumbers = CollectionUtils.findAll(numbers, oddNumberFilter);

    assertNotNull(actualNumbers);
    assertNotSame(numbers, actualNumbers);
    assertEquals(ArrayList.class, actualNumbers.getClass());
    assertEquals(5, actualNumbers.size());

    int index = 0;

    for (int number = 1; number < 10; number += 2) {
      assertEquals(number, actualNumbers.get(index++).intValue());
    }
  }

  @Test
  public void findAllWithNonMatchingFilter() {
    List<Integer> numbers = Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
    List<Integer> actualNumbers = CollectionUtils.findAll(numbers, new DefaultFilter<>(false));

    assertNotNull(actualNumbers);
    assertNotSame(numbers, actualNumbers);
    assertEquals(10, numbers.size());
    assertEquals(0, actualNumbers.size());
  }

  @Test(expected = NullPointerException.class)
  public void findAllWithNullFilter() {
    CollectionUtils.findAll(Collections.emptyList(), null);
  }

  @Test(expected = NullPointerException.class)
  public void findAllWithNullIterable() {
    CollectionUtils.findAll(null, new DefaultFilter<>(true));
  }

  @Test
  public void isEmptyCollection() {
    assertTrue(CollectionUtils.isEmpty(null));
    assertTrue(CollectionUtils.isEmpty(Collections.emptyList()));
    assertTrue(CollectionUtils.isEmpty(Collections.emptySet()));
    assertFalse(CollectionUtils.isEmpty(Collections.singletonList("test")));
    assertFalse(CollectionUtils.isEmpty(Collections.singleton("test")));
    assertFalse(CollectionUtils.isEmpty(from("test", "testing", "tested").toList()));
    assertFalse(CollectionUtils.isEmpty(from("test", "testing", "tested").toSet()));
    assertFalse(CollectionUtils.isEmpty(from(null, null, null).toList()));
    assertFalse(CollectionUtils.isEmpty(from("test", "test", "test").toSet()));
  }

  @Test
  public void iterableWithEnumeration() {
    String[] expectedElements = { "test", "testing", "tested" };
    Enumeration<String> enumeration = new Vector<>(Arrays.asList(expectedElements)).elements();

    int index = 0;

    for (String actualElement : CollectionUtils.iterable(enumeration)) {
      assertEquals(expectedElements[index++], actualElement);
    }

    assertEquals(expectedElements.length, index);
  }

  @Test(expected = NullPointerException.class)
  public void iterableWithNullEnumeration() {
    CollectionUtils.iterable((Enumeration<?>) null);
  }

  @Test
  public void iterableWithIterator() {
    String[] expectedElements = { "test", "testing", "tested" };
    Iterator<String> iterator = Arrays.asList(expectedElements).iterator();

    int index = 0;

    for (Object actualElement : CollectionUtils.iterable(iterator)) {
      assertEquals(expectedElements[index++], actualElement);
    }

    assertEquals(expectedElements.length, index);
  }

  @Test(expected = NullPointerException.class)
  public void iterableWithNullIterator() {
    CollectionUtils.iterable((Iterator<?>) null);
  }

  @Test
  public void iterator() {
    Vector<Object> expectedVector = new Vector<>(3);

    expectedVector.add("test");
    expectedVector.add("testing");
    expectedVector.add("tested");

    Iterator<?> expectedVectorIterator = CollectionUtils.iterator(expectedVector.elements());

    assertNotNull(expectedVectorIterator);

    int index = 0;

    while (expectedVectorIterator.hasNext()) {
      assertEquals(expectedVector.get(index++), expectedVectorIterator.next());
    }

    assertEquals(expectedVector.size(), index);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void iteratorModification() {
    Vector<Object> vector = new Vector<>(3);

    vector.add("test");
    vector.add("testing");
    vector.add("tested");

    assertNotNull(vector);
    assertEquals(3, vector.size());

    Iterator<?> iterator = CollectionUtils.iterator(vector.elements());

    assertNotNull(iterator);
    assertTrue(iterator.hasNext());
    assertEquals("test", iterator.next());
    assertTrue(iterator.hasNext());

    try {
      iterator.remove();
    }
    finally {
      assertEquals(3, vector.size());
    }
  }

  @Test(expected = NoSuchElementException.class)
  public void iteratorWithExhaustedEnumeration() {
    Vector<Object> vector = new Vector<>(1);

    vector.add("test");

    Iterator<?> iterator = CollectionUtils.iterator(vector.elements());

    assertNotNull(iterator);
    assertTrue(iterator.hasNext());
    assertEquals("test", iterator.next());
    assertFalse(iterator.hasNext());

    iterator.next();
  }

  @Test
  public void iteratorWithNoElementEnumeration() {
    final Iterator<?> zeroElementIterator = CollectionUtils.iterator(new Vector<>().elements());

    assertNotNull(zeroElementIterator);
    assertFalse(zeroElementIterator.hasNext());
  }

  @Test(expected = NullPointerException.class)
  public void iteratorWithNullEnumeration() {
    CollectionUtils.iterator((Enumeration<?>) null);
  }

  @Test
  public void iteratorWithSingleElementEnumeration() {
    Vector<Object> singleElementVector = new Vector<>(1);

    singleElementVector.add("test");

    Iterator<?> singleElementIterator = CollectionUtils.iterator(singleElementVector.elements());

    assertNotNull(singleElementIterator);
    assertTrue(singleElementIterator.hasNext());
    assertEquals("test", singleElementIterator.next());
    assertFalse(singleElementIterator.hasNext());
  }

  @Test
  public void shuffle() {
    List<Integer> numberList = Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
    List<Integer> shuffledNumberList = CollectionUtils.shuffle(new ArrayList<>(numberList));

    assertNotNull(shuffledNumberList);
    assertNotEquals(numberList, shuffledNumberList);
    assertShuffled(numberList, shuffledNumberList);

    List<Integer> shuffledNumberListAgain = CollectionUtils.shuffle(new ArrayList<>(shuffledNumberList));

    assertNotNull(shuffledNumberList);
    assertNotEquals(shuffledNumberList, shuffledNumberListAgain);
    assertShuffled(shuffledNumberList, shuffledNumberListAgain);
  }

  @Test
  public void shuffleEmptyList() {
    List<Integer> emptyList = Collections.emptyList();
    List<Integer> shuffledEmptyList = CollectionUtils.shuffle(emptyList);

    assertSame(emptyList, shuffledEmptyList);
    assertTrue(shuffledEmptyList.isEmpty());
  }

  @Test(expected = NullPointerException.class)
  public void shuffleNullList() {
    CollectionUtils.shuffle(null);
  }

  @Test
  public void shuffleSingleElementList() {
    List<Integer> singleElementList = Collections.singletonList(1);
    List<Integer> shuffledSingleElementList = CollectionUtils.shuffle(singleElementList);

    assertNotNull(shuffledSingleElementList);
    assertEquals(singleElementList, shuffledSingleElementList);
  }

  @Test
  public void sizeOfCollection() {
    assertEquals(0, CollectionUtils.size(null));
    assertEquals(0, CollectionUtils.size(Collections.emptyList()));
    assertEquals(0, CollectionUtils.size(Collections.emptySet()));
    assertEquals(1, CollectionUtils.size(Collections.singletonList("test")));
    assertEquals(1, CollectionUtils.size(Collections.singleton("test")));
    assertEquals(3, CollectionUtils.size(from(null, null, null).toList()));
    assertEquals(1, CollectionUtils.size(from("test", "test", "test").toSet()));
    assertEquals(2, CollectionUtils.size(from('x', "x").toSet()));
    assertEquals(2, CollectionUtils.size(from("x", "X").toSet()));
    assertEquals(2, CollectionUtils.size(from("O", "0").toSet()));
    assertEquals(3, CollectionUtils.size(from("test", "testing", "tested").toList()));
    assertEquals(3, CollectionUtils.size(from("test", "testing", "tested").toSet()));
    assertEquals(3, CollectionUtils.size(from("test", "TEST", "Test").toSet()));
  }

  @Test
  public void subList() {
    List<Integer> result = CollectionUtils.subList(Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 1, 2, 4, 8);

    assertNotNull(result);
    assertFalse(result.isEmpty());
    assertEquals(4, result.size());
    assertTrue(result.containsAll(Arrays.asList(1, 2, 4, 8)));
  }

  @Test
  public void subListWithEmptyList() {
    List<Object> result = CollectionUtils.subList(Collections.emptyList());

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void subListWithEmptyListAndIndices() {
    CollectionUtils.subList(Collections.emptyList(), 1, 2, 4, 8);
  }

  @Test
  public void subListWithListAndNoIndices() {
    List<String> result = CollectionUtils.subList(Arrays.asList("test", "testing", "tested"));

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test(expected = NullPointerException.class)
  public void subListWithNullListAndIndices() {
    CollectionUtils.subList(null, 1, 2, 4, 8);
  }

  @Test
  public void toStringFromCollection() {
    assertEquals("[]", CollectionUtils.toString(Collections.emptyList()));
    assertEquals("[test, testing, tested]", CollectionUtils.toString(Arrays.asList("test", "testing", "tested")));
  }

  @Test
  public void transformCollection() {
    List<String> collection = new ArrayList<>(Arrays.asList("test", "testing", "tested"));

    Transformer<String> transformer = StringUtils::toUpperCase;

    List<String> actualCollection = CollectionUtils.transform(collection, transformer);

    assertSame(collection, actualCollection);
    assertFalse(actualCollection.isEmpty());
    assertEquals(3, actualCollection.size());
    assertEquals("TEST", actualCollection.get(0));
    assertEquals("TESTING", actualCollection.get(1));
    assertEquals("TESTED", actualCollection.get(2));
  }

  @Test
  public void transformEmptyCollection() {
    Collection<Object> collection = Collections.emptySet();

    assertTrue(collection.isEmpty());
    assertSame(collection, CollectionUtils.transform(collection, (value) -> null));
    assertTrue(collection.isEmpty());
  }

  @Test(expected = NullPointerException.class)
  public void transformNullCollection() {
    CollectionUtils.transform(null, (value) -> null);
  }

  @Test(expected = NullPointerException.class)
  public void transformWithNullTransformer() {
    CollectionUtils.transform(Collections.emptyList(), null);
  }

  @Test
  public void unmodifiableIterator() {
    List<?> list = Arrays.asList("test", "testing", "tested");
    Iterator<?> listIterator = list.iterator();

    assertNotNull(listIterator);

    Iterator<?> unmodifiableListIterator = CollectionUtils.unmodifiableIterator(listIterator);

    assertNotNull(unmodifiableListIterator);
    assertNotSame(listIterator, unmodifiableListIterator);

    int index = 0;

    while (unmodifiableListIterator.hasNext()) {
      assertEquals(list.get(index++), unmodifiableListIterator.next());
    }

    assertEquals(list.size(), index);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void unmodifiableIteratorModification() {
    List<?> list = Arrays.asList("test", "testing", "tested");
    Iterator<?> listIterator = list.iterator();

    assertNotNull(listIterator);
    assertEquals(3, list.size());

    Iterator<?> unmodifiableListIterator = CollectionUtils.unmodifiableIterator(listIterator);

    assertNotNull(unmodifiableListIterator);
    assertNotSame(listIterator, unmodifiableListIterator);
    assertTrue(unmodifiableListIterator.hasNext());
    assertEquals("test", unmodifiableListIterator.next());
    assertTrue(unmodifiableListIterator.hasNext());

    try {
      unmodifiableListIterator.remove();
    }
    finally {
      assertEquals(3, list.size());
    }
  }

  @Test(expected = NullPointerException.class)
  public void unmodifiableIteratorWithNullIterator() {
    CollectionUtils.unmodifiableIterator(null);
  }

  private static final class Person {

    private final String firstName;
    private final String lastName;

    public Person(final String firstName, final String lastName) {
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

      return ObjectUtils.equals(getFirstName(), that.getFirstName())
        && ObjectUtils.equals(getLastName(), that.getLastName());
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
