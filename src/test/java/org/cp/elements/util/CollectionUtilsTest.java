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
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
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
import java.util.Vector;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.FilteringTransformer;
import org.cp.elements.lang.NumberUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.Transformer;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * The CollectionUtilsTest class is a test suite of test cases testing the contract and functionality 
 * of the {@link CollectionUtils} class.
 * 
 * @author John J. Blum
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
public class CollectionUtilsTest {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @SafeVarargs
  protected final <T> void assertElements(Collection<T> collection, T... elements) {
    assertThat(collection, is(notNullValue(Collection.class)));
    assertThat(collection.size(), is(equalTo(elements.length)));
    assertThat(collection.containsAll(asCollection(elements)), is(true));
  }

  protected <T> void assertShuffled(final Iterable<T> source, final Iterable<T> target) {
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
  public void emptyIterableIsNonNullEmptyIterable() {
    Iterable<?> emptyIterable = CollectionUtils.emptyIterable();

    assertThat(emptyIterable, is(notNullValue(Iterable.class)));
    assertThat(emptyIterable.iterator(), is(notNullValue(Iterator.class)));
    assertThat(emptyIterable.iterator().hasNext(), is(false));
  }

  @Test
  public void enumerationForIterator() {
    String[] elements = { "test", "testing", "tested"};
    Enumeration<String> enumeration = CollectionUtils.enumeration(asIterator(elements));

    assertThat(enumeration, is(notNullValue(Enumeration.class)));

    for (String element : elements) {
      assertThat(enumeration.hasMoreElements(), is(true));
      assertThat(enumeration.nextElement(), is(equalTo(element)));
    }
  }

  @Test
  public void enumerationForEmptyIterator() {
    Enumeration<?> enumeration = CollectionUtils.enumeration(Collections.emptyIterator());

    assertThat(enumeration, is(notNullValue(Enumeration.class)));
    assertThat(enumeration.hasMoreElements(), is(false));
  }

  @Test
  public void enumerationForNullIterator() {
    Enumeration<?> enumeration = CollectionUtils.enumeration(null);

    assertThat(enumeration, is(notNullValue(Enumeration.class)));
    assertThat(enumeration.hasMoreElements(), is(false));
  }

  @Test
  public void enumerationForSingleElementIterator() {
    Enumeration<String> enumeration = CollectionUtils.enumeration(asIterator("test"));

    assertThat(enumeration, is(notNullValue(Enumeration.class)));
    assertThat(enumeration.hasMoreElements(), is(true));
    assertThat(enumeration.nextElement(), is(equalTo("test")));
    assertThat(enumeration.hasMoreElements(), is(false));
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
  public void iterableWithEnumeration() {
    Integer[] elements = { 0, 1, 2 };
    Iterable<Integer> iterable = CollectionUtils.iterable(asEnumeration(elements));

    assertThat(iterable, is(notNullValue(Iterable.class)));

    int index = 0;

    for (Integer element : iterable) {
      assertThat(element, is(equalTo(elements[index++])));
    }

    assertThat(index, is(equalTo(elements.length)));
  }

  @Test
  public void iterableWithEmptyEnumeration() {
    Iterable<?> iterable = CollectionUtils.iterable(Collections.emptyEnumeration());

    assertThat(iterable, is(notNullValue(Iterable.class)));
    assertThat(iterable.iterator(), is(notNullValue()));
    assertThat(iterable.iterator().hasNext(), is(false));
  }

  @Test
  public void iterableWithNullEnumeration() {
    Iterable<?> iterable = CollectionUtils.iterable((Enumeration<?>) null);

    assertThat(iterable, is(notNullValue(Iterable.class)));
    assertThat(iterable.iterator(), is(notNullValue(Iterator.class)));
    assertThat(iterable.iterator().hasNext(), is(false));
  }

  @Test
  public void iterableWithIterator() {
    Integer[] elements = { 0, 1, 2 };
    Iterable<Integer> iterable = CollectionUtils.iterable(asIterator(elements));

    assertThat(iterable, is(notNullValue(Iterable.class)));

    int index = 0;

    for (Integer element : iterable) {
      assertThat(element, is(equalTo(elements[index++])));
    }

    assertThat(index, is(equalTo(elements.length)));
  }

  @Test
  public void iterableWithEmptyIterator() {
    Iterable<?> iterable = CollectionUtils.iterable(Collections.emptyIterator());

    assertThat(iterable, is(notNullValue(Iterable.class)));
    assertThat(iterable.iterator(), is(notNullValue(Iterator.class)));
    assertThat(iterable.iterator().hasNext(), is(false));
  }

  @Test
  public void iterableWithNullIterator() {
    Iterable<?> iterable = CollectionUtils.iterable((Iterator<?>) null);

    assertThat(iterable, is(notNullValue(Iterable.class)));
    assertThat(iterable.iterator(), is(notNullValue(Iterator.class)));
    assertThat(iterable.iterator().hasNext(), is(false));
  }

  @Test
  public void iteratorForEnumeration() {
    Integer[] elements = { 0, 1, 2 };
    Iterator<Integer> iterator = CollectionUtils.iterator(asEnumeration(elements));

    assertThat(iterator, is(notNullValue(Iterator.class)));

    for (Integer element : elements) {
      assertThat(iterator.hasNext(), is(true));
      assertThat(iterator.next(), is(equalTo(element)));
    }
  }

  @Test
  public void iteratorForEmptyEnumeration() {
    Iterator<?> iterator = CollectionUtils.iterator(Collections.emptyEnumeration());

    assertThat(iterator, is(notNullValue(Iterator.class)));
    assertThat(iterator.hasNext(), is(false));
  }

  @Test
  public void iteratorForNullEnumeration() {
    Iterator<?> iterator = CollectionUtils.iterator(null);

    assertThat(iterator, is(notNullValue(Iterator.class)));
    assertThat(iterator.hasNext(), is(false));
  }

  @Test
  public void iteratorForSingleElementEnumeration() {
    Iterator<?> iterator = CollectionUtils.iterator(asEnumeration("test"));

    assertThat(iterator, is(notNullValue(Iterator.class)));
    assertThat(iterator.hasNext(), is(true));
    assertThat(iterator.next(), is(equalTo("test")));
    assertThat(iterator.hasNext(), is(false));
  }

  @Test
  public void test() {
  }

  @Test
  public void nullSafeListWitList() {
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
  public void nullSafeSetWitSet() {
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

  @Test
  public void shuffleNullList() {
    assertThat(CollectionUtils.shuffle(null), is(nullValue(List.class)));
  }

  @Test
  public void shuffleSingleElementList() {
    List<Integer> singleElementList = Collections.singletonList(1);
    List<Integer> shuffledSingleElementList = CollectionUtils.shuffle(singleElementList);

    assertNotNull(shuffledSingleElementList);
    assertEquals(singleElementList, shuffledSingleElementList);
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

    Collection<String> transformedCollection = CollectionUtils.transform(collection, transformer);

    assertThat(transformedCollection, is(notNullValue(Collection.class)));
    assertThat(transformedCollection, is(not(sameInstance(collection))));
    assertThat(transformedCollection.size(), is(equalTo(collection.size())));
    assertThat(transformedCollection.containsAll(asCollection("TEST", "TESTING", "TESTED")), is(true));
  }

  @Test
  public void transformEmptyCollection() {
    Collection<Object> collection = Collections.emptySet();
    Collection<Object> transformedCollection = CollectionUtils.transform(collection, (value) -> "test");

    assertThat(transformedCollection, is(notNullValue(Collection.class)));
    assertThat(transformedCollection, is(not(sameInstance(collection))));
    assertThat(transformedCollection.isEmpty(), is(true));
  }

  @Test(expected = IllegalArgumentException.class)
  public void transformNullCollection() {
    CollectionUtils.transform(null, (value) -> null);
  }

  @Test(expected = IllegalArgumentException.class)
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

  @Test(expected = IllegalArgumentException.class)
  public void unmodifiableIteratorWithNullIterator() {
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
