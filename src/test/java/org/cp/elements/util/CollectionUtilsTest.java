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

import static org.cp.elements.lang.OperatorUtils.from;
import static org.junit.Assert.*;

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
import org.cp.elements.lang.NumberUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.support.DefaultFilter;
import org.junit.Test;

/**
 * The CollectionUtilsTest class is a test suite of test cases testing the contract and functionality 
 * of the CollectionUtils class.
 * <p/>
 * @author John J. Blum
 * @see java.util.Collection
 * @see org.cp.elements.util.CollectionUtils
 * @see org.junit.Test
 * @since 1.0.0
 */
public class CollectionUtilsTest {

  @Test
  public void testCount() {
    final Collection<String> collection = Arrays.asList("test", "fixture", "testing", "mock", "tested");

    assertNotNull(collection);
    assertEquals(5, collection.size());
    assertEquals(3, CollectionUtils.count(collection, new Filter<String>() {
      @Override public boolean accept(final String value) {
        return StringUtils.contains(value, "test");
      }
    }));
  }

  @Test
  public void testCountReturnsSize() {
    final Collection<String> collection = Arrays.asList("test", "testing", "tested");

    assertNotNull(collection);
    assertEquals(collection.size(), CollectionUtils.count(collection, new DefaultFilter<String>(true)));
  }

  @Test
  public void testCountReturnsZero() {
    final Collection<String> collection = Arrays.asList("test", "testing", "tested");

    assertNotNull(collection);
    assertEquals(0, CollectionUtils.count(collection, new DefaultFilter<String>(false)));
  }

  @Test(expected = NullPointerException.class)
  public void testCountWithNullCollection() {
    CollectionUtils.count(null, new DefaultFilter<Object>(true));
  }

  @Test(expected = NullPointerException.class)
  public void testCountWithNullFilter() {
    CollectionUtils.count(Collections.emptyList(), null);
  }

  @Test
  public void testEmptyList() {
    final List<?> expectedList = Arrays.asList("test", "testing", "tested");

    assertFalse(expectedList.isEmpty());

    final List<?> actualList = CollectionUtils.emptyList(expectedList);

    assertSame(expectedList, actualList);
    assertFalse(actualList.isEmpty());
  }

  @Test
  public void testEmptyListWithEmptyList() {
    final List<?> expectedList = new ArrayList<Object>(0);

    assertTrue(expectedList.isEmpty());

    final List<?> actualList = CollectionUtils.emptyList(expectedList);

    assertSame(expectedList, actualList);
    assertTrue(actualList.isEmpty());
  }

  @Test
  public void testEmptyListWithNullList() {
    final List<?> actualList = CollectionUtils.emptyList(null);

    assertNotNull(actualList);
    assertTrue(actualList.isEmpty());
    assertEquals(0, actualList.size());
  }

  @Test
  public void testEmptySet() {
    final Set<?> expectedSet = from("test", "testing", "tested").toSet();

    assertNotNull(expectedSet);
    assertFalse(expectedSet.isEmpty());

    final Set<?> actualSet = CollectionUtils.emptySet(expectedSet);

    assertSame(expectedSet, actualSet);
    assertFalse(actualSet.isEmpty());
  }

  @Test
  public void testEmptySetWithEmptySet() {
    final Set<?> expectedSet = new HashSet<Object>(0);

    assertTrue(expectedSet.isEmpty());

    final Set<?> actualSet = CollectionUtils.emptySet(expectedSet);

    assertSame(expectedSet, actualSet);
    assertTrue(actualSet.isEmpty());
  }

  @Test
  public void testEmptySetWithNullSet() {
    final Set<?> actualSet = CollectionUtils.emptySet(null);

    assertNotNull(actualSet);
    assertTrue(actualSet.isEmpty());
    assertEquals(0, actualSet.size());
  }

  @Test
  public void testEnumeration() {
    final List<?> expectedList = Arrays.asList("test", "testing", "tested");
    final Enumeration<?> expectedListEnumeration = CollectionUtils.enumeration(expectedList.iterator());

    assertNotNull(expectedListEnumeration);

    int index = 0;

    while (expectedListEnumeration.hasMoreElements()) {
      assertEquals(expectedList.get(index++), expectedListEnumeration.nextElement());
    }

    assertEquals(expectedList.size(), index);
  }

  @Test(expected = NullPointerException.class)
  public void testEnumerationWithNullIterator() {
    CollectionUtils.enumeration(null);
  }

  @Test
  public void testEnumerationWithNoElements() {
    final Enumeration<?> noElementEnumeration = CollectionUtils.enumeration(
      Collections.emptyList().iterator());

    assertNotNull(noElementEnumeration);
    assertFalse(noElementEnumeration.hasMoreElements());
  }

  @Test
  public void testEnumerationWithSingleElement() {
    final Enumeration<?> singleElementEnumeration = CollectionUtils.enumeration(
      Collections.singletonList("test").iterator());

    assertNotNull(singleElementEnumeration);
    assertTrue(singleElementEnumeration.hasMoreElements());
    assertEquals("test", singleElementEnumeration.nextElement());
    assertFalse(singleElementEnumeration.hasMoreElements());
  }

  @Test(expected = NoSuchElementException.class)
  public void testEnumerationWithExhaustedIterator() {
    final Enumeration<?> enumeration = CollectionUtils.enumeration(
      Collections.singletonList("test").iterator());

    assertNotNull(enumeration);
    assertTrue(enumeration.hasMoreElements());
    assertEquals("test", enumeration.nextElement());
    assertFalse(enumeration.hasMoreElements());

    enumeration.nextElement();
  }

  @Test
  public void testFilter() {
    final Collection<String> collection = new ArrayList<String>(Arrays.asList("test", "testing", "tested"));

    assertNotNull(collection);
    assertFalse(collection.isEmpty());
    assertEquals(3, collection.size());

    final Collection<String> filteredCollection = CollectionUtils.filter(collection, new Filter<String>() {
      @Override public boolean accept(final String element) {
        return "tested".equalsIgnoreCase(element);
      }
    });

    assertSame(collection, filteredCollection);
    assertFalse(collection.isEmpty());
    assertEquals(1, collection.size());
    assertTrue(collection.contains("tested"));
  }

  @Test
  public void testFilterEmptiesCollection() {
    final Collection<String> collection = new ArrayList<String>(Arrays.asList("test", "testing", "tested"));

    assertNotNull(collection);
    assertFalse(collection.isEmpty());
    assertEquals(3, collection.size());

    final Collection<String> filteredCollection = CollectionUtils.filter(collection, new DefaultFilter<String>(false));

    assertSame(collection, filteredCollection);
    assertTrue(collection.isEmpty());
  }

  @Test
  public void testFilterRetainsCollection() {
    final Collection<String> collection = Arrays.asList("test", "testing", "tested");

    assertNotNull(collection);
    assertFalse(collection.isEmpty());
    assertEquals(3, collection.size());

    final Collection<String> filteredCollection = CollectionUtils.filter(collection, new DefaultFilter<String>(true));

    assertSame(collection, filteredCollection);
    assertFalse(collection.isEmpty());
    assertEquals(3, collection.size());
  }

  @Test(expected = NullPointerException.class)
  public void testFilterWithNullCollection() {
    CollectionUtils.filter(null, new DefaultFilter<Object>(true));
  }

  @Test(expected = NullPointerException.class)
  public void testFilterWithNullFilter() {
    CollectionUtils.filter(Collections.emptyList(), null);
  }

  @Test
  public void testFind() {
    final Person janeDoe = new Person("Jane", "Doe");
    final Person jonDoe = new Person("Jon", "Doe");
    final Person pieDoe = new Person("Pie", "Doe");
    final Person jackHandy = new Person("Jack", "Handy");
    final Person sandyHandy = new Person("Sandy", "Handy");

    final List<Person> people = Arrays.asList(jackHandy, jonDoe, janeDoe, sandyHandy, pieDoe);

    final Filter<Person> doeFilter = new Filter<Person>() {
      @Override public boolean accept(final Person person) {
        return "Doe".equalsIgnoreCase(person.getLastName());
      }
    };

    final Person actualPerson = CollectionUtils.find(people, doeFilter);

    assertNotNull(actualPerson);
    assertEquals(jonDoe, actualPerson);
  }

  @Test
  public void testFindWithNonMatchingFilter() {
    final Person janeDoe = new Person("Jane", "Doe");
    final Person jonDoe = new Person("Jon", "Doe");
    final Person pieDoe = new Person("Pie", "Doe");
    final Person jackHandy = new Person("Jack", "Handy");
    final Person sandyHandy = new Person("Sandy", "Handy");

    final List<Person> people = Arrays.asList(jackHandy, jonDoe, janeDoe, sandyHandy, pieDoe);

    final Filter<Person> doeFilter = new Filter<Person>() {
      @Override public boolean accept(final Person person) {
        return ("Play".equalsIgnoreCase(person.getFirstName()) && "Doe".equalsIgnoreCase(person.getLastName()));
      }
    };

    final Person actualPerson = CollectionUtils.find(people, doeFilter);

    assertNull(actualPerson);
  }

  @Test(expected = NullPointerException.class)
  public void testFindWithNullIterable() {
    CollectionUtils.find(null, new DefaultFilter<Object>(true));
  }

  @Test(expected = NullPointerException.class)
  public void testFindWithNullFilter() {
    CollectionUtils.find(Collections.emptyList(), null);
  }

  @Test
  public void testFindAll() {
    final List<Integer> numbers = Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);

    final Filter<Integer> oddNumberFilter = new Filter<Integer>() {
      public boolean accept(final Integer number) {
        return NumberUtils.isOdd(number);
      }
    };

    final List<Integer> actualNumbers = CollectionUtils.findAll(numbers, oddNumberFilter);

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
  public void testFindAllWithNonMatchingFilter() {
    final List<Integer> numbers = Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
    final List<Integer> actualNumbers = CollectionUtils.findAll(numbers, new DefaultFilter<Integer>(false));

    assertNotNull(actualNumbers);
    assertNotSame(numbers, actualNumbers);
    assertEquals(10, numbers.size());
    assertEquals(0, actualNumbers.size());
  }

  @Test(expected = NullPointerException.class)
  public void testFindAllWithNullIterable() {
    CollectionUtils.findAll(null, new DefaultFilter<Object>(true));
  }

  @Test(expected = NullPointerException.class)
  public void testFindAllWithNullFilter() {
    CollectionUtils.findAll(Collections.emptyList(), null);
  }

  @Test
  public void testIsEmptyCollection() {
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
  public void testIterableForEnumeration() {
    String[] expectedElements = { "test", "testing", "tested" };
    Vector<String> vector = new Vector<String>(Arrays.asList(expectedElements));
    Iterable<String> iterable = CollectionUtils.iterable(vector.elements());

    assertNotNull(iterable);
    assertNotNull(iterable.iterator());

    int index = 0;

    for (String actualElement : iterable) {
      assertEquals(expectedElements[index++], actualElement);
    }

    assertEquals(expectedElements.length, index);
  }

  @Test(expected = NullPointerException.class)
  public void testIterableForEnumerationWithNull() {
    CollectionUtils.iterable((Enumeration<?>) null);
  }

  @Test
  public void testIterableForIterator() {
    final List<?> expectedList = Arrays.asList("test", "testing", "tested");
    final List<Object> actualList = new ArrayList<Object>(3);

    for (final Object item : CollectionUtils.iterable(expectedList.iterator())) {
      actualList.add(item);
    }

    assertFalse(actualList.isEmpty());
    assertEquals(expectedList.size(), actualList.size());
    assertTrue(actualList.containsAll(expectedList));
  }

  @Test(expected = NullPointerException.class)
  public void testIterableForIteratorWithNull() {
    CollectionUtils.iterable((Iterator<?>) null);
  }

  @Test
  public void testIterator() {
    final Vector<Object> expectedVector = new Vector<Object>(3);

    expectedVector.add("test");
    expectedVector.add("testing");
    expectedVector.add("tested");

    final Iterator<?> expectedVectorIterator = CollectionUtils.iterator(expectedVector.elements());

    assertNotNull(expectedVectorIterator);

    int index = 0;

    while (expectedVectorIterator.hasNext()) {
      assertEquals(expectedVector.get(index++), expectedVectorIterator.next());
    }

    assertEquals(expectedVector.size(), index);
  }

  @Test(expected = NullPointerException.class)
  public void testIteratorWithNullEnumeration() {
    CollectionUtils.iterator((Enumeration<?>) null);
  }

  @Test
  public void testIteratorWithNoElements() {
    final Iterator<?> zeroElementIterator = CollectionUtils.iterator(new Vector<Object>().elements());

    assertNotNull(zeroElementIterator);
    assertFalse(zeroElementIterator.hasNext());
  }

  @Test
  public void testIteratorWithSingleElement() {
    final Vector<Object> singleElementVector = new Vector<Object>(1);

    singleElementVector.add("test");

    final Iterator<?> singleElementIterator = CollectionUtils.iterator(singleElementVector.elements());

    assertNotNull(singleElementIterator);
    assertTrue(singleElementIterator.hasNext());
    assertEquals("test", singleElementIterator.next());
    assertFalse(singleElementIterator.hasNext());
  }

  @Test(expected = NoSuchElementException.class)
  public void testIteratorWithExhaustedEnumeration() {
    final Vector<Object> vector = new Vector<Object>(1);

    vector.add("test");

    final Iterator<?> iterator = CollectionUtils.iterator(vector.elements());

    assertNotNull(iterator);
    assertTrue(iterator.hasNext());
    assertEquals("test", iterator.next());
    assertFalse(iterator.hasNext());

    iterator.next();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testIteratorModification() {
    final Vector<Object> vector = new Vector<Object>(3);

    vector.add("test");
    vector.add("testing");
    vector.add("tested");

    assertNotNull(vector);
    assertEquals(3, vector.size());

    final Iterator<?> iterator = CollectionUtils.iterator(vector.elements());

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

  @Test
  public void testSizeOfCollection() {
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
  public void testToString() {
    assertEquals("[]", CollectionUtils.toString(Collections.emptyList()));
    assertEquals("[test, testing, tested]", CollectionUtils.toString(Arrays.asList("test", "testing", "tested")));
  }

  @Test
  public void testUnmodifiableIterator() {
    final List<?> list = Arrays.asList("test", "testing", "tested");
    final Iterator<?> listIterator = list.iterator();

    assertNotNull(listIterator);

    final Iterator<?> unmodifiableListIterator = CollectionUtils.unmodifiableIterator(listIterator);

    assertNotNull(unmodifiableListIterator);
    assertNotSame(listIterator, unmodifiableListIterator);

    int index = 0;

    while (unmodifiableListIterator.hasNext()) {
      assertEquals(list.get(index++), unmodifiableListIterator.next());
    }

    assertEquals(list.size(), index);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testUnmodifiableIteratorModification() {
    final List<?> list = Arrays.asList("test", "testing", "tested");
    final Iterator<?> listIterator = list.iterator();

    assertNotNull(listIterator);
    assertEquals(3, list.size());

    final Iterator<?> unmodifiableListIterator = CollectionUtils.unmodifiableIterator(listIterator);

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
  public void testUnmodifiableIteratorWithNullIterator() {
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

      final Person that = (Person) obj;

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
