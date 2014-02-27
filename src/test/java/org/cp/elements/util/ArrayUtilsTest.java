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

import static org.junit.Assert.*;

import java.util.Enumeration;
import java.util.Iterator;
import java.util.NoSuchElementException;

import org.cp.elements.lang.Filter;
import org.cp.elements.lang.NumberUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.support.DefaultFilter;
import org.cp.elements.test.TestUtils;
import org.junit.Test;

/**
 * The ArrayUtilsTest class is a test suite of test cases testing the contract and functionality
 * of the ArrayUtils class.
 * <p/>
 * @author John J. Blum
 * @since 1.0.0
 * @see org.cp.elements.util.ArrayUtils
 * @see org.junit.Assert
 * @see org.junit.Test
 */
public class ArrayUtilsTest {

  @Test
  public void testAsArray() {
    assertNull(ArrayUtils.asArray((Object[]) null));
    TestUtils.assertEquals(new String[] { "test", "testing", "tested" },
      ArrayUtils.asArray("test", "testing", "tested"));
  }

  @Test
  public void testCount() {
    final Integer[] array = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

    assertNotNull(array);
    assertEquals(10, array.length);
    assertEquals(5, ArrayUtils.count(array, new Filter<Integer>() {
      @Override public boolean accept(final Integer number) {
        return NumberUtils.isEven(number);
      }
    }));
  }

  @Test
  public void testCountReturnsLength() {
    final Object[] array = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

    assertEquals(array.length, ArrayUtils.count(array, new DefaultFilter<Object>(true)));
  }

  @Test
  public void testCountReturnsZero() {
    assertEquals(0, ArrayUtils.count(new Object[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, new DefaultFilter<Object>(false)));
  }

  @Test(expected = NullPointerException.class)
  public void testCountWithNullArray() {
    ArrayUtils.count(null, new DefaultFilter<Object>(true));
  }

  @Test(expected = NullPointerException.class)
  public void testCountWithNullFilter() {
    ArrayUtils.count(new Object[0], null);
  }

  @Test
  public void testEmptyArray() {
    final Object[] emptyArray = ArrayUtils.emptyArray();

    assertNotNull(emptyArray);
    assertEquals(0, emptyArray.length);

    final Object[] anotherEmptyArray = ArrayUtils.emptyArray();

    assertNotNull(anotherEmptyArray);
    assertEquals(0, anotherEmptyArray.length);
    assertNotSame(emptyArray, anotherEmptyArray);
  }

  @Test
  public void testEmptyArrayWithArray() {
    final Object[] expectedArray = { 0, 1, 2 };

    assertEquals(3, expectedArray.length);

    final Object[] actualArray = ArrayUtils.emptyArray(expectedArray);

    assertSame(expectedArray, actualArray);
    assertEquals(3, actualArray.length);
  }

  @Test
  public void testEmptyArrayWithEmptyArray() {
    final Object[] expectedArray = { };

    assertEquals(0, expectedArray.length);

    final Object[] actualArray = ArrayUtils.emptyArray(expectedArray);

    assertSame(expectedArray, actualArray);
    assertEquals(0, actualArray.length);
  }

  @Test
  public void testEmptyArrayWithNullArray() {
    final Object[] actualArray = ArrayUtils.emptyArray((Object[]) null);

    assertNotNull(actualArray);
    assertEquals(0, actualArray.length);
  }

  @Test
  public void testEnumeration() {
    final Object[] array = { "test", "testing", "tested" };
    final Enumeration<?> arrayEnumeration = ArrayUtils.enumeration(array);

    assertNotNull(arrayEnumeration);

    int index = 0;

    while (arrayEnumeration.hasMoreElements()) {
      assertEquals(array[index++], arrayEnumeration.nextElement());
    }

    assertEquals(array.length, index);
  }

  @Test(expected = NullPointerException.class)
  public void testEnumerationWithNullArray() {
    ArrayUtils.enumeration((Object[]) null);
  }

  @Test
  public void testEnumerationWithNoElements() {
    final Enumeration<?> noElementEnumeration = ArrayUtils.enumeration(ArrayUtils.emptyArray());

    assertNotNull(noElementEnumeration);
    assertFalse(noElementEnumeration.hasMoreElements());
  }

  @Test
  public void testEnumerationWithSingleElement() {
    final Enumeration<?> singleElementEnumeration = ArrayUtils.enumeration("test");

    assertNotNull(singleElementEnumeration);
    assertTrue(singleElementEnumeration.hasMoreElements());
    assertEquals("test", singleElementEnumeration.nextElement());
    assertFalse(singleElementEnumeration.hasMoreElements());
  }

  @Test(expected = NoSuchElementException.class)
  public void testEnumerationWithExhaustedArray() {
    final Enumeration<?> enumeration = ArrayUtils.enumeration("test");

    assertNotNull(enumeration);
    assertTrue(enumeration.hasMoreElements());
    assertEquals("test", enumeration.nextElement());
    assertFalse(enumeration.hasMoreElements());

    enumeration.nextElement();
  }

  @Test
  public void testFind() {
    final Person janeDoe = new Person("Jane", "Doe");
    final Person jonDoe = new Person("Jon", "Doe");
    final Person pieDoe = new Person("Pie", "Doe");
    final Person jackHandy = new Person("Jack", "Handy");
    final Person sandyHandy = new Person("Sandy", "Handy");

    final Person[] people = { jackHandy, jonDoe, janeDoe, sandyHandy, pieDoe };

    final Filter<Person> doeFilter = new Filter<Person>() {
      @Override public boolean accept(final Person person) {
        return "Doe".equalsIgnoreCase(person.getLastName());
      }
    };

    final Person actualPerson = ArrayUtils.find(people, doeFilter);

    assertNotNull(actualPerson);
    assertEquals(jonDoe, actualPerson);
  }

  @Test(expected = NullPointerException.class)
  public void testFindWithNullArray() {
    ArrayUtils.find(null, new DefaultFilter<Object>(true));
  }

  @Test(expected = NullPointerException.class)
  public void testFindWithNullFilter() {
    ArrayUtils.find(new Object[0], null);
  }

  @Test
  public void testFindWithNonMatchingFilter() {
    final Person janeDoe = new Person("Jane", "Doe");
    final Person jonDoe = new Person("Jon", "Doe");
    final Person pieDoe = new Person("Pie", "Doe");
    final Person jackHandy = new Person("Jack", "Handy");
    final Person sandyHandy = new Person("Sandy", "Handy");

    final Person[] people = { jackHandy, jonDoe, janeDoe, sandyHandy, pieDoe };

    final Filter<Person> doeFilter = new Filter<Person>() {
      @Override public boolean accept(final Person person) {
        return ("Play".equalsIgnoreCase(person.getFirstName()) && "Doe".equalsIgnoreCase(person.getLastName()));
      }
    };

    final Person actualPerson = ArrayUtils.find(people, doeFilter);

    assertNull(actualPerson);
  }

  @Test
  public void testFindAll() {
    final Integer[] numbers = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

    final Filter<Integer> evenNumberFilter = new Filter<Integer>() {
      public boolean accept(final Integer number) {
        return NumberUtils.isEven(number);
      }
    };

    final Integer[] actualNumbers = ArrayUtils.findAll(numbers, evenNumberFilter);

    assertNotNull(actualNumbers);
    assertNotSame(numbers, actualNumbers);
    assertEquals(5, actualNumbers.length);

    for (int number = 0, index = 0; number < 10; number += 2, index++) {
      assertEquals(number, actualNumbers[index].intValue());
    }
  }

  @Test(expected = NullPointerException.class)
  public void testFindAllWithNullArray() {
    ArrayUtils.findAll(null, new DefaultFilter<Object>(true));
  }

  @Test(expected = NullPointerException.class)
  public void testFindAllWithNullFilter() {
    ArrayUtils.findAll(new Object[0], null);
  }

  @Test
  public void testFindAllWithNonMatchingFilter() {
    final Integer[] numbers = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    final Integer[] actualNumbers = ArrayUtils.findAll(numbers, new DefaultFilter<Integer>(false));

    assertNotNull(actualNumbers);
    assertNotSame(numbers, actualNumbers);
    assertEquals(10, numbers.length);
    assertEquals(0, actualNumbers.length);
  }

  @Test
  public void testInsert() {
    TestUtils.assertEquals(ArrayUtils.asArray("one", "two", "three", "four"), ArrayUtils.insert("three",
      ArrayUtils.asArray("one", "two", "four"), 2));
    TestUtils.assertEquals(ArrayUtils.asArray("one", "two", "four"), ArrayUtils.prepend("one",
      ArrayUtils.asArray("two", "four")));
    TestUtils.assertEquals(ArrayUtils.asArray("one", "two", "four"), ArrayUtils.add("four",
      ArrayUtils.asArray("one", "two")));
  }

  @Test
  public void testIsEmpty() {
    assertTrue(ArrayUtils.isEmpty(null));
    assertTrue(ArrayUtils.isEmpty(new Object[0]));
    assertTrue(ArrayUtils.isEmpty(new Object[] { }));
    assertFalse(ArrayUtils.isEmpty(new Object[10]));
    assertFalse(ArrayUtils.isEmpty(new Object[] { null }));
    assertFalse(ArrayUtils.isEmpty(new Object[] { "test" }));
    assertFalse(ArrayUtils.isEmpty(new Object[] { "test", "testing", "tested" }));
  }

  @Test
  public void testIterable() {
    final Object[] array = { "test", "testing", "tested" };
    final Iterable<?> iterable = ArrayUtils.iterable(array);

    assertNotNull(iterable);

    int index = 0;

    for (Object element : iterable) {
      assertEquals(array[index++], element);
    }

    assertEquals(array.length, index);
  }

  @Test(expected = NullPointerException.class)
  public void testIterableWithNullArray() {
    try {
      ArrayUtils.iterable((Object[]) null);
    }
    catch (NullPointerException expected) {
      assertEquals("The array of elements cannot be null!", expected.getMessage());
      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testIterableWithNoElements() {
    Iterable<?> iterable = ArrayUtils.iterable();

    assertNotNull(iterable);
    assertFalse(iterable.iterator().hasNext());
  }

  @Test
  public void testIterableWithSingleElement() {
    Iterable<String> iterable = ArrayUtils.iterable("test");

    assertNotNull(iterable);
    assertTrue(iterable.iterator().hasNext());
    assertEquals("test", iterable.iterator().next());
  }

  @Test
  public void testIterator() {
    final Object[] array = { "test", "testing", "tested" };
    final Iterator<?> arrayIterator = ArrayUtils.iterator(array);

    assertNotNull(arrayIterator);

    int index = 0;

    while (arrayIterator.hasNext()) {
      assertEquals(array[index++], arrayIterator.next());
    }

    assertEquals(array.length, index);
  }

  @Test(expected = NullPointerException.class)
  public void testIteratorWithNullArray() {
    ArrayUtils.iterator((Object[]) null);
  }

  @Test
  public void testIteratorWithNoElements() {
    final Iterator<?> noElementsIterator = ArrayUtils.iterator(new Object[0]);

    assertNotNull(noElementsIterator);
    assertFalse(noElementsIterator.hasNext());
  }

  @Test
  public void testIteratorWithSingleElement() {
    final Iterator<?> singleElementIterator = ArrayUtils.iterator("test");

    assertNotNull(singleElementIterator);
    assertTrue(singleElementIterator.hasNext());
    assertEquals("test", singleElementIterator.next());
    assertFalse(singleElementIterator.hasNext());
  }

  @Test(expected = NoSuchElementException.class)
  public void testIteratorWithExhaustedArray() {
    final Iterator<?> iterator = ArrayUtils.iterator("test");

    assertNotNull(iterator);
    assertTrue(iterator.hasNext());
    assertEquals("test", iterator.next());
    assertFalse(iterator.hasNext());

    iterator.next();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testIteratorModification() {
    final Object[] array = { "test", "testing", "tested" };
    final Iterator<?> arrayIterator = ArrayUtils.iterator(array);

    assertNotNull(arrayIterator);
    assertTrue(arrayIterator.hasNext());
    assertEquals("test", arrayIterator.next());
    assertTrue(arrayIterator.hasNext());
    assertEquals(3, array.length);

    try {
      arrayIterator.remove();
    }
    finally {
      assertEquals(3, array.length);
    }

  }

  @Test
  public void testLength() {
    assertEquals(0, ArrayUtils.length(null));
    assertEquals(0, ArrayUtils.length(new Object[0]));
    assertEquals(0, ArrayUtils.length(new Object[] { }));
    assertEquals(10, ArrayUtils.length(new Object[10]));
    assertEquals(1, ArrayUtils.length(new Object[] { null }));
    assertEquals(1, ArrayUtils.length(new Object[] { "test" }));
    assertEquals(3, ArrayUtils.length(new Object[] { "test", "testing", "tested" }));
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
