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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Vector;

import org.cp.elements.lang.Filter;
import org.cp.elements.lang.FilteringTransformer;
import org.cp.elements.lang.NumberUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.Transformer;
import org.cp.elements.lang.support.DefaultFilter;
import org.cp.elements.test.TestUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * The ArrayUtilsTest class is a test suite of test cases testing the contract and functionality
 * of the ArrayUtils class.
 * 
 * @author John J. Blum
 * @see java.lang.reflect.Array
 * @see java.util.Arrays
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.cp.elements.util.ArrayUtils
 * @since 1.0.0
 */
public class ArrayUtilsTest {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @SuppressWarnings("all")
  protected <T> void assertShuffled(T[] source, T[] target) {
    assertTrue("'source' array must not be null or empty!", source != null && source.length != 0);
    assertTrue("'target' array must not be null or empty!", target != null && target.length != 0);
    assertEquals("The lengths of the 'source' and 'target' arrays must match!", source.length, target.length);

    boolean shuffled = false;

    for (int index = 0; index < source.length && !shuffled; index++) {
      shuffled |= !source[index].equals(target[index]);
    }

    assertTrue(String.format("The target array (%1$s) was not shuffled!", Arrays.toString(target)), shuffled);
  }

  @SuppressWarnings("unchecked")
  protected <T> T[] copy(T[] array) {
    T[] arrayCopy = (T[]) Array.newInstance(array[0].getClass(), array.length);
    System.arraycopy(array, 0, arrayCopy, 0, array.length);
    return arrayCopy;
  }

  @Test
  public void append() {
    String[] array = ArrayUtils.append("test", new String[0]);

    assertNotNull(array);
    assertEquals(1, array.length);
    assertEquals("test", array[0]);

    array = ArrayUtils.append("testing", array);

    assertNotNull(array);
    assertEquals(2, array.length);
    assertEquals("test", array[0]);
    assertEquals("testing", array[1]);

    array = ArrayUtils.append("tested", array);

    assertNotNull(array);
    assertEquals(3, array.length);
    assertEquals("test", array[0]);
    assertEquals("testing", array[1]);
    assertEquals("tested", array[2]);
  }

  @Test
  public void asArrayWithVarargs() {
    assertThat(ArrayUtils.asArray("test", "testing", "tested"),
      is(equalTo(new String[] { "test", "testing", "tested" })));
  }

  @Test
  public void asArrayWithNoVarargs() {
    assertThat(ArrayUtils.<Integer>asArray(), is(equalTo(new Integer[0])));
  }

  @Test
  public void asArrayWithNullVarargs() {
    assertThat(ArrayUtils.asArray((Object[]) null), is(nullValue()));
  }

  @Test
  public void asArrayWithEnumeration() {
    Enumeration<Integer> enumeration = new Vector<>(Arrays.asList(0, 1, 2)).elements();
    Integer[] numbers = ArrayUtils.asArray(enumeration, Integer.class);

    assertThat(numbers, is(notNullValue()));
    assertThat(numbers.length, is(equalTo(3)));

    for (int index = 0; index < numbers.length; index++) {
      assertThat(numbers[index], is(equalTo(index)));
    }
  }

  @Test
  public void asArrayWithEmptyEnumeration() {
    Enumeration<String> enumeration = new Vector<String>().elements();
    String[] strings = ArrayUtils.asArray(enumeration, String.class);

    assertThat(strings, is(notNullValue()));
    assertThat(strings.length, is(equalTo(0)));
  }

  @Test
  public void asArrayWithNullEnumeration() {
    Object[] array = ArrayUtils.asArray((Enumeration<Object>) null, Object.class);

    assertThat(array, is(notNullValue()));
    assertThat(array.length, is(equalTo(0)));
  }

  @Test
  public void asArrayWithIterable() {
    Iterable<Integer> iterable = () -> Arrays.asList(0, 1, 2).iterator();
    Integer[] numbers = ArrayUtils.asArray(iterable, Integer.class);

    assertThat(numbers, is(notNullValue()));
    assertThat(numbers.length, is(equalTo(3)));

    for (int index = 0; index < numbers.length; index++) {
      assertThat(numbers[index], is(equalTo(index)));
    }
  }

  @Test
  public void asArrayWithEmptyIterable() {
    Iterable<String> iterable = () -> Collections.<String>emptyList().iterator();
    String[] strings = ArrayUtils.asArray(iterable, String.class);

    assertThat(strings, is(notNullValue()));
    assertThat(strings.length, is(equalTo(0)));
  }

  @Test
  public void asArrayWithNullIterable() {
    Object[] array = ArrayUtils.asArray((Iterable<Object>) null, Object.class);

    assertThat(array, is(notNullValue()));
    assertThat(array.length, is(equalTo(0)));
  }

  @Test
  public void asArrayWithIterator() {
    Iterator<Integer> iterator = Arrays.asList(0, 1, 2).iterator();
    Integer[] numbers = ArrayUtils.asArray(iterator, Integer.class);

    assertThat(numbers, is(notNullValue()));
    assertThat(numbers.length, is(equalTo(3)));

    for (int index = 0; index < numbers.length; index++) {
      assertThat(numbers[index], is(equalTo(index)));
    }
  }

  @Test
  public void asArrayWithEmptyIterator() {
    Iterator<String> iterator = Collections.<String>emptyList().iterator();
    String[] strings = ArrayUtils.asArray(iterator, String.class);

    assertThat(strings, is(notNullValue()));
    assertThat(strings.length, is(equalTo(0)));
  }

  @Test
  public void asArrayWithNullIterator() {
    Object[] array = ArrayUtils.asArray((Iterator<Object>) null, Object.class);

    assertThat(array, is(notNullValue()));
    assertThat(array.length, is(equalTo(0)));
  }

  @Test
  public void count() {
    Integer[] array = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

    assertNotNull(array);
    assertEquals(10, array.length);
    assertEquals(5, ArrayUtils.count(array, (Filter<Integer>) NumberUtils::isEven));
  }

  @Test
  public void countReturnsLength() {
    Object[] array = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

    assertEquals(array.length, ArrayUtils.count(array, new DefaultFilter<>(true)));
  }

  @Test
  public void countReturnsZero() {
    assertEquals(0, ArrayUtils.count(new Object[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, new DefaultFilter<>(false)));
  }

  @Test(expected = IllegalArgumentException.class)
  public void countWithNullArray() {
    ArrayUtils.count(null, new DefaultFilter<>(true));
  }

  @Test(expected = IllegalArgumentException.class)
  public void countWithNullFilter() {
    ArrayUtils.count(new Object[0], null);
  }

  @Test
  public void emptyArrayIsClonedProperly() {
    Object[] emptyArray = ArrayUtils.emptyArray();

    assertThat(emptyArray, is(notNullValue()));
    assertThat(emptyArray.length, is(equalTo(0)));

    Object[] anotherEmptyArray = ArrayUtils.emptyArray();

    assertThat(anotherEmptyArray, is(notNullValue()));
    assertThat(anotherEmptyArray.length, is(equalTo(0)));
    assertThat(anotherEmptyArray, is(not(sameInstance(emptyArray))));
  }

  @Test
  public void nullSafeArrayWithNonNullNonEmptyArray() {
    String[] expectedArray = { "test", "testing", "tested" };
    String[] actualArray = ArrayUtils.nullSafeArray(expectedArray);

    assertThat(actualArray, is(sameInstance(expectedArray)));
  }

  @Test
  public void nullSafeArrayWithEmptyArray() {
    Object[] expectedArray = { };
    Object[] actualArray = ArrayUtils.nullSafeArray(expectedArray);

    assertThat(actualArray, is(sameInstance(expectedArray)));
  }

  @Test
  @SuppressWarnings("all")
  public void emptyArrayWithNullArray() {
    Integer[] actualArray = null;

    assertThat(actualArray, is(nullValue()));

    actualArray = ArrayUtils.nullSafeArray(actualArray, Integer.class);

    assertThat(actualArray, is(notNullValue()));
    assertThat(actualArray.length, is(equalTo(0)));
  }

  @Test
  public void enumeration() {
    Object[] array = { "test", "testing", "tested" };
    Enumeration<?> arrayEnumeration = ArrayUtils.enumeration(array);

    assertNotNull(arrayEnumeration);

    int index = 0;

    while (arrayEnumeration.hasMoreElements()) {
      assertEquals(array[index++], arrayEnumeration.nextElement());
    }

    assertEquals(array.length, index);
  }

  @Test(expected = NoSuchElementException.class)
  public void enumerationWithExhaustedArray() {
    Enumeration<?> enumeration = ArrayUtils.enumeration("test");

    assertNotNull(enumeration);
    assertTrue(enumeration.hasMoreElements());
    assertEquals("test", enumeration.nextElement());
    assertFalse(enumeration.hasMoreElements());

    enumeration.nextElement();
  }

  @Test
  public void enumerationWithNoElementArray() {
    Enumeration<?> noElementEnumeration = ArrayUtils.enumeration(ArrayUtils.emptyArray());

    assertNotNull(noElementEnumeration);
    assertFalse(noElementEnumeration.hasMoreElements());
  }

  @Test(expected = IllegalArgumentException.class)
  public void enumerationWithNullArray() {
    ArrayUtils.enumeration((Object[]) null);
  }

  @Test
  public void enumerationWithSingleElementArray() {
    Enumeration<?> singleElementEnumeration = ArrayUtils.enumeration("test");

    assertNotNull(singleElementEnumeration);
    assertTrue(singleElementEnumeration.hasMoreElements());
    assertEquals("test", singleElementEnumeration.nextElement());
    assertFalse(singleElementEnumeration.hasMoreElements());
  }

  @Test
  public void filter() {
    Integer[] numbers = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

    Filter<Integer> oddNumberFilter = NumberUtils::isOdd;

    Integer[] actualNumbers = ArrayUtils.filter(numbers, oddNumberFilter);

    assertSame(numbers, actualNumbers);

    for (int index = 0; index < actualNumbers.length; index++) {
      if (NumberUtils.isOdd(index)) {
        assertEquals(index, numbers[index].intValue());
      }
      else {
        assertNull(actualNumbers[index]);
      }
    }
  }

  @Test
  public void filterEmptyArray() {
    Object[] array = {};

    assertEquals(0, array.length);
    assertSame(array, ArrayUtils.filter(array, new DefaultFilter<>(false)));
    assertEquals(0, array.length);
  }

  @Test(expected = IllegalArgumentException.class)
  public void filterNullArray() {
    ArrayUtils.filter(null, new DefaultFilter<>(true));
  }

  @Test(expected = IllegalArgumentException.class)
  public void filterWithNullFilter() {
    ArrayUtils.filter(new Object[0], null);
  }

  @Test
  public void filterAndTransform() {
    String[] array = { "  ", "test", null, "testing", "", "tested" };

    FilteringTransformer<String> filteringTransformer = new FilteringTransformer<String>() {
      @Override public boolean accept(final String value) {
        return (StringUtils.length(value) > 4);
      }

      @Override public String transform(final String value) {
        return value.toUpperCase();
      }
    };

    String[] actualArray = ArrayUtils.filterAndTransform(array, filteringTransformer);

    assertSame(array, actualArray);
    assertEquals(6, actualArray.length);
    assertNull(actualArray[0]);
    assertNull(actualArray[1]);
    assertNull(actualArray[2]);
    assertEquals("TESTING", actualArray[3]);
    assertNull(actualArray[4]);
    assertEquals("TESTED", actualArray[5]);
  }

  @Test
  public void filterAndTransformIntoNullValueArray() {
    String[] array = { "TEST", "TESTING", "TESTED" };

    FilteringTransformer<String> filteringTransformer = new FilteringTransformer<String>() {
      @Override public boolean accept(final String obj) {
        return false;
      }

      @Override public String transform(final String value) {
        return value.toLowerCase();
      }
    };

    String[] actualArray = ArrayUtils.filterAndTransform(array, filteringTransformer);

    assertSame(array, actualArray);
    assertNull(actualArray[0]);
    assertNull(actualArray[1]);
    assertNull(actualArray[2]);
  }

  @Test(expected = IllegalArgumentException.class)
  public void filterAndTransformNullArray() {
    ArrayUtils.filterAndTransform(null, new FilteringTransformer<Object>() {
      @Override public boolean accept(final Object obj) {
        return false;
      }

      @Override public Object transform(final Object value) {
        return null;
      }
    });
  }

  @Test(expected = IllegalArgumentException.class)
  public void filterAndTransformWithNullFilteringTransformer() {
    ArrayUtils.filterAndTransform(new Object[0], null);
  }

  @Test
  public void find() {
    Person cookieDoe = new Person("Cookie", "Doe");
    Person janeDoe = new Person("Jane", "Doe");
    Person jonDoe = new Person("Jon", "Doe");
    Person pieDoe = new Person("Pie", "Doe");
    Person jackHandy = new Person("Jack", "Handy");
    Person sandyHandy = new Person("Sandy", "Handy");

    Person[] people = { jackHandy, jonDoe, janeDoe, sandyHandy, pieDoe, cookieDoe };

    Filter<Person> doeFilter = (person) -> "Doe".equalsIgnoreCase(person.getLastName());

    Person actualPerson = ArrayUtils.find(people, doeFilter);

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

    Person[] people = { jackHandy, jonDoe, janeDoe, sandyHandy, pieDoe, cookieDoe };

    Filter<Person> doeFilter = (person) -> ("Play".equalsIgnoreCase(person.getFirstName())
      && "Doe".equalsIgnoreCase(person.getLastName()));

    Person actualPerson = ArrayUtils.find(people, doeFilter);

    assertNull(actualPerson);
  }

  @Test(expected = IllegalArgumentException.class)
  public void findWithNullArray() {
    ArrayUtils.find(null, new DefaultFilter<>(true));
  }

  @Test(expected = IllegalArgumentException.class)
  public void findWithNullFilter() {
    ArrayUtils.find(new Object[0], null);
  }

  @Test
  public void findAll() {
    Integer[] numbers = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

    Filter<Integer> evenNumberFilter = NumberUtils::isEven;

    Integer[] actualNumbers = ArrayUtils.findAll(numbers, evenNumberFilter);

    assertNotNull(actualNumbers);
    assertNotSame(numbers, actualNumbers);
    assertEquals(5, actualNumbers.length);

    for (int number = 0, index = 0; number < 10; number += 2, index++) {
      assertEquals(number, actualNumbers[index].intValue());
    }
  }

  @Test
  public void findAllWithNonMatchingFilter() {
    Integer[] numbers = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    Integer[] actualNumbers = ArrayUtils.findAll(numbers, new DefaultFilter<>(false));

    assertNotNull(actualNumbers);
    assertNotSame(numbers, actualNumbers);
    assertEquals(10, numbers.length);
    assertEquals(0, actualNumbers.length);
  }

  @Test(expected = IllegalArgumentException.class)
  public void findAllWithNullArray() {
    ArrayUtils.findAll(null, new DefaultFilter<>(true));
  }

  @Test(expected = IllegalArgumentException.class)
  public void findAllWithNullFilter() {
    ArrayUtils.findAll(new Object[0], null);
  }

  @Test
  public void getFirst() {
    assertEquals("test", ArrayUtils.getFirst("test", "testing", "tested"));
  }

  @Test
  public void getFirstWithEmptyArray() {
    assertNull(ArrayUtils.getFirst());
  }

  @Test
  public void getFirstWithNullArray() {
    assertNull(ArrayUtils.getFirst((Object[]) null));
  }

  @Test
  public void insert() {
    TestUtils.assertEquals(ArrayUtils.asArray("one", "two", "three", "four"),
      ArrayUtils.insert("three", ArrayUtils.asArray("one", "two", "four"), 2));
    TestUtils.assertEquals(ArrayUtils.asArray("one", "two", "four"),
      ArrayUtils.prepend("one", ArrayUtils.asArray("two", "four")));
    TestUtils.assertEquals(ArrayUtils.asArray("one", "two", "four"),
      ArrayUtils.append("four", ArrayUtils.asArray("one", "two")));
  }

  @Test
  public void insertAtIllegalLowerBoundIndex() {
    exception.expect(ArrayIndexOutOfBoundsException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("(-1) is not a valid array index [0, 2]");

    ArrayUtils.insert("one", ArrayUtils.asArray("zero", "two"), -1);
  }

  @Test
  public void insertAtIllegalUpperBoundIndex() {
    exception.expect(ArrayIndexOutOfBoundsException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("(3) is not a valid array index [0, 2]");

    ArrayUtils.insert("three", ArrayUtils.asArray("one", "two"), 3);
  }

  @Test
  public void insertIntoNullArray() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("the array in which the element will be inserted cannot be null");

    ArrayUtils.insert("test", null, 0);
  }

  @Test
  public void insertNullElement() {
    TestUtils.assertEquals(ArrayUtils.asArray("one", "two", null, "four"),
      ArrayUtils.insert(null, ArrayUtils.asArray("one", "two", "four"), 2));
  }

  @Test
  public void isEmpty() {
    assertTrue(ArrayUtils.isEmpty(null));
    assertTrue(ArrayUtils.isEmpty(new Object[0]));
    assertTrue(ArrayUtils.isEmpty(new Object[] { }));
    assertFalse(ArrayUtils.isEmpty(new Object[10]));
    assertFalse(ArrayUtils.isEmpty(new Object[] { null }));
    assertFalse(ArrayUtils.isEmpty(new Object[] { "test" }));
    assertFalse(ArrayUtils.isEmpty(new Object[] { "test", "testing", "tested" }));
  }

  @Test
  public void iterable() {
    Object[] array = { "test", "testing", "tested" };
    Iterable<?> iterable = ArrayUtils.iterable(array);

    assertNotNull(iterable);

    int index = 0;

    for (Object element : iterable) {
      assertEquals(array[index++], element);
    }

    assertEquals(array.length, index);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void iterableWithNoElementArray() {
    Iterable<?> iterable = ArrayUtils.iterable();

    assertNotNull(iterable);
    assertFalse(iterable.iterator().hasNext());
  }

  @Test(expected = IllegalArgumentException.class)
  public void iterableWithNullArray() {
    try {
      ArrayUtils.iterable((Object[]) null);
    }
    catch (IllegalArgumentException expected) {
      assertEquals("The array of elements cannot be null!", expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void iterableWithSingleElementArray() {
    Iterable<String> iterable = ArrayUtils.iterable("test");

    assertNotNull(iterable);
    assertTrue(iterable.iterator().hasNext());
    assertEquals("test", iterable.iterator().next());
  }

  @Test
  public void iterator() {
    Object[] array = { "test", "testing", "tested" };
    Iterator<?> arrayIterator = ArrayUtils.iterator(array);

    assertNotNull(arrayIterator);

    int index = 0;

    while (arrayIterator.hasNext()) {
      assertEquals(array[index++], arrayIterator.next());
    }

    assertEquals(array.length, index);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void iteratorModification() {
    Object[] array = { "test", "testing", "tested" };
    Iterator<?> arrayIterator = ArrayUtils.iterator(array);

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

  @Test(expected = NoSuchElementException.class)
  public void iteratorWithExhaustedArray() {
    Iterator<?> iterator = ArrayUtils.iterator("test");

    assertNotNull(iterator);
    assertTrue(iterator.hasNext());
    assertEquals("test", iterator.next());
    assertFalse(iterator.hasNext());

    iterator.next();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void iteratorWithNoElementArray() {
    Iterator<?> noElementsIterator = ArrayUtils.iterator();

    assertNotNull(noElementsIterator);
    assertFalse(noElementsIterator.hasNext());
  }

  @Test(expected = IllegalArgumentException.class)
  public void iteratorWithNullArray() {
    ArrayUtils.iterator((Object[]) null);
  }

  @Test
  public void iteratorWithSingleElementArray() {
    Iterator<?> singleElementIterator = ArrayUtils.iterator("test");

    assertNotNull(singleElementIterator);
    assertTrue(singleElementIterator.hasNext());
    assertEquals("test", singleElementIterator.next());
    assertFalse(singleElementIterator.hasNext());
  }

  @Test
  public void length() {
    assertEquals(0, ArrayUtils.length(null));
    assertEquals(0, ArrayUtils.length(new Object[0]));
    assertEquals(0, ArrayUtils.length(new Object[] { }));
    assertEquals(10, ArrayUtils.length(new Object[10]));
    assertEquals(1, ArrayUtils.length(new Object[] { null }));
    assertEquals(1, ArrayUtils.length(new Object[] { "test" }));
    assertEquals(3, ArrayUtils.length(new Object[] { "test", "testing", "tested" }));
  }

  @Test
  public void prepend() {
    String[] array = ArrayUtils.prepend("tested", new String[0]);
    
    assertNotNull(array);
    assertEquals(1, array.length);
    assertEquals("tested", array[0]);

    array = ArrayUtils.prepend("testing", array);

    assertNotNull(array);
    assertEquals(2, array.length);
    assertEquals("testing", array[0]);
    assertEquals("tested", array[1]);

    array = ArrayUtils.prepend("test", array);

    assertNotNull(array);
    assertEquals(3, array.length);
    assertEquals("test", array[0]);
    assertEquals("testing", array[1]);
    assertEquals("tested", array[2]);
  }

  @Test
  public void shuffle() {
    Integer[] array = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    Integer[] shuffledArray = ArrayUtils.shuffle(copy(array));

    assertNotNull(shuffledArray);
    assertNotSame(array, shuffledArray);
    assertShuffled(array, shuffledArray);

    Integer[] shuffledArrayAgain = ArrayUtils.shuffle(copy(shuffledArray));

    assertNotNull(shuffledArrayAgain);
    assertNotSame(shuffledArray, shuffledArrayAgain);
    assertShuffled(shuffledArray, shuffledArrayAgain);
  }

  @Test
  public void shuffleEmptyArray() {
    Object[] emptyArray = { };
    Object[] shuffledEmptyArray = ArrayUtils.shuffle(emptyArray);

    assertSame(emptyArray, shuffledEmptyArray);
    assertEquals(0, shuffledEmptyArray.length);
  }

  @Test(expected = IllegalArgumentException.class)
  public void shuffleNullArray() {
    ArrayUtils.shuffle(null);
  }

  @Test
  public void shuffleSingleElementArray() {
    Object[] singleElementArray = { "test" };
    Object[] shuffledSingleElementArray = ArrayUtils.shuffle(singleElementArray);

    assertSame(singleElementArray, shuffledSingleElementArray);
    assertEquals("test", shuffledSingleElementArray[0]);
  }

  @Test
  public void subArray() {
    Integer[] result = ArrayUtils.subArray(new Integer[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, 1, 2, 4, 8);

    assertNotNull(result);
    assertEquals(4, result.length);
    assertTrue(Arrays.asList(result).containsAll(Arrays.asList(1, 2, 4, 8)));
  }

  @Test
  public void subArrayWithEmptyArray() {
    Object[] result = ArrayUtils.subArray(new Object[0]);

    assertNotNull(result);
    assertEquals(0, result.length);
  }

  @Test(expected = ArrayIndexOutOfBoundsException.class)
  public void subArrayWithEmptyArrayAndIndices() {
    ArrayUtils.subArray(new Object[0], 1, 2, 4);
  }

  @Test
  public void subArrayWithNoIndices() {
    String[] result = ArrayUtils.subArray(new String[] { "test", "testing", "tested" });

    assertNotNull(result);
    assertEquals(0, result.length);
  }

  @Test(expected = NullPointerException.class)
  public void subArrayWithNullArrayAndIndices() {
    ArrayUtils.subArray(null, 1, 2, 3);
  }

  @Test
  public void transform() {
    String[] array = { "test", "testing", "tested" };

    Transformer<String> transformer = String::toUpperCase;

    String[] actualArray = ArrayUtils.transform(array, transformer);

    assertSame(array, actualArray);
    assertEquals("TEST", array[0]);
    assertEquals("TESTING", array[1]);
    assertEquals("TESTED", array[2]);
  }

  @Test
  public void transformEmptyArray() {
    Object[] array = {};

    assertEquals(0, array.length);
    assertSame(array, ArrayUtils.transform(array, (value) ->  null));
    assertEquals(0, array.length);
  }

  @Test(expected = IllegalArgumentException.class)
  public void transformNullArray() {
    ArrayUtils.transform(null, (value) -> null);
  }

  @Test(expected = IllegalArgumentException.class)
  public void transformWithNullTransformer() {
    ArrayUtils.transform(new Object[0], null);
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
