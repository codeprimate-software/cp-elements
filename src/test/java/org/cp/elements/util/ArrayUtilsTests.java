/*
 * Copyright 2011-Present Author or Authors.
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
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatNullPointerException;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.function.Function;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.FilteringTransformer;
import org.cp.elements.lang.NumberUtils;
import org.cp.elements.lang.ThrowableAssertions;
import org.cp.elements.lang.Transformer;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

/**
 * Unit Tests for {@link ArrayUtils}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.util.ArrayUtils
 * @since 1.0.0
 */
class ArrayUtilsTests {

  private static final Object[] NULL_OBJECT_ARRAY = null;

  @SafeVarargs
  private <T> void assertElements(T[] array, T... elements) {

    assertThat(array).isNotNull();
    assertThat(array.length).isEqualTo(elements.length);

    int index = 0;

    for (T element : elements) {
      assertThat(array[index++]).isEqualTo(element);
    }
  }

  @SuppressWarnings("all")
  private <T> void assertShuffled(T[] source, T[] target) {

    assertThat(source != null && source.length != 0).as("'source' array cannot be null or empty").isTrue();
    assertThat(target != null && target.length != 0).as("'target' array cannot be null or empty").isTrue();
    assertThat(source.length).as("'source' and 'target' array lengths must match").isEqualTo(target.length);

    boolean shuffled = false;

    for (int index = 0; index < source.length && !shuffled; index++) {
      shuffled |= !source[index].equals(target[index]);
    }

    assertThat(shuffled).as(String.format("target array [%1$s] was not shuffled", Arrays.toString(target))).isTrue();
  }

  private <T extends Comparable<T>> void assertSorted(T[] array) {

    for (int index = 1; index < array.length; index++) {
      assertThat(array[index].compareTo(array[index - 1])).isGreaterThanOrEqualTo(0);
    }
  }

  @SuppressWarnings("unchecked")
  private <T> T[] copy(T[] array) {

    T[] arrayCopy = (T[]) Array.newInstance(array.getClass().getComponentType(), array.length);

    System.arraycopy(array, 0, arrayCopy, 0, array.length);

    return arrayCopy;
  }

  private void noSuchElementExceptionThrowingOperation(Runnable operation) {

    try {
      operation.run();
    }
    catch (NoSuchElementException expected) {

      assertThat(expected).hasMessage("No more elements");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @SafeVarargs
  private <T> T[] toArray(T... elements) {
    return elements;
  }

  @SafeVarargs
  private <T> Enumeration<T> toEnumeration(T... elements) {

    return new Enumeration<>() {

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
  private <T> Iterable<T> toIterable(T... elements) {
    return () -> toIterator(elements);
  }

  @SafeVarargs
  private <T> Iterator<T> toIterator(T... elements) {

    return new Iterator<>() {

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
  void assertNotEmptyWithArray() {

    Object[] array = { 1, 2, 3 };

    ArrayUtils.assertNotEmpty(array);
  }

  @Test
  void assertNotEmptyWithEmptyArray() {

    Object[] array = {};

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.assertNotEmpty(array))
      .withMessage("Non-empty array is required")
      .withNoCause();
  }

  @Test
  void assertNotEmptyWithNullArray() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.assertNotEmpty(null))
      .withMessage("Non-empty array is required")
      .withNoCause();
  }

  @Test
  void assertNoNullElementsWithArray() {

    Object[] array = { "test", "testing", "tested" };

    ArrayUtils.assertNoNullElements(array);
  }

  @Test
  void assertNoNullElementsWithArrayContainingNullElements() {

    Object[] array = { 1, null, 2};

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.assertNoNullElements(array))
      .withMessage("Array is required and must not contain any null elements")
      .withNoCause();
  }

  @Test
  void assertNoNullElementsWithNullArray() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.assertNoNullElements(null))
      .withMessage("Array is required and must not contain any null elements")
      .withNoCause();
  }

  @Test
  public void appendToEmptyArray() {

    String[] array = {};
    String[] newArray = ArrayUtils.append("test", array);

    assertThat(newArray).isNotNull();
    assertThat(newArray).isNotSameAs(array);
    assertElements(array);
    assertElements(newArray, "test");
  }

  @Test
  public void appendToSingleElementArray() {

    String[] array = { "test" };
    String[] newArray = ArrayUtils.append("testing", array);

    assertThat(newArray).isNotNull();
    assertThat(newArray).isNotSameAs(array);
    assertElements(array, "test");
    assertElements(newArray, "test", "testing");
  }

  @Test
  public void appendToTwoElementArray() {

    String[] array = { "test", "testing" };
    String[] newArray = ArrayUtils.append("tested", array);

    assertThat(newArray).isNotNull();
    assertThat(newArray).isNotSameAs(array);
    assertElements(array, "test", "testing");
    assertElements(newArray, "test", "testing", "tested");
  }

  @Test
  public void appendToNullArray() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.append("test", null))
      .withMessage("Array is required")
      .withNoCause();
  }

  @Test
  public void asArrayWithVarargs() {

    assertThat(ArrayUtils.asArray(true, false)).isEqualTo(new Boolean[] { true, false });
    assertThat(ArrayUtils.asArray(0, 1, 2)).isEqualTo(new Integer[] { 0, 1, 2 });
    assertThat(ArrayUtils.asArray("test", "testing", "tested")).isEqualTo(new String[] { "test", "testing", "tested" });
  }

  @Test
  public void asArrayWithNoArgs() {
    assertThat(ArrayUtils.<Integer>asArray()).isEqualTo(new Integer[0]);
  }

  @Test
  @SuppressWarnings("all")
  public void asArrayWithNull() {
    assertThat(ArrayUtils.asArray(NULL_OBJECT_ARRAY)).isNull();
  }

  @Test
  public void asArrayWithEnumeration() {

    Enumeration<Integer> enumeration = toEnumeration(0, 1, 2);

    Integer[] numbers = ArrayUtils.asArray(enumeration, Integer.class);

    assertThat(numbers).isNotNull();
    assertThat(numbers.length).isEqualTo(3);

    for (int index = 0; index < numbers.length; index++) {
      assertThat(numbers[index]).isEqualTo(index);
    }
  }

  @Test
  public void asArrayWithEmptyEnumeration() {

    Enumeration<String> enumeration = toEnumeration();

    String[] strings = ArrayUtils.asArray(enumeration, String.class);

    assertThat(strings).isNotNull();
    assertThat(strings.length).isEqualTo(0);
  }

  @Test
  public void asArrayWithNullEnumeration() {

    Object[] array = ArrayUtils.asArray((Enumeration<Object>) null, Object.class);

    assertThat(array).isNotNull();
    assertThat(array.length).isEqualTo(0);
  }

  @Test
  public void asArrayWithIterable() {

    Iterable<Integer> iterable = toIterable(0, 1, 2);

    Integer[] numbers = ArrayUtils.asArray(iterable, Integer.class);

    assertThat(numbers).isNotNull();
    assertThat(numbers.length).isEqualTo(3);

    for (int index = 0; index < numbers.length; index++) {
      assertThat(numbers[index]).isEqualTo(index);
    }
  }

  @Test
  public void asArrayWithEmptyIterable() {

    Iterable<String> iterable = toIterable();

    String[] strings = ArrayUtils.asArray(iterable, String.class);

    assertThat(strings).isNotNull();
    assertThat(strings.length).isEqualTo(0);
  }

  @Test
  public void asArrayWithNullIterable() {

    Object[] array = ArrayUtils.asArray((Iterable<Object>) null, Object.class);

    assertThat(array).isNotNull();
    assertThat(array.length).isEqualTo(0);
  }

  @Test
  public void asArrayWithIterator() {

    Iterator<Integer> iterator = toIterator(0, 1, 2);

    Integer[] numbers = ArrayUtils.asArray(iterator, Integer.class);

    assertThat(numbers).isNotNull();
    assertThat(numbers.length).isEqualTo(3);

    for (int index = 0; index < numbers.length; index++) {
      assertThat(numbers[index]).isEqualTo(index);
    }
  }

  @Test
  public void asArrayWithEmptyIterator() {

    Iterator<String> iterator = toIterator();

    String[] strings = ArrayUtils.asArray(iterator, String.class);

    assertThat(strings).isNotNull();
    assertThat(strings.length).isEqualTo(0);
  }

  @Test
  public void asArrayWithNullIterator() {

    Object[] array = ArrayUtils.asArray((Iterator<Object>) null, Object.class);

    assertThat(array).isNotNull();
    assertThat(array.length).isEqualTo(0);
  }

  @Test
  public void asEnumerationFromArray() {

    assertThatExceptionOfType(NoSuchElementException.class)
      .isThrownBy(() -> {

        Object[] array = { "test", "testing", "tested" };

        Enumeration<Object> enumeration = ArrayUtils.asEnumeration(array);

        assertThat(enumeration).isNotNull();

        for (Object element : array) {
          assertThat(enumeration.hasMoreElements()).isTrue();
          assertThat(enumeration.nextElement()).isEqualTo(element);
        }

        assertThat(enumeration.hasMoreElements()).isFalse();

        noSuchElementExceptionThrowingOperation(enumeration::nextElement);
      })
      .withNoCause();
  }

  @Test
  public void asEnumerationFromEmptyArray() {

    assertThatExceptionOfType(NoSuchElementException.class)
      .isThrownBy(() -> {

        Enumeration<Object> enumeration = ArrayUtils.asEnumeration();

        assertThat(enumeration).isNotNull();
        assertThat(enumeration.hasMoreElements()).isFalse();

        noSuchElementExceptionThrowingOperation(enumeration::nextElement);
      })
      .withNoCause();
  }

  @Test
  public void asEnumerationFromNullArray() {

    Enumeration<Object> enumeration = ArrayUtils.asEnumeration((Object[]) null);

    assertThat(enumeration).isNotNull();
    assertThat(enumeration.hasMoreElements()).isFalse();
  }

  @Test
  public void asEnumerationFromSingleElementArray() {

    Enumeration<String> enumeration = ArrayUtils.asEnumeration("test");

    assertThat(enumeration).isNotNull();
    assertThat(enumeration.hasMoreElements()).isTrue();
    assertThat(enumeration.nextElement()).isEqualTo("test");
    assertThat(enumeration.hasMoreElements()).isFalse();
  }

  @Test
  public void asIterableFromArray() {

    Object[] array = { "test", "testing", "tested" };

    Iterable<Object> arrayIterable = ArrayUtils.asIterable(array);

    assertThat(arrayIterable).isNotNull();

    int index = 0;

    for (Object element : arrayIterable) {
      assertThat(element).isEqualTo(array[index++]);
    }

    assertThat(index).isEqualTo(array.length);
  }

  @Test
  public void asIterableFromEmptyArray() {

    Iterable<?> iterable = ArrayUtils.asIterable();

    assertThat(iterable).isNotNull();
    assertThat(iterable.iterator().hasNext()).isFalse();
  }

  @Test
  public void asIterableFromNullArray() {

    Iterable<?> iterable = ArrayUtils.asIterable((Object[]) null);

    assertThat(iterable).isNotNull();
    assertThat(iterable.iterator().hasNext()).isFalse();
  }

  @Test
  public void asIterableFromSingleElementArray() {

    Iterable<String> iterable = ArrayUtils.asIterable("test");

    assertThat(iterable).isNotNull();
    assertThat(iterable.iterator().hasNext()).isTrue();
    assertThat(iterable.iterator().next()).isEqualTo("test");
  }

  @Test
  public void asIteratorFromArray() {

    assertThatExceptionOfType(NoSuchElementException.class)
      .isThrownBy(() -> {

        Object[] array = { "test", "testing", "tested" };

        Iterator<Object> arrayIterator = ArrayUtils.asIterator(array);

        assertThat(arrayIterator).isNotNull();
        assertThat(arrayIterator.hasNext()).isTrue();

        for (Object element : array) {
          assertThat(arrayIterator.hasNext()).isTrue();
          assertThat(arrayIterator.next()).isEqualTo(element);
        }

        assertThat(arrayIterator.hasNext()).isFalse();

        noSuchElementExceptionThrowingOperation(arrayIterator::next);
      })
      .withNoCause();
  }

  @Test
  public void asIteratorFromEmptyArray() {

    assertThatExceptionOfType(NoSuchElementException.class)
      .isThrownBy(() -> {

        Iterator<?> arrayIterator = ArrayUtils.asIterator();

        assertThat(arrayIterator).isNotNull();
        assertThat(arrayIterator.hasNext()).isFalse();

        noSuchElementExceptionThrowingOperation(arrayIterator::next);
      })
      .withNoCause();
  }

  @Test
  public void asIteratorFromNullArray() {

    Iterator<?> arrayIterator = ArrayUtils.asIterator((Object[]) null);

    assertThat(arrayIterator).isNotNull();
    assertThat(arrayIterator.hasNext()).isFalse();
  }

  @Test
  public void asIteratorFromSingleElementArray() {

    Iterator<String> arrayIterator = ArrayUtils.asIterator("test");

    assertThat(arrayIterator).isNotNull();
    assertThat(arrayIterator.hasNext()).isTrue();
    assertThat(arrayIterator.next()).isEqualTo("test");
    assertThat(arrayIterator.hasNext()).isFalse();
  }

  @Test
  public void countReturnsArrayLength() {

    Object[] array = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

    assertThat(ArrayUtils.count(array)).isEqualTo(array.length);
  }

  @Test
  public void countReturnsEmptyArrayLength() {
    assertThat(ArrayUtils.count(new Object[0])).isEqualTo(0);
  }

  @Test
  public void countReturnsInitializedArrayLength() {
    assertThat(ArrayUtils.count(new Object[10])).isEqualTo(10);
  }

  @Test
  public void countReturnsZeroForNullArray() {
    assertThat(ArrayUtils.count(null)).isEqualTo(0);
  }

  @Test
  public void countWithFilter() {

    Integer[] array = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };

    assertThat(ArrayUtils.count(array, (Filter<Integer>) NumberUtils::isEven)).isEqualTo(4L);
    assertThat(ArrayUtils.count(array, (Filter<Integer>) NumberUtils::isOdd)).isEqualTo(5L);
    assertThat(ArrayUtils.count(array, (Filter<Integer>) NumberUtils::isNegative)).isEqualTo(0L);
    assertThat(ArrayUtils.count(array, (Filter<Integer>) NumberUtils::isPositive)).isEqualTo(array.length);
  }

  @Test
  public void countWithFilterReturnsZero() {
    assertThat(ArrayUtils.count(new Object[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, (number) -> false)).isEqualTo(0L);
  }

  @Test
  public void countWithFilterAndNullArrayReturnsZero() {
    assertThat(ArrayUtils.count(null, (number) -> true)).isEqualTo(0L);
  }

  @Test
  public void countWithNullPredicate() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.count(new Object[0], null))
      .withMessage("Predicate is required")
      .withNoCause();
  }

  @Test
  public void deepCopyWithArray() {

    Object[] array = { "test", "testing", "tested" };
    Object[] arrayCopy = ArrayUtils.deepCopy(array);

    assertThat(arrayCopy).isNotNull();
    assertThat(arrayCopy).isNotSameAs(array);
    assertThat(arrayCopy.length).isEqualTo(array.length);

    for (int index = 0, size = arrayCopy.length; index < size; index++) {
      assertThat(arrayCopy[index]).isEqualTo(array[index]);
      assertThat(arrayCopy[index]).isNotSameAs(array[index]);
    }
  }

  @Test
  public void deepCopyWithArrayContainingNonCloneableElementsThrowsUnsupportedOperationException() {

    Object[] array = { "test", Person.newPerson("Jon", "Doe") };

    ThrowableAssertions.assertThatUnsupportedOperationException()
      .isThrownBy(args -> ArrayUtils.deepCopy(array))
      .havingMessageContaining("[clone] is not supported for object of type [Person]")
      .causedBy(CloneNotSupportedException.class)
      .withNoCause();
  }

  @Test
  public void deepCopyWithArrayHandlesNulls() {

    Object[] array = { null, "one", null, "three", null };
    Object[] arrayCopy = ArrayUtils.deepCopy(array);

    assertThat(arrayCopy).isNotNull();
    assertThat(arrayCopy.length).isEqualTo(array.length);

    for (int index = 0, size = arrayCopy.length; index < size; index++) {
      assertThat(arrayCopy[index]).isEqualTo(array[index]);
    }
  }

  @Test
  public void deepCopyWithNullArrayThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.deepCopy(null))
      .withMessage("Array is required")
      .withNoCause();
  }

  @Test
  public void deepCopyWithArrayAndCopyFunction() {

    Person[] people = { Person.newPerson("Jon", "Doe"), Person.newPerson("Jane", "Doe") };

    Object[] peopleCopy = ArrayUtils.deepCopy(people,
      person -> Person.newPerson(person.getFirstName(), person.getLastName()));

    assertThat(peopleCopy).isNotNull();
    assertThat(peopleCopy).isNotSameAs(people);
    assertThat(peopleCopy.length).isEqualTo(people.length);

    for (int index = 0, size = people.length; index < size; index++) {
      assertThat(peopleCopy[index]).isEqualTo(people[index]);
      assertThat(peopleCopy[index]).isNotSameAs(people[index]);
    }
  }

  @Test
  public void deepCopyWithNullArrayAndCopyFunctionThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.deepCopy(null, Function.identity()))
      .withMessage("Array is required")
      .withNoCause();
  }

  @Test
  public void deepCopyWithArrayAndNullCopyFunctionThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.deepCopy(ArrayUtils.emptyArray(), null))
      .withMessage("Copy Function is required")
      .withNoCause();
  }

  @Test
  public void defaultIfEmptyWithNonNullNonEmptyArrayReturnsArray() {

    Object[] array = { "test" };
    Object[] defaultArray = { "tested" };

    assertThat(ArrayUtils.defaultIfEmpty(array, defaultArray)).isSameAs(array);
  }

  @Test
  public void defaultIfEmptyArrayWithEmptyArrayReturnsDefault() {

    Object[] defaultArray = { "test" };

    assertThat(ArrayUtils.defaultIfEmpty(new Object[0], defaultArray)).isSameAs(defaultArray);
  }

  @Test
  public void defaultIfEmptyArrayWithNullArrayReturnsDefault() {

    Object[] defaultArray = { "test" };

    assertThat(ArrayUtils.defaultIfEmpty(null, defaultArray)).isSameAs(defaultArray);
  }

  @Test
  public void defaultIfEmptyWithNullArrayAndNullDefaultArrayReturnsNull() {
    assertThat(ArrayUtils.<Object[]>defaultIfEmpty(null, null)).isNull();
  }

  @Test
  public void emptyArrayIsClonedProperly() {

    Object[] emptyArray = ArrayUtils.emptyArray();

    assertThat(emptyArray).isNotNull();
    assertThat(emptyArray.length).isEqualTo(0);

    Object[] anotherEmptyArray = ArrayUtils.emptyArray();

    assertThat(anotherEmptyArray).isNotNull();
    assertThat(anotherEmptyArray.length).isEqualTo(0);
    assertThat(anotherEmptyArray).isNotSameAs(emptyArray);
  }

  @Test
  public void filter() {

    Filter<Integer> evenNumberFilter = NumberUtils::isEven;
    Filter<Integer> oddNumberFilter = NumberUtils::isOdd;

    Integer[] numbers = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    Integer[] evenNumbers = ArrayUtils.filter(numbers, evenNumberFilter);
    Integer[] oddNumbers = ArrayUtils.filter(numbers, oddNumberFilter);

    assertThat(evenNumbers).isNotNull();
    assertThat(evenNumbers.length).isEqualTo(4);
    assertThat(evenNumbers).isNotSameAs(numbers);
    assertElements(evenNumbers, 2, 4, 6, 8);
    assertThat(oddNumbers).isNotNull();
    assertThat(oddNumbers.length).isEqualTo(5);
    assertThat(oddNumbers).isNotSameAs(numbers);
    assertThat(oddNumbers).isNotSameAs(evenNumbers);
    assertElements(oddNumbers, 1, 3, 5, 7, 9);
  }

  @Test
  public void filterAllAndNothing() {

    Filter<Integer> negativeNumberFilter = NumberUtils::isNegative;
    Filter<Integer> positiveNumberFilter = NumberUtils::isPositive;

    Integer[] numbers = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    Integer[] negativeNumbers = ArrayUtils.filter(numbers, negativeNumberFilter);
    Integer[] positiveNumbers = ArrayUtils.filter(numbers, positiveNumberFilter);

    assertThat(negativeNumbers).isNotNull();
    assertThat(negativeNumbers.length).isEqualTo(0);
    assertThat(negativeNumbers).isNotSameAs(numbers);
    assertElements(negativeNumbers);
    assertThat(positiveNumbers).isNotNull();
    assertThat(positiveNumbers.length).isEqualTo(9);
    assertThat(positiveNumbers).isNotSameAs(numbers);
    assertThat(positiveNumbers).isNotSameAs(negativeNumbers);
    assertElements(positiveNumbers, 1, 2, 3, 4, 5, 6, 7, 8, 9);
  }

  @Test
  public void filterEmptyArray() {

    String[] strings = {};
    String[] actualStrings = ArrayUtils.filter(strings, (element) -> true);

    assertThat(actualStrings).isNotNull();
    assertThat(actualStrings).isNotSameAs(strings);
    assertThat(actualStrings.length).isEqualTo(strings.length);
  }

  @Test
  public void filterNullArray() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.filter(null, (element) -> true))
      .withMessage("Array is required")
      .withNoCause();
  }

  @Test
  public void filterWithNullPredicate() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.filter(new Object[0], null))
      .withMessage("Predicate is required")
      .withNoCause();
  }

  @Test
  public void filterAndTransform() {

    FilteringTransformer<String> filteringTransformer = new FilteringTransformer<>() {

      @Override
      public boolean accept(String value) {
        return !(value == null || value.trim().isEmpty());
      }

      @Override
      public String transform(String value) {
        return value.toUpperCase();
      }
    };

    String[] array = { null, "test", "", "testing", "  ", "tested" };
    String[] actualArray = ArrayUtils.filterAndTransform(array, filteringTransformer);

    assertThat(actualArray).isNotNull();
    assertThat(actualArray).isNotSameAs(array);
    assertThat(actualArray.length).isEqualTo(3);
    assertElements(actualArray, "TEST", "TESTING", "TESTED");
    assertElements(array, null, "test", "", "testing", "  ", "tested");
  }

  @Test
  public void filterArrayAndTransformEmptyArray() {

    FilteringTransformer<String> filteringTransformer = new FilteringTransformer<>() {

      @Override
      public boolean accept(String value) {
        return false;
      }

      @Override
      public String transform(String value) {
        return value.toUpperCase();
      }
    };

    String[] array = { "test", "testing", "tested" };
    String[] actualArray = ArrayUtils.filterAndTransform(array, filteringTransformer);

    assertThat(actualArray).isNotNull();
    assertThat(actualArray).isNotSameAs(array);
    assertThat(actualArray.length).isEqualTo(0);
    assertElements(array, "test", "testing", "tested");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void filterAndTransformEmptyArray() {

    FilteringTransformer<String> mockFilteringTransformer = mock(FilteringTransformer.class);

    String[] array = {};
    String[] actualArray = ArrayUtils.filterAndTransform(array, mockFilteringTransformer);

    assertThat(actualArray).isNotNull();
    assertThat(actualArray).isNotSameAs(array);
    assertThat(actualArray.length).isEqualTo(0);

    verifyNoInteractions(mockFilteringTransformer);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void filterAndTransformNullArray() {

    FilteringTransformer<Object> mockFilteringTransformer = mock(FilteringTransformer.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.filterAndTransform(null, mockFilteringTransformer))
      .withMessage("Array is required")
      .withNoCause();

    verifyNoInteractions(mockFilteringTransformer);
  }

  @Test
  public void filterAndTransformWithNullFilteringTransformer() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.filterAndTransform(new Object[0], null))
      .withMessage("Predicate is required")
      .withNoCause();
  }

  @Test
  public void find() {

    Person cookieDoe = Person.newPerson("Cookie", "Doe");
    Person froDoe = Person.newPerson("Fro", "Doe");
    Person janeDoe = Person.newPerson("Jane", "Doe");
    Person joeDoe = Person.newPerson("Joe", "Doe");
    Person jonDoe = Person.newPerson("Jon", "Doe");
    Person pieDoe = Person.newPerson("Pie", "Doe");
    Person playDoe = Person.newPerson("Play", "Doe");
    Person jackHandy = Person.newPerson("Jack", "Handy");
    Person sandyHandy = Person.newPerson("Sandy", "Handy");

    Person[] people = { jackHandy, jonDoe, joeDoe, janeDoe, sandyHandy, pieDoe, froDoe, cookieDoe, playDoe };

    Filter<Person> doeFilter = (person) -> "Doe".equalsIgnoreCase(person.getLastName());

    Person actualPerson = ArrayUtils.findOne(people, doeFilter);

    assertThat(actualPerson).isEqualTo(jonDoe);
  }

  @Test
  public void findWithNonMatchingFilter() {

    Person cookieDoe = Person.newPerson("Cookie", "Doe");
    Person froDoe = Person.newPerson("Fro", "Doe");
    Person janeDoe = Person.newPerson("Jane", "Doe");
    Person joeDoe = Person.newPerson("Joe", "Doe");
    Person jonDoe = Person.newPerson("Jon", "Doe");
    Person pieDoe = Person.newPerson("Pie", "Doe");
    Person playDoe = Person.newPerson("Play", "Doe");
    Person jackHandy = Person.newPerson("Jack", "Handy");
    Person sandyHandy = Person.newPerson("Sandy", "Handy");

    Person[] people = { jackHandy, jonDoe, joeDoe, janeDoe, sandyHandy, pieDoe, froDoe, cookieDoe, playDoe };

    Filter<Person> doeFilter = (person) -> ("Hoe".equalsIgnoreCase(person.getFirstName())
      && "Doe".equalsIgnoreCase(person.getLastName()));

    Person actualPerson = ArrayUtils.findOne(people, doeFilter);

    assertThat(actualPerson).isNull();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void findWithEmptyArray() {

    Filter<Object> mockFilter = mock(Filter.class);

    assertThat(ArrayUtils.findOne(toArray(), mockFilter)).isNull();

    verifyNoInteractions(mockFilter);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void findWithNullArray() {

    Filter<Object> mockFilter = mock(Filter.class);

    assertThat(ArrayUtils.findOne(null, mockFilter)).isNull();

    verifyNoInteractions(mockFilter);
  }

  @Test
  public void findWithNullPredicate() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.findOne(toArray(), null))
      .withMessage("Predicate is required")
      .withNoCause();
  }

  @Test
  public void findAll() {

    Integer[] numbers = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

    Filter<Integer> evenNumberFilter = NumberUtils::isEven;

    List<Integer> actualNumbers = ArrayUtils.findAll(numbers, evenNumberFilter);

    assertThat(actualNumbers).isNotNull();
    assertThat(actualNumbers.size()).isEqualTo(5);
    assertElements(actualNumbers.toArray(), 0, 2, 4, 6, 8);
  }

  @Test
  public void findAllFromEmptyArray() {

    List<?> matches = ArrayUtils.findAll(toArray(), (element) -> true);

    assertThat(matches).isNotNull();
    assertThat(matches.isEmpty()).isTrue();
  }

  @Test
  public void findAllFromNullArray() {

    List<?> matches = ArrayUtils.findAll(null, (element) -> true);

    assertThat(matches).isNotNull();
    assertThat(matches.isEmpty()).isTrue();
  }

  @Test
  public void findAllWithNonMatchingFilter() {

    Integer[] numbers = { 0, 2, 4, 8, 16, 32, 64 };

    Filter<Integer> oddNumberFilter = NumberUtils::isOdd;

    List<Integer> actualNumbers = ArrayUtils.findAll(numbers, oddNumberFilter);

    assertThat(actualNumbers).isNotNull();
    assertThat(actualNumbers.isEmpty()).isTrue();
  }

  @Test
  public void findAllWithNullPredicate() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.findAll(toArray(), null))
      .withMessage("Predicate is required")
      .withNoCause();
  }

  @Test
  public void getElementAtValidIndexInArray() {

    Object[] array = { "test", "testing", "tested" };

    assertThat(ArrayUtils.getElementAt(array, 1)).isEqualTo("testing");
    assertThat(ArrayUtils.getElementAt(array, 0)).isEqualTo("test");
    assertThat(ArrayUtils.getElementAt(array, 2)).isEqualTo("tested");
  }

  @Test
  public void getElementAtInvalidIndexInArray() {
    assertThat(ArrayUtils.getElementAt(new Object[] { "test" }, 1)).isNull();
  }

  @Test
  public void getElementAtInvalidIndexFromEmptyArray() {
    assertThat(ArrayUtils.getElementAt(new Object[0], 0)).isNull();
  }

  @Test
  public void getElementAtInvalidIndexFromNullArray() {
    assertThat(ArrayUtils.<Object>getElementAt(null, 0)).isNull();
  }

  @Test
  public void getElementAtValidIndexInArayWithDefaultValueReturnsElementAtIndex() {

    Object[] array = { "test", "testing", "tested" };

    assertThat(ArrayUtils.getElementAt(array, 1, "default")).isEqualTo("testing");
    assertThat(ArrayUtils.getElementAt(array, 0, "default")).isEqualTo("test");
    assertThat(ArrayUtils.getElementAt(array, 2, "default")).isEqualTo("tested");
  }

  @Test
  public void getElementAtInvalidIndexInArrayWithDefaultVAlueReturnsDefaultValue() {
    assertThat(ArrayUtils.getElementAt(new Object[] { "test" }, 1, "default")).isEqualTo("default");
  }

  @Test
  public void getElementAtInvalidIndexFromEmptyArrayWithDefaultValueReturnsDefaultValue() {
    assertThat(ArrayUtils.getElementAt(new Object[0], 0, "default")).isEqualTo("default");
  }

  @Test
  public void getElementAtInvalidIndexFromNullArrayWithDefaultValueReturnsDefaultValue() {
    assertThat(ArrayUtils.getElementAt(null, 0, "default")).isEqualTo("default");
  }

  @Test
  public void getFirstFromArray() {
    assertThat(ArrayUtils.getFirstElement("test", "testing", "tested")).isEqualTo("test");
  }

  @Test
  public void getFirstFromEmptyArray() {
    assertThat(ArrayUtils.<Object>getFirstElement()).isNull();
  }

  @Test
  @SuppressWarnings("all")
  public void getFirstFromNullArray() {
    assertThat(ArrayUtils.getFirstElement((Object) NULL_OBJECT_ARRAY)).isNull();
  }

  @Test
  public void getFirstFromSingleElementArray() {
    assertThat(ArrayUtils.getFirstElement("test")).isEqualTo("test");
  }

  @Test
  public void getFirstFromNonNullNonEmptyArrayWithDefaultValueReturnsFirstElement() {
    assertThat(ArrayUtils.getFirstElement(new Object[] { "test", "testing", "tested" }, "default")).isEqualTo("test");
  }

  @Test
  public void getFirstFromEmptyArrayWithDefaultValueReturnsDefaultValue() {
    assertThat(ArrayUtils.getFirstElement(new Object[0], "default")).isEqualTo("default");
  }

  @Test
  public void getFirstFromNullArrayWithDefaultValueReturnsDefaultValue() {
    assertThat(ArrayUtils.getFirstElement(null, "default")).isEqualTo("default");
  }

  @Test
  public void getFirstFromNullArrayWithNullDefaultValueReturnsNull() {
    assertThat(ArrayUtils.<Object>getFirstElement(null, null)).isNull();
  }

  @Test
  public void getLastFromArray() {
    assertThat(ArrayUtils.getLastElement("test", "testing", "tested")).isEqualTo("tested");
  }

  @Test
  public void getLastFromEmptyArray() {
    assertThat(ArrayUtils.<Object>getLastElement()).isNull();
  }

  @Test
  @SuppressWarnings("all")
  public void getLastFromNullArray() {
    assertThat(ArrayUtils.getLastElement((Object) NULL_OBJECT_ARRAY)).isNull();
  }

  @Test
  public void getLastFromSingleElementArray() {
    assertThat(ArrayUtils.getLastElement("test")).isEqualTo("test");
  }

  @Test
  public void getLastFromNonNullNonEmptyArrayWithDefaultValueReturnsLastElement() {
    assertThat(ArrayUtils.getLastElement(new Object[] { "test", "testing", "tested" }, "default")).isEqualTo("tested");
  }

  @Test
  public void getLastFromEmptyArrayWithDefaultValueReturnsDefaultValue() {
    assertThat(ArrayUtils.getLastElement(new Object[0], "default")).isEqualTo("default");
  }

  @Test
  public void getLastFromNullArrayWithDefaultValueReturnsDefaultValue() {
    assertThat(ArrayUtils.getLastElement(null, "default")).isEqualTo("default");
  }

  @Test
  public void getLastFromNullArrayWithNullDefaultValueReturnsNull() {
    assertThat(ArrayUtils.<Object>getLastElement(null, null)).isNull();
  }

  @Test
  public void indexOfElementInArrayReturnsIndex() {

    Object[] array = { "test", "testing", "tested"};

    assertThat(ArrayUtils.indexOf(array, "test")).isEqualTo(0);
    assertThat(ArrayUtils.indexOf(array, "testing")).isEqualTo(1);
    assertThat(ArrayUtils.indexOf(array, "tested")).isEqualTo(2);
  }

  @Test
  public void indexOfNonExistingElementInArrayReturnsMinusOne() {
    assertThat(ArrayUtils.indexOf(new Object[] { "test", "testing", "tested" }, "mock")).isEqualTo(-1);
  }

  @Test
  public void indexOfNonNullElementInNullArrayReturnsMinusOne() {
    assertThat(ArrayUtils.indexOf(null, "test")).isEqualTo(-1);
  }

  @Test
  public void indexOfNullElementInArrayReturnsMinusOne() {
    assertThat(ArrayUtils.indexOf(new Object[] { "test", "testing", "tested" }, null)).isEqualTo(-1);
  }

  @Test
  public void indexOfRepeatingElementInArrayReturnsFirstIndexFound() {
    assertThat(ArrayUtils.indexOf(new Object[] { "test", "testing", "testing" }, "testing")).isEqualTo(1);
  }

  @Test
  public void indexOfRepeatingNullElementInArrayReturnsFirstIndexFound() {
    assertThat(ArrayUtils.indexOf(new Object[] { "test", null, null, "testing", "testing", null }, null)).isEqualTo(1);
  }

  @Test
  public void insertIntoArray() {

    assertElements(ArrayUtils.insert("one", toArray("two", "three"), 0), "one", "two", "three");
    assertElements(ArrayUtils.insert("two", toArray("one", "three"), 1), "one", "two", "three");
    assertElements(ArrayUtils.insert("three", toArray("one", "two"), 2), "one", "two", "three");
  }

  @Test
  public void insertIntoEmptyArray() {
    assertElements(ArrayUtils.insert("one", toArray(), 0), "one");
  }

  @Test
  public void insertIntoNullArray() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.insert("test", null, 0))
      .withMessage("Array is required")
      .withNoCause();
  }

  @Test
  public void insertIntoSingleElementArray() {

    assertElements(ArrayUtils.insert("one", toArray("two"), 0), "one", "two");
    assertElements(ArrayUtils.insert("two", toArray("one"), 1), "one", "two");
  }

  @Test
  public void insertNullElementIntoArray() {

    assertElements(ArrayUtils.insert(null, toArray("one", "two"), 0), null, "one", "two");
    assertElements(ArrayUtils.insert(null, toArray("one", "two"), 1), "one", null, "two");
    assertElements(ArrayUtils.insert(null, toArray("one", "two"), 2), "one", "two", null);
  }

  @Test
  public void insertIntoArrayWithNegativeArrayIndex() {

    assertThatExceptionOfType(ArrayIndexOutOfBoundsException.class)
      .isThrownBy(() -> ArrayUtils.insert("one", toArray("zero", "two"), -1))
      .withMessage("[-1] is not a valid index [0, 2] in the array")
      .withNoCause();
  }

  @Test
  public void insertIntoArrayWithOverflowArrayIndex() {

    assertThatExceptionOfType(ArrayIndexOutOfBoundsException.class)
      .isThrownBy(() -> ArrayUtils.insert("three", toArray("one", "two"), 3))
      .withMessage("[3] is not a valid index [0, 2] in the array")
      .withNoCause();
  }

  @Test
  public void isEmptyWithEmptyArray() {

    assertThat(ArrayUtils.isEmpty(null)).isTrue();
    assertThat(ArrayUtils.isEmpty(new Object[0])).isTrue();
    assertThat(ArrayUtils.isEmpty(new Object[] {})).isTrue();
  }

  @Test
  public void isEmptyWithNonEmptyArray() {

    assertThat(ArrayUtils.isEmpty(new Object[10])).isFalse();
    assertThat(ArrayUtils.isEmpty(new Object[] { null })).isFalse();
    assertThat(ArrayUtils.isEmpty(new Object[] { "test" })).isFalse();
    assertThat(ArrayUtils.isEmpty(new Object[] { "test", "testing", "tested" })).isFalse();
  }

  @Test
  public void isNotEmptyWithEmptyArray() {

    assertThat(ArrayUtils.isNotEmpty(null)).isFalse();
    assertThat(ArrayUtils.isNotEmpty(new Object[0])).isFalse();
    assertThat(ArrayUtils.isNotEmpty(new Object[] {})).isFalse();
  }

  @Test
  public void isNotEmptyWithNonEmptyArray() {

    assertThat(ArrayUtils.isNotEmpty(new Object[1])).isTrue();
    assertThat(ArrayUtils.isNotEmpty(new Object[] { null })).isTrue();
    assertThat(ArrayUtils.isNotEmpty(new Object[] { "test" })).isTrue();
    assertThat(ArrayUtils.isNotEmpty(new Object[] { "test", "testing", "tested" })).isTrue();
  }

  @Test
  public void isSizeWithEmptyArray() {

    assertThat(ArrayUtils.isSize(ArrayUtils.EMPTY_ARRAY, 0)).isTrue();
    assertThat(ArrayUtils.isSize(ArrayUtils.EMPTY_ARRAY, 1)).isFalse();
    assertThat(ArrayUtils.isSize(ArrayUtils.EMPTY_ARRAY, 2)).isFalse();
  }

  @Test
  public void isSizeWithNullArray() {

    assertThat(ArrayUtils.isSize(null, 0)).isTrue();
    assertThat(ArrayUtils.isSize(null, 1)).isFalse();
    assertThat(ArrayUtils.isSize(null, 2)).isFalse();
  }

  @Test
  public void isSizeWithArrayAndMatchingSize() {

    assertThat(ArrayUtils.isSize(new Object[] { null }, 1)).isTrue();
    assertThat(ArrayUtils.isSize(new Object[] { null, null }, 2)).isTrue();
    assertThat(ArrayUtils.isSize(new Object[] { "mock", "test" }, 2)).isTrue();
    assertThat(ArrayUtils.isSize(new Object[] { "test", "test" }, 2)).isTrue();
    assertThat(ArrayUtils.isSize(new Object[] { 0, 1, 2 }, 3)).isTrue();
  }

  @Test
  public void isSizeWithArrayAndNonMatchingSize() {

    assertThat(ArrayUtils.isSize(new Object[] { "test", "test" }, 1)).isFalse();
    assertThat(ArrayUtils.isSize(new Object[] { 0, 1, 2 }, 2)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void isSizeOneWithNullArrayIsFalse() {
    assertThat(ArrayUtils.isSizeOne((Object[]) null)).isFalse();
  }

  @Test
  public void isSizeOneWithEmptyArrayIsFalse() {
    assertThat(ArrayUtils.isSizeOne()).isFalse();
  }

  @Test
  public void isSizeOneWithArrayHavingOneElementIsTrue() {
    assertThat(ArrayUtils.isSizeOne(1)).isTrue();
  }

  @Test
  public void isSizeOneWithArrayHavingTwoElementsIsFalse() {
    assertThat(ArrayUtils.isSizeOne(1, 2)).isFalse();
  }

  @Test
  public void attemptRemovalFromArrayIterator() {

    String[] array = { "one", "two" };

    Iterator<String> arrayIterator = ArrayUtils.asIterator(array);

    assertThat(arrayIterator).isNotNull();
    assertThat(arrayIterator.hasNext()).isTrue();
    assertThat(arrayIterator.next()).isEqualTo("one");
    assertThat(arrayIterator.hasNext()).isTrue();
    assertElements(array, "one", "two");

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(arrayIterator::remove)
      .withNoCause();

    assertElements(array, "one", "two");
    assertThat(arrayIterator.hasNext()).isTrue();
    assertThat(arrayIterator.next()).isEqualTo("two");
    assertThat(arrayIterator.hasNext()).isFalse();
  }

  @Test
  public void noNullElementsForArrayReturnsTrue() {
    assertThat(ArrayUtils.noNullElements("test", "testing", "tested", "nil", "null", "", "  ")).isTrue();
  }

  @Test
  public void noNullElementsWithArrayHavingAllNullElementsReturnsFalse() {
    assertThat(ArrayUtils.noNullElements(null, null, null)).isFalse();
  }

  @Test
  public void noNullElementsWithArrayHavingSingleNullElementReturnsFalse() {
    assertThat(ArrayUtils.noNullElements(null, "test")).isFalse();
  }

  @Test
  public void noNullElementsWithNullArrayReturnsFalse() {
    assertThat(ArrayUtils.noNullElements((Object[]) null)).isFalse();
  }

  @Test
  public void nullSafeArrayWithArray() {

    String[] expectedArray = { "test", "testing", "tested" };
    String[] actualArray = ArrayUtils.nullSafeArray(expectedArray);

    assertThat(actualArray).isSameAs(expectedArray);
  }

  @Test
  public void nullSafeArrayWithEmptyArray() {

    Character[] expectedArray = {};
    Character[] actualArray = ArrayUtils.nullSafeArray(expectedArray);

    assertThat(actualArray).isSameAs(expectedArray);
  }

  @Test
  @SuppressWarnings("all")
  public void nullSafeArrayWithNullArray() {

    Object[] actualArray = ArrayUtils.nullSafeArray(null, Integer.class);

    assertThat(actualArray).isNotNull();
    assertThat(actualArray.getClass().getComponentType()).isEqualTo(Integer.class);
    assertThat(actualArray.length).isEqualTo(0);
  }

  @Test
  public void nullSafeArrayWithNullArrayAndUnspecifiedComponentType() {

    Object actualArray = ArrayUtils.nullSafeArray(null);

    assertThat(actualArray).isNotNull();
    assertThat(actualArray.getClass().isArray()).isTrue();
    assertThat(actualArray.getClass().getComponentType()).isEqualTo(Object.class);
    assertThat(((Object[]) actualArray).length).isEqualTo(0);
  }

  @Test
  public void nullSafeArrayWithNullArrayUsedInAForEachLoop() {

    int sum = 0;

    //for (Integer number : ArrayUtils.<Integer>nullSafeArray(null)) { throws ClassCastException!
    for (Integer number : ArrayUtils.<Integer>nullSafeArray(null, Integer.class)) {
      sum += number;
    }

    assertThat(sum).isEqualTo(0);
  }

  @Test
  public void componentTypeOfNonNullArray() {

    assertThat(ArrayUtils.componentType(new Integer[0])).isEqualTo(Integer.class);
    assertThat(ArrayUtils.componentType(new String[0])).isEqualTo(String.class);
    assertThat(ArrayUtils.componentType(new Object[0])).isEqualTo(Object.class);
  }

  @Test
  public void componentTypeOfNullArray() {
    assertThat(ArrayUtils.componentType(null)).isEqualTo(Object.class);
  }

  @Test
  public void nullSafeLengthReturnsArrayLength() {

    Object[] array = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

    assertThat(ArrayUtils.nullSafeLength(array)).isEqualTo(array.length);
  }

  @Test
  public void nullSafeLengthReturnsEmptyArrayLength() {
    assertThat(ArrayUtils.nullSafeLength(new Object[0])).isEqualTo(0);
  }

  @Test
  public void nullSafeLengthReturnsInitializedArrayLength() {
    assertThat(ArrayUtils.nullSafeLength(new Object[10])).isEqualTo(10);
  }

  @Test
  public void nullSafeLengthReturnsZeroForNullArray() {
    assertThat(ArrayUtils.nullSafeLength(null)).isEqualTo(0);
  }

  @Test
  public void prependToEmptyArray() {

    String[] array = {};
    String[] actualArray = ArrayUtils.prepend("test", array);

    assertThat(actualArray).isNotNull();
    assertThat(actualArray).isNotSameAs(array);
    assertElements(actualArray, "test");
    assertElements(array);
  }

  @Test
  public void prependToSingleElementArray() {

    String[] array = { "tested" };
    String[] actualArray = ArrayUtils.prepend("testing", array);

    assertThat(actualArray).isNotNull();
    assertThat(actualArray).isNotSameAs(array);
    assertElements(actualArray, "testing", "tested");
    assertElements(array, "tested");
  }

  @Test
  public void prependToTwoElementArray() {

    String[] array = { "testing", "tested" };
    String[] actualArray = ArrayUtils.prepend("test", array);

    assertThat(actualArray).isNotNull();
    assertThat(actualArray).isNotSameAs(array);
    assertElements(actualArray, "test", "testing", "tested");
    assertElements(array, "testing", "tested");
  }

  @Test
  public void prependToNullArray() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.prepend("test", null))
      .withMessage("Array is required")
      .withNoCause();
  }

  @Test
  public void removeFromArray() {

    Object[] array = { 1, 2, 3, 4 };
    Object[] newArray = ArrayUtils.remove(array, 2);

    assertThat(newArray).isNotNull();
    assertThat(newArray).isNotSameAs(array);
    assertThat(newArray.length).isEqualTo(3);
    assertThat(Arrays.asList(newArray).containsAll(Arrays.asList(1, 2, 4))).isTrue();
    assertThat(array.length).isEqualTo(4);
  }

  @Test
  public void removeFirstElementFromArray() {

    Object[] array = { 1, 2, 3, 4 };
    Object[] newArray = ArrayUtils.remove(array, 0);

    assertThat(newArray).isNotNull();
    assertThat(newArray).isNotSameAs(array);
    assertThat(newArray.length).isEqualTo(3);
    assertThat(Arrays.asList(newArray).containsAll(Arrays.asList(2, 3, 4))).isTrue();
    assertThat(array.length).isEqualTo(4);
  }

  @Test
  public void removeLastElementFromArray() {

    Object[] array = { 1, 2, 3, 4 };
    Object[] newArray = ArrayUtils.remove(array, 3);

    assertThat(newArray).isNotNull();
    assertThat(newArray).isNotSameAs(array);
    assertThat(newArray.length).isEqualTo(3);
    assertThat(Arrays.asList(newArray).containsAll(Arrays.asList(1, 2, 3))).isTrue();
    assertThat(array.length).isEqualTo(4);
  }

  @Test
  public void removeOnlyElementFromArray() {

    Object[] array = { 1 };
    Object[] newArray = ArrayUtils.remove(array, 0);

    assertThat(newArray).isNotNull();
    assertThat(newArray).isNotSameAs(array);
    assertThat(newArray.length).isEqualTo(0);
    assertThat(array.length).isEqualTo(1);
  }

  @Test
  public void removeIllegalIndexFromArrayThrowsArrayIndexOutOfBoundsException() {

    assertThatExceptionOfType(ArrayIndexOutOfBoundsException.class)
      .isThrownBy(() -> ArrayUtils.remove(new Object[] { 1, 2, 3, 4 }, 4))
      .withMessage("[4] is not a valid index [0, 4] in the array")
      .withNoCause();
  }

  @Test
  public void removeFromNullArrayThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.remove(null, 0))
      .withMessage("Array is required")
      .withNoCause();
  }

  @Test
  public void shallowCopyWithArrayContainingObjectTypes() {

    Object[] array = { Person.newPerson("Jon", "Doe"), Person.newPerson("Jane", "Doe") };
    Object[] arrayCopy = ArrayUtils.shallowCopy(array);

    assertThat(arrayCopy).isNotNull();
    assertThat(arrayCopy).isNotSameAs(array);
    assertThat(arrayCopy.length).isEqualTo(array.length);

    for (int index = 0, size = array.length; index < size; index++) {
      assertThat(arrayCopy[index]).isEqualTo(array[index]);
      assertThat(arrayCopy[index]).isSameAs(array[index]);
    }
  }

  @Test
  public void shallowCopyWithArrayContainingPrimitiveTypes() {

    Object[] array = { "test", "testing", "tested" };
    Object[] arrayCopy = ArrayUtils.shallowCopy(array);

    assertThat(arrayCopy).isNotNull();
    assertThat(arrayCopy).isNotSameAs(array);
    assertThat(arrayCopy.length).isEqualTo(array.length);

    for (int index = 0, size = array.length; index < size; index++) {
      assertThat(arrayCopy[index]).isEqualTo(array[index]);
      assertThat(arrayCopy[index]).isSameAs(array[index]);
    }
  }

  @Test
  public void shallowCopyWithNullArray() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.shallowCopy(null))
      .withMessage("Array is required")
      .withNoCause();
  }

  @Test
  public void shuffle() {

    Integer[] array = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    Integer[] shuffledArray = ArrayUtils.shuffle(copy(array));

    assertThat(shuffledArray).isNotNull();
    assertThat(shuffledArray).isNotSameAs(array);
    assertShuffled(array, shuffledArray);

    Integer[] shuffledArrayAgain = ArrayUtils.shuffle(copy(shuffledArray));

    assertThat(shuffledArrayAgain).isNotNull();
    assertThat(shuffledArrayAgain).isNotSameAs(shuffledArray);
    assertShuffled(shuffledArray, shuffledArrayAgain);
    assertShuffled(array, shuffledArrayAgain);
  }

  @Test
  public void shuffleEmptyArray() {

    Object[] emptyArray = {};
    Object[] shuffledEmptyArray = ArrayUtils.shuffle(emptyArray);

    assertThat(shuffledEmptyArray).isSameAs(emptyArray);
  }

  @Test
  public void shuffleNullArray() {
    assertThat(ArrayUtils.<Object>shuffle(null)).isNull();
  }

  @Test
  public void shuffleSingleElementArray() {

    Object[] singleElementArray = { "test" };
    Object[] shuffledSingleElementArray = ArrayUtils.shuffle(singleElementArray);

    assertThat(shuffledSingleElementArray).isSameAs(singleElementArray);
    assertElements(shuffledSingleElementArray, "test");
  }

  @Test
  public void sortComparableObjectArray() {

    Integer[] numbers = { 2, 4, 3, 1 };
    Integer[] sortedNumbers = ArrayUtils.sort(numbers);

    assertThat(sortedNumbers).isSameAs(numbers);
    assertSorted(sortedNumbers);
  }

  @Test
  public void sortNonComparableObjectArray() {

    Person[] people = {
      Person.newPerson("Jack", "Handy"),
      Person.newPerson("Jon", "Doe"),
      Person.newPerson("Agent", "Smith"),
      Person.newPerson("Pie", "Doe")
    };

    Person[] sortedPeople = ArrayUtils.sort(people, (personOne, personTwo) ->
      ComparatorResultBuilder.<String>create()
        .doCompare(personOne.getLastName(), personTwo.getLastName())
        .doCompare(personOne.getFirstName(), personTwo.getFirstName())
        .getResult()
    );

    assertThat(sortedPeople).isSameAs(people);
    assertThat(Arrays.asList(sortedPeople)).isEqualTo(Arrays.asList(Person.newPerson("Jon", "Doe"),
      Person.newPerson("Pie", "Doe"), Person.newPerson("Jack", "Handy"), Person.newPerson("Agent", "Smith")));
  }

  @Test
  public void subArray() {

    Integer[] array = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    Integer[] subArray = ArrayUtils.subArray(array, 1, 2, 4, 8);

    assertThat(subArray).isNotNull();
    assertThat(subArray).isNotSameAs(array);
    assertThat(subArray).containsExactly(1, 2, 4, 8);
  }

  @Test
  public void subArrayWithEmptyArray() {

    Object[] subArray = ArrayUtils.subArray(new Object[0]);

    assertThat(subArray).isNotNull();
    assertThat(subArray).hasSize(0);
  }

  @Test
  public void subArrayWithEmptyArrayAndIndices() {

    assertThatExceptionOfType(ArrayIndexOutOfBoundsException.class)
      .isThrownBy(() -> ArrayUtils.subArray(new Object[0], 0))
      .withNoCause();
  }

  @Test
  public void subArrayWithNonEmptyArrayAndNoIndices() {

    String[] array = { "test", "testing", "tested" };
    String[] subArray = ArrayUtils.subArray(array);

    assertThat(subArray).isNotNull();
    assertThat(subArray).isNotSameAs(array);
    assertThat(subArray).hasSize(0);
  }

  @Test
  public void subArrayWithNullArray() {

    assertThatNullPointerException()
      .isThrownBy(() -> ArrayUtils.subArray(null, 1))
      .withNoCause();
  }

  @Test
  public void subArrayWithSingleElementArrayAndValidIndex() {

    String[] array = { "test" };
    String[] subArray = ArrayUtils.subArray(array, 0);

    assertThat(subArray).isNotNull();
    assertThat(subArray).isNotSameAs(array);
    assertThat(subArray).containsExactly("test");
  }

  @Test
  public void subArrayWithSingleElementArrayAndInvalidIndex() {

    assertThatExceptionOfType(ArrayIndexOutOfBoundsException.class)
      .isThrownBy(() -> ArrayUtils.subArray(new Integer[] { 0 }, 1))
      .withNoCause();
  }

  @Test
  public void subArrayWithOffsetAndLength() {

    Byte[] array = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    Byte[] subArray = ArrayUtils.subArray(array, 3, 3);

    assertThat(subArray).isNotNull();
    assertThat(subArray).isNotSameAs(array);
    assertThat(subArray).containsExactly((byte) 4, (byte) 5, (byte) 6);
  }

  @Test
  public void subArrayWithOffsetAndLengthUsingInvalidLength() {

    assertThatExceptionOfType(ArrayIndexOutOfBoundsException.class)
      .isThrownBy(() -> ArrayUtils.subArray(new Object[] { "test" }, 0, 10))
      .withNoCause();
  }

  @Test
  public void subArrayWithOffsetAndLengthUsingInvalidOffset() {

    assertThatExceptionOfType(ArrayIndexOutOfBoundsException.class)
      .isThrownBy(() -> ArrayUtils.subArray(new Object[0], 1, 10))
      .withNoCause();
  }

  @Test
  public void subArrayWithOffsetAndLengthUsingNullArray() {

    assertThatNullPointerException()
      .isThrownBy(() -> ArrayUtils.subArray(null, 5, 10))
      .withNoCause();
  }

  @Test
  public void swapArrayElements() {

    Integer[] array = { 0, 1, 2 };
    Integer[] actualArray = ArrayUtils.swap(array, 0, 2);

    assertThat(actualArray).isSameAs(array);
    assertElements(actualArray, 2, 1, 0);
  }

  @Test
  public void swapWithSingleElementArray() {

    String[] array = { "test" };
    String[] actualArray = ArrayUtils.swap(array, 0, 0);

    assertThat(actualArray).isSameAs(array);
    assertElements(actualArray, "test");
  }

  @Test
  public void swapWithTwoElementArray() {

    Integer[] array = { 0, 1 };
    Integer[] actualArray = ArrayUtils.swap(array, 0, 1);

    assertThat(actualArray).isSameAs(array);
    assertElements(actualArray, 1, 0);
  }

  @Test
  public void swapWithIllegalFirstIndex() {

    Integer[] array = { 0, 1, 2 };

    assertThatExceptionOfType(ArrayIndexOutOfBoundsException.class)
      .isThrownBy(() -> ArrayUtils.swap(array, -1, 1))
      .withNoCause();

    assertElements(array, 0, 1, 2);
  }

  @Test
  public void swapWithIllegalSecondIndex() {

    Integer[] array = { 0, 1, 2 };

    assertThatExceptionOfType(ArrayIndexOutOfBoundsException.class)
      .isThrownBy(() -> ArrayUtils.swap(array, 1, 3))
      .withNoCause();

    assertElements(array, 0, 1, 2);
  }

  @Test
  @SuppressWarnings("all")
  public void swapWithNullArray() {

    assertThatNullPointerException()
      .isThrownBy(() -> ArrayUtils.swap(null, 0, 10))
      .withNoCause();
  }

  @Test
  public void toStringArrayWithObjectArray() {

    String[] stringArray = ArrayUtils.toStringArray(Boolean.TRUE, 'x', 2, 3.14159d, "test");

    assertThat(stringArray).isNotNull();
    assertThat(stringArray.length).isEqualTo(5);
    assertThat(Arrays.asList(stringArray)).containsExactly("true", "x", "2", "3.14159", "test");
  }

  @Test
  public void toStringArrayWithStringArray() {

    String[] stringArray = ArrayUtils.toStringArray("test", "testing", "tested");

    assertThat(stringArray).isNotNull();
    assertThat(stringArray.length).isEqualTo(3);
    assertThat(Arrays.asList(stringArray)).containsExactly("test", "testing", "tested");
  }

  @Test
  public void toStringArrayWithArrayHavingNullElements() {

    String[] stringArray = ArrayUtils.toStringArray('x', null, "test", null, "nil");

    assertThat(stringArray).isNotNull();
    assertThat(stringArray.length).isEqualTo(5);
    assertThat(Arrays.asList(stringArray)).containsExactly("x", "null", "test", "null", "nil");
  }

  @Test
  public void toStringArrayWithNullArrayReturnsEmptyStringArray() {

    String[] stringArray = ArrayUtils.toStringArray((Object[]) null);

    assertThat(stringArray).isNotNull();
    assertThat(stringArray).isEmpty();
  }

  @Test
  public void transformArray() {

    Transformer<String> transformer = String::toUpperCase;

    String[] array = { "test", "testing", "tested" };
    String[] actualArray = ArrayUtils.transform(array, transformer);

    assertThat(actualArray).isSameAs(array);
    assertElements(actualArray, "TEST", "TESTING", "TESTED");
  }

  @Test
  public void transformEmptyArray() {

    Object[] array = {};
    Object[] transformedArray = ArrayUtils.transform(array, (value) -> null);

    assertThat(transformedArray).isSameAs(array);
  }

  @Test
  public void transformNullArray() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.transform(null, (value) -> "test"))
      .withMessage("Array is required")
      .withNoCause();
  }

  @Test
  public void transformWithNullTransformer() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayUtils.transform(new Object[0], null))
      .withMessage("Transformer is required")
      .withNoCause();
  }

  @Getter
  @EqualsAndHashCode
  @RequiredArgsConstructor(staticName = "newPerson")
  static class Person {

    @NonNull final String firstName;
    @NonNull final String lastName;

    @Override
    public String toString() {
      return String.format("%1$s %2$s", getFirstName(), getLastName());
    }
  }
}
