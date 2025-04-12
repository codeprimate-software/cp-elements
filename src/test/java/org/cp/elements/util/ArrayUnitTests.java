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
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;

import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.NumberUtils;

/**
 * Unit Tests for {@link Array}.
 *
 * @author John Blum
 * @see org.cp.elements.util.Array
 * @see org.junit.jupiter.api.Test
 * @since 2.0.0
 */
public class ArrayUnitTests {

  @Test
  void ofEmptyArray() {

    Array<?> emptyArray = Array.empty();

    assertThat(emptyArray).isNotNull();
    assertThat(emptyArray).isEmpty();
  }

  @Test
  void ofImmutableArray() {

    Array<Integer> array = Array.immutableOf(1);

    assertThat(array).isNotNull();
    assertThat(array.get(0)).isOne();

    assertThatIllegalStateException()
      .isThrownBy(() -> array.set(0, 2))
      .withMessage("Array is immutable")
      .withNoCause();

    assertThat(array.get(0)).isOne();
  }

  @Test
  void ofJavaIntegerArray() {

    Integer[] integers = { 1, 2, 3 };

    Array<Integer> array = Array.of(integers);

    assertThat(array).isNotNull();
    assertThat(array).hasSize(integers.length);
    assertThat(array).containsExactly(1, 2, 3);
  }

  @Test
  void ofJavaStringArray() {

    String[] strings = { "test", "testing", "tested" };

    Array<String> array = Array.of(strings);

    assertThat(array).isNotNull();
    assertThat(array).hasSize(strings.length);
    assertThat(array).containsExactly("test", "testing", "tested");
  }

  @Test
  void ofNullArray() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Array.of((Object[]) null))
      .withMessage("Array is required")
      .withNoCause();
  }

  @Test
  void singleton() {

    Array<String> array = Array.singleton("test");

    assertThat(array).isNotNull();
    assertThat(array).hasSize(1);
    assertThat(array).containsExactly("test");
  }

  @Test
  void nullSingleton() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Array.singleton(null))
      .withMessage("Element is required")
      .withNoCause();
  }

  @Test
  void arrayWithNullElements() {

    Array<Object> array = Array.of(1, null, 2, 3, null, null);

    assertThat(array).isNotNull();
    assertThat(array).hasSize(6);
    assertThat(array).containsExactly(1, null, 2, 3, null, null);
  }

  @Test
  void isEmpty() {

    Array<?> emptyArray = Array.empty();

    assertThat(emptyArray).isNotNull();
    assertThat(emptyArray.isEmpty()).isTrue();
    assertThat(emptyArray.isNotEmpty()).isFalse();
  }

  @Test
  void isNotEmptyForMultiElementArray() {

    Array<?> array = Array.of(1, 2, 3);

    assertThat(array).isNotNull();
    assertThat(array.isEmpty()).isFalse();
    assertThat(array.isNotEmpty()).isTrue();
  }

  @Test
  void isNotEmptyForSingleElementArray() {

    Array<?> array = Array.singleton(1);

    assertThat(array).isNotNull();
    assertThat(array.isEmpty()).isFalse();
    assertThat(array.isNotEmpty()).isTrue();
  }

  @Test
  void getAndSet() {

    Array<String> array = Array.of("test");

    assertThat(array).isNotNull();
    assertThat(array.get(0)).isEqualTo("test");

    Array<String> newArray = array.set(0, "testing");

    assertThat(newArray).isNotNull().isNotSameAs(array);
    assertThat(newArray.get(0)).isEqualTo("testing");
    assertThat(array.get(0)).isEqualTo("test");

    newArray = newArray.set(0, null);

    assertThat(newArray).isNotNull().isNotSameAs(array);
    assertThat(newArray.get(0)).isNull();
    assertThat(array.get(0)).isEqualTo("test");

    newArray= newArray.set(0, "testing");

    assertThat(newArray).isNotNull().isNotSameAs(array);
    assertThat(newArray.get(0)).isEqualTo("testing");
    assertThat(array.get(0)).isEqualTo("test");
  }

  @Test
  void findByPredicate() {

    Array<Integer> array = Array.of(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
    Array<Integer> evenArray = array.findBy(NumberUtils::isEven);

    assertThat(evenArray).isNotNull();
    assertThat(evenArray).hasSize(5);
    assertThat(evenArray).containsExactly(0, 2, 4, 6, 8);
  }

  @Test
  void findByFindsNone() {

    Array<Integer> array = Array.of(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
    Array<Integer> emptyArray = array.findBy(NumberUtils::isNegative);

    assertThat(emptyArray).isNotNull();
    assertThat(emptyArray).isEmpty();
  }

  @Test
  void findByFindsOne() {

    Array<Integer> array = Array.of(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
    Array<Integer> singleElementArray = array.findBy(number -> number == 9);

    assertThat(singleElementArray).isNotNull();
    assertThat(singleElementArray).hasSize(1);
    assertThat(singleElementArray).containsExactly(9);
  }

  @Test
  void findByWithNullPredicate() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Array.empty().findBy(null))
      .withMessage("Predicate is required")
      .withNoCause();
  }

  @Test
  void findOneFindsNone() {

    Array<Integer> array = Array.of(1, 3, 5, 7, 9);

    Optional<Integer> element = array.findOne(NumberUtils::isEven);

    assertThat(element).isNotNull();
    assertThat(element).isNotPresent();
  }

  @Test
  void findOneFindsSingleElement() {

    Array<Integer> array = Array.of(1, 2, 3, 5, 7, 9);

    Optional<Integer> two = array.findOne(NumberUtils::isEven);

    assertThat(two).isNotNull();
    assertThat(two).isPresent();
    assertThat(two.orElse(null)).isEqualTo(2);
  }

  @Test
  void findOneWithNullPredicate() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Array.empty().findOne(null))
      .withMessage("Predicate is required")
      .withNoCause();
  }

  @Test
  void lengthOfEmptyArray() {
    assertThat(Array.empty().length()).isZero();
  }

  @Test
  void lengthOfMultiElementArray() {
    assertThat(Array.of(1, 2, 3).length()).isEqualTo(3);
  }

  @Test
  void lengthOfSingleElementArray() {
    assertThat(Array.singleton(1).length()).isOne();
  }

  @Test
  void requireOneWhenFound() {

    Integer two = Array.of(1, 2, 3).requireOne(NumberUtils::isEven);

    assertThat(two).isNotNull();
    assertThat(two).isEqualTo(2);
  }

  @Test
  void requireOneWhenMissing() {

    assertThatExceptionOfType(NoSuchElementException.class)
      .isThrownBy(() -> Array.<Integer>empty().requireOne(NumberUtils::isOdd))
      .withMessage("Element not found")
      .withNoCause();
  }

  @Test
  void requireOneWithNullPredicate() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Array.empty().requireOne(null))
      .withMessage("Predicate is required")
      .withNoCause();
  }

  @Test
  void stream() {

    Array<Integer> array = Array.of(1, 2, 3);

    assertThat(array).isNotNull();
    assertThat(array).hasSize(3);
    assertThat(array).containsExactly(1, 2, 3);

    Stream<Integer> stream = array.stream();

    assertThat(stream).isNotNull();
    assertThat(stream).containsExactly(1, 2, 3);
  }

  @Test
  void transformStringsToUppercase() {

    Array<String> array = Array.of("test", "testing", "tested");

    assertThat(array).isNotNull();
    assertThat(array).hasSize(3);
    assertThat(array).containsExactly("test", "testing", "tested");

    Array<String> uppercaseStringArray = array.transform(String::toUpperCase);

    assertThat(uppercaseStringArray).isNotNull();
    assertThat(uppercaseStringArray).hasSameSizeAs(array);
    assertThat(uppercaseStringArray).containsExactly("TEST", "TESTING", "TESTED");
  }

  @Test
  void transformToStronglyTypeIntegerArray() {

    Array<String> array = Array.of("1", "2", "3");

    assertThat(array).isNotNull();
    assertThat(array).hasSize(3);
    assertThat(array).containsExactly("1", "2", "3");

    Array<Integer> numberArray = array.transform(Integer::parseInt);

    assertThat(numberArray).isNotNull();
    assertThat(numberArray).hasSameSizeAs(array);
    assertThat(numberArray).containsExactly(1, 2, 3);

    Integer[] integerArray = numberArray.toArray();

    assertThat(integerArray).isNotNull();
    assertThat(integerArray).hasSameSizeAs(numberArray);
    assertThat(integerArray).containsExactly(1, 2, 3);
  }

  @Test
  void toArray() {

    Integer[] integerArray = { 1, 2, 3 };

    Array<Integer> array = Array.of(integerArray);

    assertThat(array).isNotNull();
    assertThat(array).hasSameSizeAs(integerArray);
    assertThat(array).containsExactly(integerArray);

    Integer[] theIntegerArray = array.toArray();

    assertThat(theIntegerArray).isNotNull();
    assertThat(theIntegerArray).isNotSameAs(integerArray);
    assertThat(theIntegerArray).hasSameSizeAs(integerArray);
    assertThat(theIntegerArray).containsExactly(integerArray);
  }
}
