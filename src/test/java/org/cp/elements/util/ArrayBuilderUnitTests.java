/*
 * Copyright 2017-Present Author or Authors.
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

import java.util.stream.IntStream;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link ArrayBuilder}.
 *
 * @author John Blum
 * @see org.cp.elements.util.ArrayBuilder
 * @see org.junit.jupiter.api.Test
 * @since 2.0.0
 */
class ArrayBuilderUnitTests {

  @SafeVarargs
  private <T> void assertContains(T[] array, T... elements) {

    assertThat(array).isNotNull();
    assertThat(array).isNotEmpty();
    assertThat(array).hasSize(elements.length);
    assertThat(array).containsExactly(elements);
  }

  private void assertEmpty(ArrayBuilder<?> arrayBuilder) {
    assertSize(arrayBuilder, 0);
  }

  private void assertSize(ArrayBuilder<?> arrayBuilder, int size) {

    assertThat(arrayBuilder).isNotNull();
    assertThat(arrayBuilder.isEmpty()).isEqualTo(size == 0);
    assertThat(arrayBuilder.size()).isEqualTo(size);
  }

  @Test
  void emptyReturnsMutableEmptyArrayBuilder() {

    ArrayBuilder<Object> emptyArray = ArrayBuilder.empty();

    assertThat(emptyArray).isNotNull();
    assertThat(emptyArray.isEmpty()).isTrue();
    assertThat(emptyArray.add("test")).isSameAs(emptyArray);
    assertThat(emptyArray.size()).isOne();
  }

  @Test
  void fromVarargArray() {

    ArrayBuilder<Integer> arrayBuilder = ArrayBuilder.from(1, 2, 3);

    assertSize(arrayBuilder, 3);

    Integer[] array = arrayBuilder.build();

    assertContains(array, 1, 2, 3);
  }

  @Test
  void fromNullVarargArrayIsNullSafe() {

    ArrayBuilder<Object> arrayBuilder = ArrayBuilder.from((Object[]) null);

    assertEmpty(arrayBuilder);
  }

  @Test
  void fromIterable() {

    ArrayBuilder<String> arrayBuilder = ArrayBuilder.from(ArrayUtils.asIterable("test", "testing", "tested"));

    assertSize(arrayBuilder, 3);

    String[] array = arrayBuilder.build();

    assertContains(array, "test", "testing", "tested");
  }

  @Test
  void fromNullIterableThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayBuilder.from((Iterable<?>) null))
      .withMessage("An Iterable collection of elements is required")
      .withNoCause();
  }

  @Test
  void addElements() {

    ArrayBuilder<Integer> arrayBuilder = ArrayBuilder.empty();

    IntStream.range(0, 10).forEach(number ->
      assertThat(arrayBuilder.add(number)).isSameAs(arrayBuilder));

    assertSize(arrayBuilder, 10);

    Integer[] array = arrayBuilder.build();

    assertContains(array, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
  }

  @Test
  void addNullElement() {

    ArrayBuilder<?> arrayBuilder = ArrayBuilder.empty();

    assertEmpty(arrayBuilder);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> arrayBuilder.add(null))
      .withMessage("Element is required")
      .withNoCause();

    assertEmpty(arrayBuilder);
  }

  @Test
  void removeElements() {

    ArrayBuilder<Integer> arrayBuilder = ArrayBuilder.from(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);

    assertSize(arrayBuilder, 10);

    Integer[] array = arrayBuilder.build();

    assertContains(array, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9);

    IntStream.range(0, 5).forEach(count ->
      assertThat(arrayBuilder.remove()).isEqualTo(array[9 - count]));

    Integer[] modifiedArray = arrayBuilder.build();

    assertThat(modifiedArray).isNotSameAs(array);
    assertContains(modifiedArray, 0, 1, 2, 3, 4);
  }

  @Test
  void removeElementFromEmptyArray() {

    assertThatExceptionOfType(ArrayIndexOutOfBoundsException.class)
      .isThrownBy(() -> ArrayBuilder.empty().remove())
      .withMessage("Array is empty")
      .withNoCause();
  }

  @Test
  void toIndex() {

    ArrayBuilder<?> arrayBuilder = ArrayBuilder.empty();

    IntStream.range(0, 9).forEach(number ->
      assertThat(arrayBuilder.toIndex(number)).isEqualTo(number - 1));
  }
}
