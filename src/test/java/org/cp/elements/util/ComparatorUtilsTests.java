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
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import org.junit.Test;

/**
 * Unit Tests for {@link ComparatorUtils}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.test.TestUtils
 * @see org.cp.elements.util.ComparatorUtils
 * @since 1.0.0
 */
public class ComparatorUtilsTests {

  private static final String NULL = null;

  @Test
  public void compareIgnoreNull() {

    assertThat(ComparatorUtils.compareIgnoreNull(NULL, NULL)).isGreaterThan(0);
    assertThat(ComparatorUtils.compareIgnoreNull(null, "null")).isGreaterThan(0);
    assertThat(ComparatorUtils.compareIgnoreNull("null", null)).isLessThan(0);
    assertThat(ComparatorUtils.compareIgnoreNull("test", "test")).isEqualTo(0);
    assertThat(ComparatorUtils.compareIgnoreNull("test", "testing")).isLessThan(0);
    assertThat(ComparatorUtils.compareIgnoreNull("tested", "test")).isGreaterThan(0);
    assertThat(ComparatorUtils.compareIgnoreNull("test", "TEST")).isGreaterThan(0);
  }

  @Test
  public void invertAscending() {

    List<Number> numbers = new ArrayList<>(Arrays.asList(2, 1, 3));

    Comparator<Number> ascendingComparator = Comparator.comparingInt(Number::intValue);

    numbers.sort(ascendingComparator);

    int currentNumber = Integer.MIN_VALUE;

    for (Number number : numbers) {
      assertThat(currentNumber).isLessThan(number.intValue());
      currentNumber = number.intValue();
    }

    numbers.sort(ComparatorUtils.invert(ascendingComparator));

    currentNumber = Integer.MAX_VALUE;

    for (Number number : numbers) {
      assertThat(currentNumber).isGreaterThan(number.intValue());
      currentNumber = number.intValue();
    }
  }

  @Test
  public void invertDescending() {

    List<Number> numbers = new ArrayList<>(Arrays.asList(2, 1, 3));

    Comparator<Number> descendingComparator = (numberOne, numberTwo) -> (numberTwo.intValue() - numberOne.intValue());

    numbers.sort(descendingComparator);

    int currentNumber = Integer.MAX_VALUE;

    for (Number number : numbers) {
      assertThat(currentNumber).isGreaterThan(number.intValue());
      currentNumber = number.intValue();
    }

    numbers.sort(ComparatorUtils.invert(descendingComparator));

    currentNumber = Integer.MIN_VALUE;

    for (Number number : numbers) {
      assertThat(currentNumber).isLessThan(number.intValue());
      currentNumber = number.intValue();
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void nullSafeArgumentComparatorWithNullValues() {

    Comparator<String> mockComparator = mock(Comparator.class);

    assertThat(ComparatorUtils.nullSafeArgumentsComparator(mockComparator).compare(null, null)).isEqualTo(1);
    assertThat(ComparatorUtils.nullSafeArgumentsComparator(mockComparator).compare(null, "test")).isEqualTo(1);
    assertThat(ComparatorUtils.nullSafeArgumentsComparator(mockComparator).compare("test", null)).isEqualTo(-1);

    verifyNoInteractions(mockComparator);
  }

  @Test
  @SuppressWarnings("all")
  public void nullSafeArgumentComparatorWithNonNullValues() {

    Comparator<String> mockComparator = mock(Comparator.class);

    when(mockComparator.compare(anyString(), anyString())).thenReturn(0);

    assertThat(ComparatorUtils.nullSafeArgumentsComparator(mockComparator).compare("test", "TEST")).isEqualTo(0);

    verify(mockComparator, times(1)).compare(eq("test"), eq("TEST"));
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void nullSafeComparatorWithNonNullComparator() {

    Comparator<Comparable> mockComparator = mock(Comparator.class);

    assertThat(ComparatorUtils.nullSafeComparator(mockComparator)).isSameAs(mockComparator);
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void nullSafeComparatorWithNullComparator() {

    Comparator<Comparable> comparator = ComparatorUtils.nullSafeComparator(null);

    assertThat(comparator).isNotNull();
    assertThat(comparator.compare(null, null)).isEqualTo(1);
    assertThat(comparator.compare(null, "test")).isEqualTo(1);
    assertThat(comparator.compare("test", null)).isEqualTo(-1);
    assertThat(comparator.compare("test", "test")).isEqualTo(0);
    assertThat(comparator.compare("nil", "null")).isLessThan(0);
    assertThat(comparator.compare("null", "nil")).isGreaterThan(0);
  }
}
