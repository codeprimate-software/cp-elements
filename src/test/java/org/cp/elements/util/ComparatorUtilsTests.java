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
import static org.cp.elements.test.TestUtils.assertNegative;
import static org.cp.elements.test.TestUtils.assertPositive;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.junit.Test;

/**
 * Unit tests for {@link ComparatorUtils}.
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
  public void compareIgnoreNull() throws Exception {
    assertPositive(ComparatorUtils.compareIgnoreNull(NULL, NULL));
    assertPositive(ComparatorUtils.compareIgnoreNull(null, "null"));
    assertNegative(ComparatorUtils.compareIgnoreNull("null", null));
    assertThat(ComparatorUtils.compareIgnoreNull("test", "test")).isEqualTo(0);
    assertNegative(ComparatorUtils.compareIgnoreNull("test", "testing"));
    assertPositive(ComparatorUtils.compareIgnoreNull("tested", "test"));
    assertPositive(ComparatorUtils.compareIgnoreNull("test", "TEST"));
  }

  @Test
  public void invert() {
    List<Number> numbers = new ArrayList<>(Arrays.asList(2, 1, 3));

    Comparator<Number> numbersComparator = (numberOne, numberTwo) -> (numberOne.intValue() - numberTwo.intValue());

    Collections.sort(numbers,  numbersComparator);

    int currentNumber = Integer.MIN_VALUE;

    for (final Number number : numbers) {
      assertThat(currentNumber < number.intValue()).isTrue();
      currentNumber = number.intValue();
    }

    Collections.sort(numbers, ComparatorUtils.invert(numbersComparator));

    currentNumber = Integer.MAX_VALUE;

    for (final Number number : numbers) {
      assertThat(currentNumber > number.intValue()).isTrue();
      currentNumber = number.intValue();
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void nullSafeDelegatingComparatorWithNullValues() {
    Comparator<String> mockComparator = mock(Comparator.class);

    assertThat(ComparatorUtils.nullSafeDelegatingComparator(mockComparator).compare(null, null)).isEqualTo(1);
    assertThat(ComparatorUtils.nullSafeDelegatingComparator(mockComparator).compare(null, "test")).isEqualTo(1);
    assertThat(ComparatorUtils.nullSafeDelegatingComparator(mockComparator).compare("test", null)).isEqualTo(-1);

    verifyZeroInteractions(mockComparator);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void nullSafeDelegatingComparatorWithNonNullValues() {
    Comparator<String> mockComparator = mock(Comparator.class);

    when(mockComparator.compare(anyString(), anyString())).thenReturn(0);

    assertThat(ComparatorUtils.nullSafeDelegatingComparator(mockComparator).compare("test", "TEST")).isEqualTo(0);

    verify(mockComparator, times(1)).compare(eq("test"), eq("TEST"));
  }
}
