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

import java.util.Comparator;

import org.junit.Test;

/**
 * Unit tests for {@link ComparatorResultBuilder}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.util.ComparatorResultBuilder
 * @since 1.0.0
 */
public class ComparatorResultBuilderTests {

  @Test
  public void createIsSuccessful() {
    ComparatorResultBuilder<String> comparator = ComparatorResultBuilder.create();

    assertThat(comparator).isNotNull();
    assertThat(comparator.getResult()).isEqualTo(0);
  }

  @Test
  public void compareIsNullSafe() {
    Comparator<String> comparator = ComparatorResultBuilder.create();

    assertThat(comparator.compare(null, null)).isGreaterThan(0);
    assertThat(comparator.compare(null, "test")).isGreaterThan(0);
    assertThat(comparator.compare("test", null)).isLessThan(0);
    assertThat(comparator.compare("test", "test")).isEqualTo(0);
    assertThat(comparator.compare("nil", "null")).isLessThan(0);
    assertThat(comparator.compare("null", "nil")).isGreaterThan(0);
  }

  @Test
  public void doCompareResultsInZero() {
    int result = ComparatorResultBuilder.<Comparable>create()
      .doCompare(Boolean.TRUE, Boolean.TRUE)
      .doCompare('x', 'x')
      .doCompare(1, 1)
      .doCompare(Math.PI, Math.PI)
      .doCompare("test", "test")
      .getResult();

    assertThat(result).isEqualTo(0);
  }

  @Test
  public void doCompareShortCircuitsToGreaterThanZero() {
    int result = ComparatorResultBuilder.<Comparable>create()
      .doCompare("second", "first")
      .doCompare("first", "second")
      .getResult();

    assertThat(result).isGreaterThan(0);
  }

  @Test
  public void doCompareShortCircuitsToLessThanZero() {
    int result = ComparatorResultBuilder.<Comparable>create()
      .doCompare("first", "second")
      .doCompare("second", "first")
      .getResult();

    assertThat(result).isLessThan(0);
  }
}
