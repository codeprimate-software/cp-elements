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

import java.util.Comparator;

import org.junit.Test;

/**
 * Unit Tests for {@link ComparatorResultBuilder}.
 *
 * @author John J. Blum
 * @see java.lang.Comparable
 * @see java.util.Comparator
 * @see org.junit.Test
 * @see org.cp.elements.util.ComparatorResultBuilder
 * @since 1.0.0
 */
@SuppressWarnings({ "unchecked" })
public class ComparatorResultBuilderUnitTests {

  @Test
  public void createIsSuccessful() {

    ComparatorResultBuilder<String> comparator = ComparatorResultBuilder.create();

    assertThat(comparator).isNotNull();
    assertThat(comparator.getResult()).isZero();
  }

  @Test
  @SuppressWarnings("all")
  public void compareIsNullSafe() {

    Comparator<String> comparator = ComparatorResultBuilder.create();

    assertThat(comparator.compare(null, null)).isGreaterThan(0);
    assertThat(comparator.compare(null, "test")).isGreaterThan(0);
    assertThat(comparator.compare("test", null)).isLessThan(0);
    assertThat(comparator.compare("test", "test")).isZero();
    assertThat(comparator.compare("nil", "null")).isLessThan(0);
    assertThat(comparator.compare("null", "nil")).isGreaterThan(0);
    assertThat(comparator.compare("test", "null")).isGreaterThan(0);
  }

  @Test
  @SuppressWarnings("rawtypes")
  public void doCompareResultsInZero() {

    int result = ComparatorResultBuilder.<Comparable>create()
      .doCompare(Boolean.TRUE, Boolean.TRUE)
      .doCompare('x', 'x')
      .doCompare(2, 2)
      .doCompare(Math.PI, Math.PI)
      .doCompare("test", "test")
      .getResult();

    assertThat(result).isZero();
  }

  @Test
  @SuppressWarnings("rawtypes")
  public void doCompareShortCircuitsToGreaterThanZero() {

    int result = ComparatorResultBuilder.<Comparable>create()
      .doCompare("second", "first")
      .doCompare("first", "second")
      .getResult();

    assertThat(result).isGreaterThan(0);
  }

  @Test
  @SuppressWarnings("rawtypes")
  public void doCompareShortCircuitsToLessThanZero() {

    int result = ComparatorResultBuilder.<Comparable>create()
      .doCompare("first", "second")
      .doCompare("second", "first")
      .getResult();

    assertThat(result).isLessThan(0);
  }

  @Test
  public void invertResultIsCorrect() {

    ComparatorResultBuilder<String> comparator = ComparatorResultBuilder.<String>create()
      .doCompare("first", "second")
      .doCompare("second", "first");

    assertThat(comparator.getResult()).isLessThan(0);
    assertThat(comparator.invert().getResult()).isGreaterThan(0);
    assertThat(comparator.invert().getResult()).isLessThan(0);
  }
}
