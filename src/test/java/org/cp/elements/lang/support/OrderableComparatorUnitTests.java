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
package org.cp.elements.lang.support;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.Test;

import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Orderable;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Unit Tests for {@link OrderableComparator}.
 *
 * @author John J. Blum
 * @see java.lang.Comparable
 * @see java.util.Comparator
 * @see org.junit.Test
 * @see org.cp.elements.lang.support.OrderableComparator
 * @since 1.0.0
 */
public class OrderableComparatorUnitTests {

  @Test
  public void compareIsCorrect() {

    NumberHolder zero = new NumberHolder(0, "zero");
    NumberHolder one = new NumberHolder(1, "one");
    NumberHolder two = new NumberHolder(2, "two");
    NumberHolder three = new NumberHolder(3, "three");
    NumberHolder four = new NumberHolder(4, "four");
    NumberHolder five = new NumberHolder(5, "five");
    NumberHolder six = new NumberHolder(6, "six");
    NumberHolder seven = new NumberHolder(7, "seven");
    NumberHolder eight = new NumberHolder(8, "eight");
    NumberHolder nine = new NumberHolder(9, "nine");

    List<NumberHolder> numbers =
      new ArrayList<>(Arrays.asList(zero, one, two, four, eight, nine, six, three, seven, five));

    Collections.sort(numbers);

    List<NumberHolder> sortedNumbers = Arrays.asList(zero, one, two, three, four, five, six, seven, eight, nine);

    assertThat(numbers).isEqualTo(sortedNumbers);

    List<NumberHolder> orderedNumbers = Arrays.asList(eight, five, four, nine, one, seven, six, three, two, zero);

    numbers.sort(new OrderableComparator<>());

    assertThat(numbers).isEqualTo(orderedNumbers);
  }

  protected static class NumberHolder implements Comparable<NumberHolder>, Orderable<String> {

    private final Integer number;
    private final String name;

    protected NumberHolder(@NotNull Integer number, @NotNull String name) {

      this.number = ObjectUtils.requireObject(number, "Number is required");
      this.name = StringUtils.requireText(name, "Name [%s] of the number is required");
    }

    public @NotNull String getName() {
      return this.name;
    }

    public @NotNull Integer getNumber() {
      return this.number;
    }

    @Override
    public @NotNull String getOrder() {
      return getName();
    }

    @Override
    public int compareTo(@NotNull NumberHolder numberHolder) {
      return getNumber() - numberHolder.getNumber();
    }

    @Override
    public boolean equals(Object obj) {

      if (this == obj) {
        return true;
      }

      if (!(obj instanceof NumberHolder)) {
        return false;
      }

      final NumberHolder that = (NumberHolder) obj;

      return ObjectUtils.equals(getNumber(), that.getNumber());
    }

    @Override
    public int hashCode() {
      return ObjectUtils.hashCodeOf(getNumber());
    }

    @Override
    public @NotNull String toString() {
      return getName();
    }
  }

}
