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

package org.cp.elements.lang.support;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Orderable;
import org.junit.Test;

/**
 * The OrderableComparatorTest class is a test suite of test cases testing the contract and functionality of the
 * OrderableComparator class.
 *
 * @author John J. Blum
 * @see java.lang.Comparable
 * @see java.util.Comparator
 * @see org.cp.elements.lang.support.OrderableComparator
 * @see org.cp.elements.test.TestUtils
 * @see org.junit.Assert
 * @see org.junit.Test
 * @since 1.0.0
 */
public class OrderableComparatorTest {

  @Test
  public void testCompare() {
    final NumberHolder zero = new NumberHolder(0, "zero");
    final NumberHolder one = new NumberHolder(1, "one");
    final NumberHolder two = new NumberHolder(2, "two");
    final NumberHolder three = new NumberHolder(3, "three");
    final NumberHolder four = new NumberHolder(4, "four");
    final NumberHolder five = new NumberHolder(5, "five");
    final NumberHolder six = new NumberHolder(6, "six");
    final NumberHolder seven = new NumberHolder(7, "seven");
    final NumberHolder eight = new NumberHolder(8, "eight");
    final NumberHolder nine = new NumberHolder(9, "nine");

    final List<NumberHolder> numbers = new ArrayList<>(
      Arrays.asList(zero, one, two, four, eight, nine, six, three, seven, five));

    Collections.sort(numbers);

    final List<NumberHolder> sortedNumbers = Arrays.asList(zero, one, two, three, four, five, six, seven, eight, nine);

    assertEquals(sortedNumbers, numbers);

    final List<NumberHolder> orderedNumbers = Arrays.asList(eight, five, four, nine, one, seven, six, three, two, zero);

    Collections.sort(numbers, new OrderableComparator<>());

    assertEquals(orderedNumbers, numbers);
  }

  protected static final class NumberHolder implements Comparable<NumberHolder>, Orderable<String> {

    private final Integer number;
    private final String name;

    protected NumberHolder(final Integer number, final String name) {
      assert number != null : "The number cannot be null!";
      assert name != null : "The name of the number cannot be null!";
      this.number = number;
      this.name = name;
    }

    public String getName() {
      return name;
    }

    public Integer getNumber() {
      return number;
    }

    @Override
    public String getOrder() {
      return getName();
    }

    @Override
    @SuppressWarnings("all")
    public int compareTo(final NumberHolder numberHolder) {
      return (getNumber() - numberHolder.getNumber());
    }

    @Override
    public boolean equals(final Object obj) {
      if (obj == this) {
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
      int hashValue = 17;
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getNumber());
      return hashValue;
    }

    @Override
    public String toString() {
      return getName();
    }
  }

}
