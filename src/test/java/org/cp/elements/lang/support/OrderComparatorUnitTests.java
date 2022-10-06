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

import java.util.Arrays;

import org.junit.Test;

import org.cp.elements.lang.Ordered;
import org.cp.elements.lang.annotation.Order;

/**
 * Unit Tests for {@link OrderComparator}.
 *
 * @author John Blum
 * @see java.util.Comparator
 * @see org.junit.Test
 * @see org.cp.elements.lang.Ordered
 * @see org.cp.elements.lang.annotation.Order
 * @see org.cp.elements.lang.support.OrderComparator
 * @since 1.0.0
 */
public class OrderComparatorUnitTests {

  private final HighestOrderedType high = new HighestOrderedType();
  private final LowestOrderedType low = new LowestOrderedType();
  private final NonOrderedType normal = new NonOrderedType();

  @Test
  public void compareIsCorrect() {

    assertThat(OrderComparator.INSTANCE.compare(this.high, this.low)).isLessThan(0);
    assertThat(OrderComparator.INSTANCE.compare(this.high, this.normal)).isLessThan(0);
    assertThat(OrderComparator.INSTANCE.compare(this.normal, this.high)).isGreaterThan(0);
    assertThat(OrderComparator.INSTANCE.compare(this.normal, this.low)).isLessThan(0);
    assertThat(OrderComparator.INSTANCE.compare(this.low, this.high)).isGreaterThan(0);
    assertThat(OrderComparator.INSTANCE.compare(this.low, this.normal)).isGreaterThan(0);
  }

  @Test
  public void compareWithOrderObjectsInArray() {

    Object[] array = { this.normal, this.low, this.high };

    Arrays.sort(array, OrderComparator.INSTANCE);

    assertThat(array).containsExactly(this.high, this.normal, this.low);
  }

  @Order(Ordered.HIGHEST_PRIORITY)
  static final class HighestOrderedType { }

  @Order(Ordered.LOWEST_PRIORITY)
  static final class LowestOrderedType { }

  static final class NonOrderedType { }

}
