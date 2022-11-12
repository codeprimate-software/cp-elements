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
package org.cp.elements.lang;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;

/**
 * Unit Tests for {@link Integers}.
 *
 * @author John Blum
 * @see java.lang.Integer
 * @see org.junit.Test
 * @see org.cp.elements.lang.Integers
 * @since 1.0.0
 */
public class IntegersUnitTests {

  @Test
  public void invertNegativeNumber() {

    for (int number = 1; number < 100_000; number *= 2) {
      int negativeNumber = number * -1;
      assertThat(Integers.invert(negativeNumber)).isEqualTo(number);
    }
  }

  @Test
  public void invertPositiveNumber() {

    for (int number = 1; number < 100_000; number *= 2) {
      int negativeNumber = number * -1;
      assertThat(Integers.invert(number)).isEqualTo(negativeNumber);
    }
  }

  @Test
  public void invertPositiveNumberThenInvertNegativeNumber() {
    assertThat(Integers.invert(Integers.invert(4))).isEqualTo(4);
  }

  @Test
  public void invertZero() {
    assertThat(Integers.invert(0)).isZero();
  }

  @Test
  public void minusOneIsNotGreaterThanZero() {
    assertThat(Integers.isGreaterThanZero(-1)).isFalse();
  }

  @Test
  public void nullIsNotGreaterThanZeroIsNullSafe() {
    assertThat(Integers.isGreaterThanZero(null)).isFalse();
  }

  @Test
  public void oneIsGreaterThanZero() {
    assertThat(Integers.isGreaterThanZero(1)).isTrue();
  }

  @Test
  public void twoIsGreaterThanZero() {
    assertThat(Integers.isGreaterThanZero(2)).isTrue();
  }

  @Test
  public void zeroIsNotGreaterThanZero() {
    assertThat(Integers.isGreaterThanZero(0)).isFalse();
  }

  @Test
  public void minusOneIsNotOne() {
    assertThat(Integers.isOne(-1)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void nullIsNotOneIsNullSafe() {
    assertThat(Integers.isOne(null)).isFalse();
  }

  @Test
  public void oneIsOne() {
    assertThat(Integers.isOne(1)).isTrue();
  }

  @Test
  public void twoIsNotOne() {
    assertThat(Integers.isOne(2)).isFalse();
  }

  @Test
  public void zeroIsNotOne() {
    assertThat(Integers.isOne(0)).isFalse();
  }

  @Test
  public void minusZeroIsZero() {
    assertThat(Integers.isZero(-0)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void nullIsNotZeroIsNullSafe() {
    assertThat(Integers.isZero(null)).isFalse();
  }

  @Test
  public void oneIsNotZero() {
    assertThat(Integers.isZero(1)).isFalse();
  }

  @Test
  public void twoIsNotZero() {
    assertThat(Integers.isZero(2)).isFalse();
  }

  @Test
  public void zeroIsZero() {
    assertThat(Integers.isZero(0)).isTrue();
  }
}
