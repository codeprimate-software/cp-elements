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

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link Integers}.
 *
 * @author John Blum
 * @see java.lang.Integer
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.lang.Integers
 * @since 1.0.0
 */
public class IntegersUnitTests {

  @Test
  public void asInteger() {

    assertThat(Integers.asInteger((byte) 64)).isEqualTo(64);
    assertThat(Integers.asInteger((short) 8192)).isEqualTo(8192);
    assertThat(Integers.asInteger((long) 1_024_000)).isEqualTo(1_024_000);
    assertThat(Integers.asInteger(3.14f)).isEqualTo(3);
    assertThat(Integers.asInteger(Math.PI)).isEqualTo(3);
  }

  @Test
  public void asIntegerFromNullIsNullSafe() {
    assertThat(Integers.asInteger(null)).isNull();
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
  public void minusOneIsLessThanZero() {
    assertThat(Integers.isLessThanZero(-1)).isTrue();
  }

  @Test
  public void nullIsNotLessThanZero() {
    assertThat(Integers.isLessThanZero(null)).isFalse();
  }

  @Test
  public void oneIsNotLessThanZero() {
    assertThat(Integers.isLessThanZero(1)).isFalse();
  }

  @Test
  public void twoIsNotLessThanZero() {
    assertThat(Integers.isLessThanZero(2)).isFalse();
  }

  @Test
  public void zeroIsNotLessThanZero() {
    assertThat(Integers.isLessThanZero(0)).isFalse();
  }

  @Test
  public void minusTwoIsNotMinusOne() {
    assertThat(Integers.isMinusOne(-2)).isFalse();
  }

  @Test
  public void minusOneIsMinusOne() {
    assertThat(Integers.isMinusOne(-1)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void nullIsNotMinusOneIsNullSafe() {
    assertThat(Integers.isMinusOne(null)).isFalse();
  }

  @Test
  public void oneIsNotMinusOne() {
    assertThat(Integers.isMinusOne(1)).isFalse();
  }

  @Test
  public void twoIsNotMinusOne() {
    assertThat(Integers.isMinusOne(2)).isFalse();
  }

  @Test
  public void zeroIsNotMinusOne() {
    assertThat(Integers.isMinusOne(0)).isFalse();
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

  @Test
  public void negateNegativeNumber() {

    for (int number = 1; number < 100_000; number *= 2) {
      int negativeNumber = number * -1;
      assertThat(Integers.negate(negativeNumber)).isEqualTo(number);
    }
  }

  @Test
  public void negatePositiveNumber() {

    for (int number = 1; number < 100_000; number *= 2) {
      int negativeNumber = number * -1;
      assertThat(Integers.negate(number)).isEqualTo(negativeNumber);
    }
  }

  @Test
  public void negatePositiveNumberThenInvertNegativeNumber() {
    assertThat(Integers.negate(Integers.negate(4))).isEqualTo(4);
  }

  @Test
  public void negateZero() {
    assertThat(Integers.negate(0)).isZero();
  }
}
