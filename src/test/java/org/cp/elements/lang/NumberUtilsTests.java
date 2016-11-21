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

package org.cp.elements.lang;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;

/**
 * Unit tests for {@link NumberUtils}.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.NumberUtils
 * @see org.junit.Test
 * @since 1.0.0
 */
public class NumberUtilsTests {

  @Test
  public void getBytes() {
    byte[] expectedValues = { (byte) 0xCA, (byte) 0xFE, (byte) 0xBA, (byte) 0xBE };
    byte[] actualValues = NumberUtils.getBytes(0xCAFEBABE);

    assertThat(actualValues).isNotNull();
    assertThat(actualValues.length).isEqualTo(expectedValues.length);

    int index = 0;

    for (byte expectedValue : expectedValues) {
      assertThat(actualValues[index++]).isEqualTo(expectedValue);
    }
  }

  @Test
  public void isDecimal() {
    assertThat(NumberUtils.isDecimal(Math.PI)).isTrue();
    assertThat(NumberUtils.isDecimal(1.23d)).isTrue();
    assertThat(NumberUtils.isDecimal(12.3d)).isTrue();
    assertThat(NumberUtils.isDecimal(1.1d)).isTrue();
    assertThat(NumberUtils.isDecimal(1.01d)).isTrue();
    assertThat(NumberUtils.isDecimal(1.0001d)).isTrue();
    assertThat(NumberUtils.isDecimal(0.123d)).isTrue();
  }

  @Test
  public void isNotDecimal() {
    assertThat(NumberUtils.isDecimal(0.0d)).isFalse();
    assertThat(NumberUtils.isDecimal(-0.0d)).isFalse();
    assertThat(NumberUtils.isDecimal(1.0d)).isFalse();
    assertThat(NumberUtils.isDecimal(-1.0d)).isFalse();
    assertThat(NumberUtils.isDecimal(100.0d)).isFalse();
  }

  @Test
  public void isEven() {
    assertThat(NumberUtils.isEven(-10)).isTrue();
    assertThat(NumberUtils.isEven(-8)).isTrue();
    assertThat(NumberUtils.isEven(-6)).isTrue();
    assertThat(NumberUtils.isEven(-4)).isTrue();
    assertThat(NumberUtils.isEven(-2)).isTrue();
    assertThat(NumberUtils.isEven(0)).isTrue();
    assertThat(NumberUtils.isEven(2)).isTrue();
    assertThat(NumberUtils.isEven(4)).isTrue();
    assertThat(NumberUtils.isEven(6)).isTrue();
    assertThat(NumberUtils.isEven(8)).isTrue();
    assertThat(NumberUtils.isEven(10)).isTrue();
  }

  @Test
  public void isNotEven() {
    assertThat(NumberUtils.isEven(-11)).isFalse();
    assertThat(NumberUtils.isEven(-9)).isFalse();
    assertThat(NumberUtils.isEven(-7)).isFalse();
    assertThat(NumberUtils.isEven(-5)).isFalse();
    assertThat(NumberUtils.isEven(-3)).isFalse();
    assertThat(NumberUtils.isEven(-1)).isFalse();
    assertThat(NumberUtils.isEven(1)).isFalse();
    assertThat(NumberUtils.isEven(3)).isFalse();
    assertThat(NumberUtils.isEven(5)).isFalse();
    assertThat(NumberUtils.isEven(7)).isFalse();
    assertThat(NumberUtils.isEven(9)).isFalse();
    assertThat(NumberUtils.isEven(11)).isFalse();
  }

  @Test
  public void isBitwiseEven() {
    assertThat(NumberUtils.isBitwiseEven(-10)).isTrue();
    assertThat(NumberUtils.isBitwiseEven(-8)).isTrue();
    assertThat(NumberUtils.isBitwiseEven(-6)).isTrue();
    assertThat(NumberUtils.isBitwiseEven(-4)).isTrue();
    assertThat(NumberUtils.isBitwiseEven(-2)).isTrue();
    assertThat(NumberUtils.isBitwiseEven(0)).isTrue();
    assertThat(NumberUtils.isBitwiseEven(2)).isTrue();
    assertThat(NumberUtils.isBitwiseEven(4)).isTrue();
    assertThat(NumberUtils.isBitwiseEven(6)).isTrue();
    assertThat(NumberUtils.isBitwiseEven(8)).isTrue();
    assertThat(NumberUtils.isBitwiseEven(10)).isTrue();
  }

  @Test
  public void isNotBitwiseEven() {
    assertThat(NumberUtils.isBitwiseEven(-11)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(-9)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(-7)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(-5)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(-3)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(-1)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(1)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(3)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(5)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(7)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(9)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(11)).isFalse();
  }

  @Test
  public void isNegative() {
    assertThat(NumberUtils.isNegative(-10.0d)).isTrue();
    assertThat(NumberUtils.isNegative(-1.0d)).isTrue();
    assertThat(NumberUtils.isNegative(-0.01d)).isTrue();
  }

  @Test
  public void isNotNegative() {
    assertThat(NumberUtils.isNegative(-0.0d)).isFalse();
    assertThat(NumberUtils.isNegative(0.0d)).isFalse();
    assertThat(NumberUtils.isNegative(0.01d)).isFalse();
    assertThat(NumberUtils.isNegative(1.0d)).isFalse();
    assertThat(NumberUtils.isNegative(10.0d)).isFalse();
  }

  @Test
  public void isOdd() {
    assertThat(NumberUtils.isOdd(-11)).isTrue();
    assertThat(NumberUtils.isOdd(-9)).isTrue();
    assertThat(NumberUtils.isOdd(-7)).isTrue();
    assertThat(NumberUtils.isOdd(-5)).isTrue();
    assertThat(NumberUtils.isOdd(-3)).isTrue();
    assertThat(NumberUtils.isOdd(-1)).isTrue();
    assertThat(NumberUtils.isOdd(1)).isTrue();
    assertThat(NumberUtils.isOdd(3)).isTrue();
    assertThat(NumberUtils.isOdd(5)).isTrue();
    assertThat(NumberUtils.isOdd(7)).isTrue();
    assertThat(NumberUtils.isOdd(9)).isTrue();
    assertThat(NumberUtils.isOdd(11)).isTrue();
  }

  @Test
  public void isNotOdd() {
    assertThat(NumberUtils.isOdd(-10)).isFalse();
    assertThat(NumberUtils.isOdd(-8)).isFalse();
    assertThat(NumberUtils.isOdd(-6)).isFalse();
    assertThat(NumberUtils.isOdd(-4)).isFalse();
    assertThat(NumberUtils.isOdd(-2)).isFalse();
    assertThat(NumberUtils.isOdd(0)).isFalse();
    assertThat(NumberUtils.isOdd(2)).isFalse();
    assertThat(NumberUtils.isOdd(4)).isFalse();
    assertThat(NumberUtils.isOdd(6)).isFalse();
    assertThat(NumberUtils.isOdd(8)).isFalse();
    assertThat(NumberUtils.isOdd(10)).isFalse();
  }

  @Test
  public void isBitwiseOdd() {
    assertThat(NumberUtils.isBitwiseOdd(-11)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(-9)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(-7)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(-5)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(-3)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(-1)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(1)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(3)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(5)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(7)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(9)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(11)).isTrue();
  }

  @Test
  public void isNotBitwiseOdd() {
    assertThat(NumberUtils.isBitwiseOdd(-10)).isFalse();
    assertThat(NumberUtils.isBitwiseOdd(-8)).isFalse();
    assertThat(NumberUtils.isBitwiseOdd(-6)).isFalse();
    assertThat(NumberUtils.isBitwiseOdd(-4)).isFalse();
    assertThat(NumberUtils.isBitwiseOdd(-2)).isFalse();
    assertThat(NumberUtils.isBitwiseOdd(0)).isFalse();
    assertThat(NumberUtils.isBitwiseOdd(2)).isFalse();
    assertThat(NumberUtils.isBitwiseOdd(4)).isFalse();
    assertThat(NumberUtils.isBitwiseOdd(6)).isFalse();
    assertThat(NumberUtils.isBitwiseOdd(8)).isFalse();
    assertThat(NumberUtils.isBitwiseOdd(10)).isFalse();
  }

  @Test
  public void isPositive() {
    assertThat(NumberUtils.isPositive(0.01d)).isTrue();
    assertThat(NumberUtils.isPositive(1.0d)).isTrue();
    assertThat(NumberUtils.isPositive(10.0d)).isTrue();
  }

  @Test
  public void isNotPositive() {
    assertThat(NumberUtils.isPositive(-10.0d)).isFalse();
    assertThat(NumberUtils.isPositive(-1.0d)).isFalse();
    assertThat(NumberUtils.isPositive(-0.01d)).isFalse();
    assertThat(NumberUtils.isPositive(-0.0d)).isFalse();
    assertThat(NumberUtils.isPositive(0.0d)).isFalse();
  }

  @Test
  public void isWhole() {
    assertThat(NumberUtils.isWhole(-0.0d)).isTrue();
    assertThat(NumberUtils.isWhole(0.0d)).isTrue();
    assertThat(NumberUtils.isWhole(-1.0d)).isTrue();
    assertThat(NumberUtils.isWhole(1.0d)).isTrue();
    assertThat(NumberUtils.isWhole(100.0d)).isTrue();
  }

  @Test
  public void isNotWhole() {
    assertThat(NumberUtils.isWhole(Math.PI)).isFalse();
    assertThat(NumberUtils.isWhole(1.23d)).isFalse();
    assertThat(NumberUtils.isWhole(12.3d)).isFalse();
    assertThat(NumberUtils.isWhole(1.1d)).isFalse();
    assertThat(NumberUtils.isWhole(1.01d)).isFalse();
    assertThat(NumberUtils.isWhole(1.0001d)).isFalse();
    assertThat(NumberUtils.isWhole(0.123d)).isFalse();
  }

  @Test
  public void byteValueOfByte() {
    assertThat(NumberUtils.byteValue(8)).isEqualTo((byte) 8);
  }

  @Test
  public void byteValueOfNonByte() {
    assertThat(NumberUtils.byteValue(64.0d)).isEqualTo((byte) 64);
  }

  @Test
  public void byteValueOfNull() {
    assertThat(NumberUtils.byteValue(null)).isEqualTo((byte) 0);
  }

  @Test
  public void shortValueOfShort() {
    assertThat(NumberUtils.shortValue(16)).isEqualTo((short) 16);
  }

  @Test
  public void shortValueOfNonShort() {
    assertThat(NumberUtils.shortValue(128.0d)).isEqualTo((short) 128);
  }

  @Test
  public void shortValueOfNull() {
    assertThat(NumberUtils.shortValue(null)).isEqualTo((short) 0);
  }

  @Test
  public void intValueOfInteger() {
    assertThat(NumberUtils.intValue(32)).isEqualTo(32);
  }

  @Test
  public void intValueOfNonInteger() {
    assertThat(NumberUtils.intValue(3.14159d)).isEqualTo(3);
    assertThat(NumberUtils.intValue(256L)).isEqualTo(256);
    assertThat(NumberUtils.intValue(8.8421)).isEqualTo(8);
  }

  @Test
  public void intValueOfNull() {
    assertThat(NumberUtils.intValue(null)).isEqualTo(0);
  }

  @Test
  public void longValueOfLong() {
    assertThat(NumberUtils.longValue(64L)).isEqualTo(64L);
  }

  @Test
  public void longValueOfNonLong() {
    assertThat(NumberUtils.longValue(512.0d)).isEqualTo(512L);
  }

  @Test
  public void longValueOfNull() {
    assertThat(NumberUtils.longValue(null)).isEqualTo(0L);
  }

  @Test
  public void floatValueOfFloat() {
    assertThat(NumberUtils.floatValue(3.14159f)).isEqualTo(3.14159f);
  }

  @Test
  public void floatValueOfNonFloat() {
    assertThat(NumberUtils.floatValue(1024)).isEqualTo(1024.0f);
  }

  @Test
  public void floatValueOfNull() {
    assertThat(NumberUtils.floatValue(null)).isEqualTo(0.0f);
  }

  @Test
  public void doubleValueOfDouble() {
    assertThat(NumberUtils.doubleValue(Math.PI)).isEqualTo(Math.PI);
  }

  @Test
  public void doubleValueOfNonDouble() {
    assertThat(NumberUtils.doubleValue(2)).isEqualTo(2.0d);
    assertThat(NumberUtils.doubleValue(2.0f)).isEqualTo(2.0d);
  }

  @Test
  public void doubleValueOfNull() {
    assertThat(NumberUtils.doubleValue(null)).isEqualTo(0.0d);
  }

  @Test
  public void valueOfByte() {
    assertThat(NumberUtils.valueOf((byte) 8)).isEqualTo((byte) 8);
  }

  @Test
  public void valueOfNullByte() {
    assertThat(NumberUtils.valueOf((Byte) null)).isEqualTo((byte) 0);
  }

  @Test
  public void valueOfShort() {
    assertThat(NumberUtils.valueOf((short) 16)).isEqualTo((short) 16);
  }

  @Test
  public void valueOfNullShort() {
    assertThat(NumberUtils.valueOf((Short) null)).isEqualTo((short) 0);
  }

  @Test
  public void valueOfInteger() {
    assertThat(NumberUtils.valueOf(32)).isEqualTo(32);
  }

  @Test
  public void valueOfNullInteger() {
    assertThat(NumberUtils.valueOf((Integer) null)).isEqualTo(0);
  }

  @Test
  public void valueOfLong() {
    assertThat(NumberUtils.valueOf(64L)).isEqualTo(64L);
  }

  @Test
  public void valueOfNullLong() {
    assertThat(NumberUtils.valueOf((Long) null)).isEqualTo(0L);
  }

  @Test
  public void valueOfFloat() {
    assertThat(NumberUtils.valueOf(32.0f)).isEqualTo(32.0f);
  }

  @Test
  public void valueOfNullFloat() {
    assertThat(NumberUtils.valueOf((Float) null)).isEqualTo(0.0f);
  }

  @Test
  public void valueOfDouble() {
    assertThat(NumberUtils.valueOf(Math.PI)).isEqualTo(Math.PI);
  }

  @Test
  public void valueOfNullDouble() {
    assertThat(NumberUtils.valueOf((Double) null)).isEqualTo(0.0d);
  }
}
