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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * The NumberUtilsTest class is a test suite of test cases testing the contract and functionality of the 
 * NumberUtils class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.NumberUtils
 * @see org.junit.Assert
 * @see org.junit.Test
 * @since 1.0.0
 */
public class NumberUtilsTest {

  @Test
  public void getBytes() {
    final byte[] expectedValues = { (byte) 0xCA, (byte) 0xFE, (byte) 0xBA, (byte) 0xBE };
    final byte[] actualValues = NumberUtils.getBytes(0xCAFEBABE);

    assertNotNull(actualValues);
    assertEquals(expectedValues.length, actualValues.length);

    int index = 0;

    for (final byte expectedValue : expectedValues) {
      assertEquals(expectedValue, actualValues[index++]);
    }
  }

  @Test
  public void isDecimal() {
    assertTrue(NumberUtils.isDecimal(Math.PI));
    assertTrue(NumberUtils.isDecimal(3.21d));
    assertTrue(NumberUtils.isDecimal(1.1d));
    assertTrue(NumberUtils.isDecimal(1.01d));
    assertTrue(NumberUtils.isDecimal(1.0001d));
    assertTrue(NumberUtils.isDecimal(0.123d));
    assertFalse(NumberUtils.isDecimal(0.0d));
    assertFalse(NumberUtils.isDecimal(1.0d));
    assertFalse(NumberUtils.isDecimal(-1.0d));
    assertFalse(NumberUtils.isDecimal(100.0d));
  }

  @Test
  public void isEven() {
    assertFalse(NumberUtils.isEven(-9));
    assertTrue(NumberUtils.isEven(-8));
    assertFalse(NumberUtils.isEven(-7));
    assertTrue(NumberUtils.isEven(-6));
    assertFalse(NumberUtils.isEven(-5));
    assertTrue(NumberUtils.isEven(-4));
    assertFalse(NumberUtils.isEven(-3));
    assertTrue(NumberUtils.isEven(-2));
    assertFalse(NumberUtils.isEven(-1));
    assertTrue(NumberUtils.isEven(0));
    assertFalse(NumberUtils.isEven(1));
    assertTrue(NumberUtils.isEven(2));
    assertFalse(NumberUtils.isEven(3));
    assertTrue(NumberUtils.isEven(4));
    assertFalse(NumberUtils.isEven(5));
    assertTrue(NumberUtils.isEven(6));
    assertFalse(NumberUtils.isEven(7));
    assertTrue(NumberUtils.isEven(8));
    assertFalse(NumberUtils.isEven(9));
  }

  @Test
  public void isEvenBitwise() {
    assertFalse(NumberUtils.isBitwiseEven(-9));
    assertTrue(NumberUtils.isBitwiseEven(-8));
    assertFalse(NumberUtils.isBitwiseEven(-7));
    assertTrue(NumberUtils.isBitwiseEven(-6));
    assertFalse(NumberUtils.isBitwiseEven(-5));
    assertTrue(NumberUtils.isBitwiseEven(-4));
    assertFalse(NumberUtils.isBitwiseEven(-3));
    assertTrue(NumberUtils.isBitwiseEven(-2));
    assertFalse(NumberUtils.isBitwiseEven(-1));
    assertTrue(NumberUtils.isBitwiseEven(0));
    assertFalse(NumberUtils.isBitwiseEven(1));
    assertTrue(NumberUtils.isBitwiseEven(2));
    assertFalse(NumberUtils.isBitwiseEven(3));
    assertTrue(NumberUtils.isBitwiseEven(4));
    assertFalse(NumberUtils.isBitwiseEven(5));
    assertTrue(NumberUtils.isBitwiseEven(6));
    assertFalse(NumberUtils.isBitwiseEven(7));
    assertTrue(NumberUtils.isBitwiseEven(8));
    assertFalse(NumberUtils.isBitwiseEven(9));
  }

  @Test
  public void isNegative() {
    assertTrue(NumberUtils.isNegative(-10.0d));
    assertTrue(NumberUtils.isNegative(-1.0d));
    assertTrue(NumberUtils.isNegative(-0.01d));
    assertFalse(NumberUtils.isNegative(-0.0d));
    assertFalse(NumberUtils.isNegative(0.0d));
    assertFalse(NumberUtils.isNegative(0.01d));
    assertFalse(NumberUtils.isNegative(1.0d));
    assertFalse(NumberUtils.isNegative(10.0d));
  }

  @Test
  public void isOdd() {
    assertTrue(NumberUtils.isOdd(-9));
    assertFalse(NumberUtils.isOdd(-8));
    assertTrue(NumberUtils.isOdd(-7));
    assertFalse(NumberUtils.isOdd(-6));
    assertTrue(NumberUtils.isOdd(-5));
    assertFalse(NumberUtils.isOdd(-4));
    assertTrue(NumberUtils.isOdd(-3));
    assertFalse(NumberUtils.isOdd(-2));
    assertTrue(NumberUtils.isOdd(-1));
    assertFalse(NumberUtils.isOdd(0));
    assertTrue(NumberUtils.isOdd(1));
    assertFalse(NumberUtils.isOdd(2));
    assertTrue(NumberUtils.isOdd(3));
    assertFalse(NumberUtils.isOdd(4));
    assertTrue(NumberUtils.isOdd(5));
    assertFalse(NumberUtils.isOdd(6));
    assertTrue(NumberUtils.isOdd(7));
    assertFalse(NumberUtils.isOdd(8));
    assertTrue(NumberUtils.isOdd(9));
  }

  @Test
  public void isOddBitwise() {
    assertTrue(NumberUtils.isBitwiseOdd(-9));
    assertFalse(NumberUtils.isBitwiseOdd(-8));
    assertTrue(NumberUtils.isBitwiseOdd(-7));
    assertFalse(NumberUtils.isBitwiseOdd(-6));
    assertTrue(NumberUtils.isBitwiseOdd(-5));
    assertFalse(NumberUtils.isBitwiseOdd(-4));
    assertTrue(NumberUtils.isBitwiseOdd(-3));
    assertFalse(NumberUtils.isBitwiseOdd(-2));
    assertTrue(NumberUtils.isBitwiseOdd(-1));
    assertFalse(NumberUtils.isBitwiseOdd(0));
    assertTrue(NumberUtils.isBitwiseOdd(1));
    assertFalse(NumberUtils.isBitwiseOdd(2));
    assertTrue(NumberUtils.isBitwiseOdd(3));
    assertFalse(NumberUtils.isBitwiseOdd(4));
    assertTrue(NumberUtils.isBitwiseOdd(5));
    assertFalse(NumberUtils.isBitwiseOdd(6));
    assertTrue(NumberUtils.isBitwiseOdd(7));
    assertFalse(NumberUtils.isBitwiseOdd(8));
    assertTrue(NumberUtils.isBitwiseOdd(9));
  }

  @Test
  public void isPositive() {
    assertFalse(NumberUtils.isPositive(-10.0d));
    assertFalse(NumberUtils.isPositive(-1.0d));
    assertFalse(NumberUtils.isPositive(-0.01d));
    assertFalse(NumberUtils.isPositive(-0.0d));
    assertFalse(NumberUtils.isPositive(0.0d));
    assertTrue(NumberUtils.isPositive(0.01d));
    assertTrue(NumberUtils.isPositive(1.0d));
    assertTrue(NumberUtils.isPositive(10.0d));
  }

  @Test
  public void isWhole() {
    assertFalse(NumberUtils.isWhole(Math.PI));
    assertFalse(NumberUtils.isWhole(3.21d));
    assertFalse(NumberUtils.isWhole(1.1d));
    assertFalse(NumberUtils.isWhole(1.01d));
    assertFalse(NumberUtils.isWhole(1.0001d));
    assertFalse(NumberUtils.isWhole(0.123d));
    assertTrue(NumberUtils.isWhole(0.0d));
    assertTrue(NumberUtils.isWhole(1.0d));
    assertTrue(NumberUtils.isWhole(-1.0d));
    assertTrue(NumberUtils.isWhole(100.0d));
  }

  @Test
  public void valueOfByte() {
    assertEquals(0, NumberUtils.valueOf((Byte) null));
    assertEquals(8, NumberUtils.valueOf((byte) 8));
  }

  @Test
  public void valueOfShort() {
    assertEquals(0, NumberUtils.valueOf((Short) null));
    assertEquals(16, NumberUtils.valueOf((short) 16));
  }

  @Test
  public void valueOfInteger() {
    assertEquals(0, NumberUtils.valueOf((Integer) null));
    assertEquals(32, NumberUtils.valueOf(32));
  }

  @Test
  public void valueOfLong() {
    assertEquals(0L, NumberUtils.valueOf((Long) null));
    assertEquals(64l, NumberUtils.valueOf(64l));
  }

  @Test
  public void valueOfFloat() {
    assertEquals(0.0f, NumberUtils.valueOf((Float) null), 0);
    assertEquals(32.0f, NumberUtils.valueOf(32.0f), 0);
  }

  @Test
  public void valueOfDouble() {
    assertEquals(0.0d, NumberUtils.valueOf((Double) null), 0);
    assertEquals(Math.PI, NumberUtils.valueOf(Math.PI), 0);
  }

}
