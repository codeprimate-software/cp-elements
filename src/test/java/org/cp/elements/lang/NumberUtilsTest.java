/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang;

import static org.junit.Assert.*;

import org.junit.Test;

/**
 * The NumberUtilsTest class is a test suite of test cases testing the contract and functionality of the 
 * NumberUtils class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.NumberUtils
 * @see org.junit.Assert
 * @see org.junit.Test
 * @since 1.0.0
 */
public class NumberUtilsTest {

  @Test
  public void testGetBytes() {
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
  public void testIsDecimal() {
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
  public void testIsEven() {
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
  public void testIsNegative() {
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
  public void testIsOdd() {
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
  public void testIsPositive() {
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
  public void testIsWhole() {
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
  public void testValueOfByte() {
    assertEquals(0, NumberUtils.valueOf((Byte) null));
    assertEquals(8, NumberUtils.valueOf((byte) 8));
  }

  @Test
  public void testValueOfShort() {
    assertEquals(0, NumberUtils.valueOf((Short) null));
    assertEquals(16, NumberUtils.valueOf((short) 16));
  }

  @Test
  public void testValueOfInteger() {
    assertEquals(0, NumberUtils.valueOf((Integer) null));
    assertEquals(32, NumberUtils.valueOf(32));
  }

  @Test
  public void testValueOfLong() {
    assertEquals(0L, NumberUtils.valueOf((Long) null));
    assertEquals(64l, NumberUtils.valueOf(64l));
  }

  @Test
  public void testValueOfFloat() {
    assertEquals(0.0f, NumberUtils.valueOf((Float) null), 0);
    assertEquals(32.0f, NumberUtils.valueOf(32.0f), 0);
  }

  @Test
  public void testValueOfDouble() {
    assertEquals(0.0d, NumberUtils.valueOf((Double) null), 0);
    assertEquals(Math.PI, NumberUtils.valueOf(Math.PI), 0);
  }

}
