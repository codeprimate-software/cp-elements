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

package org.cp.elements.data.convert.support;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.cp.elements.data.convert.ConversionException;
import org.junit.Test;

/**
 * The DoubleConverterTest class is a test suite of test cases testing the contract and functionality of the
 * DoubleConverter class.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.convert.support.DoubleConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class DoubleConverterTest {

  private final DoubleConverter converter = new DoubleConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(Double.class, Double.class));
    assertTrue(converter.canConvert(Number.class, Double.class));
    assertTrue(converter.canConvert(Float.class, Double.class));
    assertTrue(converter.canConvert(Integer.class, Double.class));
    assertTrue(converter.canConvert(BigInteger.class, Double.class));
    assertTrue(converter.canConvert(BigDecimal.class, Double.class));
    assertTrue(converter.canConvert(String.class, Double.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(null, Double.class));
    assertFalse(converter.canConvert(Double.class, null));
    assertFalse(converter.canConvert(Double.class, Double.TYPE));
    assertFalse(converter.canConvert(Double.TYPE, Double.class));
    assertFalse(converter.canConvert(Double.class, String.class));
    assertFalse(converter.canConvert(Double.class, Integer.class));
    assertFalse(converter.canConvert(Double.class, Float.class));
    assertFalse(converter.canConvert(Character.class, Double.class));
    assertFalse(converter.canConvert(Boolean.class, Double.class));
    assertFalse(converter.canConvert(Object.class, Double.class));
  }

  @Test
  public void testConvert() {
    Double expected = Math.PI;
    Double actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected, actual);
  }

  @Test
  public void testConvertNumber() {
    Float expectedValue = 3.14159f;
    Double actual = converter.convert(expectedValue);

    assertNotNull(actual);
    assertNotSame(expectedValue, actual);
    assertEquals(expectedValue.doubleValue(), actual, 0.0d);

    Integer expected = 42;

    actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected.doubleValue(), actual, 0.0d);
  }

  @Test
  public void testConvertNegativeNumber() {
    Double expected = -11.23513d;
    Double actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected, actual);
  }

  @Test
  public void testConvertString() {
    String expected = "3.14159";
    Double actual = converter.convert(expected);

    assertNotNull(expected);
    assertNotSame(expected, actual);
    assertEquals(Double.parseDouble(expected), actual, 0.0d);
  }

  @Test(expected = ConversionException.class)
  public void testConvertInvalidDouble() {
    try {
      converter.convert("oneTwentyThreePointFortyFive");
    }
    catch (ConversionException e) {
      assertEquals("The Object value (oneTwentyThreePointFortyFive) is not a valid double!", e.getMessage());
      throw e;
    }
  }

}
