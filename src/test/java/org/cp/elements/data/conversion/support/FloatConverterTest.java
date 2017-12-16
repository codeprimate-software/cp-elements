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

package org.cp.elements.data.conversion.support;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.cp.elements.data.conversion.ConversionException;
import org.junit.Test;

/**
 * The FloatConverterTest class is a test suite of test cases testing the contract and functionality of the
 * FloatConverter class.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.conversion.support.FloatConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class FloatConverterTest {

  private final FloatConverter converter = new FloatConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(Float.class, Float.class));
    assertTrue(converter.canConvert(Number.class, Float.class));
    assertTrue(converter.canConvert(Integer.class, Float.class));
    assertTrue(converter.canConvert(Double.class, Float.class));
    assertTrue(converter.canConvert(BigInteger.class, Float.class));
    assertTrue(converter.canConvert(BigDecimal.class, Float.class));
    assertTrue(converter.canConvert(String.class, Float.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(null, Float.class));
    assertFalse(converter.canConvert(Float.class, null));
    assertFalse(converter.canConvert(Float.class, Float.TYPE));
    assertFalse(converter.canConvert(Float.TYPE, Float.class));
    assertFalse(converter.canConvert(Float.class, Number.class));
    assertFalse(converter.canConvert(Float.class, Double.class));
    assertFalse(converter.canConvert(Float.class, Integer.class));
    assertFalse(converter.canConvert(Float.class, String.class));
    assertFalse(converter.canConvert(Character.class, Float.class));
    assertFalse(converter.canConvert(Boolean.class, Float.class));
  }

  @Test
  public void testConvert() {
    Float expected = 3.14159f;
    Float actual = converter.convert(expected);

    assertNotNull(expected);
    assertNotSame(expected, actual);
    assertEquals(expected, actual);
  }

  @Test
  public void testConvertNumber() {
    Double expectedValue = Math.PI;
    Float actual = converter.convert(expectedValue);

    assertNotNull(actual);
    assertNotSame(expectedValue, actual);
    assertEquals(expectedValue.floatValue(), actual, 0.0f);

    Integer expected = 11235;

    actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected.floatValue(), actual, 0.0f);
  }

  @Test
  public void testConvertNegativeNumber() {
    Float expected = -11.235813f;
    Float actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected, actual);
  }

  @Test
  public void testConvertString() {
    String expected = "123.45";
    Float actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(Float.parseFloat(expected), actual, 0.0f);
  }

  @Test(expected = ConversionException.class)
  public void testConvertInvalidFloat() {
    try {
      converter.convert("one.twothree");
    }
    catch (ConversionException e) {
      assertEquals("The Object value (one.twothree) is not a valid float!", e.getMessage());
      throw e;
    }
  }

}
