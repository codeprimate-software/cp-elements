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
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.cp.elements.data.convert.ConversionException;
import org.junit.Test;

/**
 * The IntegerConverterTest class is a test suite of test cases testing the contract and functionality of the
 * IntegerConverter class.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.convert.support.IntegerConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class IntegerConverterTest {

  private final IntegerConverter converter = new IntegerConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(Integer.class, Integer.class));
    assertTrue(converter.canConvert(Number.class, Integer.class));
    assertTrue(converter.canConvert(Long.class, Integer.class));
    assertTrue(converter.canConvert(Double.class, Integer.class));
    assertTrue(converter.canConvert(BigInteger.class, Integer.class));
    assertTrue(converter.canConvert(BigDecimal.class, Integer.class));
    assertTrue(converter.canConvert(String.class, Integer.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(null, Integer.class));
    assertFalse(converter.canConvert(Integer.class, null));
    assertFalse(converter.canConvert(Integer.class, Integer.TYPE));
    assertFalse(converter.canConvert(Integer.TYPE, Integer.class));
    assertFalse(converter.canConvert(Integer.class, String.class));
    assertFalse(converter.canConvert(Integer.class, Number.class));
    assertFalse(converter.canConvert(Integer.class, Long.class));
    assertFalse(converter.canConvert(Integer.class, Double.class));
    assertFalse(converter.canConvert(Integer.class, BigInteger.class));
    assertFalse(converter.canConvert(Character.class, Integer.class));
    assertFalse(converter.canConvert(Boolean.class, Integer.class));
  }

  @Test
  public void testConvert() {
    Integer expected = 2;
    Integer actual = converter.convert(expected);

    assertNotNull(actual);
    assertSame(expected, actual);
    assertEquals(expected, actual);
  }

  @Test
  public void testConvertBigInteger() {
    BigInteger expected = new BigInteger("123456789");
    Integer actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected.intValue(), actual.intValue());
  }

  @Test
  public void testConvertDouble() {
    Double expected = 42.0d;
    Integer actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected.intValue(), actual.intValue());
  }

  @Test
  public void testConvertNegativeNumber() {
    Double expected = -42.0d;
    Integer actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected.intValue(), actual.intValue());
  }

  @Test
  public void testConvertString() {
    String expected = "123";
    Integer actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotNull(expected, actual);
    assertEquals(Integer.parseInt(expected), actual.intValue());
  }

  @Test(expected = ConversionException.class)
  public void testConvertInvalidInteger() {
    try {
      converter.convert("one");
    }
    catch (ConversionException expected) {
      assertEquals("The Object value (one) is not a valid integer!", expected.getMessage());
      throw expected;
    }
  }

}
