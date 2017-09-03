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
 * The BigDecimalConverterTest class is a test suite of test cases testing the contract and functionality of the
 * BigDecimalConverter class.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.convert.support.BigDecimalConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class BigDecimalConverterTest {

  private BigDecimalConverter converter = new BigDecimalConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(String.class, BigDecimal.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(BigDecimal.class, BigDecimal.class));
    assertFalse(converter.canConvert(null, BigDecimal.class));
    assertFalse(converter.canConvert(BigDecimal.class, null));
    assertFalse(converter.canConvert(BigDecimal.class, Double.class));
    assertFalse(converter.canConvert(BigDecimal.class, Float.class));
    assertFalse(converter.canConvert(BigDecimal.class, Integer.class));
    assertFalse(converter.canConvert(BigDecimal.class, Long.class));
    assertFalse(converter.canConvert(BigDecimal.class, BigInteger.class));
    assertFalse(converter.canConvert(BigDecimal.class, String.class));
    assertFalse(converter.canConvert(String.class, String.class));
    assertFalse(converter.canConvert(null, String.class));
    assertFalse(converter.canConvert(String.class, null));
    assertFalse(converter.canConvert(String.class, Double.class));
    assertFalse(converter.canConvert(String.class, Float.class));
    assertFalse(converter.canConvert(String.class, Integer.class));
    assertFalse(converter.canConvert(String.class, Long.class));
    assertFalse(converter.canConvert(String.class, BigInteger.class));
    assertFalse(converter.canConvert(Character.class, BigDecimal.class));
    assertFalse(converter.canConvert(Boolean.class, BigDecimal.class));
  }

  @Test
  public void testConvert() {
    String expected = "123.45";
    BigDecimal actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(new BigDecimal(expected), actual);
  }

  @Test(expected = ConversionException.class)
  public void testConvertInvalidBigDecimal() {
    try {
      converter.convert("test");
    }
    catch (ConversionException expected) {
      assertEquals("The String value (test) is not a valid BigDecimal!", expected.getMessage());
      throw expected;
    }
  }

}
