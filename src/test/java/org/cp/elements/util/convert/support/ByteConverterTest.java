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

package org.cp.elements.util.convert.support;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Date;

import org.cp.elements.util.convert.ConversionException;
import org.junit.Test;

/**
 * The ByteConverterTest class is a test suite of test cases testing the contract and functionality of the ByteConverter
 * class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.util.convert.support.ByteConverter
 * @since 1.0.0
 */
public class ByteConverterTest {

  private final ByteConverter converter = new ByteConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(Byte.class, Byte.class));
    assertTrue(converter.canConvert(Number.class, Byte.class));
    assertTrue(converter.canConvert(Integer.class, Byte.class));
    assertTrue(converter.canConvert(Double.class, Byte.class));
    assertTrue(converter.canConvert(BigInteger.class, Byte.class));
    assertTrue(converter.canConvert(BigDecimal.class, Byte.class));
    assertTrue(converter.canConvert(String.class, Byte.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(null, Byte.class));
    assertFalse(converter.canConvert(Byte.class, null));
    assertFalse(converter.canConvert(Byte.TYPE, Byte.class));
    assertFalse(converter.canConvert(Byte.class, Byte.TYPE));
    assertFalse(converter.canConvert(Byte.class, Number.class));
    assertFalse(converter.canConvert(Byte.class, String.class));
    assertFalse(converter.canConvert(Boolean.class, Byte.class));
    assertFalse(converter.canConvert(Character.class, Byte.class));
    assertFalse(converter.canConvert(Date.class, Byte.class));
    assertFalse(converter.canConvert(Object.class, Byte.class));
  }

  @Test
  public void testConvertByte() {
    assertEquals(new Byte((byte) 1), converter.convert((byte) 1));
  }

  @Test
  public void testConvertNegativeNumber() {
    assertEquals(new Byte((byte) -9), converter.convert(-9));
  }

  @Test
  public void testConvertString() {
    assertEquals(new Byte((byte) 64), converter.convert(" 64  "));
  }

  @Test(expected = ConversionException.class)
  public void testConvertInvalidByte() {
    try {
      converter.convert("two");
    }
    catch (ConversionException e) {
      assertEquals("The Object value (two) is not a valid byte!", e.getMessage());
      throw e;
    }
  }

  @Test(expected = ConversionException.class)
  public void testConvertBoolean() {
    try {
      converter.convert(true);
    }
    catch (ConversionException e) {
      assertEquals("The Object value (true) is not a valid byte!", e.getMessage());
      throw e;
    }
  }

}
