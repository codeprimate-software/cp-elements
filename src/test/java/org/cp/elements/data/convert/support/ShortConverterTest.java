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

import org.cp.elements.data.convert.ConversionException;
import org.junit.Test;

/**
 * The ShortConverterTest class is a test suite of test cases testing the contract and functionality of the
 * ShortConverter class.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.convert.support.ShortConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class ShortConverterTest {

  private final ShortConverter converter = new ShortConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(Short.class, Short.class));
    assertTrue(converter.canConvert(Number.class, Short.class));
    assertTrue(converter.canConvert(Integer.class, Short.class));
    assertTrue(converter.canConvert(Double.class, Short.class));
    assertTrue(converter.canConvert(String.class, Short.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(null, Short.class));
    assertFalse(converter.canConvert(Short.class, null));
    assertFalse(converter.canConvert(Short.class, Short.TYPE));
    assertFalse(converter.canConvert(Short.TYPE, Short.class));
    assertFalse(converter.canConvert(Short.class, String.class));
    assertFalse(converter.canConvert(Short.class, Number.class));
    assertFalse(converter.canConvert(Character.class, Short.class));
    assertFalse(converter.canConvert(Boolean.class, Short.class));
  }

  @Test
  public void testConvert() {
    Short expected = (short) 1024;
    Short actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected, actual);
  }

  @Test
  public void testConvertNumber() {
    Integer expected = 8192;
    Short actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected.shortValue(), actual.shortValue());
  }

  @Test
  public void testConvertNegativeNumber() {
    Integer expected = -99;
    Short actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected.shortValue(), actual.shortValue());
  }

  @Test
  public void testConvertString() {
    String expected = "4096";
    Short actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(Short.parseShort(expected), actual.shortValue());
  }

  @Test(expected = ConversionException.class)
  public void testConvertInvalidShort() {
    try {
      converter.convert("test");
    }
    catch (ConversionException expected) {
      assertEquals("The Object value (test) is not a valid short!", expected.getMessage());
      throw expected;
    }
  }

}
