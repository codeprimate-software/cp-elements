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
import static org.junit.Assert.assertTrue;

import java.util.Calendar;

import org.junit.Test;

/**
 * The StringConverterTest class is a test suite of test cases testing the contract and functionality of the
 * StringConverter class.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.conversion.support.StringConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class StringConverterTest {

  private final StringConverter converter = new StringConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(null, String.class));
    assertTrue(converter.canConvert(String.class, String.class));
    assertTrue(converter.canConvert(Object.class, String.class));
    assertTrue(converter.canConvert(Number.class, String.class));
    assertTrue(converter.canConvert(Calendar.class, String.class));
    assertTrue(converter.canConvert(Boolean.class, String.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(String.class, null));
    assertFalse(converter.canConvert(String.class, Character.class));
    assertFalse(converter.canConvert(String.class, Object.class));
  }

  @Test
  public void testConvert() {
    assertEquals("null", converter.convert(null));
    assertEquals("true", converter.convert(Boolean.TRUE));
    assertEquals("X", converter.convert('X'));
    assertEquals("42", converter.convert(42));
    assertEquals("3.14159", converter.convert(3.14159d));
    assertEquals("test", converter.convert("test"));
  }

  @Test
  public void testConvertObject() {
    Object obj = new Object();
    assertEquals(obj.toString(), converter.convert(obj));
  }

}
