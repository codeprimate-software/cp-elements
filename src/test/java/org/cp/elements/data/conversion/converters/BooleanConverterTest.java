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

package org.cp.elements.data.conversion.converters;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * The BooleanConverterTest class is a test suite of test cases testing the contract and functionality of the
 * BooleanConverter class.
 *
 * @author John Blum
 * @see org.cp.elements.data.conversion.converters.BooleanConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class BooleanConverterTest {

  private final BooleanConverter converter = new BooleanConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(Boolean.class, Boolean.class));
    assertTrue(converter.canConvert(Boolean.class, Boolean.TYPE));
    assertTrue(converter.canConvert(Object.class, Boolean.TYPE));
    assertTrue(converter.canConvert(Object.class, Boolean.class));
    assertTrue(converter.canConvert(Integer.class, Boolean.class));
    assertTrue(converter.canConvert(String.class, Boolean.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(null, Boolean.class));
    assertFalse(converter.canConvert(Boolean.class, null));
    assertFalse(converter.canConvert(Boolean.class, Object.class));
    assertFalse(converter.canConvert(Boolean.class, Integer.class));
    assertFalse(converter.canConvert(Boolean.class, String.class));
  }

  @Test
  public void testConvertBoolean() {
    assertTrue(converter.convert(Boolean.TRUE));
    assertTrue(converter.convert(true));
    assertFalse(converter.convert(Boolean.FALSE));
    assertFalse(converter.convert(false));
  }

  @Test
  public void testConvertBooleanStrings() {
    assertTrue(converter.convert("true"));
    assertTrue(converter.convert("True"));
    assertTrue(converter.convert("TRUE"));
    assertFalse(converter.convert("false"));
    assertFalse(converter.convert("False"));
    assertFalse(converter.convert("FALSE"));
  }

  @Test
  public void testConvertString() {
    assertFalse(converter.convert("yes"));
    assertFalse(converter.convert("y"));
    assertFalse(converter.convert("positive"));
    assertFalse(converter.convert("1"));
    assertFalse(converter.convert("0"));
    assertFalse(converter.convert("negative"));
    assertFalse(converter.convert("n"));
    assertFalse(converter.convert("no"));
  }

  @Test
  public void testConvertTrueAndFalseStrings() {
    final BooleanConverter converter = new BooleanConverter("yes", "y", "1");

    assertTrue(converter.convert(Boolean.TRUE));
    assertTrue(converter.convert(true));
    assertTrue(converter.convert("YES"));
    assertTrue(converter.convert("Yes"));
    assertTrue(converter.convert("yes"));
    assertTrue(converter.convert("Y"));
    assertTrue(converter.convert("y"));
    assertTrue(converter.convert("1"));
    assertFalse(converter.convert("yeah"));
    assertFalse(converter.convert("si"));
    assertFalse(converter.convert("why"));
    assertFalse(converter.convert("one"));
    assertFalse(converter.convert("-1"));
    assertFalse(converter.convert("no"));
    assertFalse(converter.convert("null"));
    assertFalse(converter.convert("nil"));
    assertFalse(converter.convert(null));
  }

}
