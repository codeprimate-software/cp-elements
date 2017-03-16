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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.Test;

/**
 * Unit tests for {@link BooleanUtils}.
 *
 * @author John J. Blum
 * @see java.lang.Boolean
 * @see java.util.concurrent.atomic.AtomicBoolean
 * @see org.junit.Test
 * @see org.cp.elements.lang.BooleanUtils
 * @since 1.0.0
 */
public class BooleanUtilsTests {

  @Test
  public void andIsFalse() {
    assertFalse(BooleanUtils.and(true, true, true, false));
    assertFalse(BooleanUtils.and(true, true, true, Boolean.FALSE));
    assertFalse(BooleanUtils.and(false, true, true, true));
    assertFalse(BooleanUtils.and(Boolean.FALSE, true, true, true));
    assertFalse(BooleanUtils.and(true, false, true));
    assertFalse(BooleanUtils.and(true, Boolean.FALSE, true));
    assertFalse(BooleanUtils.and(false));
    assertFalse(BooleanUtils.and(Boolean.FALSE));
    assertFalse(BooleanUtils.and(!Boolean.TRUE));
    assertFalse(BooleanUtils.and(!Boolean.TRUE, !Boolean.TRUE));
    assertFalse(BooleanUtils.and(!Boolean.TRUE, false));
  }

  @Test
  public void andIsTrue() {
    assertTrue(BooleanUtils.and(true));
    assertTrue(BooleanUtils.and(Boolean.TRUE));
    assertTrue(BooleanUtils.and(true, true, true));
    assertTrue(BooleanUtils.and(Boolean.TRUE, Boolean.TRUE, Boolean.TRUE));
    assertTrue(BooleanUtils.and(!Boolean.FALSE));
    assertTrue(BooleanUtils.and(true, !Boolean.FALSE));
    assertTrue(BooleanUtils.and(!Boolean.FALSE, !Boolean.FALSE));
  }

  @Test
  public void andNullIsFalse() {
    assertFalse(BooleanUtils.and((Boolean[]) null));
    assertFalse(BooleanUtils.and(null, null));
    assertFalse(BooleanUtils.and(true, null));
    assertFalse(BooleanUtils.and(null, true));
    assertFalse(BooleanUtils.and(true, true, true, null));
  }

  @Test
  public void negateFalseIsTrue() {
    assertTrue(BooleanUtils.negate(false));
    assertTrue(BooleanUtils.negate(Boolean.FALSE));
  }

  @Test
  public void negateNullIsTrue() {
    assertTrue(BooleanUtils.negate(null));
  }

  @Test
  public void negateTrueIsFalse() {
    assertFalse(BooleanUtils.negate(true));
    assertFalse(BooleanUtils.negate(Boolean.TRUE));
  }

  @Test
  public void orIsFalse() {
    assertFalse(BooleanUtils.or(false));
    assertFalse(BooleanUtils.or(Boolean.FALSE));
    assertFalse(BooleanUtils.or(false, false, false));
    assertFalse(BooleanUtils.or(Boolean.FALSE, Boolean.FALSE, Boolean.FALSE));
    assertFalse(BooleanUtils.or(!Boolean.TRUE, false));
    assertFalse(BooleanUtils.or(!Boolean.TRUE, Boolean.FALSE));
  }

  @Test
  public void orIsTrue() {
    assertTrue(BooleanUtils.or(true));
    assertTrue(BooleanUtils.or(Boolean.TRUE));
    assertTrue(BooleanUtils.or(false, true));
    assertTrue(BooleanUtils.or(true, false));
    assertTrue(BooleanUtils.or(true, false, false, false));
    assertTrue(BooleanUtils.or(false, false, false, true));
    assertTrue(BooleanUtils.or(!Boolean.FALSE));
    assertTrue(BooleanUtils.or(!Boolean.FALSE, false));
  }

  @Test
  public void orNullIsFalse() {
    assertFalse(BooleanUtils.or((Boolean[]) null));
    assertFalse(BooleanUtils.or(null, null));
  }

  @Test
  public void toBooleanIsSuccessful() {
    assertSame(Boolean.FALSE, BooleanUtils.toBoolean(false));
    assertSame(Boolean.TRUE, BooleanUtils.toBoolean(true));
  }

  @Test
  public void toStringIsSuccessful() {
    assertEquals("true", BooleanUtils.toString(true, "true", "false"));
    assertEquals("false", BooleanUtils.toString(false, "true", "false"));
    assertEquals("Yes", BooleanUtils.toString(true, "Yes", "No"));
    assertEquals("No", BooleanUtils.toString(false, "Yes", "No"));
    assertEquals("1", BooleanUtils.toString(true, "1", "0"));
    assertEquals("0", BooleanUtils.toString(false, "1", "0"));
    assertEquals("false", BooleanUtils.toString(true, "false", "true"));
    assertEquals("true", BooleanUtils.toString(false, "false", "true"));
    assertEquals("null", BooleanUtils.toString(true, "null", null));
    assertNull(BooleanUtils.toString(false, "null", null));
    assertEquals("not null", BooleanUtils.toString(Boolean.TRUE, "not null", "null"));
    assertEquals("null", BooleanUtils.toString(Boolean.FALSE, "not null", "null"));
    assertEquals("null", BooleanUtils.toString(null, "not null", "null"));
  }

  @Test
  public void valueOfAtomicBooleanIsFalse() {
    assertFalse(BooleanUtils.valueOf(new AtomicBoolean(false)));
  }

  @Test
  public void valueOfAtomicBooleanIsTrue() {
    assertTrue(BooleanUtils.valueOf(new AtomicBoolean(true)));
  }

  @Test
  public void valueOfNullAtomicBooleanIsFalse() {
    assertFalse(BooleanUtils.valueOf((AtomicBoolean) null));
  }

  @Test
  public void valueOfBooleanIsFalse() {
    assertFalse(BooleanUtils.valueOf(false));
    assertFalse(BooleanUtils.valueOf(Boolean.FALSE));
  }

  @Test
  public void valueOfBooleanIsTrue() {
    assertTrue(BooleanUtils.valueOf(true));
    assertTrue(BooleanUtils.valueOf(Boolean.TRUE));
  }

  @Test
  public void valueOfNullBooleanIsFalse() {
    assertFalse(BooleanUtils.valueOf((Boolean) null));
  }

  @Test
  public void xorIsFalse() {
    assertFalse(BooleanUtils.xor(false));
    assertFalse(BooleanUtils.xor(Boolean.FALSE));
    assertFalse(BooleanUtils.xor(false, false, false));
    assertFalse(BooleanUtils.xor(Boolean.FALSE, Boolean.FALSE, Boolean.FALSE));
    assertFalse(BooleanUtils.xor(false, !Boolean.TRUE, false));
    assertFalse(BooleanUtils.xor(true, false, true));
    assertFalse(BooleanUtils.xor(true, true));
    assertFalse(BooleanUtils.xor(true, Boolean.TRUE));
    assertFalse(BooleanUtils.xor(Boolean.TRUE, Boolean.TRUE));
  }

  @Test
  public void xorIsTrue() {
    assertTrue(BooleanUtils.xor(true));
    assertTrue(BooleanUtils.xor(Boolean.TRUE));
    assertTrue(BooleanUtils.xor(true, null));
    assertTrue(BooleanUtils.xor(true, false));
    assertTrue(BooleanUtils.xor(true, Boolean.FALSE));
    assertTrue(BooleanUtils.xor(true, !Boolean.TRUE));
    assertTrue(BooleanUtils.xor(true, false, false, false));
    assertTrue(BooleanUtils.xor(false, true));
    assertTrue(BooleanUtils.xor(false, false, false, true));
  }

  @Test
  public void xorNullIsFalse() {
    assertFalse(BooleanUtils.xor((Boolean[]) null));
    assertFalse(BooleanUtils.xor(null, null));
  }
}
