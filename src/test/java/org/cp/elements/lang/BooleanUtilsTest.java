/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 *
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 *
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 *
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 *
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang;

import static org.junit.Assert.*;

import org.junit.Test;

/**
 * The BooleanUtilsTest class is a test suite of test cases testing the contract and functionality of the
 * BooleanUtils class.
 * </p>
 * @author John J. Blum
 * @see java.lang.Boolean
 * @see org.cp.elements.lang.BooleanUtils
 * @see org.junit.Assert
 * @see org.junit.Test
 * @since 1.0.0
 */
public class BooleanUtilsTest {

  @Test
  public void testAnd() {
    assertFalse(BooleanUtils.and((Boolean[]) null));
    assertFalse(BooleanUtils.and(null, null));
    assertFalse(BooleanUtils.and(true, null));
    assertFalse(BooleanUtils.and(null, true));
    assertFalse(BooleanUtils.and(true, true, true, null));
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
    assertTrue(BooleanUtils.and(true));
    assertTrue(BooleanUtils.and(Boolean.TRUE));
    assertTrue(BooleanUtils.and(true, true, true));
    assertTrue(BooleanUtils.and(Boolean.TRUE, Boolean.TRUE, Boolean.TRUE));
    assertTrue(BooleanUtils.and(!Boolean.FALSE));
    assertTrue(BooleanUtils.and(true, !Boolean.FALSE));
    assertTrue(BooleanUtils.and(!Boolean.FALSE, !Boolean.FALSE));
  }

  @Test
  public void testNegate() {
    assertTrue(BooleanUtils.negate(null));
    assertTrue(BooleanUtils.negate(false));
    assertTrue(BooleanUtils.negate(Boolean.FALSE));
    assertFalse(BooleanUtils.negate(true));
    assertFalse(BooleanUtils.negate(Boolean.TRUE));
  }

  @Test
  public void testOr() {
    assertFalse(BooleanUtils.or((Boolean[]) null));
    assertFalse(BooleanUtils.or(null, null));
    assertFalse(BooleanUtils.or(false));
    assertFalse(BooleanUtils.or(Boolean.FALSE));
    assertFalse(BooleanUtils.or(false, false, false));
    assertFalse(BooleanUtils.or(Boolean.FALSE, Boolean.FALSE, Boolean.FALSE));
    assertFalse(BooleanUtils.or(!Boolean.TRUE, false));
    assertFalse(BooleanUtils.or(!Boolean.TRUE, Boolean.FALSE));
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
  public void testToBoolean() {
    assertSame(Boolean.FALSE, BooleanUtils.toBoolean(false));
    assertSame(Boolean.TRUE, BooleanUtils.toBoolean(true));
  }

  @Test
  public void testToString() {
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
  public void testValueOf() {
    assertFalse(BooleanUtils.valueOf(null));
    assertFalse(BooleanUtils.valueOf(false));
    assertFalse(BooleanUtils.valueOf(Boolean.FALSE));
    assertTrue(BooleanUtils.valueOf(true));
    assertTrue(BooleanUtils.valueOf(Boolean.TRUE));
  }

  @Test
  public void testXor() {
    assertFalse(BooleanUtils.xor((Boolean[]) null));
    assertFalse(BooleanUtils.xor(null, null));
    assertFalse(BooleanUtils.xor(false));
    assertFalse(BooleanUtils.xor(Boolean.FALSE));
    assertFalse(BooleanUtils.xor(false, false, false));
    assertFalse(BooleanUtils.xor(Boolean.FALSE, Boolean.FALSE, Boolean.FALSE));
    assertFalse(BooleanUtils.xor(false, !Boolean.TRUE, false));
    assertFalse(BooleanUtils.xor(true, false, true));
    assertFalse(BooleanUtils.xor(true, true));
    assertFalse(BooleanUtils.xor(true, Boolean.TRUE));
    assertFalse(BooleanUtils.xor(Boolean.TRUE, Boolean.TRUE));
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

}
