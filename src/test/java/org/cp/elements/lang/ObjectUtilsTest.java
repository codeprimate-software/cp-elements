/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang;

import static org.junit.Assert.*;

import org.junit.Test;

/**
 * The ObjectUtilsTest class is a test suite of test cases testing the contract and functionality
 * of the ObjectUtils class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.ObjectUtils
 * @see org.junit.Assert
 * @see org.junit.Test
 * @since 1.0.0
 */
public class ObjectUtilsTest {

  @Test
  public void testDefaultIfNullUsingNonNullValues() {
    assertEquals("test", ObjectUtils.defaultIfNull("test", null, null, null));
    assertEquals("test", ObjectUtils.defaultIfNull(null, "test"));
    assertEquals("test", ObjectUtils.defaultIfNull(null, null, null, "test"));
    assertEquals("test", ObjectUtils.defaultIfNull(null, null, "test", null));
    assertEquals("null", ObjectUtils.defaultIfNull("null", (Object[]) null));
    assertEquals("mock", ObjectUtils.defaultIfNull("mock", "test"));
    assertEquals("nil", ObjectUtils.defaultIfNull("nil", "test"));
    assertEquals("null", ObjectUtils.defaultIfNull("null", "test"));
  }

  @Test
  public void testDefaultIfNullUsingNullValues() {
    assertNull(ObjectUtils.defaultIfNull((Object[]) null));
    assertNull(ObjectUtils.defaultIfNull(null, (Object[]) null));
    assertNull(ObjectUtils.defaultIfNull(null, null, null));
  }

  @Test
  @SuppressWarnings("boxing")
  public void testEquals() {
    final Object testObject = new Object();
    assertTrue(ObjectUtils.equals(testObject, testObject));
    assertTrue(ObjectUtils.equals(true, Boolean.TRUE));
    assertTrue(ObjectUtils.equals('c', new Character('c')));
    assertTrue(ObjectUtils.equals(1, new Integer(1)));
    assertTrue(ObjectUtils.equals(Math.PI, new Double(Math.PI)));
    assertTrue(ObjectUtils.equals("test", "test"));
  }

  @Test
  public void testEqualsUsingUnequalValues() {
    assertFalse(ObjectUtils.equals(null, null));
    assertFalse(ObjectUtils.equals("test", null));
    assertFalse(ObjectUtils.equals(null, "test"));
    assertFalse(ObjectUtils.equals(true, false));
    assertFalse(ObjectUtils.equals('c', 'C'));
    assertFalse(ObjectUtils.equals('c', "c"));
    assertFalse(ObjectUtils.equals(-2, 2));
    assertFalse(ObjectUtils.equals(3.14159d, Math.PI));
    assertFalse(ObjectUtils.equals("test", "TEST"));
    assertFalse(ObjectUtils.equals("test", "testing"));
    assertFalse(ObjectUtils.equals("test", "mock"));
  }

  @Test
  public void testEqualsIgnoreNull() {
    assertTrue(ObjectUtils.equalsIgnoreNull(null, null));
    assertTrue(ObjectUtils.equalsIgnoreNull("null", "null"));
    assertTrue(ObjectUtils.equalsIgnoreNull(Boolean.FALSE, Boolean.FALSE));
    assertTrue(ObjectUtils.equalsIgnoreNull('\0', '\0'));
    assertTrue(ObjectUtils.equalsIgnoreNull(0, -0));
    assertTrue(ObjectUtils.equalsIgnoreNull(Integer.MIN_VALUE, Integer.MAX_VALUE + 1));
    assertTrue(ObjectUtils.equalsIgnoreNull(Math.PI, Math.PI));
    assertTrue(ObjectUtils.equalsIgnoreNull("testing", "testing"));
  }

  @Test
  public void testEqualsIgnoreNullUsingUnequalValues() {
    assertFalse(ObjectUtils.equalsIgnoreNull("null", null));
    assertFalse(ObjectUtils.equalsIgnoreNull(null, "null"));
    assertFalse(ObjectUtils.equalsIgnoreNull("nil", "null"));
    assertFalse(ObjectUtils.equalsIgnoreNull(Boolean.FALSE, Boolean.TRUE));
    assertFalse(ObjectUtils.equalsIgnoreNull('\0', null));
    assertFalse(ObjectUtils.equalsIgnoreNull(0, 1));
    assertFalse(ObjectUtils.equalsIgnoreNull(Integer.MIN_VALUE, Integer.MAX_VALUE));
    assertFalse(ObjectUtils.equalsIgnoreNull(Double.MIN_VALUE, Double.MAX_VALUE));
    assertFalse(ObjectUtils.equalsIgnoreNull(0.0d, -0.0d));
    assertFalse(ObjectUtils.equalsIgnoreNull("gnitset", "testing"));
  }

  @Test
  public void testHashCode() {
    assertEquals("test".hashCode(), ObjectUtils.hashCode("test"));
  }

  @Test
  public void testHashCodeWithNull() {
    assertEquals(0, ObjectUtils.hashCode(null));
  }

  @Test
  public void testToString() {
    assertNull(ObjectUtils.toString(null));
    assertEquals("true", ObjectUtils.toString(Boolean.TRUE));
    assertEquals("\0", ObjectUtils.toString('\0'));
    assertEquals("c", ObjectUtils.toString('c'));
    assertEquals("8192", ObjectUtils.toString(8192));
    assertEquals("3.14159", ObjectUtils.toString(3.14159d));
    assertEquals("test", ObjectUtils.toString("test"));
    assertEquals("null", ObjectUtils.toString("null"));
    assertEquals("nil", ObjectUtils.toString("nil"));
  }

}
