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

package org.cp.elements.util.convert.support;

import static org.junit.Assert.*;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.Calendar;
import java.util.Date;

import org.cp.elements.test.TestUtils;
import org.cp.elements.util.convert.ConversionException;
import org.junit.Test;

/**
 * The LongConverterTest class is a test suite of test cases testing the contract and functionality of the
 * LongConverter class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.convert.support.LongConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class LongConverterTest {

  private final LongConverter converter = new LongConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(Long.class, Long.class));
    assertTrue(converter.canConvert(Number.class, Long.class));
    assertTrue(converter.canConvert(Integer.class, Long.class));
    assertTrue(converter.canConvert(Double.class, Long.class));
    assertTrue(converter.canConvert(BigInteger.class, Long.class));
    assertTrue(converter.canConvert(BigDecimal.class, Long.class));
    assertTrue(converter.canConvert(Calendar.class, Long.class));
    assertTrue(converter.canConvert(Date.class, Long.class));
    assertTrue(converter.canConvert(String.class, Long.class));
    assertTrue(converter.canConvert(Timestamp.class, Long.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(null, Long.class));
    assertFalse(converter.canConvert(Long.class, null));
    assertFalse(converter.canConvert(Long.class, Long.TYPE));
    assertFalse(converter.canConvert(Long.TYPE, Long.class));
    assertFalse(converter.canConvert(Long.class, String.class));
    assertFalse(converter.canConvert(Long.class, Number.class));
    assertFalse(converter.canConvert(String.class, Number.class));
    assertFalse(converter.canConvert(Long.class, BigInteger.class));
    assertFalse(converter.canConvert(Character.class, Long.class));
    assertFalse(converter.canConvert(Boolean.class, Long.class));
  }

  @Test
  public void testConvert() {
    Long expected = 1000000000l;
    Long actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected, actual);
  }

  @Test
  public void testConvertCalendar() {
    Calendar now = TestUtils.createCalendar(2013, Calendar.NOVEMBER, 10, 19, 47, 30);
    Long actual = converter.convert(now);

    assertNotNull(actual);
    assertNotSame(now, actual);
    assertEquals(now.getTimeInMillis(), actual.longValue());
  }

  @Test
  public void testConvertDate() {
    Date date = TestUtils.createCalendar(1974, Calendar.MAY, 25).getTime();
    Long actual = converter.convert(date);

    assertNotNull(actual);
    assertNotSame(date, actual);
    assertEquals(date.getTime(), actual.longValue());
  }

  @Test
  public void testConvertNumber() {
    BigInteger expected = new BigInteger("1234567890");
    Long actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(expected.longValue(), actual.longValue());
  }

  @Test
  public void testConvertNegativeNumber() {
    String expected = "-987654321";
    Long actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(Long.parseLong(expected), actual.longValue());
  }

  @Test
  public void testConvertString() {
    String expected = "1123581321345589";
    Long actual = converter.convert(expected);

    assertNotNull(actual);
    assertNotSame(expected, actual);
    assertEquals(Long.parseLong(expected), actual.longValue());
  }

  @Test(expected = ConversionException.class)
  public void testConvertInvalidLong() {
    try {
      converter.convert("test");
    }
    catch (ConversionException expected) {
      assertEquals("The Object value (test) is not a valid long!", expected.getMessage());
      throw expected;
    }
  }

}
