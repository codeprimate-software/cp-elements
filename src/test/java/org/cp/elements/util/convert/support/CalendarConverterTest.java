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

import java.math.BigInteger;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import org.cp.elements.test.TestUtils;
import org.cp.elements.util.convert.ConversionException;
import org.junit.Test;

/**
 * The CalendarConverterTest class is a test suite of test cases testing the contract and functionality of the
 * CalendarConverter class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.convert.support.CalendarConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class CalendarConverterTest {

  private final CalendarConverter converter = new CalendarConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(Calendar.class, Calendar.class));
    assertTrue(converter.canConvert(Date.class, Calendar.class));
    assertTrue(converter.canConvert(Number.class, Calendar.class));
    assertTrue(converter.canConvert(String.class, Calendar.class));
    assertTrue(converter.canConvert(BigInteger.class, Calendar.class));
    assertTrue(converter.canConvert(Integer.class, Calendar.class));
    assertTrue(converter.canConvert(Long.class, Calendar.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(null, Calendar.class));
    assertFalse(converter.canConvert(Calendar.class, null));
    assertFalse(converter.canConvert(Calendar.class, Date.class));
    assertFalse(converter.canConvert(Calendar.class, Number.class));
    assertFalse(converter.canConvert(Calendar.class, String.class));
    assertFalse(converter.canConvert(Character.class, Calendar.class));
    assertFalse(converter.canConvert(Long.TYPE, Calendar.class));
  }

  @Test
  public void testConvertCalendar() {
    final Calendar expectedDateTime = TestUtils.createCalendar(2013, Calendar.NOVEMBER, 2);
    final Calendar actualDateTime = converter.convert(expectedDateTime);

    assertSame(expectedDateTime, actualDateTime);
  }

  @Test
  public void testConvertDate() {
    final Calendar expectedDateTime = TestUtils.createCalendar(1999, Calendar.MAY, 15);
    final Calendar actualDateTime = converter.convert(expectedDateTime.getTime());

    assertNotNull(actualDateTime);
    assertNotSame(expectedDateTime, actualDateTime);
    assertEquals(expectedDateTime, actualDateTime);
  }

  @Test
  public void testConvertNumber() {
    final Calendar expectedDateTime = TestUtils.createCalendar(2010, Calendar.NOVEMBER, 10);
    final Calendar actualDateTime = converter.convert(expectedDateTime.getTimeInMillis());

    assertNotNull(actualDateTime);
    assertNotSame(expectedDateTime, actualDateTime);
    assertEquals(expectedDateTime, actualDateTime);
  }

  @Test
  public void testConvertStringNumber() {
    final Calendar expectedDateTime = TestUtils.createCalendar(2012, Calendar.DECEMBER, 12);
    final Calendar actualDateTime = converter.convert(String.valueOf(expectedDateTime.getTimeInMillis()));

    assertNotNull(actualDateTime);
    assertNotSame(expectedDateTime, actualDateTime);
    assertEquals(expectedDateTime, actualDateTime);
  }

  @Test
  public void testConvertStringDateTime() {
    final Calendar expectedDateTime = TestUtils.createCalendar(1974, Calendar.MAY, 27);
    final Calendar actualDateTime = converter.convert(new SimpleDateFormat(CalendarConverter.DEFAULT_PATTERN).format(
      expectedDateTime.getTime()));

    assertNotNull(actualDateTime);
    assertNotSame(expectedDateTime, actualDateTime);
    assertEquals(expectedDateTime, actualDateTime);
  }

  @Test(expected = ConversionException.class)
  public void testConvertInvalidDateTimeStringFormat() {
    try {
      converter.convert("10/31/2013");
    }
    catch (ConversionException e) {
      assertEquals("The Object value (10/31/2013) is not a valid date/time!", e.getMessage());
      throw e;
    }
  }

  @Test(expected = ConversionException.class)
  public void testInvalidString() {
    try {
      converter.convert("Once upon a time...");
    }
    catch (ConversionException e) {
      assertEquals("The Object value (Once upon a time...) is not a valid date/time!", e.getMessage());
      throw e;
    }
  }

  @Test
  public void testConvertValidDateString() {
    final CalendarConverter converter = new CalendarConverter("MMMMM dd, yyyy");
    final Calendar actualDateTime = converter.convert("November 02, 2013");

    assertNotNull(actualDateTime);
    assertEquals(2, actualDateTime.get(Calendar.DAY_OF_MONTH));
    assertEquals(Calendar.NOVEMBER, actualDateTime.get(Calendar.MONTH));
    assertEquals(2013, actualDateTime.get(Calendar.YEAR));
  }

  @Test
  public void testConvertValidDateTimeString() {
    final CalendarConverter converter = new CalendarConverter("MMM dd, yyyy @ hh:mm a");
    final Calendar actualDateTime = converter.convert("Oct 31, 2013 @ 5:15 pm");

    assertNotNull(actualDateTime);
    assertEquals(31, actualDateTime.get(Calendar.DAY_OF_MONTH));
    assertEquals(Calendar.OCTOBER, actualDateTime.get(Calendar.MONTH));
    assertEquals(2013, actualDateTime.get(Calendar.YEAR));
    assertEquals(5, actualDateTime.get(Calendar.HOUR));
    assertEquals(17, actualDateTime.get(Calendar.HOUR_OF_DAY));
    assertEquals(15, actualDateTime.get(Calendar.MINUTE));
  }

}
