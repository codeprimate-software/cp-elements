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

import java.util.Calendar;

import org.cp.elements.test.TestUtils;
import org.junit.Test;

/**
 * The DateTimeUtilsTest class is a test suite of test cases testing the contract and functionality of the DateTimeUtils 
 * class.
 * <p/>
 * @author John J. Blum
 * @since 1.0.0
 * @see java.util.Calendar
 * @see org.cp.elements.lang.DateTimeUtils
 * @see org.cp.elements.test.TestUtils
 * @see org.junit.Assert
 * @see org.junit.Test
 */
public class DateTimeUtilsTest {

  @Test
  public void testClone() {
    final Calendar expectedDateTime = TestUtils.createCalendar(2011, Calendar.NOVEMBER, 9, 1, 45, 30);
    final Calendar actualDateTime = DateTimeUtils.clone(expectedDateTime);

    assertNotNull(actualDateTime);
    assertNotSame(expectedDateTime, actualDateTime);
    assertEquals(expectedDateTime, actualDateTime);
  }

  @Test
  public void testCloneWithNull() {
    assertNull(DateTimeUtils.clone(null));
  }

  @Test
  public void testCreate() {
    final Calendar expectedDateTime = TestUtils.createCalendar(2013, Calendar.OCTOBER, 19, 10, 36, 0);
    final Calendar actualDateTime = DateTimeUtils.create(expectedDateTime.getTimeInMillis());

    assertNotNull(actualDateTime);
    assertNotSame(expectedDateTime, actualDateTime);
    assertEquals(expectedDateTime, actualDateTime);
  }

  @Test
  public void testTruncate() {
    final Calendar dateTime = TestUtils.createCalendar(2011, Calendar.NOVEMBER, 8, 16, 15, 30);
    final Calendar expectedDateTime = TestUtils.createCalendar(2011, Calendar.NOVEMBER, 8);
    final Calendar actualDateTime = DateTimeUtils.truncate(dateTime);

    assertNotNull(actualDateTime);
    assertSame(dateTime, actualDateTime);
    assertEquals(expectedDateTime, actualDateTime);
  }

  @Test
  public void testTruncateWithNoTime() {
    final Calendar expectedDateTime = TestUtils.createCalendar(2011, Calendar.NOVEMBER, 7);

    assertNotNull(expectedDateTime);
    assertEquals(0, expectedDateTime.get(Calendar.HOUR_OF_DAY));
    assertEquals(0, expectedDateTime.get(Calendar.MINUTE));
    assertEquals(0, expectedDateTime.get(Calendar.SECOND));
    assertEquals(0, expectedDateTime.get(Calendar.MILLISECOND));

    final Calendar actualDateTime = DateTimeUtils.truncate(expectedDateTime);

    assertNotNull(actualDateTime);
    assertSame(expectedDateTime, actualDateTime);
    assertEquals(0, actualDateTime.get(Calendar.HOUR_OF_DAY));
    assertEquals(0, actualDateTime.get(Calendar.MINUTE));
    assertEquals(0, actualDateTime.get(Calendar.SECOND));
    assertEquals(0, actualDateTime.get(Calendar.MILLISECOND));
  }

  @Test
  public void testTruncateWithNull() {
    assertNull(DateTimeUtils.truncate(null));
  }

}
