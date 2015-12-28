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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import java.util.Calendar;

import org.cp.elements.test.TestUtils;
import org.junit.Test;

/**
 * The DateTimeUtilsTest class is a test suite of test cases testing the contract and functionality of the DateTimeUtils 
 * class.
 *
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
