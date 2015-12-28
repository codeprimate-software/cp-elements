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

package org.cp.elements.enums;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Calendar;

import org.junit.Test;

/**
 * The WeekdayTest class is a test suite of test cases testing the contract and functionality of the Weekday Enum.
 *
 * @author John J. Blum
 * @see org.cp.elements.enums.Weekday
 * @see org.junit.Test
 * @since 1.0.0
 */
public class WeekdayTest {

  protected static final int[] WEEKDAYS = {
    Calendar.SUNDAY,
    Calendar.MONDAY,
    Calendar.TUESDAY,
    Calendar.WEDNESDAY,
    Calendar.THURSDAY,
    Calendar.FRIDAY,
    Calendar.SATURDAY
  };

  @Test
  public void testIsWeekday() {
    assertFalse(Weekday.SUNDAY.isWeekday());
    assertTrue(Weekday.MONDAY.isWeekday());
    assertTrue(Weekday.TUESDAY.isWeekday());
    assertTrue(Weekday.WEDNESDAY.isWeekday());
    assertTrue(Weekday.THURSDAY.isWeekday());
    assertTrue(Weekday.FRIDAY.isWeekday());
    assertFalse(Weekday.SATURDAY.isWeekday());
  }

  @Test
  public void testIsWeekend() {
    assertTrue(Weekday.SUNDAY.isWeekend());
    assertFalse(Weekday.MONDAY.isWeekend());
    assertFalse(Weekday.TUESDAY.isWeekend());
    assertFalse(Weekday.WEDNESDAY.isWeekend());
    assertFalse(Weekday.THURSDAY.isWeekend());
    assertFalse(Weekday.FRIDAY.isWeekend());
    assertTrue(Weekday.SATURDAY.isWeekend());
  }

  @Test
  public void testValueOf() {
    int position = 1;

    for (Weekday weekday : Weekday.values()) {
      assertEquals(weekday, Weekday.valueOf(weekday.name()));
      assertEquals(WEEKDAYS[position - 1], weekday.getCalendarConstant());
      assertEquals(position++, weekday.getPosition());
    }
  }

  @Test
  public void testValueOfAbbreviation() {
    for (Weekday weekday : Weekday.values()) {
      assertEquals(weekday, Weekday.valueOfAbbreviation(weekday.getAbbreviation()));
    }

    assertEquals(Weekday.FRIDAY, Weekday.valueOfAbbreviation("fri"));
    assertEquals(Weekday.SATURDAY, Weekday.valueOfAbbreviation("Sat"));
    assertEquals(Weekday.SUNDAY, Weekday.valueOfAbbreviation("SUN"));
    assertNull(Weekday.valueOfAbbreviation("Sunday"));
    assertNull(Weekday.valueOfAbbreviation("May"));
    assertNull(Weekday.valueOfAbbreviation("2014"));
  }

  @Test
  public void testValueOfCalendarConstant() {
    int index = 0;

    for (Weekday weekday : Weekday.values()) {
      assertEquals(weekday, Weekday.valueOfCalendarConstant(WEEKDAYS[index++]));
    }

    assertEquals(7, index);
    assertNull(Weekday.valueOfCalendarConstant(Calendar.DECEMBER));
  }

  @Test
  public void testValueOfName() {
    for (Weekday weekday : Weekday.values()) {
      assertEquals(weekday, Weekday.valueOfName(weekday.getName()));
    }

    assertEquals(Weekday.FRIDAY, Weekday.valueOfName("friday"));
    assertEquals(Weekday.SATURDAY, Weekday.valueOfName("Saturday"));
    assertEquals(Weekday.SUNDAY, Weekday.valueOfName("SUNDAY"));
    assertNull(Weekday.valueOfName("Mon"));
    assertNull(Weekday.valueOfName("Tues"));
    assertNull(Weekday.valueOfName("Thurs"));
    assertNull(Weekday.valueOfName("October"));
    assertNull(Weekday.valueOfName("2014"));
  }

  @Test
  public void testValueOfPosition() {
    int position = 0;

    for (Weekday weekday : Weekday.values()) {
      assertEquals(weekday, Weekday.valueOfPosition(++position));
    }

    assertEquals(7, position);
    assertNull(Weekday.valueOfPosition(8));
    assertNull(Weekday.valueOfPosition(-7));
  }

}
