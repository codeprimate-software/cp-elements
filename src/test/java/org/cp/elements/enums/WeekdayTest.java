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

package org.cp.elements.enums;

import static org.junit.Assert.*;

import java.util.Calendar;

import org.junit.Test;

/**
 * The WeekdayTest class is a test suite of test cases testing the contract and functionality of the Weekday Enum.
 * <p/>
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
