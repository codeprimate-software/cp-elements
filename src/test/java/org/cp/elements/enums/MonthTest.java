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
 * The MonthTest class is a test suite of test cases testing the contract and functionality of the Month Enum.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.enums.Month
 * @see org.junit.Test
 * @since 1.0.0
 */
public class MonthTest {

  protected static final int[] MONTHS = {
    Calendar.JANUARY,
    Calendar.FEBRUARY,
    Calendar.MARCH,
    Calendar.APRIL,
    Calendar.MAY,
    Calendar.JUNE,
    Calendar.JULY,
    Calendar.AUGUST,
    Calendar.SEPTEMBER,
    Calendar.OCTOBER,
    Calendar.NOVEMBER,
    Calendar.DECEMBER
  };

  @Test
  public void testValueOf() {
    int position = 1;

    for (Month month : Month.values()) {
      assertEquals(month, Month.valueOf(month.name()));
      assertEquals(MONTHS[position - 1], month.getCalendarConstant());
      assertEquals(position++, month.getPosition());
    }
  }

  @Test
  public void testValueOfAbbreviation() {
    for (Month month : Month.values()) {
      assertEquals(month, Month.valueOfAbbreviation(month.getAbbreviation()));
    }

    assertEquals(Month.MAY, Month.valueOfAbbreviation("may"));
    assertEquals(Month.JANUARY, Month.valueOfAbbreviation("Jan"));
    assertEquals(Month.AUGUST, Month.valueOfAbbreviation("AUG"));
    assertNull(Month.valueOfAbbreviation("July"));
    assertNull(Month.valueOfAbbreviation("Sept"));
    assertNull(Month.valueOfAbbreviation("December"));
  }

  @Test
  public void testValueOfCalendarConstant() {
    int index = 0;

    for (Month month : Month.values()) {
      assertEquals(month, Month.valueOfCalendarConstant(MONTHS[index++]));
    }

    assertEquals(12, index);
    assertNull(Month.valueOfCalendarConstant(13));
    assertNull(Month.valueOfCalendarConstant(-12));
  }

  @Test
  public void testValueOfName() {
    for (Month month : Month.values()) {
      assertEquals(month, Month.valueOfName(month.getName()));
    }

    assertEquals(Month.MAY, Month.valueOfName("may"));
    assertEquals(Month.JANUARY, Month.valueOfName("January"));
    assertEquals(Month.AUGUST, Month.valueOfName("AUGUST"));
    assertNull(Month.valueOfName("Jul"));
    assertNull(Month.valueOfName("Octobre"));
  }

  @Test
  public void testValueOfPosition() {
    int position = 0;

    for (Month month : Month.values()) {
      assertEquals(month, Month.valueOfPosition(++position));
    }

    assertNull(Month.valueOfPosition(-1));
    assertNull(Month.valueOfPosition(13));
  }

}
