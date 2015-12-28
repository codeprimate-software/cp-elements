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
import static org.junit.Assert.assertNull;

import java.util.Calendar;

import org.junit.Test;

/**
 * The MonthTest class is a test suite of test cases testing the contract and functionality of the Month Enum.
 *
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
