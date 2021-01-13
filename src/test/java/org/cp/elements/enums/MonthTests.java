/*
 * Copyright 2011-Present Author or Authors.
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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import java.util.Calendar;

import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link Month} Enum.
 *
 * @author John J. Blum
 * @see java.time.Month
 * @see java.util.Calendar
 * @see org.junit.Test
 * @see org.cp.elements.enums.Month
 * @since 1.0.0
 */
public class MonthTests {

  protected static final int[] CALENDAR_MONTHS = {
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
  public void valueOf() {
    int index = 0;

    for (Month month : Month.values()) {
      assertThat(Month.valueOf(month.name()), is(equalTo(month)));
      assertThat(month.ordinal(), is(equalTo(index)));
      assertThat(month.getCalendarMonth(), is(equalTo(CALENDAR_MONTHS[index])));
      assertThat(month.getPosition(), is(equalTo(++index)));
    }
  }

  @Test
  public void valueOfAbbreviationReturnsMonth() {
    for (Month month : Month.values()) {
      assertThat(Month.valueOfAbbreviation(month.getAbbreviation()), is(equalTo(month)));
    }
  }

  @Test
  public void valueOfAbbreviationIgnoresCaseReturnsMonth() {
    assertThat(Month.valueOfAbbreviation("jan"), is(equalTo(Month.JANUARY)));
    assertThat(Month.valueOfAbbreviation("May"), is(equalTo(Month.MAY)));
    assertThat(Month.valueOfAbbreviation("AUG"), is(equalTo(Month.AUGUST)));
  }

  @Test
  public void valueOfAbbreviationReturnsNull() {
    assertThat(Month.valueOfAbbreviation("Ju"), is(nullValue(Month.class)));
    assertThat(Month.valueOfAbbreviation("Sept"), is(nullValue(Month.class)));
    assertThat(Month.valueOfAbbreviation("December"), is(nullValue(Month.class)));
  }

  @Test
  public void valueOfCalendarMonthReturnsMonth() {
    int index = 0;

    for (Month month : Month.values()) {
      assertThat(Month.valueOfCalendarMonth(CALENDAR_MONTHS[index++]), is(equalTo(month)));
    }

    assertThat(index, is(equalTo(12)));
  }

  @Test
  public void valueOfInvalidCalendarMonthIsNull() {
    assertThat(Month.valueOfCalendarMonth(-11), is(nullValue(Month.class)));
    assertThat(Month.valueOfCalendarMonth(12), is(nullValue(Month.class)));
    assertThat(Month.valueOfCalendarMonth(13), is(nullValue(Month.class)));
  }

  @Test
  public void valueOfJavaTimeMonthReturnsMonth() {
    for (Month month : Month.values()) {
      assertThat(Month.valueOfJavaTimeMonth(month.getJavaTimeMonth()), is(equalTo(month)));
    }
  }

  @Test
  public void valueOfJavaTimeMonthReturnsNull() {
    assertThat(Month.valueOfJavaTimeMonth(null), is(nullValue(Month.class)));
  }

  @Test
  public void valueOfNameReturnsMonth() {
    for (Month month : Month.values()) {
      assertThat(Month.valueOfName(month.getName()), is(equalTo(month)));
    }
  }

  @Test
  public void valueOfNameIgnoresCaseReturnsMonth() {
    assertThat(Month.valueOfName("january"), is(equalTo(Month.JANUARY)));
    assertThat(Month.valueOfName("May"), is(equalTo(Month.MAY)));
    assertThat(Month.valueOfName("AUGUST"), is(equalTo(Month.AUGUST)));
  }

  @Test
  public void valueOfNameReturnsNull() {
    assertThat(Month.valueOfName("Jul"), is(nullValue(Month.class)));
    assertThat(Month.valueOfName("Octobre"), is(nullValue(Month.class)));
    assertThat(Month.valueOfName("Daycember"), is(nullValue(Month.class)));
  }

  @Test
  public void valueOfPositionReturnsMonth() {
    int position = 0;

    for (Month month : Month.values()) {
      assertThat(Month.valueOfPosition(++position), is(equalTo(month)));
    }

    assertThat(position, is(equalTo(12)));
  }

  @Test
  public void valueOfPositionReturnsNull() {
    assertThat(Month.valueOfPosition(0), is(nullValue(Month.class)));
    assertThat(Month.valueOfPosition(-7), is(nullValue(Month.class)));
    assertThat(Month.valueOfPosition(13), is(nullValue(Month.class)));
  }
}
