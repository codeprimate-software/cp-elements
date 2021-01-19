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

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Calendar;

import org.junit.Test;

/**
 * Unit Tests for {@link Month}.
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
      assertThat(Month.valueOf(month.name())).isEqualTo(month);
      assertThat(month.ordinal()).isEqualTo(index);
      assertThat(month.getCalendarMonth()).isEqualTo(CALENDAR_MONTHS[index]);
      assertThat(month.getPosition()).isEqualTo(++index);
    }
  }

  @Test
  public void valueOfAbbreviationReturnsMonth() {

    for (Month month : Month.values()) {
      assertThat(Month.valueOfAbbreviation(month.getAbbreviation())).isEqualTo(month);
    }
  }

  @Test
  public void valueOfAbbreviationIgnoresCaseReturnsMonth() {

    assertThat(Month.valueOfAbbreviation("jan")).isEqualTo(Month.JANUARY);
    assertThat(Month.valueOfAbbreviation("May")).isEqualTo(Month.MAY);
    assertThat(Month.valueOfAbbreviation("AUG")).isEqualTo(Month.AUGUST);
  }

  @Test
  public void valueOfAbbreviationReturnsNull() {

    assertThat(Month.valueOfAbbreviation("Ju")).isNull();
    assertThat(Month.valueOfAbbreviation("Sept")).isNull();
    assertThat(Month.valueOfAbbreviation("December")).isNull();
  }

  @Test
  public void valueOfCalendarMonthReturnsMonth() {

    int index = 0;

    for (Month month : Month.values()) {
      assertThat(Month.valueOfCalendarMonth(CALENDAR_MONTHS[index++])).isEqualTo(month);
    }

    assertThat(index).isEqualTo(12);
  }

  @Test
  public void valueOfInvalidCalendarMonthIsNull() {

    assertThat(Month.valueOfCalendarMonth(-11)).isNull();
    assertThat(Month.valueOfCalendarMonth(12)).isNull();
    assertThat(Month.valueOfCalendarMonth(13)).isNull();
  }

  @Test
  public void valueOfJavaTimeMonthReturnsMonth() {

    for (Month month : Month.values()) {
      assertThat(Month.valueOfJavaTimeMonth(month.getJavaTimeMonth())).isEqualTo(month);
    }
  }

  @Test
  public void valueOfJavaTimeMonthReturnsNull() {
    assertThat(Month.valueOfJavaTimeMonth(null)).isNull();
  }

  @Test
  public void valueOfNameReturnsMonth() {

    for (Month month : Month.values()) {
      assertThat(Month.valueOfName(month.getName())).isEqualTo(month);
    }
  }

  @Test
  public void valueOfNameIgnoresCaseReturnsMonth() {

    assertThat(Month.valueOfName("january")).isEqualTo(Month.JANUARY);
    assertThat(Month.valueOfName("May")).isEqualTo(Month.MAY);
    assertThat(Month.valueOfName("AUGUST")).isEqualTo(Month.AUGUST);
  }

  @Test
  public void valueOfNameReturnsNull() {

    assertThat(Month.valueOfName("Jul")).isNull();
    assertThat(Month.valueOfName("Octobre")).isNull();
    assertThat(Month.valueOfName("Daycember")).isNull();
  }

  @Test
  public void valueOfPositionReturnsMonth() {

    int position = 0;

    for (Month month : Month.values()) {
      assertThat(Month.valueOfPosition(++position)).isEqualTo(month);
    }

    assertThat(position).isEqualTo(12);
  }

  @Test
  public void valueOfPositionReturnsNull() {

    assertThat(Month.valueOfPosition(0)).isNull();
    assertThat(Month.valueOfPosition(-7)).isNull();
    assertThat(Month.valueOfPosition(13)).isNull();
  }
}
