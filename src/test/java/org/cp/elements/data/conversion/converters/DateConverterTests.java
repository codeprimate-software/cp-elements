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

package org.cp.elements.data.conversion.converters;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertFalse;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.text.ParseException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Calendar;
import java.util.Date;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.test.TestUtils;
import org.junit.Test;

/**
 * Unit tests for {@link DateConverter}.
 *
 * @author John J. Blum
 * @see java.util.Date
 * @see org.junit.Test
 * @see org.cp.elements.data.conversion.converters.DateConverter
 * @since 1.0.0
 */
public class DateConverterTests {

  private final DateConverter converter = new DateConverter();

  @Test
  public void canConvertToDateReturnsTrue() {

    assertThat(this.converter.canConvert(Calendar.class, Date.class)).isTrue();
    assertThat(this.converter.canConvert(Date.class, Date.class)).isTrue();
    assertThat(this.converter.canConvert(Number.class, Date.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, Date.class)).isTrue();
    assertThat(this.converter.canConvert(BigDecimal.class, Date.class)).isTrue();
    assertThat(this.converter.canConvert(BigInteger.class, Date.class)).isTrue();
    assertThat(this.converter.canConvert(Integer.class, Date.class)).isTrue();
    assertThat(this.converter.canConvert(Long.class, Date.class)).isTrue();
    assertThat(this.converter.canConvert(Float.class, Date.class)).isTrue();
    assertThat(this.converter.canConvert(Double.class, Date.class)).isTrue();
  }

  @Test
  public void canConvertNulToDateReturnsTrue() {
    assertThat(this.converter.canConvert(null, Date.class)).isTrue();
  }

  @Test
  public void cannotConvertToDateReturnsFalse() {

    assertFalse(this.converter.canConvert(Date.class, null));
    assertFalse(this.converter.canConvert(Date.class, Calendar.class));
    assertFalse(this.converter.canConvert(Date.class, Object.class));
    assertFalse(this.converter.canConvert(Date.class, Number.class));
    assertFalse(this.converter.canConvert(Date.class, String.class));
    assertFalse(this.converter.canConvert(Date.class, Timestamp.class));
    assertFalse(this.converter.canConvert(String.class, LocalDate.class));
    assertFalse(this.converter.canConvert(String.class, LocalDateTime.class));
  }

  @Test
  public void convertCalendarToDate() {

    Calendar expectedDateTime = TestUtils.createCalendar(2013, Calendar.NOVEMBER, 2);

    assertThat(this.converter.convert(expectedDateTime)).isEqualTo(expectedDateTime.getTime());
  }

  @Test
  public void convertDateToDate() {

    Date now = new Date(System.currentTimeMillis());

    assertThat(this.converter.convert(now)).isEqualTo(now);
    assertThat(this.converter.convert(now)).isSameAs(now);
  }

  @Test
  public void convertDoubleToDate() {

    Double value = Math.PI;

    assertThat(this.converter.convert(value)).isEqualTo(new Date(value.intValue()));
  }

  @Test
  public void convertIntegerToDate() {

    Integer timestamp = Long.valueOf(System.currentTimeMillis()).intValue();

    assertThat(this.converter.convert(timestamp)).isEqualTo(new Date(timestamp));
  }

  @Test
  public void convertNullToDateWithDefaultValueReturnsDate() {

    Date now = new Date();

    assertThat(this.converter.withDefaultValue(now).convert(null)).isEqualTo(now);
  }

  @Test(expected = ConversionException.class)
  public void convertNullToDateWithNoDefaultValueThrowsException() {

    try {
      this.converter.convert(null);
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("Cannot convert [null] to [java.util.Date]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void convertStringDateTimeToDate() throws ParseException {

    String dateTimeString = "01/12/2018 16:27:30 pm";
    Date dateTime = this.converter.getDateFormat().parse(dateTimeString);

    assertThat(this.converter.convert(dateTimeString)).isEqualTo(dateTime);
  }

  @Test
  public void convertStringTimestampToDate() {

    Calendar expectedDateTime = TestUtils.createCalendar(2012, Calendar.DECEMBER, 12);

    this.converter.convert(String.valueOf(expectedDateTime.getTimeInMillis()));
  }

  @Test(expected = ConversionException.class)
  public void convertInvalidDateTimeStringThrowsException() {

    try {
      this.converter.convert("2018-01-12");
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("[2018-01-12] is not a valid date/time");
      assertThat(expected).hasCauseInstanceOf(ParseException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ConversionException.class)
  public void convertInvalidStringThrowsException() {

    try {
      this.converter.convert("Once upon a time...");
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("[Once upon a time...] is not a valid date/time");
      assertThat(expected).hasCauseInstanceOf(ParseException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void convertValidDateFormattedString() {

    DateConverter converter = new DateConverter("MMMMM dd, yyyy");

    assertThat(converter.convert("January 12, 2018"))
      .isEqualTo(TestUtils.createCalendar(2018, Calendar.JANUARY, 12).getTime());
  }

  @Test
  public void convertValidDateTimeFormattedString() {

    DateConverter converter = new DateConverter("MMM dd, yyyy @ hh:mm a");

    assertThat(converter.convert("May 09, 2017 @ 1:24 pm"))
      .isEqualTo(TestUtils.createCalendar(2017, Calendar.MAY, 9, 13, 24, 0).getTime());

  }
}
