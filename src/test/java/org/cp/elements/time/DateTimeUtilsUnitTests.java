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
package org.cp.elements.time;

import static org.assertj.core.api.Assertions.assertThat;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Calendar;

import org.junit.Test;

import org.cp.elements.test.TestUtils;

/**
 * Unit Tests for {@link DateTimeUtils}.
 *
 * @author John J. Blum
 * @see java.util.Calendar
 * @see org.junit.Test
 * @see org.cp.elements.test.TestUtils
 * @see org.cp.elements.time.DateTimeUtils
 * @since 1.0.0
 */
public class DateTimeUtilsUnitTests {

  @Test
  public void cloneIsCorrect() {

    Calendar expectedDateTime = TestUtils.createCalendar(2011, Calendar.NOVEMBER, 9, 1, 45, 30);
    Calendar actualDateTime = DateTimeUtils.clone(expectedDateTime);

    assertThat(actualDateTime).isNotNull();
    assertThat(actualDateTime).isNotSameAs(expectedDateTime);
    assertThat(actualDateTime).isEqualTo(expectedDateTime);
  }

  @Test
  public void cloneWithNullIsNullSafe() {
    assertThat(DateTimeUtils.clone(null)).isNull();
  }

  @Test
  public void createIsCorrect() {

    Calendar expectedDateTime = TestUtils.createCalendar(2013, Calendar.OCTOBER, 19, 10, 36, 0);
    Calendar actualDateTime = DateTimeUtils.create(expectedDateTime.getTimeInMillis());

    assertThat(actualDateTime).isNotNull();
    assertThat(actualDateTime).isNotSameAs(expectedDateTime);
    assertThat(actualDateTime).isEqualTo(expectedDateTime);
  }

  @Test
  public void fromInstantToMilliseconds() {

    Instant now = Instant.now();

    assertThat(DateTimeUtils.toMilliseconds(now)).isEqualTo(now.toEpochMilli());
  }

  @Test
  public void fromNullInstantToMillisecondsIsNullSafe() {
    assertThat(DateTimeUtils.toMilliseconds((Instant) null)).isZero();
  }

  @Test
  public void fromLocalDateTimeToMilliseconds() {

    LocalDateTime dateTime = LocalDateTime.of(2022, Month.NOVEMBER.getJavaTimeMonth().getValue(), 7,
      11, 17, 30);

    assertThat(DateTimeUtils.toMilliseconds(dateTime))
      .isEqualTo(dateTime.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
  }

  @Test
  public void fromNullLocalDateTimeToMillisecondsIsNullSafe() {
    assertThat(DateTimeUtils.toMilliseconds((LocalDateTime) null)).isZero();
  }

  @Test
  public void fromZonedDateTimeToMilliseconds() {

    ZonedDateTime dateTime = ZonedDateTime.of(2022, Month.NOVEMBER.getJavaTimeMonth().getValue(), 7,
      11, 21, 30, 0, ZoneId.systemDefault());

    assertThat(DateTimeUtils.toMilliseconds(dateTime)).isEqualTo(dateTime.toInstant().toEpochMilli());
  }

  @Test
  public void fromNullZonedDateTimeToMillisecondsIsNullSafe() {
    assertThat(DateTimeUtils.toMilliseconds((ZonedDateTime) null)).isZero();
  }

  @Test
  public void truncateIsCorrect() {

    Calendar dateTime = TestUtils.createCalendar(2011, Calendar.NOVEMBER, 8, 16, 15, 30);
    Calendar expectedDateTime = TestUtils.createCalendar(2011, Calendar.NOVEMBER, 8);
    Calendar actualDateTime = DateTimeUtils.truncate(dateTime);

    assertThat(actualDateTime).isNotNull();
    assertThat(actualDateTime).isSameAs(dateTime);
    assertThat(actualDateTime).isEqualTo(expectedDateTime);
  }

  @Test
  public void truncateWithNoTimeIsCorrect() {

    Calendar expectedDateTime = TestUtils.createCalendar(2011, Calendar.NOVEMBER, 7);

    assertThat(expectedDateTime).isNotNull();
    assertThat(expectedDateTime.get(Calendar.HOUR_OF_DAY)).isEqualTo(0);
    assertThat(expectedDateTime.get(Calendar.MINUTE)).isEqualTo(0);
    assertThat(expectedDateTime.get(Calendar.SECOND)).isEqualTo(0);
    assertThat(expectedDateTime.get(Calendar.MILLISECOND)).isEqualTo(0);

    Calendar actualDateTime = DateTimeUtils.truncate(expectedDateTime);

    assertThat(actualDateTime).isNotNull();
    assertThat(actualDateTime).isSameAs(expectedDateTime);
    assertThat(actualDateTime.get(Calendar.HOUR_OF_DAY)).isEqualTo(0);
    assertThat(actualDateTime.get(Calendar.MINUTE)).isEqualTo(0);
    assertThat(actualDateTime.get(Calendar.SECOND)).isEqualTo(0);
    assertThat(actualDateTime.get(Calendar.MILLISECOND)).isEqualTo(0);
  }

  @Test
  public void truncateWithNullIsNullSafe() {
    assertThat(DateTimeUtils.truncate(null)).isNull();
  }
}
