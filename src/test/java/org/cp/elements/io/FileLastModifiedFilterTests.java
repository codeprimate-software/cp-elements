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
package org.cp.elements.io;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileFilter;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Calendar;
import java.util.Date;

import org.cp.elements.time.Month;
import org.junit.Test;

/**
 * Unit Tests for {@link FileLastModifiedFilter}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see java.time.LocalDateTime
 * @see java.util.Calendar
 * @see java.util.Date
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.FileLastModifiedFilter
 * @since 1.0.0
 */
@SuppressWarnings("all")
public class FileLastModifiedFilterTests {

  private File mockFile(String name) {
    return mock(File.class, name);
  }

  private Calendar newCalendar(int year, Month month, int day) {

    Calendar dateTime = Calendar.getInstance();

    dateTime.clear();
    dateTime.set(year, month.getCalendarMonth(), day);

    return dateTime;
  }

  private Date newDate(int year, Month month, int day) {
    return newCalendar(year, month, day).getTime();
  }

  private LocalDateTime newLocalDateTime(int year, Month month, int day) {
    return LocalDateTime.of(year, month.getJavaTimeMonth(), day, 0, 0, 0, 0);
  }

  private long toMilliseconds(int year, Month month, int day) {
    return newCalendar(year, month, day).getTimeInMillis();
  }

  private String toString(Calendar dateTime) {
    return toString(dateTime.getTime());
  }

  private String toString(Date dateTime) {
    return new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.S").format(dateTime);
  }

  private String toString(LocalDateTime dateTime) {
    return dateTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.S.n"));
  }

  private void acceptTest(FileFilter fileFilter, boolean before, boolean on, boolean after) {

    File mockFileAfter = mockFile("fileAfter");
    File mockFileBefore = mockFile("fileBefore");
    File mockFileOn = mockFile("fileOn");

    when(mockFileBefore.lastModified()).thenReturn(toMilliseconds(2004, Month.DECEMBER, 31));
    when(mockFileOn.lastModified()).thenReturn(toMilliseconds(2014, Month.DECEMBER, 6));
    when(mockFileAfter.lastModified()).thenReturn(toMilliseconds(2024, Month.APRIL, 1));

    assertThat(fileFilter.accept(mockFileBefore)).isEqualTo(before);
    assertThat(fileFilter.accept(mockFileOn)).isEqualTo(on);
    assertThat(fileFilter.accept(mockFileAfter)).isEqualTo(after);

    verify(mockFileAfter, atLeast(1)).lastModified();
    verify(mockFileBefore, atLeast(1)).lastModified();
    verify(mockFileOn, atLeast(1)).lastModified();
  }

  protected void acceptTimeSpanTest(FileFilter fileFilter,
      boolean before, boolean onBegin, boolean during, boolean onEnd, boolean after) {

    File fileAfter = mockFile("fileAfter");
    File fileBefore = mockFile("fileBefore");
    File fileDuring = mockFile("fileDuring");
    File fileOnBegin = mockFile("fileOnBegin");
    File fileOnEnd = mockFile("fileOnEnd");

    when(fileBefore.lastModified()).thenReturn(toMilliseconds(2004, Month.DECEMBER, 31));
    when(fileOnBegin.lastModified()).thenReturn(toMilliseconds(2014, Month.DECEMBER, 6));
    when(fileDuring.lastModified()).thenReturn(toMilliseconds(2015, Month.APRIL, 1));
    when(fileOnEnd.lastModified()).thenReturn(toMilliseconds(2016, Month.DECEMBER, 5));
    when(fileAfter.lastModified()).thenReturn(toMilliseconds(2024, Month.APRIL, 1));

    assertThat(fileFilter.accept(fileBefore)).isEqualTo(before);
    assertThat(fileFilter.accept(fileOnBegin)).isEqualTo(onBegin);
    assertThat(fileFilter.accept(fileDuring)).isEqualTo(during);
    assertThat(fileFilter.accept(fileOnEnd)).isEqualTo(onEnd);
    assertThat(fileFilter.accept(fileAfter)).isEqualTo(after);

    verify(fileAfter, atLeast(1)).lastModified();
    verify(fileBefore, atLeast(1)).lastModified();
    verify(fileDuring, atLeast(1)).lastModified();
    verify(fileOnBegin, atLeast(1)).lastModified();
    verify(fileOnEnd, atLeast(1)).lastModified();
  }

  @Test
  public void acceptAfterUsingTimestamp() {
    acceptTest(FileLastModifiedFilter.after(toMilliseconds(2014, Month.DECEMBER, 6)), false, false, true);
  }

  @Test
  public void acceptAfterUsingCalendar() {
    acceptTest(FileLastModifiedFilter.after(newCalendar(2014, Month.DECEMBER, 6)), false, false, true);
  }

  @Test
  public void acceptAfterUsingDate() {
    acceptTest(FileLastModifiedFilter.after(newDate(2014, Month.DECEMBER, 6)), false, false, true);
  }

  @Test
  public void acceptAfterUsingLocalDateTime() {
    acceptTest(FileLastModifiedFilter.after(newLocalDateTime(2014, Month.DECEMBER, 6)), false, false, true);
  }

  @Test
  public void acceptBeforeUsingTimestamp() {
    acceptTest(FileLastModifiedFilter.before(toMilliseconds(2014, Month.DECEMBER, 6)), true, false, false);
  }

  @Test
  public void acceptBeforeUsingCalendar() {
    acceptTest(FileLastModifiedFilter.before(newCalendar(2014, Month.DECEMBER, 6)), true, false, false);
  }

  @Test
  public void acceptBeforeUsingDate() {
    acceptTest(FileLastModifiedFilter.before(newDate(2014, Month.DECEMBER, 6)), true, false, false);
  }

  @Test
  public void acceptBeforeUsingLocalDateTime() {
    acceptTest(FileLastModifiedFilter.before(newLocalDateTime(2014, Month.DECEMBER, 6)), true, false, false);
  }

  @Test
  public void acceptDuringUsingTimestamp() {
    acceptTimeSpanTest(FileLastModifiedFilter.during(toMilliseconds(2014, Month.DECEMBER, 6),
      toMilliseconds(2016, Month.DECEMBER, 5)), false, true, true, true, false);
  }

  @Test
  public void acceptDuringUsingCalendar() {
    acceptTimeSpanTest(FileLastModifiedFilter.during(newCalendar(2014, Month.DECEMBER, 6),
      newCalendar(2016, Month.DECEMBER, 5)), false, true, true, true, false);
  }

  @Test
  public void acceptDuringUsingDate() {
    acceptTimeSpanTest(FileLastModifiedFilter.during(newDate(2014, Month.DECEMBER, 6),
      newDate(2016, Month.DECEMBER, 5)), false, true, true, true, false);
  }

  @Test
  public void acceptDuringUsingLocalDateTime() {
    acceptTimeSpanTest(FileLastModifiedFilter.during(newLocalDateTime(2014, Month.DECEMBER, 6),
      newLocalDateTime(2016, Month.DECEMBER, 5)), false, true, true, true, false);
  }

  @Test
  public void acceptDuringExclusive() {
    acceptTimeSpanTest(ComposableFileFilter.and(FileLastModifiedFilter.after(toMilliseconds(2014, Month.DECEMBER, 6)),
      FileLastModifiedFilter.before(toMilliseconds(2016, Month.DECEMBER, 5))), false, false, true, false, false);
  }

  @Test
  public void acceptOnUsingTimestamp() {
    acceptTest(FileLastModifiedFilter.on(toMilliseconds(2014, Month.DECEMBER, 6)), false, true, false);
  }

  @Test
  public void acceptOnUsingCalendar() {
    acceptTest(FileLastModifiedFilter.on(newCalendar(2014, Month.DECEMBER, 6)), false, true, false);
  }

  @Test
  public void acceptOnUsingDate() {
    acceptTest(FileLastModifiedFilter.on(newDate(2014, Month.DECEMBER, 6)), false, true, false);
  }

  @Test
  public void acceptOnUsingLocalDateTime() {
    acceptTest(FileLastModifiedFilter.on(newLocalDateTime(2014, Month.DECEMBER, 6)), false, true, false);
  }

  @Test
  public void acceptOnOrAfter() {

    long lastModified  = toMilliseconds(2014, Month.DECEMBER, 6);

    acceptTest(ComposableFileFilter.or(FileLastModifiedFilter.on(lastModified),
      FileLastModifiedFilter.after(lastModified)), false, true, true);
  }

  @Test
  public void acceptOnOrBefore() {

    long lastModified  = toMilliseconds(2014, Month.DECEMBER, 6);

    acceptTest(ComposableFileFilter.or(FileLastModifiedFilter.on(lastModified),
      FileLastModifiedFilter.before(lastModified)), true, true, false);
  }

  @Test
  public void acceptOutsideUsingTimestamp() {
    acceptTimeSpanTest(FileLastModifiedFilter.outside(toMilliseconds(2014, Month.DECEMBER, 6),
      toMilliseconds(2016, Month.DECEMBER, 5)), true, true, false, true, true);
  }

  @Test
  public void acceptOutsideUsingCalendar() {
    acceptTimeSpanTest(FileLastModifiedFilter.outside(newCalendar(2014, Month.DECEMBER, 6),
      newCalendar(2016, Month.DECEMBER, 5)), true, true, false, true, true);
  }

  @Test
  public void acceptOutsideUsingDate() {
    acceptTimeSpanTest(FileLastModifiedFilter.outside(newDate(2014, Month.DECEMBER, 6),
      newDate(2016, Month.DECEMBER, 5)), true, true, false, true, true);
  }

  @Test
  public void acceptOutsideUsingLocalDateTime() {
    acceptTimeSpanTest(FileLastModifiedFilter.outside(newLocalDateTime(2014, Month.DECEMBER, 6),
      newLocalDateTime(2016, Month.DECEMBER, 5)), true, true, false, true, true);
  }

  @Test
  public void acceptOutsideExclusive() {
    acceptTimeSpanTest(ComposableFileFilter.or(FileLastModifiedFilter.before(toMilliseconds(2014, Month.DECEMBER, 6)),
      FileLastModifiedFilter.after(toMilliseconds(2016, Month.DECEMBER, 5))), true, false, false, false, true);
  }
}
