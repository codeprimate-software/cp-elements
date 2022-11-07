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
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.File;
import java.io.FileFilter;
import java.time.LocalDate;
import java.time.ZoneId;

import org.junit.Test;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.time.Month;

/**
 * Unit Tests for {@link FileLastModifiedFilter}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.FileLastModifiedFilter
 * @since 1.0.0
 */
@SuppressWarnings("all")
public class FileLastModifiedFilterUnitTests {

  private @NotNull File mockFile(@NotNull String name) {
    return mock(File.class, name);
  }

  private long toMilliseconds(int year, Month month, int day) {

    return LocalDate.of(year, month.getJavaTimeMonth().getValue(), day)
      .atStartOfDay(ZoneId.systemDefault())
      .toInstant()
      .toEpochMilli();
  }

  private void acceptTest(@NotNull FileFilter fileFilter, boolean before, boolean on, boolean after) {

    File mockFileAfter = mockFile("fileAfter");
    File mockFileBefore = mockFile("fileBefore");
    File mockFileOn = mockFile("fileOn");

    doReturn(toMilliseconds(2004, Month.DECEMBER, 31)).when(mockFileBefore).lastModified();
    doReturn(toMilliseconds(2014, Month.DECEMBER, 6)).when(mockFileOn).lastModified();
    doReturn(toMilliseconds(2024, Month.APRIL, 1)).when(mockFileAfter).lastModified();

    assertThat(fileFilter.accept(mockFileBefore)).isEqualTo(before);
    assertThat(fileFilter.accept(mockFileOn)).isEqualTo(on);
    assertThat(fileFilter.accept(mockFileAfter)).isEqualTo(after);

    verify(mockFileAfter, atLeast(1)).lastModified();
    verify(mockFileBefore, atLeast(1)).lastModified();
    verify(mockFileOn, atLeast(1)).lastModified();
    verifyNoMoreInteractions(mockFileAfter, mockFileBefore, mockFileOn);
  }

  private void acceptTimeSpanTest(@NotNull FileFilter fileFilter,
      boolean before, boolean onBegin, boolean during, boolean onEnd, boolean after) {

    File fileAfter = mockFile("fileAfter");
    File fileBefore = mockFile("fileBefore");
    File fileDuring = mockFile("fileDuring");
    File fileOnBegin = mockFile("fileOnBegin");
    File fileOnEnd = mockFile("fileOnEnd");

    doReturn(toMilliseconds(2004, Month.DECEMBER, 31)).when(fileBefore).lastModified();
    doReturn(toMilliseconds(2014, Month.DECEMBER, 6)).when(fileOnBegin).lastModified();
    doReturn(toMilliseconds(2015, Month.APRIL, 1)).when(fileDuring).lastModified();
    doReturn(toMilliseconds(2016, Month.DECEMBER, 5)).when(fileOnEnd).lastModified();
    doReturn(toMilliseconds(2024, Month.APRIL, 1)).when(fileAfter).lastModified();

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
    verifyNoMoreInteractions(fileAfter, fileBefore, fileDuring, fileOnBegin, fileOnEnd);
  }

  @Test
  public void acceptAfter() {
    acceptTest(FileLastModifiedFilter.after(toMilliseconds(2014, Month.DECEMBER, 6)),
      false, false, true);
  }

  @Test
  public void acceptBefore() {
    acceptTest(FileLastModifiedFilter.before(toMilliseconds(2014, Month.DECEMBER, 6)),
      true, false, false);
  }

  @Test
  public void acceptDuring() {
    acceptTimeSpanTest(FileLastModifiedFilter.during(toMilliseconds(2014, Month.DECEMBER, 6),
      toMilliseconds(2016, Month.DECEMBER, 5)),
      false, true, true, true, false);
  }

  @Test
  public void acceptDuringExclusive() {
    acceptTimeSpanTest(ComposableFileFilter.and(FileLastModifiedFilter.after(toMilliseconds(2014, Month.DECEMBER, 6)),
      FileLastModifiedFilter.before(toMilliseconds(2016, Month.DECEMBER, 5))),
      false, false, true, false, false);
  }

  @Test
  public void acceptOn() {
    acceptTest(FileLastModifiedFilter.on(toMilliseconds(2014, Month.DECEMBER, 6)),
      false, true, false);
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
  public void acceptOutside() {
    acceptTimeSpanTest(FileLastModifiedFilter.outside(toMilliseconds(2014, Month.DECEMBER, 6),
      toMilliseconds(2016, Month.DECEMBER, 5)), true, false, false, false, true);
  }

  @Test
  public void acceptOutsideInclusive() {

    long afterMilliseconds = toMilliseconds(2016, Month.DECEMBER, 5);
    long beforeMilliseconds = toMilliseconds(2014, Month.DECEMBER, 6);

    FileFilter onOrAfter = ComposableFileFilter.or(FileLastModifiedFilter.on(afterMilliseconds),
      FileLastModifiedFilter.after(afterMilliseconds));

    FileFilter onOrBefore = ComposableFileFilter.or(FileLastModifiedFilter.on(beforeMilliseconds),
      FileLastModifiedFilter.before(beforeMilliseconds));

    acceptTimeSpanTest(ComposableFileFilter.or(onOrBefore, onOrAfter),
      true, true, false, true, true);
  }
}
