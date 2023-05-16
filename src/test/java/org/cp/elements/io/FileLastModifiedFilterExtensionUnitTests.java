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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.File;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.Month;
import java.time.Year;
import java.time.ZoneId;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.annotation.NotNull;

/**
 * Unit Tests for {@link FileLastModifiedFilterExtension}.
 *
 * @author John Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see java.time.LocalTime
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.FileLastModifiedFilterExtension
 * @since 1.0.0
 */
public class FileLastModifiedFilterExtensionUnitTests {

  private @NotNull File mockFile(@NotNull LocalTime time) {
    return mockFile(LocalDate.now(), time);
  }

  private @NotNull File mockFile(@NotNull LocalDate date, @NotNull LocalTime time) {

    File mockFile = mock(File.class);

    long lastModifiedMilliseconds = LocalDateTime.of(date, time)
      .atZone(ZoneId.systemDefault())
      .toInstant()
      .toEpochMilli();

    doReturn(lastModifiedMilliseconds).when(mockFile).lastModified();

    return mockFile;
  }

  @Test
  public void afterTimeIsCorrect() {

    File mockFileOne = mockFile(LocalTime.of(9, 0, 0));
    File mockFileTwo = mockFile(LocalTime.of(17, 30, 45));

    LocalTime afterTime = LocalTime.of(13, 30, 15);

    assertThat(FileLastModifiedFilterExtension.afterTime(afterTime).accept(mockFileOne)).isFalse();
    assertThat(FileLastModifiedFilterExtension.afterTime(afterTime).accept(mockFileTwo)).isTrue();

    verify(mockFileOne, times(1)).lastModified();
    verify(mockFileTwo, times(1)).lastModified();
    verifyNoMoreInteractions(mockFileOne, mockFileTwo);
  }

  @Test
  public void afterTimeWithNullLocalTime() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FileLastModifiedFilterExtension.afterTime(null))
      .withMessage("Time is required")
      .withNoCause();
  }

  @Test
  public void beforeTimeIsCorrect() {

    File mockFileOne = mockFile(LocalTime.of(9, 0, 0));
    File mockFileTwo = mockFile(LocalTime.of(17, 30, 45));

    LocalTime beforeTime = LocalTime.of(13, 30, 15);

    assertThat(FileLastModifiedFilterExtension.beforeTime(beforeTime).accept(mockFileOne)).isTrue();
    assertThat(FileLastModifiedFilterExtension.beforeTime(beforeTime).accept(mockFileTwo)).isFalse();

    verify(mockFileOne, times(1)).lastModified();
    verify(mockFileTwo, times(1)).lastModified();
    verifyNoMoreInteractions(mockFileOne, mockFileTwo);
  }

  @Test
  public void beforeTimeWithNullLocalTime() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FileLastModifiedFilterExtension.beforeTime(null))
      .withMessage("Time is required")
      .withNoCause();
  }

  @Test
  public void duringTimeIsCorrect() {

    File mockFileOne = mockFile(LocalTime.of(9, 0, 0));
    File mockFileTwo = mockFile(LocalTime.of(17, 30, 45));
    File mockFileThree = mockFile(LocalTime.of(15, 16, 30));

    LocalTime onAfterTime = LocalTime.of(10, 30, 45);
    LocalTime onBeforeTime = LocalTime.of(15, 30, 45);

    assertThat(FileLastModifiedFilterExtension.duringTime(onAfterTime, onBeforeTime).accept(mockFileOne)).isFalse();
    assertThat(FileLastModifiedFilterExtension.duringTime(onAfterTime, onBeforeTime).accept(mockFileTwo)).isFalse();
    assertThat(FileLastModifiedFilterExtension.duringTime(onAfterTime, onBeforeTime).accept(mockFileThree)).isTrue();

    verify(mockFileOne, times(1)).lastModified();
    verify(mockFileTwo, times(1)).lastModified();
    verify(mockFileThree, times(1)).lastModified();
    verifyNoMoreInteractions(mockFileOne, mockFileTwo, mockFileThree);
  }

  @Test
  public void duringTimeIgnoresDate() {

    File mockFileOne = mockFile(LocalDate.of(Year.now().getValue() + 10, Month.FEBRUARY, 14),
      LocalTime.of(9, 0, 0));

    File mockFileTwo = mockFile(LocalDate.of(Year.now().getValue() - 10, Month.DECEMBER, 2),
        LocalTime.of(17, 30, 45));

    File mockFileThree = mockFile(LocalTime.of(15, 16, 30));

    LocalTime onAfterTime = LocalTime.of(9, 0, 0);
    LocalTime onBeforeTime = LocalTime.of(17, 30, 45);

    assertThat(FileLastModifiedFilterExtension.duringTime(onAfterTime, onBeforeTime).accept(mockFileOne)).isTrue();
    assertThat(FileLastModifiedFilterExtension.duringTime(onAfterTime, onBeforeTime).accept(mockFileTwo)).isTrue();
    assertThat(FileLastModifiedFilterExtension.duringTime(onAfterTime, onBeforeTime).accept(mockFileThree)).isTrue();

    verify(mockFileOne, times(1)).lastModified();
    verify(mockFileTwo, times(1)).lastModified();
    verify(mockFileThree, times(1)).lastModified();
    verifyNoMoreInteractions(mockFileOne, mockFileTwo, mockFileThree);
  }

  @Test
  public void duringTimeWithNullAfterTime() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FileLastModifiedFilterExtension.duringTime(null, LocalTime.now()))
      .withMessage("Time on or after is required")
      .withNoCause();
  }

  @Test
  public void duringTimeWithNullBeforeTime() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FileLastModifiedFilterExtension.duringTime(LocalTime.now(), null))
      .withMessage("Time on or before is required")
      .withNoCause();
  }

  @Test
  public void onTimeIsCorrect() {

    LocalTime onTime = LocalTime.of(13, 30, 15);

    File mockFileOne = mockFile(LocalTime.of(9, 0, 0));
    File mockFileTwo = mockFile(LocalTime.of(17, 30, 45));
    File mockFileThree = mockFile(LocalDate.of(Year.now().getValue() + 10, Month.NOVEMBER, 7), onTime);

    assertThat(FileLastModifiedFilterExtension.onTime(onTime).accept(mockFileOne)).isFalse();
    assertThat(FileLastModifiedFilterExtension.onTime(onTime).accept(mockFileTwo)).isFalse();
    assertThat(FileLastModifiedFilterExtension.onTime(onTime).accept(mockFileThree)).isTrue();

    verify(mockFileOne, times(1)).lastModified();
    verify(mockFileTwo, times(1)).lastModified();
    verify(mockFileThree, times(1)).lastModified();
    verifyNoMoreInteractions(mockFileOne, mockFileTwo, mockFileThree);
  }

  @Test
  public void onTimeWithNullLocalTime() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FileLastModifiedFilterExtension.onTime(null))
      .withMessage("Time is required")
      .withNoCause();
  }

  @Test
  public void outsideTimeIsCorrect() {

    File mockFileOne = mockFile(LocalTime.of(9, 0, 0));
    File mockFileTwo = mockFile(LocalTime.of(17, 30, 45));
    File mockFileThree = mockFile(LocalTime.of(15, 16, 30));

    LocalTime beforeTime = LocalTime.of(10, 30, 45);
    LocalTime afterTime = LocalTime.of(15, 30, 45);

    assertThat(FileLastModifiedFilterExtension.outsideTime(beforeTime, afterTime).accept(mockFileOne)).isTrue();
    assertThat(FileLastModifiedFilterExtension.outsideTime(beforeTime, afterTime).accept(mockFileTwo)).isTrue();
    assertThat(FileLastModifiedFilterExtension.outsideTime(beforeTime, afterTime).accept(mockFileThree)).isFalse();

    verify(mockFileOne, times(1)).lastModified();
    verify(mockFileTwo, times(1)).lastModified();
    verify(mockFileThree, times(1)).lastModified();
    verifyNoMoreInteractions(mockFileOne, mockFileTwo, mockFileThree);
  }

  @Test
  public void outsideTimeIgnoresDates() {

    File mockFileOne = mockFile(LocalDate.of(Year.now().getValue() + 10, Month.FEBRUARY, 14),
      LocalTime.of(9, 0, 0));

    File mockFileTwo = mockFile(LocalDate.of(Year.now().getValue() - 10, Month.DECEMBER, 2),
      LocalTime.of(17, 30, 45));

    File mockFileThree = mockFile(LocalTime.of(15, 16, 30));

    LocalTime beforeTime = LocalTime.of(10, 30, 45);
    LocalTime afterTime = LocalTime.of(15, 30, 45);

    assertThat(FileLastModifiedFilterExtension.outsideTime(beforeTime, afterTime).accept(mockFileOne)).isTrue();
    assertThat(FileLastModifiedFilterExtension.outsideTime(beforeTime, afterTime).accept(mockFileTwo)).isTrue();
    assertThat(FileLastModifiedFilterExtension.outsideTime(beforeTime, afterTime).accept(mockFileThree)).isFalse();

    verify(mockFileOne, times(1)).lastModified();
    verify(mockFileTwo, times(1)).lastModified();
    verify(mockFileThree, times(1)).lastModified();
    verifyNoMoreInteractions(mockFileOne, mockFileTwo, mockFileThree);
  }

  @Test
  public void outsideTimeWithNullBeforeTime() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FileLastModifiedFilterExtension.outsideTime(null, LocalTime.now()))
      .withMessage("Time before is required")
      .withNoCause();
  }

  @Test
  public void outsideTimeWithNullAfterTime() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FileLastModifiedFilterExtension.outsideTime(LocalTime.now(), null))
      .withMessage("Time after is required")
      .withNoCause();
  }
}
