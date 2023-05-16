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
package org.cp.elements.util.zip;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.junit.jupiter.api.Test;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.test.annotation.UnitTest;
import org.cp.elements.util.SystemException;

/**
 * Unit Tests for {@link ZipUtils}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.zip.ZipUtils
 * @since 1.0.0
 */
@UnitTest
public class ZipUtilsTests {

  @Test
  @SuppressWarnings("all")
  public void newZipEntryUsesFileNameAsZipEntryName() {

    long lastModified = 12345L;

    File mockFile = mock(File.class);

    doReturn("MockFile.ext").when(mockFile).getName();
    doReturn(1024L).when(mockFile).getTotalSpace();
    doReturn(lastModified).when(mockFile).lastModified();

    ZipEntry zipEntry = ZipUtils.newZipEntry(null, mockFile);

    assertThat(zipEntry).isNotNull();
    assertThat(zipEntry.getName()).isEqualTo("MockFile.ext");
    assertThat(zipEntry.getSize()).isEqualTo(1024L);
    assertThat(zipEntry.getTime()).isEqualTo(lastModified);

    verify(mockFile, times(1)).getName();
    verify(mockFile, times(1)).getTotalSpace();
    verify(mockFile, times(1)).lastModified();
    verifyNoMoreInteractions(mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void newZipEntryUsesRelativeFilePathAsZipEntryName() throws IOException {

    long lastModified = 12345L;

    File mockDirectory = mock(File.class);
    File mockFile = mock(File.class);

    doReturn(true).when(mockDirectory).isDirectory();
    doReturn("directory").when(mockDirectory).getName();
    doReturn("/absolute/path/to/directory/relative/path/to/MockFile.ext").when(mockFile).getAbsolutePath();
    doReturn(2024L).when(mockFile).getTotalSpace();
    doReturn(lastModified).when(mockFile).lastModified();

    ZipEntry zipEntry = ZipUtils.newZipEntry(mockDirectory, mockFile);

    assertThat(zipEntry).isNotNull();
    assertThat(zipEntry.getName()).isEqualTo("directory/relative/path/to/MockFile.ext");
    assertThat(zipEntry.getSize()).isEqualTo(2024L);
    assertThat(zipEntry.getTime()).isEqualTo(lastModified);

    verify(mockDirectory, times(1)).isDirectory();
    verify(mockDirectory, times(1)).getName();
    verify(mockFile, times(1)).getAbsolutePath();
    verify(mockFile, never()).getName();
    verify(mockFile, times(1)).getTotalSpace();
    verify(mockFile, times(1)).lastModified();
  }

  @Test(expected = IllegalArgumentException.class)
  public void newZipEntryWithNull() {

    try {
      ZipUtils.newZipEntry(FileSystemUtils.TEMPORARY_DIRECTORY, null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("File is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("all")
  public void resolveZipEntryNameReturnsFileNameWhenDirectoryIsNull() {

    File mockFile = mock(File.class);

    when(mockFile.getName()).thenReturn("MockFile.ext");

    assertThat(ZipUtils.resolveZipEntryName(null, mockFile)).isEqualTo("MockFile.ext");

    verify(mockFile, times(1)).getName();
    verifyNoMoreInteractions(mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void resolveZipEntryNameReturnsFileNameWhenDirectoryIsNotADirectory() {

    File mockDirectory = mock(File.class);
    File mockFile = mock(File.class);

    when(mockDirectory.isDirectory()).thenReturn(false);
    when(mockFile.getName()).thenReturn("MockFile.ext");

    assertThat(ZipUtils.resolveZipEntryName(mockDirectory, mockFile)).isEqualTo("MockFile.ext");

    verify(mockDirectory, times(1)).isDirectory();
    verifyNoMoreInteractions(mockDirectory);
    verify(mockFile, times(1)).getName();
    verifyNoMoreInteractions(mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void resolveZipEntryNameReturnsFileNameWhenDirectoryNameIsEmpty() {

    File mockDirectory = mock(File.class);
    File mockFile = mock(File.class);

    when(mockDirectory.isDirectory()).thenReturn(true);
    when(mockDirectory.getName()).thenReturn(StringUtils.EMPTY_STRING);
    when(mockFile.getName()).thenReturn("MockFile.ext");

    assertThat(ZipUtils.resolveZipEntryName(mockDirectory, mockFile)).isEqualTo("MockFile.ext");

    verify(mockDirectory, times(1)).isDirectory();
    verify(mockDirectory, times(1)).getName();
    verify(mockFile, never()).getAbsolutePath();
    verify(mockFile, times(1)).getName();
  }

  @Test
  @SuppressWarnings("all")
  public void resolveZipEntryNameReturnsFileNameWhenDirectoryNameAndFilePathDoNotMatch() {

    File mockDirectory = mock(File.class);
    File mockFile = mock(File.class);

    when(mockDirectory.isDirectory()).thenReturn(true);
    when(mockDirectory.getName()).thenReturn("directory");
    when(mockFile.getAbsolutePath()).thenReturn("/absolute/path/to/MockFile.ext");
    when(mockFile.getName()).thenReturn("MockFile.ext");

    assertThat(ZipUtils.resolveZipEntryName(mockDirectory, mockFile)).isEqualTo("MockFile.ext");

    verify(mockDirectory, times(1)).isDirectory();
    verify(mockDirectory, times(1)).getName();
    verify(mockFile, times(1)).getAbsolutePath();
    verify(mockFile, times(1)).getName();
  }

  @Test
  @SuppressWarnings("all")
  public void resolveZipEntryNameReturnsRelativeFilePathWhenFileIsRelativeToDirectory() {

    File mockDirectory = mock(File.class);
    File mockFile = mock(File.class);

    when(mockDirectory.isDirectory()).thenReturn(true);
    when(mockDirectory.getName()).thenReturn("directory");
    when(mockFile.getAbsolutePath()).thenReturn("/absolute/path/to/directory/relative/path/to/MockFile.ext");

    assertThat(ZipUtils.resolveZipEntryName(mockDirectory, mockFile))
      .isEqualTo("directory/relative/path/to/MockFile.ext");

    verify(mockDirectory, times(1)).isDirectory();
    verify(mockDirectory, times(1)).getName();
    verify(mockFile, times(1)).getAbsolutePath();
    verify(mockFile, never()).getName();
  }

  @Test(expected = IllegalArgumentException.class)
  public void unzipWithNullZipFile() throws IOException {

    try {
      ZipUtils.unzip(null, FileSystemUtils.TEMPORARY_DIRECTORY);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("ZIP file is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void zipWithInvalidDirectory() throws IOException {

    try {
      ZipUtils.zip(new File("/absolute/path/to/invalid/directory"));
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("[/absolute/path/to/invalid/directory] is not a valid directory");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void zipWithNullDirectory() throws IOException {

    try {
      ZipUtils.zip(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("[null] is not a valid directory");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("all")
  public void zipEntryWithFile() throws IOException {

    long lastModified = System.currentTimeMillis();

    File mockDirectory = mock(File.class);
    File mockFile = mock(File.class);

    when(mockDirectory.isDirectory()).thenReturn(true);
    when(mockDirectory.getName()).thenReturn("directory");
    when(mockFile.getAbsolutePath()).thenReturn("/absolute/path/to/MockFile.ext");
    when(mockFile.getName()).thenReturn("MockFile.ext");
    when(mockFile.getTotalSpace()).thenReturn(8192L);
    when(mockFile.lastModified()).thenReturn(lastModified);

    ZipOutputStream mockOutputStream = mock(ZipOutputStream.class);

    doNothing().when(mockOutputStream).putNextEntry(any(ZipEntry.class));
    doNothing().when(mockOutputStream).closeEntry();

    assertThat(ZipUtils.zipEntry(mockDirectory, mockFile, mockOutputStream)).isSameAs(mockOutputStream);

    verify(mockDirectory, times(1)).isDirectory();
    verify(mockDirectory, times(1)).getName();
    verify(mockFile, times(1)).getAbsolutePath();
    verify(mockFile, times(1)).getName();
    verify(mockFile, times(1)).getTotalSpace();
    verify(mockFile, times(1)).lastModified();
    verify(mockOutputStream, times(1)).putNextEntry(isA(ZipEntry.class));
    verify(mockOutputStream, times(1)).closeEntry();
  }

  @Test(expected = SystemException.class)
  public void zipEntryWithZipEntryThrowsSystemExceptionWhenZippingEntryFails() throws IOException {

    ZipEntry mockZipEntry = mock(ZipEntry.class);

    ZipOutputStream mockOutputStream = mock(ZipOutputStream.class);

    when(mockZipEntry.getName()).thenReturn("MockZipEntry");
    doThrow(new IOException("test")).when(mockOutputStream).putNextEntry(any(ZipEntry.class));

    try {
      ZipUtils.zipEntry(mockZipEntry, mockOutputStream);
    }
    catch (SystemException expected) {

      assertThat(expected).hasMessage("Failed to zip entry [MockZipEntry]");
      assertThat(expected).hasCauseInstanceOf(IOException.class);
      assertThat(expected.getCause()).hasMessage("test");
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockOutputStream, times(1)).putNextEntry(eq(mockZipEntry));
      verify(mockOutputStream, never()).closeEntry();
      verify(mockZipEntry, times(1)).getName();
    }
  }

  @Test
  public void zipEntryWithZipEntryHandlesIOExceptionOnCloseEntry() throws IOException {

    ZipEntry mockZipEntry = mock(ZipEntry.class);

    ZipOutputStream mockOutputStream = mock(ZipOutputStream.class);

    when(mockZipEntry.getName()).thenReturn("MockZipEntry");
    doNothing().when(mockOutputStream).putNextEntry(any(ZipEntry.class));
    doThrow(new IOException("test")).when(mockOutputStream).closeEntry();

    assertThat(ZipUtils.zipEntry(mockZipEntry, mockOutputStream)).isSameAs(mockOutputStream);

    verify(mockOutputStream, times(1)).putNextEntry(eq(mockZipEntry));
    verify(mockOutputStream, times(1)).closeEntry();
    verify(mockZipEntry, never()).getName();
  }
}
