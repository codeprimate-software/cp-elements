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

package org.cp.elements.tools.io;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.io.FileUtils.newFile;
import static org.cp.elements.tools.io.ListFiles.newListFiles;
import static org.cp.elements.util.ArrayUtils.asArray;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.test.AbstractBaseTestSuite;
import org.junit.Test;

/**
 * Unit tests for {@link ListFiles}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Spy
 * @see org.cp.elements.tools.io.ListFiles
 * @since 1.0.0
 */
public class ListFilesTests extends AbstractBaseTestSuite {

  private File mockDirectory(String name) {

    File mockDirectory = mock(File.class, name);

    when(mockDirectory.isDirectory()).thenReturn(true);
    when(mockDirectory.getName()).thenReturn(name);

    return mockDirectory;
  }

  private File mockFile(String name) {

    File mockFile = mock(File.class, name);

    when(mockFile.isDirectory()).thenReturn(false);
    when(mockFile.getName()).thenReturn(name);

    return mockFile;
  }

  @Test
  public void resolveArgumentWithSingleArgumentResolvesSingleArgument() {
    assertThat(ListFiles.resolveArgument(asArray("/path/to/directory"))).isEqualTo("/path/to/directory");
  }

  @Test
  public void resolveArgumentWithMultipleArgumentsResolvesFirstArgument() {
    assertThat(ListFiles.resolveArgument(asArray("/path/to/directory", "/path/to/another/directory")))
      .isEqualTo("/path/to/directory");
  }

  @Test(expected = IllegalArgumentException.class)
  public void resolveArgumentWithNullArgumentsThrowsIllegalArgumentException() {
    try {
      ListFiles.resolveArgument(null);
    }
    catch (Exception expected) {
      assertThat(expected).hasMessage("Directory is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void resolveArgumentWithEmptyArgumentsThrowsIllegalArgumentException() {
    try {
      ListFiles.resolveArgument(StringUtils.EMPTY_STRING_ARRAY);
    }
    catch (Exception expected) {
      assertThat(expected).hasMessage("Directory is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void resolveArgumentWithAnEmptyArgumentThrowsIllegalArgumentException() {
    try {
      ListFiles.resolveArgument(new String[] { null });
    }
    catch (Exception expected) {
      assertThat(expected).hasMessage("Directory is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void resolveDirectoryWithValidDirectory() {
    assertThat(ListFiles.resolveDirectory(asArray(FileSystemUtils.WORKING_DIRECTORY.getAbsolutePath())))
      .isEqualTo(FileSystemUtils.WORKING_DIRECTORY);
  }

  @Test
  public void resolvesDirectoryWithSingleValidDirectory() {
    assertThat(ListFiles.resolveDirectory(asArray(FileSystemUtils.USER_HOME_DIRECTORY.getAbsolutePath(),
      "/path/to/invalid/directory"))).isEqualTo(FileSystemUtils.USER_HOME_DIRECTORY);
  }

  @Test(expected = IllegalArgumentException.class)
  public void resolveDirectoryWithInvalidDirectoryThrowsIllegalArgumentException() {
    try {
      ListFiles.resolveDirectory(asArray("/path/to/invalid/directory"));
    }
    catch (IllegalArgumentException expected) {
      assertThat(expected).hasMessage("Argument [/path/to/invalid/directory] is not a valid directory");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void resolveDirectoryWithFileThrowsIllegalArgumentException() {

    String pathname = getLocation(ListFiles.class).getAbsolutePath();

    try {
      ListFiles.resolveDirectory(asArray(pathname));
    }
    catch (IllegalArgumentException expected) {
      assertThat(expected).hasMessage("Argument [%s] is not a valid directory", pathname);
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void newListFilesReturnsCurrentWorkingDirectory() {

    ListFiles listFiles = newListFiles();

    assertThat(listFiles).isNotNull();
    assertThat(listFiles.getDirectory()).isEqualTo(FileSystemUtils.WORKING_DIRECTORY);
  }

  @Test
  public void newListFilesWithUserHomeDirectory() {

    ListFiles listFiles = newListFiles(FileSystemUtils.USER_HOME_DIRECTORY);

    assertThat(listFiles).isNotNull();
    assertThat(listFiles.getDirectory()).isEqualTo(FileSystemUtils.USER_HOME_DIRECTORY);
  }

  @Test(expected = IllegalArgumentException.class)
  public void newListFilesWithInvalidDirectoryThrowsIllegalArgumentException() {
    try {
      newListFiles(newFile("/path/to/invalid/directory"));
    }
    catch (IllegalArgumentException expected) {
      assertThat(expected).hasMessage("File [/path/to/invalid/directory] is not a valid directory");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void newListFilesWithFileThrowsIllegalArgumentException() {

    File listFilesClass = getLocation(ListFiles.class);

    assertThat(listFilesClass).isNotNull();
    assertThat(listFilesClass.isFile()).isTrue();

    try {
      newListFiles(listFilesClass);
    }
    catch (IllegalArgumentException expected) {
      assertThat(expected).hasMessage("File [%s] is not a valid directory", listFilesClass.getAbsolutePath());
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void newListFilesWithNullThrowsIllegalArgumentException() {
    try {
      newListFiles(null);
    }
    catch (IllegalArgumentException expected) {
      assertThat(expected).hasMessage("File [null] is not a valid directory");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void constructListFilesWithTemporaryDirectory() {

    ListFiles listFiles = new ListFiles(FileSystemUtils.TEMPORARY_DIRECTORY);

    assertThat(listFiles).isNotNull();
    assertThat(listFiles.getDirectory()).isEqualTo(FileSystemUtils.TEMPORARY_DIRECTORY);
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructListFilesWithInvalidDirectoryThrowsIllegalArgumentException() {
    try {
      new ListFiles(newFile("/path/to/invalid/directory"));
    }
    catch (IllegalArgumentException expected) {
      assertThat(expected).hasMessage("File [/path/to/invalid/directory] is not a valid directory");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void runPrintsHeaderAndListFiles() {

    ListFiles listFiles = spy(newListFiles(FileSystemUtils.TEMPORARY_DIRECTORY));

    doAnswer(invocation -> invocation.getArgument(0)).when(listFiles).printHeader(any(File.class));
    doNothing().when(listFiles).listFiles(any(File.class), any());

    listFiles.run();

    verify(listFiles, times(1)).printHeader(eq(FileSystemUtils.TEMPORARY_DIRECTORY));
    verify(listFiles, times(1)).listFiles(
      eq(FileSystemUtils.TEMPORARY_DIRECTORY), eq(StringUtils.EMPTY_STRING));
  }

  @Test(expected = IllegalArgumentException.class)
  public void listFilesWithInvalidDirectoryThrowsIllegalArgumentException() {
    try {
      newListFiles().listFiles(newFile("/path/to/invalid/directory"), StringUtils.SINGLE_SPACE);
    }
    catch (IllegalArgumentException expected) {
      assertThat(expected).hasMessage("File [/path/to/invalid/directory] is not a valid directory");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void listFilesWithFileThrowsIllegalArgumentException() {

    File listFilesClass = getLocation(ListFiles.class);

    assertThat(listFilesClass).isNotNull();
    assertThat(listFilesClass.isFile()).isTrue();

    try {
      newListFiles().listFiles(listFilesClass, StringUtils.SINGLE_SPACE);
    }
    catch (IllegalArgumentException expected) {
      assertThat(expected).hasMessage("File [%s] is not a valid directory", listFilesClass.getAbsolutePath());
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void buildDirectoryContentIndentWithStar() {
    assertThat(newListFiles().buildDirectoryContentIndent("*")).isEqualTo("*   |");
  }

  @Test
  public void buildDirectoryContentWithNullIndent() {
    assertThat(newListFiles().buildDirectoryContentIndent(null)).isEqualTo(" |");
  }

  @Test
  public void buildDirectoryContentWithEmptyIndent() {
    assertThat(newListFiles().buildDirectoryContentIndent(StringUtils.EMPTY_STRING)).isEqualTo(" |");
  }

  @Test
  public void buildDirectoryContentWithBlankIndent() {
    assertThat(newListFiles().buildDirectoryContentIndent("   ")).isEqualTo(" |");
  }

  @Test
  public void concatIndentPlusDirectoryNameWithIndent() {
    assertThat(newListFiles().concatIndentAndDirectoryName(" |", newFile("/absolute/path/to/directory")))
      .isEqualTo(" |- +++ directory");
  }

  @Test
  public void concatIndentPlusDirectoryNameWithEmptyIndent() {
    assertThat(newListFiles().concatIndentAndDirectoryName("  ", newFile("relative/path/to/directory")))
      .isEqualTo("+++ directory");
  }

  @Test
  public void concatIndentPlusDirectoryNameWithNullIndent() {
    assertThat(newListFiles().concatIndentAndDirectoryName(null, newFile("directory")))
      .isEqualTo("+++ directory");
  }

  @Test
  public void concatIndentPlusFileNameWithIndent() {
    assertThat(newListFiles().concatIndentAndFileName(" |", newFile("/absolute/path/to/file")))
      .isEqualTo(" |  --- file");
  }

  @Test
  public void concatIndentPlusFileNameWithEmptyIndent() {
    assertThat(newListFiles().concatIndentAndFileName(StringUtils.EMPTY_STRING, newFile("/absolute/path/to/file")))
      .isEqualTo("  --- file");
  }

  @Test
  public void sortIsCorrect() {

    File mockDirectoryOne = mockDirectory("/path/to/directory/one");
    File mockDirectoryTwo = mockDirectory("/path/to/directory/two");
    File mockFileOne = mockFile("/path/to/file/one");
    File mockFileTwo = mockFile("/path/to/file/two");

    File[] files = asArray(mockFileTwo, mockDirectoryOne, mockFileOne, mockDirectoryTwo);
    File[] sortedFiles = newListFiles().sort(files);

    assertThat(sortedFiles).isSameAs(files);
    assertThat(sortedFiles).containsExactly(mockDirectoryOne, mockDirectoryTwo, mockFileOne, mockFileTwo);
  }
}
