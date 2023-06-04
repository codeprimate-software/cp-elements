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
package org.cp.elements.tools.io;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
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

import org.junit.jupiter.api.Test;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.test.AbstractTestSuite;

/**
 * Unit Tests for {@link ListFiles}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Spy
 * @see org.cp.elements.tools.io.ListFiles
 * @since 1.0.0
 */
public class ListFilesTests extends AbstractTestSuite {

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

  @Test
  public void resolveArgumentWithNullArgumentsThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ListFiles.resolveArgument(null))
      .withMessage("Directory is required")
      .withNoCause();
  }

  @Test
  public void resolveArgumentWithEmptyArgumentsThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ListFiles.resolveArgument(StringUtils.EMPTY_STRING_ARRAY))
      .withMessage("Directory is required")
      .withNoCause();
  }

  @Test
  public void resolveArgumentWithAnEmptyArgumentThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ListFiles.resolveArgument(new String[] { null }))
      .withMessage("Directory is required")
      .withNoCause();
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

  @Test
  public void resolveDirectoryWithInvalidDirectoryThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ListFiles.resolveDirectory(asArray("/path/to/invalid/directory")))
      .withMessage("Argument [/path/to/invalid/directory] is not a valid directory")
      .withNoCause();
  }

  @Test
  public void resolveDirectoryWithFileThrowsIllegalArgumentException() {

    String pathname = getLocation(ListFiles.class).getAbsolutePath();

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ListFiles.resolveDirectory(asArray(pathname)))
      .withMessage("Argument [%s] is not a valid directory", pathname)
      .withNoCause();
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

  @Test
  public void newListFilesWithInvalidDirectoryThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> newListFiles(newFile("/path/to/invalid/directory")))
      .withMessage("File [/path/to/invalid/directory] is not a valid directory")
      .withNoCause();
  }

  @Test
  public void newListFilesWithFileThrowsIllegalArgumentException() {

    File listFilesClass = getLocation(ListFiles.class);

    assertThat(listFilesClass).isNotNull();
    assertThat(listFilesClass.isFile()).isTrue();

    assertThatIllegalArgumentException()
      .isThrownBy(() -> newListFiles(listFilesClass))
      .withMessage("File [%s] is not a valid directory", listFilesClass.getAbsolutePath())
      .withNoCause();
  }

  @Test
  public void newListFilesWithNullThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> newListFiles(null))
      .withMessage("File [null] is not a valid directory")
      .withNoCause();
  }

  @Test
  public void constructListFilesWithTemporaryDirectory() {

    ListFiles listFiles = new ListFiles(FileSystemUtils.TEMPORARY_DIRECTORY);

    assertThat(listFiles).isNotNull();
    assertThat(listFiles.getDirectory()).isEqualTo(FileSystemUtils.TEMPORARY_DIRECTORY);
  }

  @Test
  public void constructListFilesWithInvalidDirectoryThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ListFiles(newFile("/path/to/invalid/directory")))
      .withMessage("File [/path/to/invalid/directory] is not a valid directory")
      .withNoCause();
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

  @Test
  public void listFilesWithInvalidDirectoryThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> newListFiles().listFiles(newFile("/path/to/invalid/directory"),
        StringUtils.SINGLE_SPACE))
      .withMessage("File [/path/to/invalid/directory] is not a valid directory")
      .withNoCause();
  }

  @Test
  public void listFilesWithFileThrowsIllegalArgumentException() {

    File listFilesClass = getLocation(ListFiles.class);

    assertThat(listFilesClass).isNotNull();
    assertThat(listFilesClass.isFile()).isTrue();

    assertThatIllegalArgumentException()
      .isThrownBy(() -> newListFiles().listFiles(listFilesClass, StringUtils.SINGLE_SPACE))
      .withMessage("File [%s] is not a valid directory", listFilesClass.getAbsolutePath())
      .withNoCause();
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
