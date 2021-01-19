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
import static org.cp.elements.util.ArrayUtils.asIterable;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.cp.elements.lang.StringUtils;
import org.cp.elements.test.AbstractBaseTestSuite;
import org.cp.elements.test.TestUtils;
import org.cp.elements.util.stream.StreamUtils;
import org.junit.Test;

/**
 * Unit Tests {@link FileExtensionFilter}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.cp.elements.io.FileExtensionFilter
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @since 1.0.0
 */
public class FileExtensionFilterTests extends AbstractBaseTestSuite {

  private File newFile(String pathname) {
    return new File(pathname);
  }

  private File newFile(File parent, String pathname) {
    return new File(parent, pathname);
  }

  @Test
  public void constructWithClassExtension() {

    FileExtensionFilter fileFilter = new FileExtensionFilter("class");

    Set<String> fileExtensions = fileFilter.getFileExtensions();

    assertThat(fileExtensions).isInstanceOf(Set.class);
    assertThat(fileExtensions.size()).isEqualTo(1);
    assertThat(fileExtensions.contains("class")).isTrue();
    assertThat(fileExtensions.contains(".class")).isFalse();
  }

  @Test
  public void constructWithDotJavaExtension() {

    FileExtensionFilter fileFilter = new FileExtensionFilter(".java");

    Set<String> fileExtensions = fileFilter.getFileExtensions();

    assertThat(fileExtensions).isInstanceOf(Set.class);
    assertThat(fileExtensions.size()).isEqualTo(1);
    assertThat(fileExtensions.contains(".java")).isFalse();
    assertThat(fileExtensions.contains("java")).isTrue();
  }

  @Test
  public void constructWithAnArrayOfFileExtensions() {

    String[] expectedFileExtensions = { "ada", ".c", ".cpp", "groovy", "java", ".rb" };

    FileExtensionFilter fileFilter = new FileExtensionFilter(expectedFileExtensions);

    Set<String> actualFileExtensions = fileFilter.getFileExtensions();

    assertThat(actualFileExtensions).isInstanceOf(Set.class);
    assertThat(actualFileExtensions.size()).isEqualTo(expectedFileExtensions.length);
    assertThat(actualFileExtensions.containsAll(StreamUtils.stream(expectedFileExtensions)
      .map((fileExtension) -> fileExtension.startsWith(StringUtils.DOT_SEPARATOR) ? fileExtension
        .substring(1) : fileExtension)
      .collect(Collectors.toSet()))).isTrue();
  }

  @Test
  public void constructWithAnIterableCollectionOfFileExtensions() {

    Iterable<String> expectedFileExtensions = asIterable(".groovy", "java");

    FileExtensionFilter fileFilter = new FileExtensionFilter(expectedFileExtensions);

    Set<String> actualFileExtensions = fileFilter.getFileExtensions();

    assertThat(actualFileExtensions).isInstanceOf(Set.class);
    assertThat(actualFileExtensions.size()).isEqualTo(2);
    assertThat(actualFileExtensions.containsAll(StreamUtils.stream(expectedFileExtensions)
      .map((fileExtension) -> fileExtension.startsWith(StringUtils.DOT_SEPARATOR) ? fileExtension.substring(1)
        : fileExtension)
      .collect(Collectors.toSet()))).isTrue();
  }

  @Test
  public void constructWithBlankEmptyAndNullFileExtensions() {

    FileExtensionFilter fileFilter = new FileExtensionFilter("  ", "", null);

    Set<String> fileExtensions = fileFilter.getFileExtensions();

    assertThat(fileExtensions).isInstanceOf(Set.class);
    assertThat(fileExtensions.isEmpty()).isTrue();
  }

  @Test
  public void constructWithNullArrayOfFileExtensions() {

    FileExtensionFilter fileFilter = new FileExtensionFilter((String[]) null);

    Set<String> fileExtensions = fileFilter.getFileExtensions();

    assertThat(fileExtensions).isInstanceOf(Set.class);
    assertThat(fileExtensions.isEmpty()).isTrue();
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructWithNullIterableCollectionFoFileExtensions() {
    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> new FileExtensionFilter((Iterable<String>) null),
      () -> "Iterable cannot be null");
  }

  @Test
  public void acceptsAllFilesWithOrWithoutAnExtension() {

    FileExtensionFilter fileFilter = new FileExtensionFilter();

    Set<String> fileExtensions = fileFilter.getFileExtensions();

    assertThat(fileExtensions).isInstanceOf(Set.class);
    assertThat(fileExtensions.isEmpty()).isTrue();

    File fileWithExtension = newFile("/path/to/file.ext");
    File fileWithNoExtension = newFile("/path/to/some/file");

    assertThat(fileFilter.accept(fileWithExtension)).isTrue();
    assertThat(fileFilter.accept(fileWithNoExtension)).isTrue();
  }

  @Test
  public void acceptsClassFiles() {

    FileExtensionFilter classFileFilter = new FileExtensionFilter("class");

    Set<String> fileExtensions = classFileFilter.getFileExtensions();

    assertThat(fileExtensions).isInstanceOf(Set.class);
    assertThat(fileExtensions.size()).isEqualTo(1);
    assertThat(fileExtensions.contains("class")).isTrue();

    File cFile = newFile("absolute/path/to/source.c");
    File classFile = newFile("/class/path/to/java/file.class");
    File javaFile = newFile("/path/to/file.java");

    assertThat(classFileFilter.accept(cFile)).isFalse();
    assertThat(classFileFilter.accept(classFile)).isTrue();
    assertThat(classFileFilter.accept(javaFile)).isFalse();
    assertThat(classFileFilter.accept(FileSystemUtils.WORKING_DIRECTORY)).isFalse();
  }

  @Test
  public void acceptsJavaClassFilesAndFilesWithoutAnExtension() {

    FileExtensionFilter fileFilter = new FileExtensionFilter("java", ".CLASS", ".");

    Set<String> fileExtensions = fileFilter.getFileExtensions();

    assertThat(fileExtensions).isInstanceOf(Set.class);
    assertThat(fileExtensions.size()).isEqualTo(3);
    assertThat(fileExtensions.containsAll(Arrays.asList("class", "java", ""))).isTrue();
    assertThat(fileFilter.accept(newFile("/path/to/source.java"))).isTrue();
    assertThat(fileFilter.accept(newFile("class/path/to/file.class"))).isTrue();
    assertThat(fileFilter.accept(newFile("/path/to/file/with/no/extension"))).isTrue();
    assertThat(fileFilter.accept(newFile("file."))).isTrue();
    assertThat(fileFilter.accept(newFile("file"))).isTrue();
    assertThat(fileFilter.accept(newFile("/path/to/source.cpp"))).isFalse();
    assertThat(fileFilter.accept(newFile("/path/to/source.c"))).isFalse();
    assertThat(fileFilter.accept(newFile("/path/to/class.file"))).isFalse();
  }

  @Test
  public void iterationOfFileExtensions() {

    String[] expectedFileExtensions = { "a", "b", "c", "d", "e" };

    FileExtensionFilter fileExtensionFilter = new FileExtensionFilter(expectedFileExtensions);

    List<String> actualFileExtensions = new ArrayList<>(expectedFileExtensions.length);

    for (String fileExtension : fileExtensionFilter) {
      actualFileExtensions.add(fileExtension);
    }

    assertThat(actualFileExtensions.size()).isEqualTo(expectedFileExtensions.length);
    assertThat(actualFileExtensions.containsAll(Arrays.asList(expectedFileExtensions))).isTrue();
  }
}
