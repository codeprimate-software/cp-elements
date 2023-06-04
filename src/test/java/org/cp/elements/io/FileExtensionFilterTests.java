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

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.test.AbstractTestSuite;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.stream.StreamUtils;

/**
 * Unit Tests for {@link FileExtensionFilter}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.io.FileExtensionFilter
 * @see org.cp.elements.test.AbstractTestSuite
 * @since 1.0.0
 */
public class FileExtensionFilterTests extends AbstractTestSuite {

  private @NotNull File newFile(@NotNull String pathname) {
    return new File(pathname);
  }

  @SuppressWarnings("unused")
  private @NotNull File newFile(@Nullable File parent, @NotNull String pathname) {
    return new File(parent, pathname);
  }

  @Test
  public void constructFileExtensionFilterWithClassExtension() {

    FileExtensionFilter fileFilter = new FileExtensionFilter("class");

    Set<String> fileExtensions = fileFilter.getFileExtensions();

    assertThat(fileExtensions).isNotNull();
    assertThat(fileExtensions).hasSize(1);
    assertThat(fileExtensions.contains("class")).isTrue();
    assertThat(fileExtensions.contains(".class")).isFalse();
  }

  @Test
  public void constructFileExtensionFilterWithDotJavaExtension() {

    FileExtensionFilter fileFilter = new FileExtensionFilter(".java");

    Set<String> fileExtensions = fileFilter.getFileExtensions();

    assertThat(fileExtensions).isNotNull();
    assertThat(fileExtensions).hasSize(1);
    assertThat(fileExtensions.contains("java")).isTrue();
    assertThat(fileExtensions.contains(".java")).isFalse();
  }

  @Test
  public void constructFileExtensionFilterWithArrayOfFileExtensions() {

    String[] expectedFileExtensions = { "ada", ".c", ".cpp", "groovy", "java", ".kt", ".rb" };

    FileExtensionFilter fileFilter = new FileExtensionFilter(expectedFileExtensions);

    Set<String> actualFileExtensions = fileFilter.getFileExtensions();

    assertThat(actualFileExtensions).isNotNull();
    assertThat(actualFileExtensions).hasSize(expectedFileExtensions.length);
    assertThat(actualFileExtensions.containsAll(StreamUtils.stream(expectedFileExtensions)
      .map(FileExtensionFilter.FILE_EXTENSION_RESOLVER)
      .collect(Collectors.toSet())))
      .isTrue();
  }

  @Test
  public void constructFileExtensionFilterWithIterableOfFileExtensions() {

    Iterable<String> expectedFileExtensions = ArrayUtils.asIterable(".groovy", "java", ".kt");

    FileExtensionFilter fileFilter = new FileExtensionFilter(expectedFileExtensions);

    Set<String> actualFileExtensions = fileFilter.getFileExtensions();

    assertThat(actualFileExtensions).isNotNull();
    assertThat(actualFileExtensions).hasSize(3);
    assertThat(actualFileExtensions.containsAll(StreamUtils.stream(expectedFileExtensions)
      .map(FileExtensionFilter.FILE_EXTENSION_RESOLVER)
      .collect(Collectors.toSet())))
      .isTrue();
  }

  @Test
  public void constructFileExtensionFilterWithBlankEmptyAndNullFileExtensions() {

    FileExtensionFilter fileFilter = new FileExtensionFilter("  ", "", null);

    Set<String> fileExtensions = fileFilter.getFileExtensions();

    assertThat(fileExtensions).isNotNull();
    assertThat(fileExtensions).isEmpty();
  }

  @Test
  public void constructFileExtensionFilterWithNullArrayOfFileExtensionsIsNullSafe() {

    FileExtensionFilter fileFilter = new FileExtensionFilter((String[]) null);

    Set<String> fileExtensions = fileFilter.getFileExtensions();

    assertThat(fileExtensions).isNotNull();
    assertThat(fileExtensions).isEmpty();
  }

  @Test
  public void constructFileExtensionsWithNullIterableOfFileExtensionsIsNullSafe() {

    FileExtensionFilter fileFilter = new FileExtensionFilter((Iterable<String>) null);

    Set<String> fileExtensions = fileFilter.getFileExtensions();

    assertThat(fileExtensions).isNotNull();
    assertThat(fileExtensions).isEmpty();
  }

  @Test
  public void acceptsAllFiles() {

    FileExtensionFilter fileFilter = new FileExtensionFilter();

    Set<String> fileExtensions = fileFilter.getFileExtensions();

    assertThat(fileExtensions).isNotNull();
    assertThat(fileExtensions).isEmpty();

    File fileWithExtension = newFile("/path/to/file.ext");
    File fileWithNoExtension = newFile("/path/to/some/file");
    File fileWithNoName = newFile("/path/to/some/no/named/file/.ext");

    assertThat(fileFilter.accept(fileWithExtension)).isTrue();
    assertThat(fileFilter.accept(fileWithNoExtension)).isTrue();
    assertThat(fileFilter.accept(fileWithNoName)).isTrue();
  }

  @Test
  public void acceptsClassFiles() {

    FileExtensionFilter classFileFilter = new FileExtensionFilter("class");

    Set<String> fileExtensions = classFileFilter.getFileExtensions();

    assertThat(fileExtensions).isNotNull();
    assertThat(fileExtensions).hasSize(1);
    assertThat(fileExtensions).containsExactly("class");

    File cFile = newFile("/absolute/path/to/source.c");
    File classFile = newFile("/class/path/to/java/file.class");
    File groovyFile = newFile("/path/to/file.groovy");
    File javaFile = newFile("/path/to/file.java");
    File kotlinFile = newFile("/path/to/file.kt");

    assertThat(classFileFilter.accept(cFile)).isFalse();
    assertThat(classFileFilter.accept(classFile)).isTrue();
    assertThat(classFileFilter.accept(groovyFile)).isFalse();
    assertThat(classFileFilter.accept(javaFile)).isFalse();
    assertThat(classFileFilter.accept(kotlinFile)).isFalse();
    assertThat(classFileFilter.accept(FileSystemUtils.WORKING_DIRECTORY)).isFalse();
  }

  @Test
  public void acceptsJavaClassFilesAndFilesWithNoExtension() {

    FileExtensionFilter fileFilter = new FileExtensionFilter("java", ".CLASS", ".");

    Set<String> fileExtensions = fileFilter.getFileExtensions();

    assertThat(fileExtensions).isNotNull();
    assertThat(fileExtensions).hasSize(3);
    assertThat(fileExtensions).containsExactlyInAnyOrder("class", "java", "");
    assertThat(fileFilter.accept(newFile("/path/to/source.java"))).isTrue();
    assertThat(fileFilter.accept(newFile("classpath/to/file.class"))).isTrue();
    assertThat(fileFilter.accept(newFile("/path/to/file/with/no/extension"))).isTrue();
    assertThat(fileFilter.accept(newFile("file."))).isTrue();
    assertThat(fileFilter.accept(newFile("file"))).isTrue();
    assertThat(fileFilter.accept(newFile(".ext"))).isFalse();
    assertThat(fileFilter.accept(newFile("/path/to/source.c"))).isFalse();
    assertThat(fileFilter.accept(newFile("/path/to/source.cpp"))).isFalse();
    assertThat(fileFilter.accept(newFile("/path/to/source.groovy"))).isFalse();
    assertThat(fileFilter.accept(newFile("/path/to/source.kotlin"))).isFalse();
    assertThat(fileFilter.accept(newFile("/path/to/class.file"))).isFalse();
  }

  @Test
  public void iterationOfFileExtensions() {

    String[] expectedFileExtensions = { "a", "b", "c", "d", "e", "f" };

    FileExtensionFilter fileFilter = new FileExtensionFilter(expectedFileExtensions);

    List<String> actualFileExtensions = new ArrayList<>(expectedFileExtensions.length);

    for (String fileExtension : fileFilter) {
      actualFileExtensions.add(fileExtension);
    }

    assertThat(actualFileExtensions).hasSize(expectedFileExtensions.length);
    assertThat(actualFileExtensions).containsExactly(expectedFileExtensions);
  }

  @Test
  public void rejectsNullFilesIsNullSafe() {

    FileExtensionFilter fileFilter = new FileExtensionFilter("null", "nil");

    assertThat(fileFilter.getFileExtensions()).containsExactlyInAnyOrder("null", "nil");
    assertThat(fileFilter.accept(null)).isFalse();
  }
}
