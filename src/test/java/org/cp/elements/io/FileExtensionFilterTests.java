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

import static org.cp.elements.util.ArrayUtils.asIterable;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.cp.elements.lang.StringUtils;
import org.cp.elements.test.AbstractBaseTestSuite;
import org.cp.elements.util.stream.StreamUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Test suite of test cases testing the contract and functionality of the {@link FileExtensionFilter} class.
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

  @Rule
  public ExpectedException exception = ExpectedException.none();

  protected File newFile(String pathname) {
    return new File(pathname);
  }

  protected File newFile(File parent, String pathname) {
    return new File(parent, pathname);
  }

  @Test
  public void constructWithClassExtension() {
    FileExtensionFilter fileFilter = new FileExtensionFilter("class");

    Set<String> fileExtensions = fileFilter.getFileExtensions();

    assertThat(fileExtensions, is(instanceOf(Set.class)));
    assertThat(fileExtensions.size(), is(equalTo(1)));
    assertThat(fileExtensions.contains("class"), is(true));
    assertThat(fileExtensions.contains(".class"), is(false));
  }

  @Test
  public void constructWithDotJavaExtension() {
    FileExtensionFilter fileFilter = new FileExtensionFilter(".java");

    Set<String> fileExtensions = fileFilter.getFileExtensions();

    assertThat(fileExtensions, is(instanceOf(Set.class)));
    assertThat(fileExtensions.size(), is(equalTo(1)));
    assertThat(fileExtensions.contains(".java"), is(false));
    assertThat(fileExtensions.contains("java"), is(true));
  }

  @Test
  public void constructWithAnArrayOfFileExtensions() {
    String[] expectedFileExtensions = { "ada", ".c", ".cpp", "groovy", "java", ".rb" };

    FileExtensionFilter fileFilter = new FileExtensionFilter(expectedFileExtensions);

    Set<String> actualFileExtensions = fileFilter.getFileExtensions();

    assertThat(actualFileExtensions, is(instanceOf(Set.class)));
    assertThat(actualFileExtensions.size(), is(equalTo(expectedFileExtensions.length)));
    assertThat(actualFileExtensions.containsAll(StreamUtils.stream(expectedFileExtensions)
      .map((fileExtension) -> fileExtension.startsWith(StringUtils.DOT_SEPARATOR) ? fileExtension.substring(1) : fileExtension)
      .collect(Collectors.toSet())), is(true));
  }

  @Test
  public void constructWithAnIterableCollectionOfFileExtensions() {
    Iterable<String> expectedFileExtensions = asIterable(".groovy", "java");

    FileExtensionFilter fileFilter = new FileExtensionFilter(expectedFileExtensions);

    Set<String> actualFileExtensions = fileFilter.getFileExtensions();

    assertThat(actualFileExtensions, is(instanceOf(Set.class)));
    assertThat(actualFileExtensions.size(), is(equalTo(2)));
    assertThat(actualFileExtensions.containsAll(StreamUtils.stream(expectedFileExtensions)
      .map((fileExtension) -> fileExtension.startsWith(StringUtils.DOT_SEPARATOR) ? fileExtension.substring(1)
        : fileExtension)
      .collect(Collectors.toSet())), is(true));
  }

  @Test
  public void constructWithBlankEmptyAndNullFileExtensions() {
    FileExtensionFilter fileFilter = new FileExtensionFilter("  ", "", null);

    Set<String> fileExtensions = fileFilter.getFileExtensions();

    assertThat(fileExtensions, is(instanceOf(Set.class)));
    assertThat(fileExtensions.isEmpty(), is(true));
  }

  @Test
  public void constructWithNullArrayOfFileExtensions() {
    FileExtensionFilter fileFilter = new FileExtensionFilter((String[]) null);

    Set<String> fileExtensions = fileFilter.getFileExtensions();

    assertThat(fileExtensions, is(instanceOf(Set.class)));
    assertThat(fileExtensions.isEmpty(), is(true));
  }

  @Test
  public void constructWithNullIterableCollectionFoFileExtensions() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Iterable cannot be null");

    new FileExtensionFilter((Iterable<String>) null);
  }

  @Test
  public void acceptsAllFilesWithOrWithoutAnExtension() {
    FileExtensionFilter fileFilter = new FileExtensionFilter();

    Set<String> fileExtensions = fileFilter.getFileExtensions();

    assertThat(fileExtensions, is(instanceOf(Set.class)));
    assertThat(fileExtensions.isEmpty(), is(true));

    File fileWithExtension = newFile("/path/to/file.ext");
    File fileWithNoExtension = newFile("/path/to/some/file");

    assertThat(fileFilter.accept(fileWithExtension), is(true));
    assertThat(fileFilter.accept(fileWithNoExtension), is(true));
  }

  @Test
  public void acceptsClassFiles() {
    FileExtensionFilter classFileFilter = new FileExtensionFilter("class");

    Set<String> fileExtensions = classFileFilter.getFileExtensions();

    assertThat(fileExtensions, is(instanceOf(Set.class)));
    assertThat(fileExtensions.size(), is(equalTo(1)));
    assertThat(fileExtensions.contains("class"), is(true));

    File cFile = newFile("absolute/path/to/source.c");
    File classFile = newFile("/class/path/to/java/file.class");
    File javaFile = newFile("/path/to/file.java");

    assertThat(classFileFilter.accept(cFile), is(false));
    assertThat(classFileFilter.accept(classFile), is(true));
    assertThat(classFileFilter.accept(javaFile), is(false));
    assertThat(classFileFilter.accept(FileSystemUtils.WORKING_DIRECTORY), is(false));
  }

  @Test
  public void acceptsJavaClassFilesAndFilesWithoutAnExtension() {
    FileExtensionFilter fileFilter = new FileExtensionFilter("java", ".CLASS", ".");

    Set<String> fileExtensions = fileFilter.getFileExtensions();

    assertThat(fileExtensions, is(instanceOf(Set.class)));
    assertThat(fileExtensions.size(), is(equalTo(3)));
    assertThat(fileExtensions.containsAll(Arrays.asList("class", "java", "")), is(true));
    assertThat(fileFilter.accept(newFile("/path/to/source.java")), is(true));
    assertTrue(fileFilter.accept(newFile("class/path/to/file.class")));
    assertTrue(fileFilter.accept(newFile("/path/to/file/with/no/extension")));
    assertTrue(fileFilter.accept(newFile("file.")));
    assertTrue(fileFilter.accept(newFile("file")));
    assertFalse(fileFilter.accept(newFile("/path/to/source.cpp")));
    assertFalse(fileFilter.accept(newFile("/path/to/source.c")));
    assertFalse(fileFilter.accept(newFile("/path/to/class.file")));
  }

  @Test
  public void iterationOfFileExtensions() {
    String[] expectedFileExtensions = { "a", "b", "c", "d", "e" };
    FileExtensionFilter fileExtensionFilter = new FileExtensionFilter(expectedFileExtensions);

    List<String> actualFileExtensions = new ArrayList<>(expectedFileExtensions.length);

    for (String fileExtension : fileExtensionFilter) {
      actualFileExtensions.add(fileExtension);
    }

    assertThat(actualFileExtensions.size(), is(equalTo(expectedFileExtensions.length)));
    assertThat(actualFileExtensions.containsAll(Arrays.asList(expectedFileExtensions)), is(true));
  }
}
