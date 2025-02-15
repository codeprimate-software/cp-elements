/*
 * Copyright 2017-Present Author or Authors.
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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import java.io.File;
import java.net.URI;
import java.net.URL;
import java.nio.file.Path;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.ObjectUtils;

/**
 * Unit Tests for {@link JarUtils}.
 *
 * @author John Blum
 * @see org.cp.elements.util.zip.JarUtils
 * @see org.junit.jupiter.api.Test
 * @since 2.0.0
 */
class JarUtilsUnitTests {

  @Test
  void isJarFileReferenceReturnsTrue() {

    Stream.of("jar:file:/", "jar:file:/path/to/non-existing.jar", "jar:file:/path/to/test.jar!/bin/shell-script.sh")
      .map(Path::of)
      .forEach(path -> assertThat(JarUtils.isJarFileReference(path)).isTrue());
  }

  @Test
  void isJarFileResourceReturnsFalse() {

    Stream.of(FileSystemUtils.WORKING_DIRECTORY, File.separator, ObjectUtils.locateClass(getClass().getName()))
      .map(resource -> resource instanceof URL url
        ? ObjectUtils.doSafely(args -> url.toURI())
        : URI.create(FileSystemUtils.FILE_SCHEME.concat(resource.toString())))
      .map(Path::of)
      .forEach(path -> assertThat(JarUtils.isJarFileReference(path)).isFalse());
  }

  @Test
  void isJarFileResourceForNullIsNullSafeReturnsFalse() {
    assertThat(JarUtils.isJarFileReference(null)).isFalse();
  }

  @Test
  void resolvesJarFileFromJarFileResourcePath() {

    Path path = Path.of("jar:file:/Users/jxblum/.m2/repository/org/codeprimate/domain/cp-domain/0.2.0/cp-domain-0.2.0.jar"
      + "!/org/cp/domain/core/model/Person.class");

    Path resolvedPath = JarUtils.resolveJarFilepPath(path);

    assertThat(resolvedPath).isNotNull();
    assertThat(resolvedPath.toString())
      .isEqualTo("jar:file:/Users/jxblum/.m2/repository/org/codeprimate/domain/cp-domain/0.2.0/cp-domain-0.2.0.jar");
  }

  @Test
  void resolvesJarFileFromInvalidJarFiles() {

    Stream.of(FileSystemUtils.WORKING_DIRECTORY, "file:/path/to/test.jar", "  ", "", null)
      .map(resource -> resource != null ? Path.of(resource.toString()) : null)
      .forEach(path -> assertThatIllegalArgumentException()
        .isThrownBy(() -> JarUtils.resolveJarFilepPath(path))
        .withMessage("Expected [%s] to be a path to a JAR file", path)
        .withNoCause());
  }
}
