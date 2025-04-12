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

import java.io.File;
import java.nio.file.Path;
import java.util.jar.JarFile;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * Abstract utility class for processing {@link JarFile JAR files}.
 *
 * @author John Blum
 * @see java.util.jar.JarFile
 * @see org.cp.elements.util.zip.ZipUtils
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public abstract class JarUtils extends ZipUtils {

  public static final String JAR_FILE_RESOURCE_PATH_SEPARATOR = "!";
  public static final String JAR_FILE_RESOURCE_PATH = JAR_FILE_RESOURCE_PATH_SEPARATOR.concat(File.separator);
  public static final String JAR_FILE_SCHEME = "jar:file:";

  /**
   * Determines whether the given {@link Path} represents a location of or resource path in a {@link JarFile JAR file}.
   *
   * @param path {@link Path} to evaluate.
   * @return a boolean vale indicating whether the given {@link Path} represents a location of or resource path
   * in a {@link JarFile JAR file}.
   * @see java.nio.file.Path
   */
  @NullSafe
  public static boolean isJarFileReference(Path path) {
    return path != null && path.toString().startsWith(JAR_FILE_SCHEME);
  }

  /**
   * Resolves the {@link Path} to the {@link JarFile} from a possible resource path in the {@link Path JAR file}.
   *
   * @param jarFile {@link Path JAR filepath} to evaluate; required.
   * @return the resolved {@link Path} to the {@link JarFile} from a possible resource path
   * in the {@link Path JAR file}.
   * @throws IllegalArgumentException if JAR file {@link Path} is {@literal null}.
   * @see java.nio.file.Path
   */
  public static @NotNull Path resolveJarFilepPath(@NotNull Path jarFile) {

    Assert.isTrue(isJarFileReference(jarFile), "Expected [%s] to be a path to a JAR file", jarFile);

    String jarFilePathname = jarFile.toString();

    int index = jarFilePathname.indexOf(JAR_FILE_RESOURCE_PATH_SEPARATOR);

    String resolvedJarFilePathname = index > -1
      ? jarFilePathname.substring(0, index)
      : jarFilePathname;

    return Path.of(resolvedJarFilePathname);
  }
}
