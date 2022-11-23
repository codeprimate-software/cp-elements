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
package org.cp.elements.tools;

import java.net.URL;
import java.util.Arrays;
import java.util.Optional;

import org.cp.elements.lang.ClassUtils;

/**
 * The {@link ClassLocator} class is command-line tool used to locate one or more {@link Class classes}
 * given their binary names.
 *
 * @author John J. Blum
 * @see java.lang.Class
 * @see java.net.URL
 * @see org.cp.elements.lang.ClassUtils
 * @since 1.0.0
 */
public class ClassLocator {

  public static void main(String[] args) {
    Arrays.stream(args).forEach(binaryClassName -> {
      System.out.printf("class [%1$s] %2$s%n", binaryClassName, resolve(binaryClassName)
        .map(url -> String.format("was found in [%s]", url)).orElse("was not found"));
    });
  }

  private static Optional<URL> resolve(String binaryClassName) {
    return Optional.ofNullable(ClassUtils.locateClass(binaryClassName));
  }

  private ClassLocator() { }

}
