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

package org.cp.elements.tools;

import java.net.URL;

import org.cp.elements.lang.ClassUtils;

/**
 * The ClassLocator class is command-line tool used to locate one or more classes given their binary names.
 *
 * @author John J. Blum
 * @see java.lang.Class
 * @see org.cp.elements.lang.ClassUtils
 * @since 1.0.0
 */
public class ClassLocator {

  public static void main(final String[] args) {
    for (String binaryName : args) {
      URL url = ClassUtils.locateClass(binaryName);
      System.out.printf("class [%1$s] %2$s%n", binaryName, (url != null ? String.format("is found in [%1$s]", url)
        : "was not found"));
    }
  }

}
