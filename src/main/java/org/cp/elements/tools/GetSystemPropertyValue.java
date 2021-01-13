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

import static org.cp.elements.data.struct.SystemPropertyValue.newSystemPropertyValue;

import java.util.Arrays;

/**
 * The {@link GetSystemPropertyValue} class is a command-line utility class for getting the value
 * of a Java System Property.
 *
 * @author John J. Blum
 * @see java.lang.System#getProperty(String)
 * @see org.cp.elements.data.struct.SystemPropertyValue
 * @since 1.0.0
 */
public class GetSystemPropertyValue {

  /* (non-Javadoc) */
  public static void main(final String[] args) {
    if (args.length < 1) {
      System.err.printf("$ java ... %s systemProperty [systemProperty]*%n", GetSystemPropertyValue.class.getName());
      System.exit(1);
    }

    Arrays.stream(args).forEach(systemProperty ->  System.out.println(newSystemPropertyValue(systemProperty)));
  }
}
