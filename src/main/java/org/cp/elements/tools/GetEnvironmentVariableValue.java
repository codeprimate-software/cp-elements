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

import static org.cp.elements.data.struct.EnvironmentVariableValue.newEnvironmentVariableValue;

import java.util.Arrays;

/**
 * The {@link GetEnvironmentVariableValue} class is a command-line utility class for the getting the value
 * of a System Environment Variable.
 *
 * @author John J. Blum
 * @see java.lang.System#getenv()
 * @since 1.0.0
 */
public class GetEnvironmentVariableValue {

  /* (non-Javadoc) */
  public static void main(String[] args) {
    if (args.length < 1) {
      System.err.printf("$ java ... %s environmentVariable [environmentVariable]*%n",
        GetEnvironmentVariableValue.class.getName());
      System.exit(1);
    }

    Arrays.stream(args).forEach(environmentVariable ->
      System.out.println(newEnvironmentVariableValue(environmentVariable)));
  }
}
