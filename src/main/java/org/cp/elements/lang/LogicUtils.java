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

package org.cp.elements.lang;

/**
 * The LogicUtils class is a utility class for implementing logic operations.
 * 
 * @author John J. Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class LogicUtils {

  /**
   * Implementation of exclusive OR (XOR), meaning 1 and only 1 value can be true.  If both values are true or both
   * values are false the result of XOR is false.
   * 
   * @param value1 the first value in the XOR expression.
   * @param value2 the second value in the XOR expression.
   * @return true if an only if 1 of the 2 values is true.
   */
  public static boolean xor(final boolean value1, final boolean value2) {
    return ((value1 || value2) && !(value1 && value2));
  }

}
