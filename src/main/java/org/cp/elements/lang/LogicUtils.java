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
package org.cp.elements.lang;

/**
 * Abstract utility class containing common logic operations.
 *
 * @author John J. Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class LogicUtils {

  /**
   * Implementation of {@literal exclusive OR (XOR)}.
   * <p>
   * In {@literal exclusive OR (XOR)}, one, and only one, value can be {@literal true}. If both values
   * are {@literal true} or both values are {@literal false}, then the {@link Boolean result} of {@literal XOR}
   * is {@literal false}.
   *
   * @param valueOne {@link Boolean first value} in the {@literal XOR expression}.
   * @param valueTwo {@link Boolean second value} in the {@literal XOR expression}.
   * @return {@literal true} only if one of the two {@link Boolean values} is {@literal true},
   * otherwise, returns {@literal false}.
   */
  public static boolean xor(boolean valueOne, boolean valueTwo) {
    return (valueOne || valueTwo) && !(valueOne && valueTwo);
  }
}
