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
 * {@link Builder} is an interface defining a contract for objects implementing the Builder Software Design Pattern.
 *
 * @author John Blum
 * @see java.lang.FunctionalInterface
 * @see <a href="https://en.wikipedia.org/wiki/Builder_pattern">Builder Software Design Pattern</a>
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface Builder<T> {

  /**
   * Builds an object of type {@code T}.
   *
   * @return the built object.
   */
  T build();

}
