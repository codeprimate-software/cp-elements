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
 * The {@link Builder} interface defines a contract for {@link Object objects} who's {@link Class types}
 * implement the {@literal Builder Software Design Pattern}.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object} to {@literal build}.
 * @see java.lang.FunctionalInterface
 * @see <a href="https://en.wikipedia.org/wiki/Builder_pattern">Builder Software Design Pattern</a>
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface Builder<T> {

  /**
   * Builds an {@link Object} of type {@link T}.
   *
   * @return the built {@link Object}.
   */
  T build();

}
