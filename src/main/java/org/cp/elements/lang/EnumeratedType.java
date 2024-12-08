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
package org.cp.elements.lang;

/**
 * Abstract Data Type (ADT) defining a {@link Class} that is 1 of many {@link Enum enumerated values}.
 * <p/>
 * For example, an instance of a {@literal Currency} {@link Class type} might be {@literal USD}, {@literal EURO},
 * {@literal YEN} {@literal CRYPTO}, etc. That is, the {@literal Currency} {@link Class type} is not itself
 * an {@link Enum}, but is classified by a single {@link Enum enumerated value}.
 *
 * @author John Blum
 * @see java.lang.Enum
 * @see java.lang.FunctionalInterface
 * @since 2.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface EnumeratedType<T extends Enum<T>> {

  /**
   * Return the single {@link Enum enumerated value} classifying this {@link Class type}.
   *
   * @return an {@link Enum enumerated value} classifying this {@link Class type}.
   * @see java.lang.Enum
   */
  T getType();

}
