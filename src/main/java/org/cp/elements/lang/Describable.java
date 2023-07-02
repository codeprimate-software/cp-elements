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
 * Interface defining a contract for {@link Object Objects} that can be described.
 * <p>
 * The description can be any {@link Class type} the user decides.
 *
 * @author John Blum
 * @param <T> {@link Class type} of the {@link Object} describing {@literal this} implementing {@link Object}.
 * @see java.lang.FunctionalInterface
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface Describable<T> {

  /**
   * Gets a {@link Object description} for {@literal this} {@link Object}.
   *
   * @return a {@link Object description} for {@literal this} {@link Object}.
   */
  T getDescriptor();

}
