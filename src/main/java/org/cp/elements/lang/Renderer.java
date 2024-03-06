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
 * Interface defining a contract for implementing objects responsible for rendering another object as a {@link String}.
 *
 * @author John J. Blum
 * @param <T> {@link Class type} of {@link Object} to render.
 * @see java.lang.FunctionalInterface
 * @see java.lang.String
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface Renderer<T> {

  /**
   * Renders the given {@link Object} of generic type {@link T} as a {@link String}.
   *
   * @param obj {@link Object} to render as a {@link String}.
   * @return a {@link String} representation of the given  {@link Object}.
   */
  String render(T obj);

}
