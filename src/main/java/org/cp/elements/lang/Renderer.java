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
 * The {@link Renderer} interface defines a contract for objects responsible for rendering a target object
 * as a {@link String}.
 *
 * @author John J. Blum
 * @param <T> the class type of the object to render.
 * @see java.lang.FunctionalInterface
 * @see java.lang.String
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface Renderer<T> {

  /**
   * Renders the specified Object of generic type T as a String.
   *
   * @param obj the Object to render as a String.
   * @return a String representation of the specified Object.
   * @see java.lang.String
   */
  String render(T obj);

}
