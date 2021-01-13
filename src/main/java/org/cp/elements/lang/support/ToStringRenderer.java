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
package org.cp.elements.lang.support;

import org.cp.elements.lang.Renderer;

/**
 * Implementation of the {@link Renderer} interface that calls {@link Object#toString()}
 * on the specified {@link Object} to render.
 *
 * @author John J. Blum
 * @param <T> {@link Class} type of the {@link Object} to render.
 * @see java.lang.String
 * @see org.cp.elements.lang.Renderer
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ToStringRenderer<T> implements Renderer<T> {

  public static final ToStringRenderer<Object> INSTANCE = new ToStringRenderer<>();

  /**
   * Renders the specified Object using the Object.toString method.
   *
   * @param obj the Object to render as a String.
   * @return a String representation of the specified Object.
   * @see java.lang.String#valueOf(Object)
   */
  @Override
  public String render(T obj) {
    return String.valueOf(obj);
  }
}
