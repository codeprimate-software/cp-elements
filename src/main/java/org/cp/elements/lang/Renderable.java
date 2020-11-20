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
 * The {@link Renderable} interface defines a contract for objects that can be rendered as a {@link String}
 * using the given {@link Renderer}.
 *
 * @author John Blum
 * @see org.cp.elements.lang.Renderer
 * @since 1.0.0
 */
public interface Renderable {

  /**
   * Renders this {@link Object} as a {@link String} using the given {@link Renderer}.
   *
   * @param renderer {@link Renderer} used to render this {@link Object} as a {@link String}.
   * @return a {@link String} rendered from this {@link Object} using the given {@link Renderer}.
   * @see org.cp.elements.lang.Renderer#render(Object)
   */
  @SuppressWarnings({ "rawtypes", "unchecked" })
  default String render(Renderer renderer) {
    return renderer.render(this);
  }
}
