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

import org.cp.elements.lang.support.SimpleTypeResolver;

/**
 * Interface defining a strategy to resolve the {@link Class type} of a source {@link Object}.
 *
 * @author John Blum
 * @since 2.0.0
 */
public interface TypeResolver {

  /**
   * Factory method used to return a default instance of {@link TypeResolver}.
   *
   * @return a default instance of {@link TypeResolver}.
   * @see org.cp.elements.lang.support.SimpleTypeResolver
   */
  static TypeResolver getInstance() {
    return SimpleTypeResolver.INSTANCE;
  }

  /**
   * Resolves the {@link Class type} of the given {@link Object}.
   *
   * @param target {@link Object} from which to resolve the {@link Class type}.
   * @return the resolved {@link Class type} of the given {@link Object}.
   * @throws TypeNotFoundException if {@link Class type} of {@link Object} cannot be resolved.
   */
  Class<?> resolveType(Object target);

}
