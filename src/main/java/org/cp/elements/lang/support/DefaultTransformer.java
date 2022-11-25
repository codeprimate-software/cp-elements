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

import org.cp.elements.lang.Transformer;

/**
 * The DefaultTransformer class is a no-op {@link Transformer} implementation that returns all objects unaltered.
 *
 * @author John J. Blum
 * @param <T> {@link Class type} of the {@link Object} to transform.
 * @see org.cp.elements.lang.Transformer
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class DefaultTransformer<T> implements Transformer<T> {

  public static final DefaultTransformer<Object> INSTANCE = new DefaultTransformer<>();

  /**
   * Transforms the value by not transforming the value at all.
   *
   * @param value value to transform.
   * @return the given value unaltered.
   */
  @Override
  public T transform(T value) {
    return value;
  }
}
