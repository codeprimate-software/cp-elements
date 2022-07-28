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
package org.cp.elements.util.stream;

import java.util.stream.Stream;

/**
 * {@link FunctionalInterface} used to classify and declare a type as a producer of a Java {@link Stream}.
 *
 * @author John Blum
 * @param <T> {@link Class type} of the elements in the {@link Stream}.
 * @see java.util.stream.Stream
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface Streamable<T> {

  /**
   * Returns a {@link Stream} of elements from this {@link Streamable} object.
   *
   * @return a {@link Stream} of elements from this {@link Streamable} object.
   * @see java.util.stream.Stream
   */
  Stream<T> stream();

}
