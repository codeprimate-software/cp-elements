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

import java.util.Arrays;
import java.util.Spliterator;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.CollectionUtils;

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
   * Factory method used to Constructs a new {@link Streamable} initialized with an empty {@link Stream}.
   *
   * @param <T> {@link Class type} of the elements in the {@link Stream}.
   * @return a new, empty {@link Streamable} object.
   */
  static @NotNull <T> Streamable<T> empty() {
    return StreamUtils::empty;
  }

  /**
   * Factory method used to construct a new {@link Streamable} from the given array of elements.
   *
   * @param <T> {@link Class type} of the elements in the array.
   * @param array array of {@link Object elements} to stream.
   * @return a new {@link Streamable} object from the given array of elements.
   * @see java.util.Arrays#stream(Object[])
   */
  @SafeVarargs
  static @NotNull <T> Streamable<T> from(T... array) {
    return () -> Arrays.stream(array);
  }

  /**
   * Factory method used to construct a new {@link Streamable} initialized with the given {@link Iterable}
   * collection of elements.
   *
   * @param <T> {@link Class type} of the elements in the {@link Iterable}.
   * @param iterable {@link Iterable} collection of {@link Object elements} to stream.
   * @return a new {@link Streamable} object from the given {@link Iterable} collection of elements.
   * @see java.util.stream.StreamSupport#stream(Spliterator, boolean)
   * @see java.lang.Iterable
   */
  @NullSafe
  static @NotNull <T> Streamable<T> from(@Nullable Iterable<T> iterable) {
    return () -> StreamSupport.stream(CollectionUtils.nullSafeIterable(iterable).spliterator(), false);
  }

  /**
   * Returns a {@link Stream} of elements from this {@link Streamable} object.
   *
   * @return a {@link Stream} of elements from this {@link Streamable} object.
   * @see java.util.stream.Stream
   */
  Stream<T> stream();

}
