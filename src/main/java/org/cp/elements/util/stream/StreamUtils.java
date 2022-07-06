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
import java.util.Collection;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract utility class containing a collection of methods for processing Java 8 {@link Stream Streams}.
 *
 * @author John J. Blum
 * @see java.util.Arrays
 * @see java.util.stream.Stream
 * @see java.util.stream.StreamSupport
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class StreamUtils {

  /**
   * Returns an empty {@link Stream}.
   *
   * @param <T> {@link Class type} of elements in the {@link Stream}.
   * @return an empty {@link Stream}.
   * @see java.util.stream.Stream
   * @see #stream(Object[])
   */
  @SuppressWarnings("all")
  public static <T> Stream<T> empty() {
    return stream();
  }

  /**
   * Null-safe method used to evaluate a given {@link Stream} for a {@literal null} reference.
   *
   * @param <T> {@link Class type} of elements in the {@link Stream}.
   * @param stream {@link Stream} reference to evaluate for {@literal null}.
   * @return the given {@link Stream} if not {@literal null}
   * or return an {@literal empty} {@link Stream} otherwise.
   * @see java.util.stream.Stream
   * @see #empty()
   */
  @NullSafe
  public static @NotNull <T> Stream<T> nullSafeStream(@Nullable Stream<T> stream) {
    return stream != null ? stream : empty();
  }

  /**
   * Returns a {@link Stream} from the given array of elements.
   *
   * @param <T> {@link Class type} of elements in the array.
   * @param elements array of elements to convert into a {@link Stream}.
   * @return a {@link Stream} containing the elements in the array.
   * @see java.util.stream.Stream
   */
  @SafeVarargs
  public static @NotNull <T> Stream<T> stream(T... elements) {
    return Arrays.stream(elements);
  }

  /**
   * Returns a {@link Stream} from the given {@link Iterable} collection of elements.
   *
   * @param <T> {@link Class type} of elements in the {@link Iterable}.
   * @param iterable {@link Iterable} collection of elements to convert into a {@link Stream}.
   * @return a {@link Stream} containing the elements in the {@link Iterable}.
   * @throws IllegalArgumentException if the {@link Iterable} is null.
   * @see java.util.stream.Stream
   * @see java.lang.Iterable
   */
  public static @NotNull <T> Stream<T> stream(@NotNull Iterable<T> iterable) {

    Assert.notNull(iterable, "Iterable is required");

    return iterable instanceof Collection
      ? ((Collection<T>) iterable).stream()
      : StreamSupport.stream(iterable.spliterator(), false);
  }
}
