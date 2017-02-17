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

package org.cp.elements.util.stream;

import static org.cp.elements.util.CollectionUtils.asList;

import java.util.Arrays;
import java.util.Collection;
import java.util.stream.Stream;

import org.cp.elements.lang.Assert;

/**
 * The StreamUtils class is a collection of utility methods for working with Java 8 {@link Stream}s
 *
 * @author John J. Blum
 * @see java.util.stream.Stream
 * @see org.cp.elements.util.CollectionUtils
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class StreamUtils {

  /**
   * Returns a {@link Stream} from the array of elements.
   *
   * @param <T> Class type of the elements in the array.
   * @param elements array of elements to convert into a {@link Stream}.
   * @return a {@link Stream} for the elements in the array.
   * @see java.util.stream.Stream
   */
  @SafeVarargs
  public static <T> Stream<T> stream(T... elements) {
    return Arrays.stream(elements);
  }

  /**
   * Returns a {@link Stream} from the {@link Iterable} collection of elements.
   *
   * @param <T> Class type of the elements in the {@link Iterable}.
   * @param iterable {@link Iterable} collection of elements to convert into a {@link Stream}.
   * @return a {@link Stream} for the elements in the {@link Iterable}.
   * @throws IllegalArgumentException if {@link Iterable} is null.
   * @see java.util.stream.Stream
   * @see java.lang.Iterable
   */
  @SuppressWarnings("unchecked")
  public static <T> Stream<T> stream(Iterable<T> iterable) {
    Assert.notNull(iterable, "Iterable cannot be null");
    return (iterable instanceof Collection ? ((Collection) iterable).stream() : asList(iterable).stream());
  }
}
