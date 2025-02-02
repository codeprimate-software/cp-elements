/*
 * Copyright 2017-Present Author or Authors.
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
package org.cp.elements.data.oql.support;

import java.util.function.Function;
import java.util.stream.Stream;

import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.stream.StreamUtils;
import org.cp.elements.util.stream.Streamable;

/**
 * Interface defining a {@literal group} of similar {@link Object objects} from a {@link Iterable collection}.
 *
 * @param <S> {@link Class type} of {@link Object} from which the {@link Object value}
 * used in the {@literal grouping} is calculated.
 * @see org.cp.elements.util.stream.Streamable
 * @see java.lang.FunctionalInterface
 * @see java.util.function.Function
 * @see java.lang.Iterable
 */
@FunctionalInterface
public interface Grouping<S> extends Iterable<Function<S, ?>>, Streamable<Function<S, ?>> {

  /**
   * Factory method used to construct a new {@link Grouping} function defined by
   * the array of {@link Function Functions} applied to the {@link Object target}
   * to determine groups.
   *
   * @param <S> {@link Class type} of the {@link Object target}.
   * @param functions array of {@link Function Functions} to apply to the {@link Object target}
   * for computing groups.
   * @return a new {@link Grouping} function.
   * @see Function
   */
  @SafeVarargs
  static <S> Grouping<S> of(Function<S, ?>... functions) {
    return ArrayUtils.asIterable(ArrayUtils.nullSafeArray(functions))::iterator;
  }

  /**
   * Determines the {@link Integer group number} of the given {@link Object}.
   * <p>
   * The algorithm uses the configured {@link Function functions} applied to the given {@link Object target},
   * hashing then summing the resulting values of the calculation.
   *
   * @param target {@link Object} to evaluate.
   * @return the {@link Integer group number} of the given {@link Object}.
   */
  default int group(S target) {

    return stream()
      .map(function -> function.apply(target))
      .map(Object::hashCode)
      .reduce(Integer::sum)
      .orElse(0);
  }

  @Override
  default Stream<Function<S, ?>> stream() {
    return StreamUtils.stream(this);
  }
}
