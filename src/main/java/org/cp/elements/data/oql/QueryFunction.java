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
package org.cp.elements.data.oql;

import org.cp.elements.lang.Nameable;
import org.cp.elements.util.ArrayUtils;

/**
 * Abstract Data Type (ADT) modeling an {@literal OQL} function.
 *
 * @author John Blum
 * @param <S> {@link Class type} of {@link Object} on which this function is applied.
 * @param <T> {@link Class type} of {@link Object value resulting} from the computation of this function
 * applied to the given {@link S target}.
 * @see org.cp.elements.lang.Nameable
 * @since 2.0.0
 */
public interface QueryFunction<S, T> extends Nameable<String> {

  /**
   * Apply the computation of this {@link QueryFunction} to the given {@link S objects}.
   *
   * @param array array of {@link S Objects} on which this {@link QueryFunction} is applied.
   * @see #apply(Iterable)
   */
  @SuppressWarnings("unchecked")
  default T apply(S... array) {
    return apply(ArrayUtils.asIterable(ArrayUtils.nullSafeArray(array)));
  }

  /**
   * Apply the computation of this {@link QueryFunction} to the given {@link S objects}.
   *
   * @param iterable {@link Iterable} of {@link S Objects} on which this {@link QueryFunction} is applied.
   * @see #apply(Object[])
   */
  T apply(Iterable<S> iterable);

}
