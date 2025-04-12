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
package org.cp.elements.data.oql.provider;

import java.util.stream.Stream;

import org.cp.elements.data.oql.QueryExecutor;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * Parallel implementation of the {@link QueryExecutor}.
 *
 * @author John Blum
 * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
 * @param <T> {@link Class type} of the {@link Object projected objects}.
 * @see org.cp.elements.data.oql.provider.SimpleQueryExecutor
 * @see org.cp.elements.data.oql.QueryExecutor
 * @since 2.0.0
 */
public class ParallelQueryExecutor<S, T> extends SimpleQueryExecutor<S, T> {

  @NullSafe
  @Override
  protected <E> Stream<E> stream(Iterable<E> iterable) {
    return super.stream(iterable).parallel();
  }
}
