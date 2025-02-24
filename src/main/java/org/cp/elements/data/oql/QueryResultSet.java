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

import java.util.Collection;
import java.util.Collections;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.stream.StreamUtils;
import org.cp.elements.util.stream.Streamable;

/**
 * Data structure used to aggregate a collection of {@link QueryResult} objects.
 *
 * @author John Blum
 * @param <T> {@link Class type} of the individual query results contained in this result set.
 * @see org.cp.elements.util.stream.Streamable
 * @see org.cp.elements.data.oql.QueryResult
 * @see java.lang.FunctionalInterface
 * @see java.lang.Iterable
 * @since 2.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface QueryResultSet<T> extends Iterable<QueryResult<T>>, Streamable<QueryResult<T>> {

  static <T> QueryResultSet<T> empty() {
    return Collections::emptyIterator;
  }

  @NullSafe
  @SafeVarargs
  static <T> QueryResultSet<T> of(QueryResult<T>... results) {
    return of(ArrayUtils.asIterable(ArrayUtils.nullSafeArray(results, QueryResult.class)));
  }

  @NullSafe
  static <T> QueryResultSet<T> of(Iterable<QueryResult<T>> results) {
    return CollectionUtils.nullSafeIterable(results)::iterator;
  }

  default boolean isEmpty() {
    return size() == 0;
  }

  default Collection<QueryResult<T>> findBy(Predicate<QueryResult<T>> predicate) {
    return stream().filter(predicate).toList();
  }

  default Optional<QueryResult<T>> findOne(Predicate<QueryResult<T>> predicate) {
    return stream().filter(predicate).findFirst();
  }

  default int size() {
    return Long.valueOf(stream().count()).intValue();
  }

  @Override
  default Stream<QueryResult<T>> stream() {
    return StreamUtils.stream(this);
  }
}
