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
import java.util.Objects;
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

  /**
   * Factory method used to construct a new, empty {@link QueryResultSet}.
   *
   * @param <T> {@link Class type} of {@link Objects} in the {@link QueryResult QueryResults}.
   * @return a new, empty {@link QueryResultSet}.
   */
  static <T> QueryResultSet<T> empty() {
    return Collections::emptyIterator;
  }

  /**
   * Factory method used to construct a new {@link QueryResult} initialized with
   * the given array of {@link QueryResult QueryResults}.
   *
   * @param <T> {@link Class type} of {@link Objects} in the {@link QueryResult QueryResults}.
   * @param results array of {@link QueryResult QueryResults}
   * @return a new {@link QueryResult} initialized with the given array of {@link QueryResult QueryResults}.
   * @see #of(Iterable)
   */
  @NullSafe
  @SafeVarargs
  static <T> QueryResultSet<T> of(QueryResult<T>... results) {
    return of(ArrayUtils.asIterable(ArrayUtils.nullSafeArray(results, QueryResult.class)));
  }

  /**
   * Factory method used to construct a new {@link QueryResult} initialized with
   * the given {@link Iterable} of {@link QueryResult QueryResults}.
   *
   * @param <T> {@link Class type} of {@link Objects} in the {@link QueryResult QueryResults}.
   * @param results {@link Iterable} of {@link QueryResult QueryResults}
   * @return a new {@link QueryResult} initialized with the given {@link Iterable} of {@link QueryResult QueryResults}.
   * @see #of(QueryResult[])
   * @see java.lang.Iterable
   */
  @NullSafe
  static <T> QueryResultSet<T> of(Iterable<QueryResult<T>> results) {
    return CollectionUtils.nullSafeIterable(results)::iterator;
  }

  /**
   * Determines whether this {@link QueryResultSet} is empty.
   *
   * @return a boolean value indicating whether this {@link QueryResultSet} is empty.
   * @see #size()
   */
  default boolean isEmpty() {
    return size() == 0;
  }

  /**
   * Finds all the {@link QueryResult QueryResults} in this {@link QueryResultSet} matching the given {@link Predicate}.
   *
   * @param predicate {@link Predicate} used to match {@link QueryResult QueryResults} in this {@link QueryResultSet}.
   * @return all the {@link QueryResult QueryResults} in this {@link QueryResultSet}
   * matching the given {@link Predicate}.
   * @see java.util.function.Predicate
   * @see #findOne(Predicate)
   */
  default Collection<QueryResult<T>> findBy(Predicate<QueryResult<T>> predicate) {
    return stream().filter(predicate).toList();
  }

  /**
   * Finds a single {@link QueryResult} in this {@link QueryResultSet} matching the given {@link Predicate}.
   *
   * @param predicate {@link Predicate} used to match a single {@link QueryResult} in this {@link QueryResultSet}.
   * @return a single the {@link QueryResult} in this {@link QueryResultSet} matching the given {@link Predicate}.
   * @see java.util.function.Predicate
   * @see #findBy(Predicate)
   * @see java.util.Optional
   */
  default Optional<QueryResult<T>> findOne(Predicate<QueryResult<T>> predicate) {
    return stream().filter(predicate).findFirst();
  }

  /**
   * Returns the {@link Long number} of {@link QueryResult QueryResults} in this {@link QueryResultSet}.
   *
   * @return the {@link Long number} of {@link QueryResult QueryResults} in this {@link QueryResultSet}.
   */
  default int size() {
    return Long.valueOf(stream().count()).intValue();
  }

  /**
   * Streams the {@link QueryResult QueryResults} in this {@link QueryResultSet}.
   *
   * @return a {@link Stream} of {@link QueryResult QueryResults} in this {@link QueryResultSet}.
   * @see java.util.stream.Stream
   * @see QueryResult
   */
  @Override
  default Stream<QueryResult<T>> stream() {
    return StreamUtils.stream(this);
  }
}
