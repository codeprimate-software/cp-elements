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

import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;
import java.util.stream.Stream;

import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.stream.StreamUtils;
import org.cp.elements.util.stream.Streamable;

/**
 * {@link Iterable} data structure of OQL {@link QueryArgument query arguments}.
 *
 * @author John Blum
 * @see java.lang.Iterable
 * @see org.cp.elements.data.oql.QueryArgument
 * @see org.cp.elements.util.stream.Streamable
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public interface QueryArguments extends Iterable<QueryArgument<?>>, Streamable<QueryArgument<?>> {

  /**
   * Factory method used to construct an empty collection of {@link QueryArguments}.
   *
   * @return an empty collection of {@link QueryArguments}.
   */
  static QueryArguments empty() {
    return Collections::emptyIterator;
  }

  /**
   * Factory method used to construct a new {@link QueryArguments} aggregate initialized from
   * the array of {@link QueryArgument} objects.
   *
   * @param arguments array of {@link QueryArgument} objects used to construct and initialize the aggregate.
   * @return a new {@link QueryArguments} aggregate initialized from the array of {@link QueryArgument} objects.
   * @see #from(Iterable)
   */
  @NullSafe
  static QueryArguments of(QueryArgument<?>... arguments) {
    return of(Arrays.asList(ArrayUtils.nullSafeArray(arguments, QueryArgument.class)));
  }

  /**
   * Factory method used to construct a new {@link QueryArguments} aggregate initialized from
   * the {@link Iterable} of {@link QueryArgument} objects.
   *
   * @param arguments {@link Iterable} of {@link QueryArgument} objects used to construct and initialize the aggregate.
   * @return a new {@link QueryArguments} aggregate initialized from the {@link Iterable} of {@link QueryArgument}
   * objects.
   * @see #from(Object[])
   */
  @NullSafe
  static QueryArguments of(Iterable<QueryArgument<?>> arguments) {
    return CollectionUtils.nullSafeIterable(arguments)::iterator;
  }

  /**
   * Looks up and finds an {@link Optional} {@link QueryArgument} by {@link String name}.
   *
   * @param <T> {@link Class type} of argument value.
   * @param name {@link String} containing the name of the {@link QueryArgument} to find in this aggregate.
   * @return an {@link Optional} {@link QueryArgument} with the given {@link String name}.
   * @see java.util.Optional
   */
  @SuppressWarnings("unchecked")
  default <T> Optional<QueryArgument<T>> findBy(String name) {

    return stream()
      .filter(argument -> argument.getName().equals(name))
      .map(queryArgument -> (QueryArgument<T>) queryArgument)
      .findFirst();
  }

  /**
   * Looks up and finds an required {@link QueryArgument} by {@link String name}.
   *
   * @param <T> {@link Class type} of argument value.
   * @param name {@link String} containing the name of the {@link QueryArgument} to find in this aggregate.
   * @return a {@link QueryArgument} with the given {@link String name}.
   * @throws QueryException if a {@link QueryArgument} by {@link String name} is not found.
   * @see #findBy(String)
   */
  @SuppressWarnings("unchecked")
  default <T> QueryArgument<T> requireBy(String name) {

    return findBy(name)
      .map(queryArgument -> (QueryArgument<T>) queryArgument)
      .orElseThrow(() -> new QueryException("Query argument with name [%s] not found".formatted(name)));
  }

  /**
   * Returns a {@link Stream} over the {@link QueryArgument QueryArguments} in this aggregate.
   *
   * @return a {@link Stream} over the {@link QueryArgument QueryArguments} in this aggregate.
   * @see java.util.stream.Stream
   * @see QueryArgument
   */
  @Override
  default Stream<QueryArgument<?>> stream() {
    return StreamUtils.stream(this);
  }
}
