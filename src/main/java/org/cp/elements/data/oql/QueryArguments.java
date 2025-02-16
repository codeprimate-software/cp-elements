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

import org.cp.elements.util.ArrayUtils;
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

  static QueryArguments empty() {
    return Collections::emptyIterator;
  }

  static QueryArguments of(QueryArgument<?>... arguments) {
    return of(Arrays.asList(ArrayUtils.nullSafeArray(arguments)));
  }

  static QueryArguments of(Iterable<QueryArgument<?>> arguments) {
    return arguments::iterator;
  }

  default Optional<QueryArgument<?>> findBy(String name) {
    return stream().filter(argument -> argument.getName().equals(name)).findFirst();
  }

  default QueryArgument<?> requireBy(String name) {
    return findBy(name).orElseThrow(() ->
      new QueryException("Query argument with name [%s] not found".formatted(name)));
  }

  @Override
  default Stream<QueryArgument<?>> stream() {
    return StreamUtils.stream(this);
  }
}
