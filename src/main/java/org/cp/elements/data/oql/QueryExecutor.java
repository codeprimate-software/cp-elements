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

import org.cp.elements.lang.annotation.Overload;

/**
 * {@literal OQL} component capable of executing an {@literal OQL} {@link Query}.
 *
 * @param <S> {@link Class type} defining the {@link Object elements} in the {@link Iterable collection} to query.
 * @param <T> {@link Class type} of {@link Object project elements} in the {@link Iterable result}.
 * @see org.cp.elements.data.oql.QueryArguments
 * @see org.cp.elements.data.oql.QueryArgument
 * @see org.cp.elements.data.oql.Query
 * @see java.lang.Iterable
 */
@FunctionalInterface
public interface QueryExecutor<S, T> {

  @Overload
  default Iterable<T> execute(Query<S, T> query, QueryArgument<?>... arguments) {
    return execute(query, QueryArguments.of(arguments));
  }

  Iterable<T> execute(Query<S, T> query, Iterable<QueryArgument<?>> arguments);

}
