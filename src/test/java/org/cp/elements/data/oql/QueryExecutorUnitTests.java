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
package org.cp.elements.data.oql;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Arrays;

import org.junit.jupiter.api.Test;

import org.cp.elements.util.stream.StreamUtils;
import org.mockito.ArgumentMatcher;
import org.mockito.ArgumentMatchers;

/**
 * Unit Tests for {@link QueryExecutor}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.QueryExecutor
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
public class QueryExecutorUnitTests {

  @Test
  @SuppressWarnings("unchecked")
  void executeQueryWithQueryArguments() {

    Iterable<?> mockIterable = mock(Iterable.class);

    Query<Object, Object> mockQuery = mock(Query.class);

    QueryArgument<Object> mockQueryArgumentOne = mock(QueryArgument.class);
    QueryArgument<Object> mockQueryArgumentTwo = mock(QueryArgument.class);

    QueryExecutor<Object, Object> mockQueryExecutor = mock(QueryExecutor.class);

    doCallRealMethod().when(mockQueryExecutor).execute(any(Query.class), any(QueryArgument[].class));
    doReturn(mockIterable).when(mockQueryExecutor).execute(any(Query.class), any(Iterable.class));

    Iterable<?> results = mockQueryExecutor.execute(mockQuery, mockQueryArgumentOne, mockQueryArgumentTwo);

    assertThat(results).isSameAs(mockIterable);

    verify(mockQueryExecutor, times(1)).execute(eq(mockQuery), eq(mockQueryArgumentOne), eq(mockQueryArgumentTwo));
    verify(mockQueryExecutor, times(1)).execute(eq(mockQuery),
      argThat(QueryArgumentIterableMatcher.of(mockQueryArgumentOne, mockQueryArgumentTwo)));
    verifyNoInteractions(mockQuery, mockQueryArgumentOne, mockQueryArgumentTwo);
    verifyNoMoreInteractions(mockQueryExecutor);
  }

  @Test
  @SuppressWarnings("unchecked")
  void executeQueryWithNullQueryArgumentsIsNullSafe() {

    Iterable<?> mockIterable = mock(Iterable.class);

    Query<Object, Object> mockQuery = mock(Query.class);
    QueryExecutor<Object, Object> mockQueryExecutor = mock(QueryExecutor.class);

    doCallRealMethod().when(mockQueryExecutor).execute(any(Query.class), ArgumentMatchers.<QueryArgument<?>[]>any());
    doReturn(mockIterable).when(mockQueryExecutor).execute(any(Query.class), any(Iterable.class));

    Iterable<?> results = mockQueryExecutor.execute(mockQuery, (QueryArgument<Object>[]) null);

    assertThat(results).isSameAs(mockIterable);

    verify(mockQueryExecutor, times(1)).execute(eq(mockQuery), ArgumentMatchers.<QueryArgument<Object>[]>isNull());
    verify(mockQueryExecutor, times(1)).execute(eq(mockQuery), argThat(QueryArgumentIterableMatcher.empty()));
    verifyNoMoreInteractions(mockQueryExecutor);
    verifyNoInteractions(mockQuery);
  }

  @Test
  @SuppressWarnings("unchecked")
  void executeQueryWithNoQueryArguments() {

    Iterable<?> mockIterable = mock(Iterable.class);

    Query<Object, Object> mockQuery = mock(Query.class);
    QueryExecutor<Object, Object> mockQueryExecutor = mock(QueryExecutor.class);

    doCallRealMethod().when(mockQueryExecutor).execute(any(Query.class), any(QueryArgument[].class));
    doReturn(mockIterable).when(mockQueryExecutor).execute(any(Query.class), any(Iterable.class));

    Iterable<?> results = mockQueryExecutor.execute(mockQuery);

    assertThat(results).isSameAs(mockIterable);

    verify(mockQueryExecutor, times(1)).execute(eq(mockQuery));
    verify(mockQueryExecutor, times(1)).execute(eq(mockQuery), argThat(QueryArgumentIterableMatcher.empty()));
    verifyNoMoreInteractions(mockQueryExecutor);
    verifyNoInteractions(mockQuery);
  }

  // TODO: Add to Codeprimate Extensions
  record QueryArgumentIterableMatcher(QueryArgument<?>[] expectedQueryArguments)
      implements ArgumentMatcher<Iterable<QueryArgument<?>>> {

    static QueryArgumentIterableMatcher empty() {
      return of();
    }

    static QueryArgumentIterableMatcher of(QueryArgument<?>... arguments) {
      return new QueryArgumentIterableMatcher(arguments);
    }

    @Override
    public boolean matches(Iterable<QueryArgument<?>> actualQueryArguments) {

      if (actualQueryArguments != null) {

        QueryArgument<?>[] actualQueryArgumentArray =
          StreamUtils.stream(actualQueryArguments).toArray(QueryArgument[]::new);

        return Arrays.equals(actualQueryArgumentArray, expectedQueryArguments());
      }

      return false;
    }
  }
}
