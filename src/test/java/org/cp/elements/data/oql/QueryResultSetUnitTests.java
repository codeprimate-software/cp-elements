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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Collection;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;
import java.util.Spliterator;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link QueryResultSet}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.QueryResultSet
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
public class QueryResultSetUnitTests {

  @Test
  void empty() {

    QueryResultSet<?> empty = QueryResultSet.empty();

    assertThat(empty).isNotNull();
    assertThat(empty).isEmpty();
  }

  @Test
  @SuppressWarnings("unchecked")
  void ofArray() {

    QueryResult<Object> mockQueryResultOne = mock(QueryResult.class);
    QueryResult<Object> mockQueryResultTwo = mock(QueryResult.class);

    QueryResultSet<Object> resultSet = QueryResultSet.of(mockQueryResultOne, mockQueryResultTwo);

    assertThat(resultSet).isNotNull();
    assertThat(resultSet).hasSize(2);
    assertThat(resultSet).containsExactly(mockQueryResultOne, mockQueryResultTwo);

    verifyNoInteractions(mockQueryResultOne, mockQueryResultTwo);
  }

  @Test
  void ofEmptyArray() {

    QueryResultSet<Object> resultSet = QueryResultSet.of();

    assertThat(resultSet).isNotNull();
    assertThat(resultSet).isEmpty();
  }

  @Test
  void ofNulArrayIsNullSafe() {

    QueryResultSet<Object> resultSet = QueryResultSet.of((QueryResult<Object>[]) null);

    assertThat(resultSet).isNotNull();
    assertThat(resultSet).isEmpty();
  }

  @Test
  @SuppressWarnings("unchecked")
  void ofSingleElementArray() {

    QueryResult<Object> mockQueryResult = mock(QueryResult.class);

    QueryResultSet<Object> resultSet = QueryResultSet.of(mockQueryResult);

    assertThat(resultSet).isNotNull();
    assertThat(resultSet).hasSize(1);
    assertThat(resultSet).containsExactly(mockQueryResult);

    verifyNoInteractions(mockQueryResult);
  }

  @Test
  @SuppressWarnings("unchecked")
  void ofIterable() {

    QueryResult<Object> mockQueryResultOne = mock(QueryResult.class);
    QueryResult<Object> mockQueryResultTwo = mock(QueryResult.class);

    Set<QueryResult<Object>> queryResultSet = Set.of(mockQueryResultOne, mockQueryResultTwo);

    QueryResultSet<Object> resultSet = QueryResultSet.of(queryResultSet);

    assertThat(resultSet).isNotNull();
    assertThat(resultSet).hasSize(2);
    assertThat(resultSet).containsExactlyInAnyOrder(mockQueryResultOne, mockQueryResultTwo);

    verifyNoInteractions(mockQueryResultOne, mockQueryResultTwo);
  }

  @Test
  void ofEmptyIterable() {

    QueryResultSet<Object> resultSet = QueryResultSet.of(Collections::emptyIterator);

    assertThat(resultSet).isNotNull();
    assertThat(resultSet).isEmpty();
  }

  @Test
  void ofNullIterableIsNullSafe() {

    QueryResultSet<Object> resultSet = QueryResultSet.of((Iterable<QueryResult<Object>>) null);

    assertThat(resultSet).isNotNull();
    assertThat(resultSet).isEmpty();
  }

  @Test
  @SuppressWarnings("unchecked")
  void ofSingleElementIterable() {

    QueryResult<Object> mockQueryResult = mock(QueryResult.class);

    Set<QueryResult<Object>> queryResultSet = Set.of(mockQueryResult);

    QueryResultSet<Object> resultSet = QueryResultSet.of(queryResultSet);

    assertThat(resultSet).isNotNull();
    assertThat(resultSet).hasSize(1);
    assertThat(resultSet).containsExactly(mockQueryResult);

    verifyNoInteractions(mockQueryResult);
  }

  @Test
  @SuppressWarnings("unchecked")
  void findsNone() {

    QueryResult<Object> mockQueryResultOne = mock(QueryResult.class);
    QueryResult<Object> mockQueryResultTwo = mock(QueryResult.class);

    QueryResultSet<Object> resultSet = QueryResultSet.of(mockQueryResultOne, mockQueryResultTwo);

    Optional<QueryResult<Object>> result = resultSet.findOne(queryResult -> false);

    assertThat(result).isNotNull();
    assertThat(result).isNotPresent();
  }

  @Test
  @SuppressWarnings("unchecked")
  void findsOne() {

    QueryResult<Object> mockQueryResultOne = mock(QueryResult.class);
    QueryResult<Object> mockQueryResultTwo = mock(QueryResult.class);

    QueryResultSet<Object> resultSet = QueryResultSet.of(mockQueryResultOne, mockQueryResultTwo);

    Optional<QueryResult<Object>> result = resultSet.findOne(queryResult -> true);

    assertThat(result).isNotNull();
    assertThat(result).isPresent();
    assertThat(result.orElse(null)).isEqualTo(mockQueryResultOne);
  }

  @Test
  @SuppressWarnings("unchecked")
  void findsSingleElement() {

    QueryResult<Object> mockQueryResultOne = mock(QueryResult.class);
    QueryResult<Object> mockQueryResultTwo = mock(QueryResult.class);

    QueryResultSet<Object> resultSet = QueryResultSet.of(mockQueryResultOne, mockQueryResultTwo);

    Collection<QueryResult<Object>> results = resultSet.findBy(queryResult -> queryResult == mockQueryResultOne);

    assertThat(results).isNotNull();
    assertThat(results).hasSize(1);
    assertThat(results).containsExactly(mockQueryResultOne);
  }

  @Test
  @SuppressWarnings("unchecked")
  void findsTwo() {

    QueryResult<Object> mockQueryResultOne = mock(QueryResult.class);
    QueryResult<Object> mockQueryResultTwo = mock(QueryResult.class);
    QueryResult<Object> mockQueryResultThree = mock(QueryResult.class);

    QueryResultSet<Object> resultSet = QueryResultSet.of(mockQueryResultOne, mockQueryResultTwo, mockQueryResultThree);

    Collection<QueryResult<Object>> results = resultSet.findBy(queryResult ->
      queryResult == mockQueryResultOne || queryResult == mockQueryResultThree);

    assertThat(results).isNotNull();
    assertThat(results).hasSize(2);
    assertThat(results).containsExactly(mockQueryResultOne, mockQueryResultThree);
  }

  @Test
  @SuppressWarnings("unchecked")
  void findsZero() {

    QueryResult<Object> mockQueryResultOne = mock(QueryResult.class);
    QueryResult<Object> mockQueryResultTwo = mock(QueryResult.class);

    QueryResultSet<Object> resultSet = QueryResultSet.of(mockQueryResultOne, mockQueryResultTwo);

    Collection<QueryResult<Object>> results = resultSet.findBy(queryResult -> false);

    assertThat(results).isNotNull();
    assertThat(results).isEmpty();
  }

  @Test
  void stream() {

    Spliterator<?> mockSpliterator = mock(Spliterator.class);
    QueryResultSet<?> mockResultSet = mock(QueryResultSet.class);

    doReturn(mockSpliterator).when(mockResultSet).spliterator();
    doCallRealMethod().when(mockResultSet).stream();

    Stream<?> stream = mockResultSet.stream();

    assertThat(stream).isNotNull();

    verify(mockResultSet, times(1)).stream();
    verify(mockResultSet, times(1)).spliterator();
    verifyNoMoreInteractions(mockResultSet);
  }
}
