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
import static org.assertj.core.api.AssertionsForClassTypes.assertThatExceptionOfType;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Collections;
import java.util.Optional;
import java.util.Set;
import java.util.Spliterator;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link QueryArguments}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.QueryArguments
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
public class QueryArgumentsUnitTests {

  @SuppressWarnings("unchecked")
  private <T> QueryArgument<T> mockQueryArgument(String name) {
    QueryArgument<T> mockArgument = mock(QueryArgument.class, name);
    doReturn(name).when(mockArgument).getName();
    return mockArgument;
  }

  @Test
  void empty() {

    QueryArguments arguments = QueryArguments.empty();

    assertThat(arguments).isNotNull();
    assertThat(arguments).isEmpty();
  }

  @Test
  void ofArray() {

    QueryArgument<?> mockArgumentOne = mock(QueryArgument.class);
    QueryArgument<?> mockArgumentTwo = mock(QueryArgument.class);

    QueryArguments arguments = QueryArguments.of(mockArgumentOne, mockArgumentTwo);

    assertThat(arguments).isNotNull();
    assertThat(arguments).hasSize(2);
    assertThat(arguments).containsExactly(mockArgumentOne, mockArgumentTwo);

    verifyNoInteractions(mockArgumentOne, mockArgumentTwo);
  }

  @Test
  void ofNoElementArray() {

    QueryArguments arguments = QueryArguments.of();

    assertThat(arguments).isNotNull();
    assertThat(arguments).isEmpty();
  }

  @Test
  void ofNullArray() {

    QueryArguments arguments = QueryArguments.of((QueryArgument<?>[]) null);

    assertThat(arguments).isNotNull();
    assertThat(arguments).isEmpty();
  }

  @Test
  void ofSingleElementArray() {

    QueryArgument<?> mockArgument = mock(QueryArgument.class);

    QueryArguments arguments = QueryArguments.of(mockArgument);

    assertThat(arguments).isNotNull();
    assertThat(arguments).hasSize(1);
    assertThat(arguments).containsExactly(mockArgument);

    verifyNoInteractions(mockArgument);
  }

  @Test
  void ofIterable() {

    QueryArgument<?> mockArgumentOne = mock(QueryArgument.class);
    QueryArgument<?> mockArgumentTwo = mock(QueryArgument.class);

    Set<QueryArgument<?>> argumentSet = Set.of(mockArgumentOne, mockArgumentTwo);

    QueryArguments arguments = QueryArguments.of(argumentSet);

    assertThat(arguments).isNotNull();
    assertThat(arguments).hasSize(2);
    assertThat(arguments).containsExactlyInAnyOrder(mockArgumentOne, mockArgumentTwo);

    verifyNoInteractions(mockArgumentOne, mockArgumentTwo);
  }

  @Test
  void ofEmptyIterable() {

    QueryArguments arguments = QueryArguments.of(Collections::emptyIterator);

    assertThat(arguments).isNotNull();
    assertThat(arguments).isEmpty();
  }

  @Test
  void ofNullIterable() {

    QueryArguments arguments = QueryArguments.of((Iterable<QueryArgument<?>>) null);

    assertThat(arguments).isNotNull();
    assertThat(arguments).isEmpty();
  }

  @Test
  void ofSingleElementIterable() {

    QueryArgument<?> mockArgument = mock(QueryArgument.class);

    QueryArguments arguments = QueryArguments.of(Collections.singleton(mockArgument));

    assertThat(arguments).isNotNull();
    assertThat(arguments).hasSize(1);
    assertThat(arguments).containsExactly(mockArgument);

    verifyNoInteractions(mockArgument);
  }

  @Test
  void findByNameFindsArgument() {

    QueryArgument<?> mockArgumentOne = mockQueryArgument("TestArg");
    QueryArgument<?> mockArgumentTwo = mockQueryArgument("MockArg");

    QueryArguments arguments = QueryArguments.of(mockArgumentOne, mockArgumentTwo);

    assertThat(arguments).isNotNull();
    assertThat(arguments).hasSize(2);

    Optional<QueryArgument<Object>> argument = arguments.findBy("TestArg");

    assertThat(argument).isPresent();
    assertThat(argument.orElse(null)).isSameAs(mockArgumentOne);

    verify(mockArgumentOne, times(1)).getName();
    verifyNoMoreInteractions(mockArgumentOne);
    verifyNoInteractions(mockArgumentTwo);
  }

  @Test
  void findByNameFindsNoArgument() {

    QueryArgument<?> mockArgument = mockQueryArgument("MockArg");

    QueryArguments arguments = QueryArguments.of(mockArgument);

    assertThat(arguments).isNotNull();
    assertThat(arguments).hasSize(1);

    Optional<QueryArgument<Object>> argument = arguments.findBy("mockArg");

    assertThat(argument).isNotNull();
    assertThat(argument).isNotPresent();

    verify(mockArgument, times(1)).getName();
    verifyNoMoreInteractions(mockArgument);
  }

  @Test
  void requireArgument() {

    QueryArgument<?> mockArgument = mockQueryArgument("MockArg");

    QueryArguments arguments = QueryArguments.of(mockArgument);

    assertThat(arguments).isNotNull();
    assertThat(arguments).hasSize(1);

    QueryArgument<Object> argument = arguments.requireBy("MockArg");

    assertThat(argument).isNotNull();
    assertThat(argument).isSameAs(mockArgument);

    verify(mockArgument, times(1)).getName();
    verifyNoMoreInteractions(mockArgument);
  }

  @Test
  void requireNonExistingArgument() {

    assertThatExceptionOfType(QueryException.class)
      .isThrownBy(() -> QueryArguments.empty().requireBy("TestArg"))
      .withMessage("Query argument with name [TestArg] not found")
      .withNoCause();
  }

  @Test
  void streamCallsIterableSpliterator() {

    QueryArguments mockArguments = mock(QueryArguments.class);
    Spliterator<?> mockSpliterator = mock(Spliterator.class);

    doReturn(mockSpliterator).when(mockArguments).spliterator();
    doCallRealMethod().when(mockArguments).stream();

    Stream<?> stream = mockArguments.stream();

    assertThat(stream).isNotNull();

    verify(mockArguments, times(1)).stream();
    verify(mockArguments, times(1)).spliterator();
    verifyNoMoreInteractions(mockArguments);
  }
}
