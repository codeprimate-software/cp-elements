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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Arrays;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.oql.Oql.Projection;
import org.cp.elements.data.oql.Oql.Select;

/**
 * Unit Tests for {@link QueryContext}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.QueryContext
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
@SuppressWarnings("unchecked")
public class QueryContextUnitTests {

  @Test
  void from() {

    Iterable<Object> mockCollection = mock(Iterable.class);
    Projection<Object, Object> mockProjection = mock(Projection.class);
    Select<Object, Object> mockSelect = mock(Select.class);
    Query<Object, Object> mockQuery = mock(Query.class);

    doReturn(mockProjection).when(mockSelect).getProjection();
    doReturn(mockSelect).when(mockQuery).selection();
    doReturn(mockCollection).when(mockQuery).collection();

    QueryContext<Object, Object> queryContext = QueryContext.from(mockQuery);

    assertThat(queryContext).isNotNull();
    assertThat(queryContext.query()).isEqualTo(mockQuery);
    assertThat(queryContext.metadata()).isNotNull();
    assertThat(queryContext.getCollection()).isEqualTo(mockCollection);
    assertThat(queryContext.getProjection()).isEqualTo(mockProjection);

    verify(mockQuery, times(1)).collection();
    verify(mockQuery, times(1)).selection();
    verify(mockSelect).getProjection();
    verifyNoInteractions(mockProjection, mockCollection);
    verifyNoMoreInteractions(mockQuery, mockSelect);
  }

  @Test
  void fromNullQuery() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> QueryContext.from(null))
      .withMessage("Query is required")
      .withNoCause();
  }

  @Test
  void fromNullMetadata() {

    Query<?, ?> mockQuery = mock(Query.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new QueryContext<>(mockQuery, null))
      .withMessage("Map of metadata is required")
      .withNoCause();

    verifyNoInteractions(mockQuery);
  }

  @Test
  void putGetKeyValue() {

    Query<Object, Object> mockQuery = mock(Query.class);

    QueryContext<Object, Object> queryContext = QueryContext.from(mockQuery);

    assertThat(queryContext.<Object>get("testKey")).isNull();

    queryContext.put("testKey", 1);

    assertThat(queryContext.<Object>get("testKey")).isEqualTo(1);

    queryContext.put("testKey", "mockValue");

    assertThat(queryContext.<Object>get("testKey")).isEqualTo("mockValue");

    verifyNoInteractions(mockQuery);
  }

  @Test
  void putInvalidKey() {

    Query<?, ?> mockQuery = mock(Query.class);

    Arrays.asList("  ", "", null).forEach(key ->
        assertThatIllegalArgumentException()
          .isThrownBy(() -> QueryContext.from(mockQuery).put(key, "test"))
          .withMessage("Key [%s] is required", key)
          .withNoCause());

    verifyNoInteractions(mockQuery);
  }

  @Test
  void putNullValue() {

    Query<?, ?> mockQuery = mock(Query.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> QueryContext.from(mockQuery).put("testKey", null))
      .withMessage("Value is required")
      .withNoCause();

    verifyNoInteractions(mockQuery);
  }
}
