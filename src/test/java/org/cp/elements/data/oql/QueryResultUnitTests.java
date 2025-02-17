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

import java.util.Map;

import org.junit.jupiter.api.Test;

import org.cp.elements.security.model.User;

/**
 * Unit Tests for {@link QueryResult}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.QueryResult
 * @see org.junit.jupiter.api.Test
 * @since 2.0.0
 */
public class QueryResultUnitTests {

  @Test
  void queryResultForUser() {

    QueryResult<User<Integer>> result = QueryResult.<User<Integer>>builder()
      .withMap(Map.of("name", "jonDoe", "id", 1))
      .build();

    assertThat(result).isNotNull();
    assertThat(result.<String>get("name")).isEqualTo("jonDoe");
    assertThat(result.<Integer>get("id")).isOne();

    User<Integer> jonDoe = result.map(queryResult ->
      User.<Integer>named(queryResult.get("name")).identifiedBy(queryResult.get("id")));

    assertThat(jonDoe).isNotNull();
    assertThat(jonDoe.getName()).isEqualTo("jonDoe");
    assertThat(jonDoe.getId()).isOne();
  }

  @Test
  void mapWithNullFunction() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> QueryResult.builder().build().map(null))
      .withMessage("Function is required")
      .withNoCause();
  }
}
