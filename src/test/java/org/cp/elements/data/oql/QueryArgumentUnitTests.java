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

import java.util.Arrays;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link QueryArgument}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.QueryArgument
 * @see org.junit.jupiter.api.Test
 * @since 2.0.0
 */
public class QueryArgumentUnitTests {

  @Test
  void fromNameValue() {

    QueryArgument<Object> argument = QueryArgument.from("testName", 1);

    assertThat(argument).isNotNull();
    assertThat(argument.name()).isEqualTo("testName");
    assertThat(argument.getName()).isEqualTo(argument.name());
    assertThat(argument.value()).isEqualTo(1);
  }

  @Test
  void fromNameNullValue() {

    QueryArgument<Object> argument = QueryArgument.from("mockName", null);

    assertThat(argument).isNotNull();
    assertThat(argument.name()).isEqualTo("mockName");
    assertThat(argument.value()).isNull();
  }

  @Test
  void fromIllegalName() {

    Arrays.asList("  ", "", null).forEach(name ->
        assertThatIllegalArgumentException()
          .isThrownBy(() -> QueryArgument.from(name, "test"))
          .withMessage("Name [%s] is required", name)
          .withNoCause());
  }
}
