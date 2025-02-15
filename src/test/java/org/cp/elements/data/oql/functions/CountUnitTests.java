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
package org.cp.elements.data.oql.functions;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Arrays;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link Count}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.functions.Count
 * @see org.junit.jupiter.api.Test
 * @since 2.0.0
 */
public class CountUnitTests {

  @Test
  void newCount() {

    Count<?> count = Count.all();

    assertThat(count).isNotNull();
    assertThat(count.getName()).isEqualTo(Count.DEFAULT_NAME);
  }

  @Test
  void newNamedCount() {

    Count<?> count = Count.all().named("TestCount");

    assertThat(count).isNotNull();
    assertThat(count.getName()).isEqualTo("TestCount");
  }

  @Test
  void countAll() {
    assertThat(Count.all().apply(1, 2, 3, 4, 5, 6, 7, 8, 9)).isEqualTo(9);
  }

  @Test
  void countNone() {

    Arrays.asList(null, new Object[0]).forEach(array ->
      assertThat(Count.all().apply(array)).isEqualTo(0));
  }
}
