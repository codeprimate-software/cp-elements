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
package org.cp.elements.data.struct.tabular;

import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.ThrowableAssertions;

/**
 * Unit Tests for {@link AbstractTable}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.AbstractTable
 * @since 1.0.0
 */
public class AbstractTableUnitTests {

  private final AbstractTable table = new TestTable();

  @Test
  public void addColumnThrowsUnsupportedOperationException() {

    ThrowableAssertions.assertThatUnsupportedOperationException()
      .isThrownBy(args -> this.table.add(mock(Column.class)))
      .withNoCause();
  }

  @Test
  public void addRowThrowsUnsupportedOperationException() {

    ThrowableAssertions.assertThatUnsupportedOperationException()
      .isThrownBy(args -> this.table.add(mock(Row.class)))
      .withNoCause();
  }

  private static final class TestTable extends AbstractTable { }

}
