/*
 * Copyright 2016 Author or Authors.
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;

import java.util.Arrays;
import java.util.Collections;

import org.junit.Test;

/**
 * Unit tests for {@link AbstractView}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.AbstractView
 * @since 1.0.0
 */
public class AbstractViewTests {

  @Test
  public void abstractViewOfColumnsAndRows() {

    Column mockColumnOne = mock(Column.class);
    Column mockColumnTwo = mock(Column.class);

    Row mockRow = mock(Row.class);

    View view = AbstractView.of(Arrays.asList(mockColumnOne, mockColumnTwo), Collections.singletonList(mockRow));

    assertThat(view).isNotNull();
    assertThat(view.columns()).containsExactly(mockColumnOne, mockColumnTwo);
    assertThat(view.rows()).containsExactly(mockRow);
  }

  @Test
  public void abstractViewWithOneColumnNoRows() {

    Column mockColumn = mock(Column.class);

    View view = AbstractView.of(Collections.singletonList(mockColumn), null);

    assertThat(view).isNotNull();
    assertThat(view.columns()).containsExactly(mockColumn);
    assertThat(view.rows()).isEmpty();
  }

  @Test(expected = IllegalArgumentException.class)
  public void abstractViewWithNoColumnsThrowsIllegalArgumentException() {

    try {
      AbstractView.of(Collections::emptyIterator, null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Columns [empty] are required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void abstractViewWithNullColumnsThrowsIllegalArgumentException() {

    try {
      AbstractView.of(null, null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Columns [null] are required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void setAndGetName() {

    AbstractView view = new TestView();

    assertThat(view.getName()).isNull();

    view.setName("TestView");

    assertThat(view.getName()).isEqualTo("TestView");

    view.setName(null);

    assertThat(view.getName()).isNull();
    assertThat(view.<TestView>named("MockView")).isSameAs(view);
    assertThat(view.getName()).isEqualTo("MockView");
  }

  @Test(expected = IllegalStateException.class)
  public void columnsForNonStructuredViewThrowsIllegalStateException() {

    try {
      new TestView().columns();
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("Columns for this View have not been defined");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void iteratorFowEmptyViewReturnsEmptyiterator() {

    AbstractView view = new TestView();

    assertThat(view).isNotNull();
    assertThat(view).isEmpty();
    assertThat(view.iterator().hasNext()).isFalse();
  }

  @Test
  public void toStringReturnsName() {

    AbstractView view = new TestView();

    view.setName("TestView");

    assertThat(view.getName()).isEqualTo("TestView");
    assertThat(view.toString()).isEqualTo("TestView");
  }

  static class TestView extends AbstractView {}

}
