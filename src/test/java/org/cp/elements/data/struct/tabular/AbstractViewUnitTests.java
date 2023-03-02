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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;

import java.util.Arrays;
import java.util.Collections;

import org.junit.Test;

import org.cp.elements.util.ArrayUtils;

/**
 * Unit Tests for {@link AbstractView}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.AbstractView
 * @since 1.0.0
 */
public class AbstractViewUnitTests {

  @Test
  public void abstractViewOfColumnsAndRows() {

    Column<?> mockColumnOne = mock(Column.class);
    Column<?> mockColumnTwo = mock(Column.class);

    Row mockRow = mock(Row.class);

    View view = AbstractView.of(Arrays.asList(mockColumnOne, mockColumnTwo), Collections.singletonList(mockRow));

    assertThat(view).isNotNull();
    assertThat(view.columns()).containsExactly(mockColumnOne, mockColumnTwo);
    assertThat(view.rows()).containsExactly(mockRow);

    verifyNoInteractions(mockColumnOne, mockColumnTwo, mockRow);
  }

  @Test
  public void abstractViewOfIterableColumnsAndArrayOfRows() {

    Column<?> mockColumnOne = mock(Column.class);
    Column<?> mockColumnTwo = mock(Column.class);

    Row mockRowOne = mock(Row.class);
    Row mockRowTwo = mock(Row.class);
    Row mockRowThree = mock(Row.class);

    View view = AbstractView.of(ArrayUtils.asIterable(mockColumnOne, mockColumnTwo),
      mockRowOne, mockRowTwo, mockRowThree);

    assertThat(view).isNotNull();
    assertThat(view.columns()).containsExactly(mockColumnOne, mockColumnTwo);
    assertThat(view.rows()).containsExactly(mockRowOne, mockRowTwo, mockRowThree);

    verifyNoInteractions(mockColumnOne, mockColumnTwo, mockRowOne, mockRowTwo, mockRowThree);
  }

  @Test
  public void abstractViewWithOneColumnNoRows() {

    Column<?> mockColumn = mock(Column.class);

    View view = AbstractView.of(Collections.singletonList(mockColumn));

    assertThat(view).isNotNull();
    assertThat(view.columns()).containsExactly(mockColumn);
    assertThat(view.rows()).isEmpty();

    verifyNoInteractions(mockColumn);
  }

  @Test
  public void abstractViewWithNoColumnsThrowsException() {

    Row mockRow = mock(Row.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> AbstractView.of(Collections::emptyIterator, mockRow))
      .withMessage("Columns are required")
      .withNoCause();

    verifyNoInteractions(mockRow);
  }

  @Test
  public void abstractViewWithNullColumnsThrowsException() {

    Row mockRow = mock(Row.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> AbstractView.of(null, mockRow))
      .withMessage("Columns are required")
      .withNoCause();

    verifyNoInteractions(mockRow);
  }

  @Test
  public void setAndGetName() {

    AbstractView view = new TestView();

    assertThat(view).isNotNull();
    assertThat(view.getName()).isNull();

    view.setName("TestView");

    assertThat(view.getName()).isEqualTo("TestView");

    view.setName("MockView");

    assertThat(view.getName()).isEqualTo("MockView");

    view.setName(null);

    assertThat(view.getName()).isNull();

  }

  @Test
  public void namedViewIsCorrect() {

    AbstractView view = new TestView();

    assertThat(view).isNotNull();
    assertThat(view.getName()).isNull();
    assertThat(view.<AbstractView>named("TestView")).isSameAs(view);
    assertThat(view.getName()).isEqualTo("TestView");
    assertThat(view.<AbstractView>named("MockView")).isSameAs(view);
    assertThat(view.getName()).isEqualTo("MockView");
    assertThat(view.<AbstractView>named(null)).isSameAs(view);
    assertThat(view.getName()).isNull();
  }

  @Test
  public void columnsForNonStructuredViewThrowsIllegalStateException() {

    assertThatIllegalStateException()
      .isThrownBy(() -> new TestView().columns())
      .withMessage("Columns for this View have not been defined")
      .withNoCause();
  }

  @Test
  public void iteratorForEmptyViewReturnsEmptyIterator() {

    AbstractView view = new TestView();

    assertThat(view).isNotNull();
    assertThat(view).isEmpty();
    assertThat(view.iterator().hasNext()).isFalse();
  }

  @Test
  public void toStringReturnsName() {

    AbstractView view = new TestView().named("TestView");

    assertThat(view).isNotNull();
    assertThat(view.getName()).isEqualTo("TestView");
    assertThat(view.toString()).isEqualTo("TestView");
  }

  static class TestView extends AbstractView { }

}
