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
package org.cp.elements.data.oql.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Collections;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.assertj.core.api.InstanceOfAssertFactories;
import org.cp.elements.data.oql.Oql;
import org.cp.elements.security.model.User;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Unit Tests for {@link SelectClause}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 3.0.0
 */
@ExtendWith(MockitoExtension.class)
class SelectClauseUnitTests {

  @Mock
  private Oql.Projection<?, ?> mockProjection;

  @Test
  void select() {

    SelectClause<?, ?> select = SelectClause.select(this.mockProjection);

    assertThat(select).isNotNull();
    assertThat(select.isDistinct()).isEqualTo(Oql.Select.DEFAULT_DISTINCT);
    assertThat(select.getProjection()).isInstanceOf(SelectClause.ProjectionWrapper.class);
    assertThat(select.getProjection())
      .asInstanceOf(InstanceOfAssertFactories.type(SelectClause.ProjectionWrapper.class))
      .extracting(SelectClause.ProjectionWrapper::getProjection)
      .isEqualTo(this.mockProjection);
  }

  @Test
  void selectDistinct() {

    doReturn(User.class).when(this.mockProjection).getFromType();

    SelectClause<?, ?> select = SelectClause.select(this.mockProjection);

    assertThat(select).isNotNull();
    assertThat(select.isDistinct()).isFalse();
    assertThat(select.getProjection()).isInstanceOf(SelectClause.ProjectionWrapper.class);

    Oql.From<?, ?> from = select.distinct().from(Collections.emptySet());

    assertThat(select.isDistinct()).isTrue();
    assertThat(from).isNotNull();
    assertThat(from.getSelection()).isEqualTo(select);

    verify(this.mockProjection, times(1)).getFromType();
    verifyNoMoreInteractions(this.mockProjection);
  }

  @Test
  void selectClauseDistinctBuilderWithNoSelect() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new SelectClause.DistinctBuilder<>(null))
      .withMessage("Select is required")
      .withNoCause();
  }

  @Test
  void selectWithNoProjection() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SelectClause.select(null))
      .withMessage("Projection is required")
      .withNoCause();
  }
}
