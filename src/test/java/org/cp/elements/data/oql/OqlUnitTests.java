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
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.function.Function;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.oql.Oql.Projection;
import org.cp.elements.data.oql.Oql.Select;
import org.cp.elements.data.oql.provider.SimpleOqlProvider;
import org.cp.elements.lang.Constants;
import org.cp.elements.security.model.User;
import org.cp.elements.util.CollectionUtils;
import org.testcontainers.shaded.org.apache.commons.lang3.StringUtils;

/**
 * Unit Tests for {@link Oql}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.Oql
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
public class OqlUnitTests {

  @Test
  void defaultProviderIsSimpleOqlProvider() {
    assertThat(Oql.defaultProvider()).isInstanceOf(SimpleOqlProvider.class);
  }

  @Test
  void projectAsTypeReturnsType() {

    Oql.Projection<Object, ?> projection = Oql.Projection.as(User.class);

    assertThat(projection).isNotNull();
    assertThat(projection.getType()).isEqualTo(User.class);
    assertThat(projection.getFromType()).isEqualTo(Object.class);
  }

  @Test
  void projectAsNullTypeThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Oql.Projection.as(null))
      .withMessage("Type is required")
      .withNoCause();
  }

  @Test
  void projectionMappedWithFunction() {

    Function<String, String> function = StringUtils::reverse;

    Projection<String, String> projection = Oql.Projection.<String, String>as(String.class)
      .fromType(String.class)
      .mappedWith(function);

    assertThat(projection).isNotNull();
    assertThat(projection.getFromType()).isEqualTo(String.class);
    assertThat(projection.getType()).isEqualTo(String.class);
    assertThat(projection.map("DOG")).isEqualTo("GOD");
  }

  @Test
  void projectionMappedWithNullFunction() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Oql.Projection.as(Object.class).mappedWith(null))
      .withMessage("Object mapping function is required")
      .withNoCause();
  }

  @Test
  void projectStarReturnsProjectionMappingTargetToItself() {

    Oql.Projection<Object, Object> projection = Oql.Projection.star();

    assertThat(projection).isNotNull();
    assertThat(projection.map("TEST")).isEqualTo("TEST");
  }

  @Test
  void projectStarMappedWithThrowsUnsupportedOperationException() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> Oql.Projection.star().mappedWith(Function.identity()))
      .withMessage(Constants.OPERATION_NOT_SUPPORTED)
      .withNoCause();
  }

  @Test
  void projectStarFromNullType() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Oql.Projection.star().fromType(null))
      .withMessage("From type is required")
      .withNoCause();
  }

  @Test
  void selectDistinctIsUnsupportedByDefault() {

    Select<?, ?> select = mock(Select.class);

    doCallRealMethod().when(select).distinct();

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> select.distinct().from(CollectionUtils.emptyIterable()))
      .withMessage(Constants.UNSUPPORTED_OPERATION)
      .withNoCause();

    verify(select, times(1)).distinct();
    verify(select, never()).from(any());
    verifyNoMoreInteractions(select);
  }
}
