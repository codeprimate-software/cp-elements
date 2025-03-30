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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.Constants;
import org.cp.elements.util.CollectionUtils;

/**
 * Unit Tests for {@link Oql.Select}
 *
 * @author John Blum
 * @see Oql.Select
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
class OqlSelectUnitTests {

  @Test
  void defaultDistinctIsFalse() {

    Oql.Select<?, ?> mockSelect = mock(Oql.Select.class);

    doCallRealMethod().when(mockSelect).isDistinct();

    assertThat(mockSelect.isDistinct()).isFalse();

    verify(mockSelect, times(1)).isDistinct();
    verifyNoMoreInteractions(mockSelect);
  }

  @Test
  void defaultDistinctIsUnsupported() {

    Oql.Select<?, ?> select = mock(Oql.Select.class);

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
