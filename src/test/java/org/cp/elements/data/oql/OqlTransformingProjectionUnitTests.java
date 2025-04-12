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
package org.cp.elements.data.oql;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Iterator;
import java.util.Spliterator;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link Oql.TransformingProjection}.
 *
 * @author John Blum
 * @see Oql.TransformingProjection
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
class OqlTransformingProjectionUnitTests {

  @Test
  void defaultIteratorIsEmpty() {

    Oql.TransformingProjection<?, ?> mockTransformingProjection = mock(Oql.TransformingProjection.class);

    doCallRealMethod().when(mockTransformingProjection).iterator();

    Iterator<?> iterator = mockTransformingProjection.iterator();

    assertThat(iterator).isNotNull();
    assertThat(iterator.hasNext()).isFalse();

    verify(mockTransformingProjection, times(1)).iterator();
    verifyNoMoreInteractions(mockTransformingProjection);
  }

  @Test
  void defaultStreamIsEmpty() {

    Oql.TransformingProjection<?, ?> mockTransformingProjection = mock(Oql.TransformingProjection.class);
    Spliterator<?> mockSpliterator = mock(Spliterator.class);

    doCallRealMethod().when(mockTransformingProjection).stream();
    doReturn(mockSpliterator).when(mockTransformingProjection).spliterator();

    Stream<?> stream = mockTransformingProjection.stream();

    assertThat(stream).isNotNull();
    assertThat(stream).isEmpty();

    verify(mockTransformingProjection, times(1)).stream();
    verify(mockTransformingProjection, times(1)).spliterator();
    verifyNoMoreInteractions(mockTransformingProjection);
  }
}
