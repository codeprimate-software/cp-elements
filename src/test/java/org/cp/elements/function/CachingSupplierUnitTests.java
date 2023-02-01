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
package org.cp.elements.function;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Supplier;
import java.util.stream.IntStream;

import org.junit.Test;

/**
 * Unit Tests for {@link CachingSupplier}.
 *
 * @author John Blum
 * @see java.util.function.Supplier
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.function.CachingSupplier
 * @since 1.0.1
 */
public class CachingSupplierUnitTests {

  @Test
  public void fromSupplierIsCorrect() {

    Supplier<?> mockSupplier = mock(Supplier.class);

    doReturn("test").when(mockSupplier).get();

    Supplier<?> cachingSupplier = CachingSupplier.from(mockSupplier);

    assertThat(cachingSupplier).isNotNull();
    assertThat(cachingSupplier).isNotSameAs(mockSupplier);
    assertThat(cachingSupplier.get()).isEqualTo("test");
    assertThat(cachingSupplier.get()).isEqualTo("test");

    verify(mockSupplier, times(1)).get();
    verifyNoMoreInteractions(mockSupplier);
  }

  @Test
  public void fromNullSupplierThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> CachingSupplier.from(null))
      .withMessage("Supplier is required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void cachingSupplierFunctionalityIsCorrect() {

    AtomicInteger counter = new AtomicInteger(0);

    Supplier<Integer> mockSupplier = mock(Supplier.class);

    doAnswer(invocation -> counter.incrementAndGet()).when(mockSupplier).get();

    CachingSupplier<Integer> cachingSupplier = CachingSupplier.from(mockSupplier);

    assertThat(cachingSupplier).isNotNull();
    assertThat(cachingSupplier).isNotSameAs(mockSupplier);

    IntStream.range(1, 10).forEach(count -> assertThat(cachingSupplier.get()).isOne());

    cachingSupplier.clear();

    IntStream.range(1, 10).forEach(count -> assertThat(cachingSupplier.get()).isEqualTo(2));

    verify(mockSupplier, times(2)).get();
    verifyNoMoreInteractions(mockSupplier);
  }
}
