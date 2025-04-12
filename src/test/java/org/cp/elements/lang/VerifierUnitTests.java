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
package org.cp.elements.lang;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.jupiter.api.Test;

import org.mockito.InOrder;

/**
 * Unit Tests for {@link Verifier}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Verifier
 * @since 2.0.0
 */
class VerifierUnitTests {

  @Test
  void andThenWithNonNullVerifier() {

    Verifiable<?> mockVerifiable = mock(Verifiable.class);

    Verifier mockVerifierOne = mock(Verifier.class);
    Verifier mockVerifierTwo = mock(Verifier.class);

    doCallRealMethod().when(mockVerifierOne).andThen(any());

    Verifier composedVerifier = mockVerifierOne.andThen(mockVerifierTwo);

    assertThat(composedVerifier).isNotNull();
    assertThat(composedVerifier).isNotSameAs(mockVerifierOne);
    assertThat(composedVerifier).isNotSameAs(mockVerifierTwo);

    composedVerifier.verify(mockVerifiable);

    InOrder order = inOrder(mockVerifierOne, mockVerifierTwo);

    order.verify(mockVerifierOne, times(1)).andThen(eq(mockVerifierTwo));
    order.verify(mockVerifierOne, times(1)).verify(eq(mockVerifiable));
    order.verify(mockVerifierTwo, times(1)).verify(eq(mockVerifiable));

    verifyNoMoreInteractions(mockVerifierOne, mockVerifierTwo);
    verifyNoInteractions(mockVerifiable);
  }

  @Test
  void andThenWithNullVerifier() {

    Verifier mockVerifier = mock(Verifier.class);

    doCallRealMethod().when(mockVerifier).andThen(any());

    assertThat(mockVerifier.andThen(null)).isSameAs(mockVerifier);

    verify(mockVerifier, times(1)).andThen(isNull());
    verifyNoMoreInteractions(mockVerifier);
  }
}
