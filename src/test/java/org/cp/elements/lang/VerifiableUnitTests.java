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
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalStateException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Unit Tests for {@link Verifiable}
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.junit.jupiter.api.extension.ExtendWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.jupiter.MockitoExtension
 * @see org.cp.elements.lang.Verifiable
 * @see org.cp.elements.lang.Verifier
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
public class VerifiableUnitTests {

  @Mock
  @SuppressWarnings("rawtypes")
  private Verifiable<Verifiable> mockVerifiable;

  @Test
  void isValidReturnsTrue() {

    doReturn(this.mockVerifiable).when(this.mockVerifiable).validate();
    doCallRealMethod().when(this.mockVerifiable).isValid();

    assertThat(this.mockVerifiable.isValid()).isTrue();

    verify(this.mockVerifiable, times(1)).validate();
    verifyNoMoreInteractions(this.mockVerifiable);
  }

  @Test
  void isValidReturnsFalse() {

    doThrow(newIllegalStateException("Invalid")).when(this.mockVerifiable).validate();
    doCallRealMethod().when(this.mockVerifiable).isValid();

    assertThat(this.mockVerifiable.isValid()).isFalse();

    verify(this.mockVerifiable, times(1)).isValid();
    verify(this.mockVerifiable, times(1)).validate();
    verifyNoMoreInteractions(this.mockVerifiable);
  }

  @Test
  void validateReturnsSelf() {

    doCallRealMethod().when(this.mockVerifiable).validate();

    assertThat(this.mockVerifiable.validate()).isSameAs(this.mockVerifiable);

    verify(this.mockVerifiable, times(1)).validate();
    verifyNoMoreInteractions(this.mockVerifiable);
  }

  @Test
  void verifyCallsVerifierVerifyWithThis() {

    Verifier mockVerifier = mock(Verifier.class);

    doCallRealMethod().when(this.mockVerifiable).verify(any(Verifier.class));

    assertThat(this.mockVerifiable.verify(mockVerifier)).isSameAs(this.mockVerifiable);

    verify(this.mockVerifiable, times(1)).verify(eq(mockVerifier));
    verify(mockVerifier, times(1)).verify(eq(this.mockVerifiable));
    verifyNoMoreInteractions(this.mockVerifiable, mockVerifier);
  }
}
