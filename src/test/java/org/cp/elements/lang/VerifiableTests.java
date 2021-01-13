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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link Verifiable}
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.lang.Verifiable
 * @see org.cp.elements.lang.Verifier
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class VerifiableTests {

  @Mock
  private Verifiable<Verifiable> mockVerifiable;

  @Test
  public void isValidReturnsTrue() {

    when(this.mockVerifiable.isValid()).thenCallRealMethod();
    when(this.mockVerifiable.validate()).thenReturn(this.mockVerifiable);

    assertThat(this.mockVerifiable.isValid()).isTrue();

    verify(this.mockVerifiable, times(1)).validate();
  }

  @Test
  public void isValidReturnsFalse() {

    when(this.mockVerifiable.isValid()).thenCallRealMethod();
    when(this.mockVerifiable.validate()).thenThrow(newIllegalStateException("Invalid"));

    assertThat(this.mockVerifiable.isValid()).isFalse();

    verify(this.mockVerifiable, times(1)).validate();
  }

  @Test
  public void verifyCallsVerifierVerifyWithThis() {

    Verifier mockVerifier = mock(Verifier.class);

    when(this.mockVerifiable.verify(any(Verifier.class))).thenCallRealMethod();

    assertThat(this.mockVerifiable.verify(mockVerifier)).isSameAs(this.mockVerifiable);

    verify(mockVerifier, times(1)).verify(eq(this.mockVerifiable));
  }
}
