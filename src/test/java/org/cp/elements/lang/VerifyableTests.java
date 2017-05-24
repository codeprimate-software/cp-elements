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

package org.cp.elements.lang;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalStateException;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link Verifyable}
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class VerifyableTests {

  @Mock
  private Verifyable<Verifyable> mockVerifyable;

  @Test
  public void isValidIsTrue() {
    when(mockVerifyable.isValid()).thenCallRealMethod();
    when(mockVerifyable.validate()).thenReturn(mockVerifyable);

    assertThat(mockVerifyable.isValid()).isTrue();

    verify(mockVerifyable, times(1)).validate();
  }

  @Test
  public void isValidIsFalse() {
    when(mockVerifyable.isValid()).thenCallRealMethod();
    when(mockVerifyable.validate()).thenThrow(newIllegalStateException("Invalid"));

    assertThat(mockVerifyable.isValid()).isFalse();

    verify(mockVerifyable, times(1)).validate();
  }
}
