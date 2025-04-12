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
import static org.cp.elements.lang.ThrowableAssertions.assertThatThrowableOfType;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.ThrowableOperation;

/**
 * Unit Tests for {@link ThrowableSupplier}.
 *
 * @author John Blum
 * @see org.cp.elements.function.ThrowableSupplier
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
class ThrowableSupplierUnitTests {

  @Test
  @SuppressWarnings("unchecked")
  void getCallsGetThrowingException() throws Exception {

    ThrowableSupplier<Object> mockThrowableConsumer = mock(ThrowableSupplier.class);

    doCallRealMethod().when(mockThrowableConsumer).get();
    doReturn("test").when(mockThrowableConsumer).getThrowingException();

    assertThat(mockThrowableConsumer.get()).isEqualTo("test");

    verify(mockThrowableConsumer, times(1)).get();
    verify(mockThrowableConsumer, times(1)).getThrowingException();
    verifyNoMoreInteractions(mockThrowableConsumer);
  }

  @Test
  @SuppressWarnings("unchecked")
  void getThrowsException() throws Exception {

    ThrowableSupplier<Object> mockThrowableConsumer = mock(ThrowableSupplier.class);

    doCallRealMethod().when(mockThrowableConsumer).get();
    doThrow(new Exception("TEST")).when(mockThrowableConsumer).getThrowingException();

    assertThatThrowableOfType(IllegalStateException.class)
      .isThrownBy(ThrowableOperation.fromSupplier(mockThrowableConsumer))
      .havingMessage("Failed to get supplied value")
      .causedBy(Exception.class)
      .havingMessage("TEST")
      .withNoCause();

    verify(mockThrowableConsumer, times(1)).get();
    verify(mockThrowableConsumer, times(1)).getThrowingException();
    verifyNoMoreInteractions(mockThrowableConsumer);
  }
}
