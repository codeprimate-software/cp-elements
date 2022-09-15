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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import org.junit.Test;

/**
 * Unit Tests for {@link NullSafeOperations}.
 *
 * @author John Blum
 * @see java.util.function.Consumer
 * @see java.util.function.Function
 * @see java.util.function.Supplier
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.NullSafeOperations
 * @since 1.0.0
 */
public class NullSafeOperationsUnitTests {

  @Test
  @SuppressWarnings("unchecked")
  public void ifNotNullDoWithNonNullTargetObject() {

    Consumer<Object> mockConsumer = mock(Consumer.class);

    NullSafeOperations.ifNotNullDo("test", mockConsumer);

    verify(mockConsumer, times(1)).accept(eq("test"));
    verifyNoMoreInteractions(mockConsumer);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void ifNotNullDoWithNullTargetObject() {

    Consumer<Object> mockConsumer = mock(Consumer.class);

    NullSafeOperations.ifNotNullDo(null, mockConsumer);

    verifyNoInteractions(mockConsumer);
  }

  @Test
  public void ifNotNullDoWithNullConsumerThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> NullSafeOperations.ifNotNullDo("test", null))
      .withMessage("The Consumer used to process the target Object is required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void ifNotNullDoOrReturnCallsFunction() {

    Function<Object, Object> mockFunction = mock(Function.class);
    Supplier<Object> mockSupplier = mock(Supplier.class);

    doReturn("mock").when(mockFunction).apply(anyString());

    assertThat(NullSafeOperations.ifNotNullDoOrReturn("test", mockFunction, mockSupplier)).isEqualTo("mock");

    verify(mockFunction, times(1)).apply(eq("test"));
    verifyNoMoreInteractions(mockFunction);
    verifyNoInteractions(mockSupplier);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void ifNotNullDoOrReturnCallsSupplier() {

    Function<Object, Object> mockFunction = mock(Function.class);
    Supplier<Object> mockSupplier = mock(Supplier.class);

    doReturn("fake").when(mockSupplier).get();

    assertThat(NullSafeOperations.ifNotNullDoOrReturn(null, mockFunction, mockSupplier)).isEqualTo("fake");

    verify(mockSupplier, times(1)).get();
    verifyNoMoreInteractions(mockSupplier);
    verifyNoInteractions(mockFunction);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void ifNotNullDoOrReturnWithNullFunction() {

    Supplier<Object> mockSupplier = mock(Supplier.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> NullSafeOperations.ifNotNullDoOrReturn("test", null, mockSupplier))
      .withMessage("The Function used to process the target Object is required")
      .withNoCause();

    verifyNoInteractions(mockSupplier);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void ifNotNullDoOrReturnWithNullSupplier() {

    Function<Object, Object> mockFunction = mock(Function.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> NullSafeOperations.ifNotNullDoOrReturn("test", mockFunction, null))
      .withMessage("The Supplier to call when the target Object is null is required")
      .withNoCause();

    verifyNoInteractions(mockFunction);
  }
}
