/*
 * Copyright 2016 Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package org.cp.elements.function;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import org.junit.Test;

/**
 * Unit Tests for {@link FunctionUtils}
 *
 * @author John Blum
 * @see java.util.function.Function
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.function.FunctionUtils
 * @since 1.0.0
 */
public class FunctionUtilsUnitTests {

  @Test
  @SuppressWarnings("unchecked")
  public void toConsumerFromFunction() {

    Function<Object, Object> mockFunction = mock(Function.class);

    Consumer<Object> consumer = FunctionUtils.toConsumer(mockFunction);

    assertThat(consumer).isNotNull();

    consumer.accept("TEST");

    verify(mockFunction, times(1)).apply(eq("TEST"));
    verifyNoMoreInteractions(mockFunction);
  }

  @Test(expected = IllegalArgumentException.class)
  public void toConsumerFromNullFunction() {

    try {
      FunctionUtils.toConsumer(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Function is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void toFunctionFromConsumer() {

    Consumer<Object> mockConsumer = mock(Consumer.class);

    Function<Object, Object> function = FunctionUtils.toFunction(mockConsumer);

    assertThat(function).isNotNull();
    assertThat(function.apply("TEST")).isEqualTo("TEST");

    verify(mockConsumer, times(1)).accept(eq("TEST"));
    verifyNoMoreInteractions(mockConsumer);
  }

  @Test(expected = IllegalArgumentException.class)
  public void toFunctionFromNullConsumer() {

    try {
      FunctionUtils.toFunction((Consumer<?>) null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Consumer is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void toFunctionFromSupplier() {

    Supplier<Object> mockSupplier = mock(Supplier.class);

    doReturn("TEST").when(mockSupplier).get();

    Function<Object, Object> function = FunctionUtils.toFunction(mockSupplier);

    assertThat(function).isNotNull();
    assertThat(function.apply(null)).isEqualTo("TEST");

    verify(mockSupplier, times(1)).get();
    verifyNoMoreInteractions(mockSupplier);
  }

  @Test(expected = IllegalArgumentException.class)
  public void toFunctionFromNullSupplier() {

    try {
      FunctionUtils.toFunction((Supplier<?>) null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Supplier is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void toSupplierFromFunction() {

    Function<Object, Object> mockFunction = mock(Function.class);

    doReturn("TEST").when(mockFunction).apply(any());

    Supplier<Object> supplier = FunctionUtils.toSupplier(mockFunction);

    assertThat(supplier).isNotNull();
    assertThat(supplier.get()).isEqualTo("TEST");

    verify(mockFunction, times(1)).apply(isNull());
    verifyNoMoreInteractions(mockFunction);
  }

  @Test(expected = IllegalArgumentException.class)
  public void toSupplierFromNullFunction() {

    try {
      FunctionUtils.toSupplier(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Function is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }
}
