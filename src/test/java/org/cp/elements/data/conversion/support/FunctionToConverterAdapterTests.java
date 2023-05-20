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
package org.cp.elements.data.conversion.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;

import java.util.function.Function;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link FunctionToConverterAdapter}.
 *
 * @author John Blum
 * @see java.util.function.Function
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.conversion.support.FunctionToConverterAdapter
 * @since 1.0.0
 */
public class FunctionToConverterAdapterTests {

  @Test
  @SuppressWarnings("unchecked")
  public void constructsConverterFromFunction() {

    Function<Object, Object> mockFunction = mock(Function.class);

    FunctionToConverterAdapter<Object, Object> converter = new FunctionToConverterAdapter<>(mockFunction);

    assertThat(converter).isNotNull();
    assertThat(converter.getFunction()).isEqualTo(mockFunction);

    verifyNoInteractions(mockFunction);
  }

  @Test
  public void constructWithNullFunctionThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new FunctionToConverterAdapter<>(null))
      .withMessage("Function is required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void ofFunctionReturnsConverter() {

    Function<Object, Object> mockFunction = mock(Function.class);

    FunctionToConverterAdapter<Object, Object> converter = FunctionToConverterAdapter.of(mockFunction);

    assertThat(converter).isNotNull();
    assertThat(converter.getFunction()).isEqualTo(mockFunction);

    verifyNoInteractions(mockFunction);
  }

  @Test
  public void ofNullFunctionThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FunctionToConverterAdapter.of(null))
      .withMessage("Function is required")
      .withNoCause();
  }

  @Test
  public void convertCallsFunctionApply() {

    Function<String, Number> mockFunction = Integer::parseInt;

    assertThat(FunctionToConverterAdapter.of(mockFunction).convert("1")).isEqualTo(1);
  }

  @Test
  public void convertCallsFunctionApplyUsingAdapterExtension() {

    Function<String, Number> mockFunction = Integer::parseInt;

    assertThat(new StringToNumberConverter(mockFunction).convert("1")).isEqualTo(1);
  }

  @Test
  public void convertCallsFunctionExtensionApply() {
    assertThat(FunctionToConverterAdapter.of(StringToNumberFunction.INSTANCE).convert("1")).isEqualTo(1);
  }

  static final class StringToNumberConverter extends FunctionToConverterAdapter<String, Number> {

    StringToNumberConverter(Function<String, Number> function) {
      super(function);
    }
  }

  static final class StringToNumberFunction implements Function<String, Number> {

    static final StringToNumberFunction INSTANCE = new StringToNumberFunction();

    @Override
    public Number apply(String value) {
      return Integer.parseInt(value);
    }
  }
}
