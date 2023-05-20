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
package org.cp.elements.lang.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.mock;

import java.util.function.Function;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.Transformer;

/**
 * Unit Tests for {@link FunctionToTransformerAdapter}.
 *
 * @author John Blum
 * @see java.util.function.Function
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Transformer
 * @see org.cp.elements.lang.support.FunctionToTransformerAdapter
 * @since 1.0.0
 */
public class FunctionToTransformerAdapterTests {

  @Test
  @SuppressWarnings("unchecked")
  public void constructTransformerWithFunction() {

    Function<Object, Object> mockFunction = mock(Function.class);

    FunctionToTransformerAdapter<Object> transformer = new FunctionToTransformerAdapter<>(mockFunction);

    assertThat(transformer).isNotNull();
    assertThat(transformer.getFunction()).isEqualTo(mockFunction);
  }

  @Test
  public void constructTransformerWithNullThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new FunctionToTransformerAdapter<>(null))
      .withMessage("Function is required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void ofFunctionReturnsTransformer() {

    Function<Object, Object> mockFunction = mock(Function.class);

    FunctionToTransformerAdapter<Object> transformer = FunctionToTransformerAdapter.of(mockFunction);

    assertThat(transformer).isNotNull();
    assertThat(transformer.getFunction()).isEqualTo(mockFunction);
  }

  @Test
  public void ofNullFunctionThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FunctionToTransformerAdapter.of(null))
      .withMessage("Function is required")
      .withNoCause();
  }

  @Test
  public void transformerTransfomCallsFunctionApply() {

    Function<String, String> toUpperCase = String::toUpperCase;

    Transformer<String> transformer = FunctionToTransformerAdapter.of(toUpperCase);

    assertThat(transformer).isNotNull();
    assertThat(transformer.transform("test")).isEqualTo("TEST");
  }
}
