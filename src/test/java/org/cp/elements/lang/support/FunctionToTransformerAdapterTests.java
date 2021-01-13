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
import static org.mockito.Mockito.mock;

import java.util.function.Function;

import org.cp.elements.lang.Transformer;
import org.junit.Test;

/**
 * Unit tests for {@link FunctionToTransformerAdapter}.
 *
 * @author John Blum
 * @see java.util.function.Function
 * @see org.junit.Test
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

  @Test(expected = IllegalArgumentException.class)
  public void constructTransformerWithNullThrowsIllegalArgumentException() {

    try {
      new FunctionToTransformerAdapter<>(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Function is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void ofFunctionReturnsTransformer() {

    Function<Object, Object> mockFunction = mock(Function.class);

    FunctionToTransformerAdapter<Object> transformer = FunctionToTransformerAdapter.of(mockFunction);

    assertThat(transformer).isNotNull();
    assertThat(transformer.getFunction()).isEqualTo(mockFunction);
  }

  @Test(expected = IllegalArgumentException.class)
  public void ofNullFunctionThrowsIllegalArgumentException() {

    try {
      FunctionToTransformerAdapter.of(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Function is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void transformerTransfomCallsFunctionApply() {

    Function<String, String> toUpperCase = String::toUpperCase;

    Transformer<String> transformer = FunctionToTransformerAdapter.of(toUpperCase);

    assertThat(transformer).isNotNull();
    assertThat(transformer.transform("test")).isEqualTo("TEST");
  }
}
