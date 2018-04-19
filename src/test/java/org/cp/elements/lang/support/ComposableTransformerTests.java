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

package org.cp.elements.lang.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;

import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.Transformer;
import org.junit.Test;

/**
 * Unit tests for {@link ComposableTransformer}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Transformer
 * @see org.cp.elements.lang.support.ComposableTransformer
 * @since 1.0.0
 */
public class ComposableTransformerTests {

  @Test
  public void composeNullTransformersWithBuilderReturnsNull() {
    assertThat(ComposableTransformer.builder().compose(null, null)).isNull();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void composeSingleTransformersWithBuilderReturnsSingleTransformer() {

    Transformer mockTransformer = mock(Transformer.class);

    assertThat(ComposableTransformer.builder().compose(mockTransformer, null)).isEqualTo(mockTransformer);
    assertThat(ComposableTransformer.builder().compose(null, mockTransformer)).isEqualTo(mockTransformer);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void composeTransformersWithBuilderReturnsCompositeTransformer() {

    Transformer mockTransformerOne = mock(Transformer.class);
    Transformer mockTransformerTwo = mock(Transformer.class);
    Transformer composite = ComposableTransformer.builder().compose(mockTransformerOne, mockTransformerTwo);

    assertThat(composite).isNotNull();
    assertThat(composite).isNotSameAs(mockTransformerOne);
    assertThat(composite).isNotSameAs(mockTransformerTwo);
    assertThat(composite).isInstanceOf(ComposableTransformer.class);
    assertThat(((ComposableTransformer) composite).getTransformerOne()).isEqualTo(mockTransformerOne);
    assertThat(((ComposableTransformer) composite).getTransformerTwo()).isEqualTo(mockTransformerTwo);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void transformComplex() {

    Transformer<Integer> dividingTransformer = (value) -> (value / 2);
    Transformer<Integer> doublingTransformer = (value) -> (value * 2);
    Transformer<Integer> incrementingTransformer = (value) -> (value + 1);
    Transformer<Integer> multiplyingTransformer = (value) -> (value * value);
    Transformer<Integer> squareRootTransformer = (value) -> (int) Math.sqrt(value);

    Transformer<Integer> compositeTransformer = ComposableTransformer.<Integer>builder()
      .compose(incrementingTransformer, incrementingTransformer, incrementingTransformer);

    compositeTransformer = ComposableTransformer.<Integer>builder().compose(compositeTransformer, doublingTransformer);
    compositeTransformer = ComposableTransformer.<Integer>builder().compose(compositeTransformer, multiplyingTransformer);
    compositeTransformer = ComposableTransformer.<Integer>builder().compose(compositeTransformer, doublingTransformer);
    compositeTransformer = ComposableTransformer.<Integer>builder().compose(compositeTransformer, dividingTransformer);
    compositeTransformer = ComposableTransformer.<Integer>builder().compose(compositeTransformer, squareRootTransformer);
    compositeTransformer = ComposableTransformer.<Integer>builder().compose(compositeTransformer, incrementingTransformer);
    compositeTransformer = ComposableTransformer.<Integer>builder().compose(compositeTransformer, incrementingTransformer);
    compositeTransformer = ComposableTransformer.<Integer>builder().compose(compositeTransformer, dividingTransformer);

    assertThat(compositeTransformer).isNotNull();
    assertThat(compositeTransformer).isInstanceOf(ComposableTransformer.class);

    Integer value = compositeTransformer.transform(1);

    assertThat(value).isNotNull();
    assertThat(value.intValue()).isEqualTo(5);
  }

  @Test
  public void transformSimple() {

    Transformer<String> lowercaseTransformer = String::toLowerCase;
    Transformer<String> nullSafeTransformer = String::valueOf;
    Transformer<String> titleCaseTransformer = StringUtils::capitalize;
    Transformer<String> composite = ComposableTransformer.<String>builder().compose(nullSafeTransformer,
      ComposableTransformer.<String>builder().compose(lowercaseTransformer, titleCaseTransformer));

    assertThat(composite).isInstanceOf(ComposableTransformer.class);
    assertThat(composite.transform("TEST")).isEqualTo("Test");
    assertThat(composite.transform("Mock")).isEqualTo("Mock");
    assertThat(composite.transform("value")).isEqualTo("Value");
    assertThat(composite.transform(null)).isEqualTo("Null");
  }
}
