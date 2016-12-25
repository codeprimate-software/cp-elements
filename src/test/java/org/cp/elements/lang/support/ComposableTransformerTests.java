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
import static org.cp.elements.util.ArrayUtils.getFirst;
import static org.cp.elements.util.ArrayUtils.iterable;
import static org.cp.elements.util.CollectionUtils.toList;
import static org.cp.elements.util.CollectionUtils.toSet;
import static org.mockito.Mockito.mock;

import java.util.Arrays;
import java.util.Iterator;

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

  @SuppressWarnings("unchecked")
  protected <T> Transformer<T> mockTransformer(String... name) {
    return mock(Transformer.class, getFirst(name, "MockTransformer"));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void composeWithArray() {
    Transformer[] transformerArray = { mockTransformer("one"), mockTransformer("two"), mockTransformer("three") };

    Transformer compositeTransformer = ComposableTransformer.compose(transformerArray);

    assertThat(compositeTransformer).isNotNull();
    assertThat(compositeTransformer).isInstanceOf(ComposableTransformer.class);
    assertThat(toSet((ComposableTransformer) compositeTransformer).containsAll(Arrays.asList(transformerArray)))
      .isTrue();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void composeWithEmptyArray() {
    assertThat(ComposableTransformer.compose(new Transformer[0])).isNull();
  }

  @Test
  public void composeWithNullArray() {
    assertThat(ComposableTransformer.compose((Transformer<Object>[]) null)).isNull();
  }

  @Test
  public void composeWithOneElementArray() {
    Transformer<?> mockTransformer = mockTransformer();

    assertThat(ComposableTransformer.compose(mockTransformer)).isSameAs(mockTransformer);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void composeWithTwoElementArray() {
    Transformer mockTransformerOne = mockTransformer("one");
    Transformer mockTransformerTwo = mockTransformer("two");

    Transformer[] transformerArray = { mockTransformerOne, mockTransformerTwo };

    Transformer compositeTransformer = ComposableTransformer.compose(transformerArray);

    transformerArray[0] = null;
    transformerArray[1] = null;

    assertThat(compositeTransformer).isNotNull();
    assertThat(compositeTransformer).isInstanceOf(ComposableTransformer.class);

    Iterator<Transformer> transformerIterator = ((ComposableTransformer) compositeTransformer).iterator();

    assertThat(transformerIterator).isNotNull();
    assertThat(transformerIterator.hasNext()).isTrue();
    assertThat(transformerIterator.next()).isSameAs(mockTransformerOne);
    assertThat(transformerIterator.hasNext()).isTrue();
    assertThat(transformerIterator.next()).isSameAs(mockTransformerTwo);
    assertThat(transformerIterator.hasNext()).isFalse();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void composeWithIterable() {
    Iterable<Transformer<Object>> iterable =
      iterable(mockTransformer("one"), mockTransformer("two"), mockTransformer("three"));

    Transformer<Object> compositeTransformer = ComposableTransformer.compose(iterable);

    assertThat(compositeTransformer).isNotNull();
    assertThat(compositeTransformer).isInstanceOf(ComposableTransformer.class);
    assertThat(toSet((ComposableTransformer) compositeTransformer).containsAll(toList(iterable)));
  }

  @Test
  public void composeWithEmptyIterable() {
    assertThat(ComposableTransformer.compose(iterable())).isNull();
  }

  @Test
  public void composeWithNullIterable() {
    assertThat(ComposableTransformer.compose((Iterable<Transformer<Object>>) null)).isNull();
  }

  @Test
  public void composeWithSingleElementIterable() {
    Transformer<Object> mockTransformer = mockTransformer();

    assertThat(ComposableTransformer.compose(iterable(mockTransformer))).isSameAs(mockTransformer);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void transform() {
    Transformer<Integer> dividingTransformer = (value) -> (value / 2);
    Transformer<Integer> doublingTransformer = (value) -> (value * 2);
    Transformer<Integer> incrementingTransformer = (value) -> (value + 1);
    Transformer<Integer> multiplyingTransformer = (value) -> (value * value);
    Transformer<Integer> squareRootTransformer = (value) -> (int) Math.sqrt(value);

    Transformer<Integer> compositeTransformer = ComposableTransformer.compose(incrementingTransformer,
      incrementingTransformer, incrementingTransformer);

    compositeTransformer = ComposableTransformer.compose(compositeTransformer, doublingTransformer);
    compositeTransformer = ComposableTransformer.compose(compositeTransformer, multiplyingTransformer);
    compositeTransformer = ComposableTransformer.compose(compositeTransformer, doublingTransformer);
    compositeTransformer = ComposableTransformer.compose(compositeTransformer, dividingTransformer);
    compositeTransformer = ComposableTransformer.compose(compositeTransformer, squareRootTransformer);
    compositeTransformer = ComposableTransformer.compose(compositeTransformer, incrementingTransformer,
      incrementingTransformer);
    compositeTransformer = ComposableTransformer.compose(compositeTransformer, dividingTransformer);

    assertThat(compositeTransformer).isNotNull();

    Integer value = compositeTransformer.transform(1);

    assertThat(value).isNotNull();
    assertThat(value.intValue()).isEqualTo(5);
  }
}
