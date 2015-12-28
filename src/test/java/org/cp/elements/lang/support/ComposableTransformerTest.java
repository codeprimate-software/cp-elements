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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;

import org.cp.elements.lang.Transformer;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.junit.Test;

/**
 * The ComposableTransformerTest class is a test suite of test cases testing the contract and functionality
 * of the ComposableTransformer class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Transformer
 * @see org.cp.elements.lang.support.ComposableTransformer
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.jmock.Mockery
 * @see org.junit.Test
 * @since 1.0.0
 */
public class ComposableTransformerTest extends AbstractMockingTestSuite {

  @Test
  @SuppressWarnings("unchecked")
  public void composeWithArray() {
    Transformer mockTransformerOne = mock(Transformer.class, "testComposeWithArray.one");
    Transformer mockTransformerTwo = mock(Transformer.class, "testComposeWithArray.two");

    Transformer[] transformerArray = { mockTransformerOne, mockTransformerTwo };

    Transformer composedTransformer = ComposableTransformer.compose(transformerArray);

    assertTrue(composedTransformer instanceof ComposableTransformer);

    transformerArray[0] = null;
    transformerArray[1] = null;

    Iterator<Transformer> transformerIterator = ((ComposableTransformer) composedTransformer).iterator();

    assertNotNull(transformerIterator);
    assertTrue(transformerIterator.hasNext());
    assertSame(mockTransformerOne, transformerIterator.next());
    assertTrue(transformerIterator.hasNext());
    assertSame(mockTransformerTwo, transformerIterator.next());
    assertFalse(transformerIterator.hasNext());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void composeWithNull() {
    assertNull(ComposableTransformer.compose((Transformer<Object>[]) null));
  }

  @Test
  public void composeWithOne() {
    Transformer mockTransformer = mock(Transformer.class, "testComposeWithOne");
    assertSame(mockTransformer, ComposableTransformer.compose(mockTransformer));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void composeWithTwo() {
    Transformer mockTransformerOne = mock(Transformer.class, "testComposeWithTwo.One");
    Transformer mockTransformerTwo = mock(Transformer.class, "testComposeWithTwo.Two");
    Transformer composedTransformer = ComposableTransformer.compose(mockTransformerOne, mockTransformerTwo);

    assertTrue(composedTransformer instanceof ComposableTransformer);

    Iterator<Transformer> transformerIterator = ((ComposableTransformer) composedTransformer).iterator();

    assertNotNull(transformerIterator);
    assertTrue(transformerIterator.hasNext());
    assertSame(mockTransformerOne, transformerIterator.next());
    assertTrue(transformerIterator.hasNext());
    assertSame(mockTransformerTwo, transformerIterator.next());
    assertFalse(transformerIterator.hasNext());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void transform() {
    Transformer<Integer> incrementingTransformer = (value) -> (value + 1);

    Transformer<Integer> doublingTransformer = (value) -> (value * 2);

    Transformer<Integer> multiplyingTransformer = (value) -> (value * value);

    Transformer<Integer> dividingTransformer = (value) -> (value / 2);

    Transformer<Integer> squareRootTransformer = (value) -> (int) Math.sqrt(value);

    Transformer<Integer> composedTransformer = ComposableTransformer.compose(incrementingTransformer,
      incrementingTransformer, incrementingTransformer);

    composedTransformer = ComposableTransformer.compose(composedTransformer, doublingTransformer);
    composedTransformer = ComposableTransformer.compose(composedTransformer, multiplyingTransformer);
    composedTransformer = ComposableTransformer.compose(composedTransformer, doublingTransformer);
    composedTransformer = ComposableTransformer.compose(composedTransformer, dividingTransformer);
    composedTransformer = ComposableTransformer.compose(composedTransformer, squareRootTransformer);
    composedTransformer = ComposableTransformer.compose(composedTransformer, incrementingTransformer,
      incrementingTransformer);
    composedTransformer = ComposableTransformer.compose(composedTransformer, dividingTransformer);

    Integer value = composedTransformer.transform(1);

    assertNotNull(value);
    assertEquals(5, value.intValue());
  }

}
