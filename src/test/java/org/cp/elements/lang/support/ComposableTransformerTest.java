/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang.support;

import static org.junit.Assert.*;

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
