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

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.cp.elements.test.AbstractMockingTestSuite;
import org.jmock.Expectations;
import org.junit.Test;

/**
 * The ComposableRunnableTest class is a test suite of test cases testing the contract and functionality of the
 * ComposableRunnable class.
 *
 * @author John J. Blum
 * @see java.lang.Runnable
 * @see org.cp.elements.lang.support.ComposableRunnable
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.junit.Test
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ComposableRunnableTest extends AbstractMockingTestSuite {

  @Test
  public void testCompose() {
    Runnable mockRunnableLeft = mockContext.mock(Runnable.class, "left");
    Runnable mockRunnableRight = mockContext.mock(Runnable.class, "right");

    assertNull(ComposableRunnable.compose(null, null));
    assertSame(mockRunnableLeft, ComposableRunnable.compose(mockRunnableLeft, null));
    assertSame(mockRunnableRight, ComposableRunnable.compose(null, mockRunnableRight));

    Runnable composedRunnable = ComposableRunnable.compose(mockRunnableLeft, mockRunnableRight);

    assertTrue(composedRunnable instanceof ComposableRunnable);
  }

  @Test
  public void testComposeRunnableArray() {
    Runnable mockRunnableZero = mockContext.mock(Runnable.class, "zero");
    Runnable mockRunnableOne = mockContext.mock(Runnable.class, "one");
    Runnable mockRunnableTwo = mockContext.mock(Runnable.class, "two");
    Runnable mockRunnableThree = mockContext.mock(Runnable.class, "three");

    assertNull(ComposableRunnable.compose((Runnable[]) null));
    assertSame(mockRunnableZero, ComposableRunnable.compose(mockRunnableZero));

    Runnable composedRunnable = ComposableRunnable.compose(mockRunnableZero, mockRunnableOne, mockRunnableTwo,
      mockRunnableThree);

    assertTrue(composedRunnable instanceof ComposableRunnable);
  }

  @Test
  public void testRunOne() {
    final Runnable expectedRunnable = mockContext.mock(Runnable.class, "left");

    mockContext.checking(new Expectations() {{
      oneOf(expectedRunnable).run();
    }});

    Runnable actualRunnable = ComposableRunnable.compose(expectedRunnable);

    assertSame(expectedRunnable, actualRunnable);

    actualRunnable.run();
  }

  @Test
  public void testRunTwo() {
    final Runnable mockRunnableLeft = mockContext.mock(Runnable.class, "left");
    final Runnable mockRunnableRight = mockContext.mock(Runnable.class, "right");

    mockContext.checking(new Expectations() {{
      oneOf(mockRunnableLeft).run();
      oneOf(mockRunnableRight).run();
    }});

    Runnable composedRunnabe = ComposableRunnable.compose(mockRunnableLeft, mockRunnableRight);

    assertTrue(composedRunnabe instanceof ComposableRunnable);

    composedRunnabe.run();
  }

  @Test
  public void testRunFour() {
    final Runnable mockRunnableZero = mockContext.mock(Runnable.class, "zero");
    final Runnable mockRunnableOne = mockContext.mock(Runnable.class, "one");
    final Runnable mockRunnableTwo = mockContext.mock(Runnable.class, "two");
    final Runnable mockRunnableThree = mockContext.mock(Runnable.class, "three");

    mockContext.checking(new Expectations() {{
      oneOf(mockRunnableZero).run();
      oneOf(mockRunnableOne).run();
      oneOf(mockRunnableTwo).run();
      oneOf(mockRunnableThree).run();
    }});

    Runnable composedRunnable = ComposableRunnable.compose(mockRunnableZero, mockRunnableOne, mockRunnableTwo,
      mockRunnableThree);

    assertTrue(composedRunnable instanceof ComposableRunnable);

    composedRunnable.run();
  }

}
