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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link ComposableRunnable} class.
 *
 * @author John J. Blum
 * @see java.lang.Runnable
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.support.ComposableRunnable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ComposableRunnableTests {

  @Test
  public void compose() {
    Runnable mockRunnableLeft = mock(Runnable.class, "left");
    Runnable mockRunnableRight = mock(Runnable.class, "right");

    assertNull(ComposableRunnable.compose(null, null));
    assertSame(mockRunnableLeft, ComposableRunnable.compose(mockRunnableLeft, null));
    assertSame(mockRunnableRight, ComposableRunnable.compose(null, mockRunnableRight));

    Runnable composedRunnable = ComposableRunnable.compose(mockRunnableLeft, mockRunnableRight);

    assertTrue(composedRunnable instanceof ComposableRunnable);
  }

  @Test
  public void composeRunnableArray() {
    Runnable mockRunnableZero = mock(Runnable.class, "zero");
    Runnable mockRunnableOne = mock(Runnable.class, "one");
    Runnable mockRunnableTwo = mock(Runnable.class, "two");
    Runnable mockRunnableThree = mock(Runnable.class, "three");

    assertNull(ComposableRunnable.compose((Runnable[]) null));
    assertSame(mockRunnableZero, ComposableRunnable.compose(mockRunnableZero));

    Runnable composedRunnable = ComposableRunnable.compose(mockRunnableZero, mockRunnableOne, mockRunnableTwo,
      mockRunnableThree);

    assertTrue(composedRunnable instanceof ComposableRunnable);
  }

  @Test
  public void runOne() {
    Runnable mockRunnable = mock(Runnable.class);
    Runnable actualRunnable = ComposableRunnable.compose(mockRunnable);

    assertSame(mockRunnable, actualRunnable);

    actualRunnable.run();

    verify(mockRunnable, times(1)).run();
  }

  @Test
  public void runTwo() {
    Runnable mockRunnableLeft = mock(Runnable.class, "left");
    Runnable mockRunnableRight = mock(Runnable.class, "right");
    Runnable composedRunnable = ComposableRunnable.compose(mockRunnableLeft, mockRunnableRight);

    assertTrue(composedRunnable instanceof ComposableRunnable);

    composedRunnable.run();

    verify(mockRunnableLeft, times(1)).run();
    verify(mockRunnableRight, times(1)).run();
  }

  @Test
  public void runFour() {
    Runnable mockRunnableZero = mock(Runnable.class, "zero");
    Runnable mockRunnableOne = mock(Runnable.class, "one");
    Runnable mockRunnableTwo = mock(Runnable.class, "two");
    Runnable mockRunnableThree = mock(Runnable.class, "three");

    Runnable composedRunnable = ComposableRunnable.compose(mockRunnableZero, mockRunnableOne, mockRunnableTwo,
      mockRunnableThree);

    assertTrue(composedRunnable instanceof ComposableRunnable);

    composedRunnable.run();

    verify(mockRunnableZero, times(1)).run();
    verify(mockRunnableOne, times(1)).run();
    verify(mockRunnableTwo, times(1)).run();
    verify(mockRunnableThree, times(1)).run();
  }
}
