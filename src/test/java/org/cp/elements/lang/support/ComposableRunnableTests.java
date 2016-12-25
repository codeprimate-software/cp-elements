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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.cp.elements.util.ArrayUtils;
import org.junit.Test;

/**
 * Unit tests for {@link ComposableRunnable}.
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

  protected Runnable mockRunnable(String... name) {
    return mock(Runnable.class, getFirst(name, "MockRunnable"));
  }

  @Test
  public void composeTwoRunnables() {
    Runnable mockRunnableLeft = mockRunnable("left");
    Runnable mockRunnableRight = mockRunnable("right");

    assertThat(ComposableRunnable.getInstance().compose(null, null)).isNull();
    assertThat(ComposableRunnable.getInstance().compose(mockRunnableLeft, null)).isSameAs(mockRunnableLeft);
    assertThat(ComposableRunnable.getInstance().compose(null, mockRunnableRight)).isSameAs(mockRunnableRight);

    Runnable composedRunnable = ComposableRunnable.getInstance().compose(mockRunnableLeft, mockRunnableRight);

    assertThat(composedRunnable).isInstanceOf(ComposableRunnable.class);
  }

  @Test
  public void composeRunnableArray() {
    Runnable mockRunnableZero = mockRunnable("zero");
    Runnable mockRunnableOne = mockRunnable("one");
    Runnable mockRunnableTwo = mockRunnable("two");
    Runnable mockRunnableThree = mockRunnable("three");

    assertThat(ComposableRunnable.getInstance().compose((Runnable[]) null)).isNull();
    assertThat(ComposableRunnable.getInstance().compose(mockRunnableZero)).isSameAs(mockRunnableZero);

    Runnable composedRunnable = ComposableRunnable.getInstance().compose(mockRunnableZero, mockRunnableOne,
      mockRunnableTwo, mockRunnableThree);

    assertThat(composedRunnable).isInstanceOf(ComposableRunnable.class);
  }

  @Test
  public void composeIterableRunnable() {
    Runnable mockRunnableZero = mockRunnable("zero");
    Runnable mockRunnableOne = mockRunnable("one");
    Runnable mockRunnableTwo = mockRunnable("two");
    Runnable mockRunnableThree = mockRunnable("three");

    assertThat(ComposableRunnable.getInstance().compose((Iterable<Runnable>) null)).isNull();
    assertThat(ComposableRunnable.getInstance().compose(ArrayUtils.iterable(mockRunnableOne))).isSameAs(mockRunnableOne);

    Runnable composedRunnable = ComposableRunnable.getInstance().compose(mockRunnableZero, mockRunnableOne,
      mockRunnableTwo, mockRunnableThree);

    assertThat(composedRunnable).isInstanceOf(ComposableRunnable.class);
  }

  @Test
  public void runOne() {
    Runnable mockRunnable = mockRunnable("one");
    Runnable actualRunnable = ComposableRunnable.getInstance().compose(mockRunnable);

    assertThat(actualRunnable).isSameAs(mockRunnable);

    actualRunnable.run();

    verify(mockRunnable, times(1)).run();
  }

  @Test
  public void runTwo() {
    Runnable mockRunnableLeft = mockRunnable("left");
    Runnable mockRunnableRight = mockRunnable("right");
    Runnable composedRunnable = ComposableRunnable.getInstance().compose(mockRunnableLeft, mockRunnableRight);

    assertThat(composedRunnable).isInstanceOf(ComposableRunnable.class);

    composedRunnable.run();

    verify(mockRunnableLeft, times(1)).run();
    verify(mockRunnableRight, times(1)).run();
  }

  @Test
  public void runFour() {
    Runnable mockRunnableZero = mockRunnable("zero");
    Runnable mockRunnableOne = mockRunnable("one");
    Runnable mockRunnableTwo = mockRunnable("two");
    Runnable mockRunnableThree = mockRunnable("three");

    Runnable composedRunnable = ComposableRunnable.getInstance().compose(mockRunnableZero, mockRunnableOne,
      mockRunnableTwo, mockRunnableThree);

    assertThat(composedRunnable).isInstanceOf(ComposableRunnable.class);

    composedRunnable.run();

    verify(mockRunnableZero, times(1)).run();
    verify(mockRunnableOne, times(1)).run();
    verify(mockRunnableTwo, times(1)).run();
    verify(mockRunnableThree, times(1)).run();
  }
}
