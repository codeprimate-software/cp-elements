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
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.cp.elements.util.ArrayUtils;
import org.junit.jupiter.api.Test;

/**
 * Unit tests for {@link ComposableRunnable}.
 *
 * @author John J. Blum
 * @see java.lang.Runnable
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.support.ComposableRunnable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ComposableRunnableTests {

  private Runnable mockRunnable(String... name) {
    return mock(Runnable.class, ArrayUtils.getFirstElement(name, "MockRunnable"));
  }

  @Test
  public void composeNoRunnables() {
    assertThat(ComposableRunnable.builder().compose(null, null)).isNull();
  }

  @Test
  public void composeSingleRunnable() {

    Runnable mockRunnable = mockRunnable("MockRunnable");

    assertThat(ComposableRunnable.builder().compose(mockRunnable, null)).isSameAs(mockRunnable);
    assertThat(ComposableRunnable.builder().compose(null, mockRunnable)).isSameAs(mockRunnable);
  }

  @Test
  public void composeTwoRunnables() {

    Runnable mockRunnableOne = mockRunnable("one");
    Runnable mockRunnableTwo = mockRunnable("two");
    Runnable composedRunnable = ComposableRunnable.builder().compose(mockRunnableOne, mockRunnableTwo);

    assertThat(composedRunnable).isInstanceOf(ComposableRunnable.class);
    assertThat(((ComposableRunnable) composedRunnable).getRunnableOne()).isEqualTo(mockRunnableOne);
    assertThat(((ComposableRunnable) composedRunnable).getRunnableTwo()).isEqualTo(mockRunnableTwo);
  }

  @Test
  public void composeRunnableArray() {

    Runnable mockRunnableZero = mockRunnable("zero");
    Runnable mockRunnableOne = mockRunnable("one");
    Runnable mockRunnableTwo = mockRunnable("two");
    Runnable mockRunnableThree = mockRunnable("three");

    assertThat(ComposableRunnable.builder().compose((Runnable[]) null)).isNull();
    assertThat(ComposableRunnable.builder().compose(mockRunnableZero)).isSameAs(mockRunnableZero);

    Runnable composedRunnable =
      ComposableRunnable.builder().compose(mockRunnableZero, mockRunnableOne, mockRunnableTwo, mockRunnableThree);

    assertThat(composedRunnable).isInstanceOf(ComposableRunnable.class);
  }

  @Test
  public void composeRunnableIterable() {

    Runnable mockRunnableZero = mockRunnable("zero");
    Runnable mockRunnableOne = mockRunnable("one");
    Runnable mockRunnableTwo = mockRunnable("two");
    Runnable mockRunnableThree = mockRunnable("three");

    assertThat(ComposableRunnable.builder().compose((Iterable<Runnable>) null)).isNull();
    assertThat(ComposableRunnable.builder().compose(ArrayUtils.asIterable(mockRunnableOne))).isSameAs(mockRunnableOne);

    Runnable composedRunnable =
      ComposableRunnable.builder().compose(mockRunnableZero, mockRunnableOne, mockRunnableTwo, mockRunnableThree);

    assertThat(composedRunnable).isInstanceOf(ComposableRunnable.class);
  }

  @Test
  public void runOne() {

    Runnable mockRunnable = mockRunnable("one");
    Runnable actualRunnable = ComposableRunnable.builder().compose(mockRunnable);

    assertThat(actualRunnable).isSameAs(mockRunnable);

    actualRunnable.run();

    verify(mockRunnable, times(1)).run();
  }

  @Test
  public void runTwo() {

    Runnable mockRunnableOne = mockRunnable("left");
    Runnable mockRunnableTwo = mockRunnable("right");
    Runnable composedRunnable = ComposableRunnable.builder().compose(mockRunnableOne, mockRunnableTwo);

    assertThat(composedRunnable).isInstanceOf(ComposableRunnable.class);

    composedRunnable.run();

    verify(mockRunnableOne, times(1)).run();
    verify(mockRunnableTwo, times(1)).run();
  }

  @Test
  public void runFour() {

    Runnable mockRunnableZero = mockRunnable("zero");
    Runnable mockRunnableOne = mockRunnable("one");
    Runnable mockRunnableTwo = mockRunnable("two");
    Runnable mockRunnableThree = mockRunnable("three");

    Runnable composedRunnable =
      ComposableRunnable.builder().compose(mockRunnableZero, mockRunnableOne, mockRunnableTwo, mockRunnableThree);

    assertThat(composedRunnable).isInstanceOf(ComposableRunnable.class);

    composedRunnable.run();

    verify(mockRunnableZero, times(1)).run();
    verify(mockRunnableOne, times(1)).run();
    verify(mockRunnableTwo, times(1)).run();
    verify(mockRunnableThree, times(1)).run();
  }
}
