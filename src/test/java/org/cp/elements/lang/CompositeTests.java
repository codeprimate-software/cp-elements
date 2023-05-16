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

package org.cp.elements.lang;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Arrays;
import java.util.Collections;
import java.util.stream.StreamSupport;

import org.cp.elements.util.ArrayUtils;
import org.junit.jupiter.api.Test;

/**
 * Unit tests for {@link Composite}.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Composite
 * @since 1.0.0
 */
public class CompositeTests {

  private Doable mockDoable(String name) {
    return mock(Doable.class, name);
  }

  @Test
  public void composeTwo() {

    Doable mockDoableOne = mockDoable("one");
    Doable mockDoableTwo = mockDoable("two");
    Doable composite = CompositeDoable.builder().compose(mockDoableOne, mockDoableTwo);

    assertThat(composite).isNotNull();

    composite.doIt();

    verify(mockDoableOne, times(1)).doIt();
    verify(mockDoableTwo, times(1)).doIt();
  }

  @Test
  public void composeArray() {

    Doable[] mockDoables = { mockDoable("one"), mockDoable("two"), mockDoable("three") };

    Doable composite = CompositeDoable.builder().compose(mockDoables);

    assertThat(composite).isNotNull();

    composite.doIt();

    Arrays.stream(mockDoables).forEach(mockDoable ->
      verify(mockDoable, times(1)).doIt());
  }

  @Test
  public void composeSingleElementArray() {

    Doable[] mockDoables = { mockDoable("one") };

    Doable composite = CompositeDoable.builder().compose(mockDoables);

    assertThat(composite).isNotNull();
    assertThat(composite).isEqualTo(mockDoables[0]);

    composite.doIt();

    Arrays.stream(mockDoables).forEach(mockDoable ->
      verify(mockDoable, times(1)).doIt());
  }

  @Test
  public void composeNullArray() {
    assertThat(CompositeDoable.builder().compose((Doable[]) null)).isNull();
  }

  @Test
  public void composeEmptyArray() {
    assertThat(CompositeDoable.builder().compose()).isNull();
  }

  @Test
  public void composeIterable() {

    Iterable<Doable> mockDoables =
      ArrayUtils.asIterable(mockDoable("one"), mockDoable("two"), mockDoable("three"));

    Doable composite = CompositeDoable.builder().compose(mockDoables);

    assertThat(composite).isNotNull();

    composite.doIt();

    StreamSupport.stream(mockDoables.spliterator(), false).forEach(mockDoable ->
      verify(mockDoable, times(1)).doIt());
  }

  @Test
  public void composeSingleElementIterable() {

    Iterable<Doable> mockDoables = ArrayUtils.asIterable(mockDoable("one"));

    Doable composite = CompositeDoable.builder().compose(mockDoables);

    assertThat(composite).isNotNull();
    assertThat(composite).isEqualTo(mockDoables.iterator().next());

    composite.doIt();

    StreamSupport.stream(mockDoables.spliterator(), false).forEach(mockDoable ->
      verify(mockDoable, times(1)).doIt());
  }

  @Test
  public void composeNullIterable() {
    assertThat(CompositeDoable.builder().compose((Iterable<Doable>) null)).isNull();
  }

  @Test
  public void composeEmptyIterable() {
    assertThat(CompositeDoable.builder().compose(Collections::emptyIterator)).isNull();
  }

  @FunctionalInterface
  interface Doable {
    void doIt();
  }

  static class CompositeDoable implements Composite<Doable>, Doable {

    private static final CompositeDoable INSTANCE = new CompositeDoable() {

      @Override public void doIt() {
        throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
      }
    };

    private final Doable one;
    private final Doable two;

    static CompositeDoable builder() {
      return INSTANCE;
    }

    private CompositeDoable() {
      one = two = () -> {};
    }

    CompositeDoable(Doable one, Doable two) {
      this.one = one;
      this.two = two;
    }

    @Override
    public Doable compose(Doable one, Doable two) {
      return one == null ? two : (two == null ? one : new CompositeDoable(one, two));
    }

    @Override
    @SuppressWarnings("all")
    public void doIt() {
      one.doIt();
      two.doIt();
    }
  }
}
