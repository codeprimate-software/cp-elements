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
package org.cp.elements.util.stream;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Arrays;
import java.util.stream.Stream;

import org.junit.Test;

/**
 * Unit Tests for {@link Streamable}.
 *
 * @author John Blum
 * @see java.util.stream.Stream
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.stream.Streamable
 * @since 1.0.0
 */
public class StreamableUnitTests {

  @Test
  public void emptyStream() {

    Streamable<?> streamable = Streamable.empty();

    assertThat(streamable).isNotNull();

    Stream<?> stream = streamable.stream();

    assertThat(stream).isNotNull();
    assertThat(stream).isEmpty();
  }

  @Test
  public void fromArray() {

    Streamable<Integer> streamable = Streamable.from(1, 2, 3);

    assertThat(streamable).isNotNull();

    Stream<Integer> stream = streamable.stream();

    assertThat(stream).isNotNull();
    assertThat(stream).containsExactly(1, 2, 3);
  }

  @Test
  public void fromNoElementArray() {

    Streamable<Integer> streamable = Streamable.from();

    assertThat(streamable).isNotNull();

    Stream<Integer> stream = streamable.stream();

    assertThat(stream).isNotNull();
    assertThat(stream).isEmpty();
  }

  @Test
  public void fromSingleElementArray() {

    Streamable<Integer> streamable = Streamable.from(1);

    assertThat(streamable).isNotNull();

    Stream<Integer> stream = streamable.stream();

    assertThat(stream).isNotNull();
    assertThat(stream).containsExactly(1);
  }

  @Test
  public void fromIterable() {

    Streamable<Integer> streamable = Streamable.from(Arrays.asList(1, 2, 3));

    assertThat(streamable).isNotNull();

    Stream<Integer> stream = streamable.stream();

    assertThat(stream).isNotNull();
    assertThat(stream).containsExactly(1, 2, 3);
  }

  @Test
  public void fromNullIterable() {

    Streamable<?> streamable = Streamable.from((Iterable<?>) null);

    assertThat(streamable).isNotNull();

    Stream<?> stream = streamable.stream();

    assertThat(stream).isNotNull();
    assertThat(stream).isEmpty();
  }
}
