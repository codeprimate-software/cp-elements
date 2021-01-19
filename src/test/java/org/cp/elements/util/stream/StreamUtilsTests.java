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
import static org.cp.elements.util.ArrayUtils.asIterable;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.cp.elements.test.TestUtils;
import org.junit.Test;

/**
 * Unit Tests for {@link StreamUtils}.
 *
 * @author John J. Blum
 * @see java.util.stream.Stream
 * @see org.junit.Test
 * @see org.cp.elements.util.stream.StreamUtils
 * @since 1.0.0
 */
@SuppressWarnings("unchecked")
public class StreamUtilsTests {

  @Test
  public void streamFromArray() {

    String[] array = { "test", "testing", "tested" };

    Stream<String> stream = StreamUtils.stream(array);

    assertThat(stream).isNotNull();

    List<String> list = stream.collect(Collectors.toList());

    assertThat(list).isNotNull();
    assertThat(list.size()).isEqualTo(array.length);
    assertThat(list.containsAll(Arrays.asList(array))).isTrue();
  }

  @Test
  public void streamFromSingleElementArray() {

    Stream<String> stream = StreamUtils.stream("test");

    assertThat(stream).isNotNull();

    List<String> list = stream.collect(Collectors.toList());

    assertThat(list).isNotNull();
    assertThat(list.size()).isEqualTo(1);
    assertThat(list.get(0)).isEqualTo("test");
  }

  @Test
  public void streamFromEmptyArray() {

    Stream<Object> stream = StreamUtils.stream();

    assertThat(stream).isNotNull();
    assertThat(stream.count()).isEqualTo(0L);
  }

  @Test(expected = NullPointerException.class)
  public void streamFromNullArray() {
    StreamUtils.stream((Object[]) null);
  }

  @Test
  public void streamFromIterable() {

    Stream<Integer> stream = StreamUtils.stream(asIterable(1, 2, 3));

    assertThat(stream).isNotNull();

    Object[] array = stream.toArray();

    assertThat(array).isNotNull();
    assertThat(array.length).isEqualTo(3);

    for (int index = 0; index < array.length; index++) {
      assertThat(array[index]).isEqualTo(index + 1);
    }
  }

  @Test
  public void streamFromSingleElementIterable() {

    Stream<Integer> stream = StreamUtils.stream(asIterable(1));

    assertThat(stream).isNotNull();

    Object[] array = stream.toArray();

    assertThat(array).isNotNull();
    assertThat(array.length).isEqualTo(1);
    assertThat(array[0]).isEqualTo(1);
  }

  @Test
  public void streamFromEmptyIterable() {

    Stream<Object> stream = StreamUtils.stream(asIterable());

    assertThat(stream).isNotNull();
    assertThat(stream.count()).isEqualTo(0L);
  }

  @Test(expected = IllegalArgumentException.class)
  public void streamFromNullIterable() {
    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> StreamUtils.stream((Iterable<?>) null),
      () -> "Iterable cannot be null");
  }

  @Test
  public void streamFromCollection() {

    List<Integer> list = Arrays.asList(1, 2, 3);
    Stream<Integer> stream = StreamUtils.stream(list);

    assertThat(stream).isNotNull();

    List<Integer> result = stream.collect(Collectors.toList());

    assertThat(result).isNotNull();
    assertThat(result.size()).isEqualTo(list.size());
    assertThat(result.containsAll(list)).isTrue();
  }
}
