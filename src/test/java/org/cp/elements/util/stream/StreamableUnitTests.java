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

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link Streamable}.
 *
 * @author John Blum
 * @see java.util.stream.Stream
 * @see org.junit.jupiter.api.Test
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

    Streamable<Integer> streamable = Streamable.from(List.of(1, 2, 3));

    assertThat(streamable).isNotNull();

    Stream<Integer> stream = streamable.stream();

    assertThat(stream).isNotNull();
    assertThat(stream).containsExactly(1, 2, 3);
  }

  @Test
  void fromNoElementIterable() {

    Streamable<Integer> streamable = Streamable.from(Collections::emptyIterator);

    assertThat(streamable).isNotNull();

    Stream<Integer> stream = streamable.stream();

    assertThat(stream).isNotNull();
    assertThat(stream).isEmpty();
  }

  @Test
  public void fromNullIterable() {

    Streamable<?> streamable = Streamable.from((Iterable<?>) null);

    assertThat(streamable).isNotNull();

    Stream<?> stream = streamable.stream();

    assertThat(stream).isNotNull();
    assertThat(stream).isEmpty();
  }

  @Test
  void fromSingleElementIterable() {

    Streamable<Integer> streamable = Streamable.from(Set.of(1));

    assertThat(streamable).isNotNull();

    Stream<Integer> stream = streamable.stream();

    assertThat(stream).isNotNull();
    assertThat(stream).containsExactly(1);
  }

  @Test
  void isEmpty() {

    Streamable<?> streamable = Streamable.from(Collections::emptyIterator);

    assertThat(streamable).isNotNull();
    assertThat(streamable.isEmpty()).isTrue();
    assertThat(streamable.isNotEmpty()).isFalse();
  }

  @Test
  void isNotEmpty() {

    Streamable<?> streamable = Streamable.from(Set.of(1));

    assertThat(streamable).isNotNull();
    assertThat(streamable.isEmpty()).isFalse();
    assertThat(streamable.isNotEmpty()).isTrue();
  }

  @Test
  void filter() {

    Streamable<Integer> streamable = Streamable.from(1, 2, 3, 4, 5);

    assertThat(streamable).isNotNull();

    Streamable<Integer> evens = streamable.filter(integer -> integer % 2 == 0);

    assertThat(evens).isNotNull();
    assertThat(evens).isNotSameAs(streamable);
    assertThat(evens.stream()).containsExactly(2, 4);
  }

  @Test
  void filtersEverything() {

    Streamable<Integer> streamable = Streamable.from(1, 2, 3, 4, 5);

    assertThat(streamable).isNotNull();

    Streamable<Integer> filteredStreamable = streamable.filter(integer -> false);

    assertThat(filteredStreamable).isNotNull();
    assertThat(filteredStreamable).isNotSameAs(streamable);
    assertThat(filteredStreamable.stream()).isEmpty();
  }

  @Test
  void filtersNothing() {

    Streamable<Integer> streamable = Streamable.from(1, 2, 3, 4, 5);

    assertThat(streamable).isNotNull();

    Streamable<Integer> evens = streamable.filter(integer -> true);

    assertThat(evens).isNotNull();
    assertThat(evens).isNotSameAs(streamable);
    assertThat(evens.stream()).containsExactly(1, 2, 3, 4, 5);
  }

  @Test
  void findsNone() {

    Streamable<Integer> streamable = Streamable.from(1, 2, 3, 4, 5);

    assertThat(streamable).isNotNull();

    Optional<Integer> element = streamable.find(integer -> integer == 12);

    assertThat(element).isNotNull();
    assertThat(element).isNotPresent();
  }

  @Test
  void findsOne() {

    Streamable<Integer> streamable = Streamable.from(1, 2, 3, 4, 5);

    assertThat(streamable).isNotNull();

    Optional<Integer> element = streamable.find(integer -> integer == 2);

    assertThat(element).isNotNull();
    assertThat(element).isPresent();
    assertThat(element.orElse(null)).isEqualTo(2);
  }

  @Test
  void sizeIsOne() {

    Streamable<?> streamable = Streamable.from(1);

    assertThat(streamable).isNotNull();
    assertThat(streamable.size()).isOne();
  }

  @Test
  void sizeIsTwo() {

    Streamable<?> streamable = Streamable.from(1, 2);

    assertThat(streamable).isNotNull();
    assertThat(streamable.size()).isEqualTo(2);
  }

  @Test
  void sizeIsZero() {

    Streamable<?> streamable = Streamable.from();

    assertThat(streamable).isNotNull();
    assertThat(streamable.size()).isZero();

    streamable = Streamable.from((Iterable<?>) null);

    assertThat(streamable).isNotNull();
    assertThat(streamable.size()).isZero();
  }

  @Test
  void toList() {

    Streamable<Integer> streamable = Streamable.from(1, 2, 3);

    assertThat(streamable).isNotNull();

    List<Integer> list = streamable.toList();

    assertThat(list).isNotNull();
    assertThat(list).containsExactly(1, 2, 3);
  }

  @Test
  void toListFromNullIterable() {

    Streamable<Integer> streamable = Streamable.from((Iterable<Integer>) null);

    assertThat(streamable).isNotNull();

    List<Integer> list = streamable.toList();

    assertThat(list).isNotNull();
    assertThat(list).isEmpty();
  }

  @Test
  void toSet() {

    Streamable<Integer> streamable = Streamable.from(1, 2, 3);

    assertThat(streamable).isNotNull();

    Set<Integer> list = streamable.toSet();

    assertThat(list).isNotNull();
    assertThat(list).containsExactlyInAnyOrder(1, 2, 3);
  }

  @Test
  void toSetFromNullIterable() {

    Streamable<Integer> streamable = Streamable.from((Iterable<Integer>) null);

    assertThat(streamable).isNotNull();

    Set<Integer> list = streamable.toSet();

    assertThat(list).isNotNull();
    assertThat(list).isEmpty();
  }
}
