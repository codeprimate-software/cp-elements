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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatNullPointerException;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.NumberUtils;
import org.cp.elements.util.ArrayUtils;

/**
 * Unit Tests for {@link StreamUtils}.
 *
 * @author John J. Blum
 * @see java.lang.Iterable
 * @see java.util.Enumeration
 * @see java.util.Iterator
 * @see java.util.stream.Stream
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.stream.StreamUtils
 * @since 1.0.0
 */
public class StreamUtilsUnitTests {

  @Test
  public void emptyStreamIsCorrect() {

    Stream<?> empty = StreamUtils.empty();

    assertThat(empty).isNotNull();
    assertThat(empty).isEmpty();
  }

  @Test
  public void nullSafeStreamWithNonNullStream() {

    Stream<?> mockStream = mock(Stream.class);

    assertThat(StreamUtils.nullSafeStream(mockStream)).isSameAs(mockStream);

    verifyNoInteractions(mockStream);
  }

  @Test
  public void nullSafeStreamWithNullStreamIsNullSafe() {

    Stream<?> stream = StreamUtils.nullSafeStream(null);

    assertThat(stream).isNotNull();
    assertThat(stream).isEmpty();
  }

  @Test
  void nullSafeStreamableWithNonNullStreamable() {

    Streamable<?> mockStreamable = mock(Streamable.class);

    assertThat(StreamUtils.nullSafeStreamable(mockStreamable)).isSameAs(mockStreamable);

    verifyNoInteractions(mockStreamable);
  }

  @Test
  void nullSafeStreamableWithNullStreamableIsNullSafe() {

    Streamable<?> streamable = StreamUtils.nullSafeStreamable(null);

    assertThat(streamable).isNotNull();
    assertThat(streamable.stream()).isEmpty();
  }

  @Test
  public void streamFromArray() {

    String[] array = { "test", "testing", "tested" };

    Stream<String> stream = StreamUtils.stream(array);

    assertThat(stream).isNotNull();

    List<String> list = stream.collect(Collectors.toList());

    assertThat(list).isNotNull();
    assertThat(list).hasSize(array.length);
    assertThat(list).containsExactly(array);
  }

  @Test
  public void streamFromSingleElementArray() {

    Stream<String> stream = StreamUtils.stream("test");

    assertThat(stream).isNotNull();

    List<String> list = stream.collect(Collectors.toList());

    assertThat(list).isNotNull();
    assertThat(list).hasSize(1);
    assertThat(list).containsExactly("test");
  }

  @Test
  public void streamFromEmptyArray() {

    Stream<Object> stream = StreamUtils.stream();

    assertThat(stream).isNotNull();
    assertThat(stream).isEmpty();
  }

  @Test
  @SuppressWarnings("all")
  public void streamFromNullArray() {

    assertThatNullPointerException()
      .isThrownBy(() -> StreamUtils.stream((Object[]) null))
      .withNoCause();
  }

  @Test
  public void streamFromIterable() {

    Stream<Integer> stream = StreamUtils.stream(ArrayUtils.asIterable(1, 2, 3));

    assertThat(stream).isNotNull();

    Object[] array = stream.toArray();

    assertThat(array).isNotNull();
    assertThat(array).hasSize(3);

    for (int index = 0; index < array.length; index++) {
      assertThat(array[index]).isEqualTo(index + 1);
    }
  }

  @Test
  public void streamFromSingleElementIterable() {

    Stream<Integer> stream = StreamUtils.stream(ArrayUtils.asIterable(1));

    assertThat(stream).isNotNull();

    Object[] array = stream.toArray();

    assertThat(array).isNotNull();
    assertThat(array).hasSize(1);
    assertThat(array).containsExactly(1);
  }

  @Test
  public void streamFromEmptyIterable() {

    Stream<Object> stream = StreamUtils.stream(ArrayUtils.asIterable());

    assertThat(stream).isNotNull();
    assertThat(stream).isEmpty();
  }

  @Test
  public void streamFromNullIterable() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> StreamUtils.stream((Iterable<?>) null))
      .withMessage("Iterable is required")
      .withNoCause();
  }

  @Test
  public void streamFromIterator() {

    Iterator<Integer> iterator = Arrays.asList(1, 2, 3).iterator();

    Stream<Integer> stream = StreamUtils.stream(iterator);

    assertThat(stream).isNotNull();

    Collection<Integer> collection = stream.collect(Collectors.toList());

    assertThat(collection).isNotNull();
    assertThat(collection).hasSize(3);
    assertThat(collection).containsExactly(1, 2, 3);
  }

  @Test
  public void streamFromSingleElementIterator() {

    Iterator<Integer> iterator = Collections.singleton(1).iterator();

    Stream<Integer> stream = StreamUtils.stream(iterator);

    assertThat(stream).isNotNull();

    Collection<Integer> collection = stream.collect(Collectors.toSet());

    assertThat(collection).isNotNull();
    assertThat(collection).hasSize(1);
    assertThat(collection).containsExactly(1);
  }

  @Test
  public void streamFromEmptyIterator() {

    Stream<?> stream = StreamUtils.stream(Collections.emptyIterator());

    assertThat(stream).isNotNull();

    Collection<?> collection = stream.collect(Collectors.toSet());

    assertThat(collection).isNotNull();
    assertThat(collection).isEmpty();
  }

  @Test
  public void streamFromNullIterator() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> StreamUtils.stream((Iterator<?>) null))
      .withMessage("Iterator is required")
      .withNoCause();
  }

  @Test
  public void streamFromEnumeration() {

    Enumeration<Integer> enumeration = ArrayUtils.asEnumeration(1, 2, 3);

    Stream<Integer> stream = StreamUtils.stream(enumeration);

    assertThat(stream).isNotNull();

    Collection<Integer> collection = stream.collect(Collectors.toList());

    assertThat(collection).isNotNull();
    assertThat(collection).hasSize(3);
    assertThat(collection).containsExactly(1, 2, 3);
  }

  @Test
  public void streamFromSingleElementEnumeration() {

    Enumeration<Integer> enumeration = ArrayUtils.asEnumeration(1);

    Stream<Integer> stream = StreamUtils.stream(enumeration);

    assertThat(stream).isNotNull();

    Collection<Integer> collection = stream.collect(Collectors.toList());

    assertThat(collection).isNotNull();
    assertThat(collection).hasSize(1);
    assertThat(collection).containsExactly(1);
  }

  @Test
  public void streamFromEmptyEnumeration() {

    Stream<?> stream = StreamUtils.stream(Collections.emptyEnumeration());

    assertThat(stream).isNotNull();

    Collection<?> collection = stream.collect(Collectors.toList());

    assertThat(collection).isNotNull();
    assertThat(collection).isEmpty();
  }

  @Test
  public void streamFromNullEnumeration() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> StreamUtils.stream((Enumeration<?>) null))
      .withMessage("Enumeration is required")
      .withNoCause();
  }

  @Test
  public void streamFromCollection() {

    List<Integer> list = Arrays.asList(1, 2, 3);

    Stream<Integer> stream = StreamUtils.stream(list);

    assertThat(stream).isNotNull();

    List<Integer> result = stream.collect(Collectors.toList());

    assertThat(result).isNotNull();
    assertThat(result).hasSize(list.size());
    assertThat(result).containsExactly(1, 2, 3);
  }

  @Test
  public void fromStreamToUnfilteredNonTransformedList() {

    Stream<Integer> stream = Arrays.stream(ArrayUtils.asArray(1, 2, 3));

    List<Integer> list = StreamUtils.toList(stream);

    assertThat(list).isNotNull();
    assertThat(list).hasSize(3);
    assertThat(list).containsExactly(1, 2, 3);
  }

  @Test
  public void fromStreamToTransformedList() {

    Stream<Integer> stream = Arrays.stream(ArrayUtils.asArray(1, 2, 3));

    List<Numbers> list = StreamUtils.toList(stream, Numbers::valueOfNumber);

    assertThat(list).isNotNull();
    assertThat(list).hasSize(3);
    assertThat(list).containsExactly(Numbers.ONE, Numbers.TWO, Numbers.THREE);
  }

  @Test
  public void fromStreamToFilteredList() {

    Stream<Integer> stream = Arrays.stream(ArrayUtils.asArray(1, 2, 3));

    Predicate<Integer> isEven = NumberUtils::isEven;

    List<Integer> list = StreamUtils.toList(stream, isEven);

    assertThat(list).isNotNull();
    assertThat(list).hasSize(1);
    assertThat(list).containsExactly(2);
  }

  @Test
  public void fromStreamToFilteredTransformedList() {

    Stream<Integer> stream = Arrays.stream(ArrayUtils.asArray(1, 2, 3));

    Predicate<Integer> isEven = NumberUtils::isEven;

    Function<Integer, Numbers> toNumbers = Numbers::valueOfNumber;

    List<Numbers> list = StreamUtils.toList(stream, isEven, toNumbers);

    assertThat(list).isNotNull();
    assertThat(list).hasSize(1);
    assertThat(list).containsExactly(Numbers.TWO);
  }

  @Test
  public void toListFromNullStream() {

    List<?> list = StreamUtils.toList(null);

    assertThat(list).isNotNull();
    assertThat(list).isEmpty();
  }

  @Test
  public void toListWithNullPredicateAndNullFunctionIsNullSafe() {

    Stream<Integer> stream = Arrays.stream(ArrayUtils.asArray(1, 2, 3));

    List<Integer> list = StreamUtils.toList(stream, null, null);

    assertThat(list).isNotNull();
    assertThat(list).hasSize(3);
    assertThat(list).containsExactly(1, 2, 3);
  }

  @Test
  public void fromStreamToUnfilteredNonTransformedSet() {

    Stream<Integer> stream = Arrays.stream(ArrayUtils.asArray(1, 2, 3));

    Set<Integer> set = StreamUtils.toSet(stream);

    assertThat(set).isNotNull();
    assertThat(set).hasSize(3);
    assertThat(set).containsExactlyInAnyOrder(1, 2, 3);
  }

  @Test
  public void fromStreamToTransformedSet() {

    Stream<Integer> stream = Arrays.stream(ArrayUtils.asArray(1, 2, 3));

    Set<Numbers> set = StreamUtils.toSet(stream, Numbers::valueOfNumber);

    assertThat(set).isNotNull();
    assertThat(set).hasSize(3);
    assertThat(set).containsExactlyInAnyOrder(Numbers.ONE, Numbers.TWO, Numbers.THREE);
  }

  @Test
  public void fromStreamToFilteredSet() {

    Stream<Integer> stream = Arrays.stream(ArrayUtils.asArray(1, 2, 3));

    Predicate<Integer> isOdd = NumberUtils::isOdd;

    Set<Integer> set = StreamUtils.toSet(stream, isOdd);

    assertThat(set).isNotNull();
    assertThat(set).hasSize(2);
    assertThat(set).containsExactlyInAnyOrder(1, 3);
  }

  @Test
  public void fromStreamToFilteredTransformedSet() {

    Stream<Integer> stream = Arrays.stream(ArrayUtils.asArray(1, 2, 3));

    Predicate<Integer> isOdd = NumberUtils::isOdd;

    Function<Integer, Numbers> toNumbers = Numbers::valueOfNumber;

    Set<Numbers> set = StreamUtils.toSet(stream, isOdd, toNumbers);

    assertThat(set).isNotNull();
    assertThat(set).hasSize(2);
    assertThat(set).containsExactlyInAnyOrder(Numbers.ONE, Numbers.THREE);
  }

  @Test
  public void toSetFromNullStreamIsNullSafe() {

    Set<?> set = StreamUtils.toSet(null);

    assertThat(set).isNotNull();
    assertThat(set).isEmpty();
  }

  @Test
  public void toSetWithNullPredicateAndNullFunctionIsNullSafe() {

    Stream<Integer> stream = Arrays.stream(ArrayUtils.asArray(1, 2, 3));

    Set<Integer> set = StreamUtils.toSet(stream, null, null);

    assertThat(set).isNotNull();
    assertThat(set).hasSize(3);
    assertThat(set).containsExactlyInAnyOrder(1, 2, 3);
  }

  enum Numbers {

    ONE(1), TWO(2), THREE(3);

    static Numbers valueOfNumber(int number) {

      return Arrays.stream(Numbers.values())
        .filter(numberEnum -> numberEnum.getNumber() == number)
        .findFirst()
        .orElse(null);
    }

    private final int number;

    Numbers(int number) {
      this.number = number;
    }

    int getNumber() {
      return this.number;
    }
  }
}
