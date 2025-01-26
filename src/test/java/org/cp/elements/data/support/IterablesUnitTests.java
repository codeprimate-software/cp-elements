/*
 * Copyright 2017-Present Author or Authors.
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
package org.cp.elements.data.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.caching.Cache;
import org.cp.elements.data.struct.tabular.Row;
import org.cp.elements.data.struct.tabular.Table;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.MapBuilder;
import org.cp.elements.util.stream.StreamUtils;

/**
 * Unit Tests for {@link Iterables}.
 *
 * @author John Blum
 * @see org.cp.elements.data.support.Iterables
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
public class IterablesUnitTests {

  @SuppressWarnings("unchecked")
  private <T> Cache<?, T> mockCache(String name, T... values) {

    Cache.Entry<?, T>[] entries = Arrays.stream(values)
      .map(this::mockCacheEntry)
      .toList()
      .toArray(new Cache.Entry[0]);

    return mockCache(name, entries);
  }

  @SuppressWarnings("unchecked")
  private <T> Cache<?, T> mockCache(String name, Cache.Entry<?, T>... entries) {

    Cache<?, T> mockCache = mock(Cache.class, name);

    Iterable<Cache.Entry<?, T>> iterableEntries = ArrayUtils.asIterable(entries);

    doReturn(iterableEntries.spliterator()).when(mockCache).spliterator();
    doCallRealMethod().when(mockCache).stream();

    return mockCache;
  }

  @SuppressWarnings("unchecked")
  private <T> Cache.Entry<?, T> mockCacheEntry(T value) {
    Cache.Entry<?, T> mockCacheEntry = mock(Cache.Entry.class);
    doReturn(value).when(mockCacheEntry).getValue();
    return mockCacheEntry;
  }

  private Row mockRow(int index) {
    Row mockRow = mock(Row.class);
    doReturn(index).when(mockRow).index();
    return mockRow;
  }

  private Table mockTable(String name, int rowCount) {

    Table mockTable = mock(Table.class, name);
    List<Row> rows = IntStream.range(0, rowCount).mapToObj(this::mockRow).toList();

    doReturn(rows.iterator()).when(mockTable).iterator();
    doCallRealMethod().when(mockTable).stream();

    return mockTable;
  }

  @Test
  void emptyIterable() {

    Iterable<?> iterable = Iterables.empty();

    assertThat(iterable).isNotNull();
    assertThat(iterable).isEmpty();
  }

  @Test
  void iterableFromArray() {

    Iterable<Integer> iterable = Iterables.from(1, 2, 3);

    assertThat(iterable).isNotNull();
    assertThat(iterable).containsExactly(1, 2, 3);
  }

  @Test
  void iterableFromSingleElementArray() {

    Iterable<Integer> iterable = Iterables.from(1);

    assertThat(iterable).isNotNull();
    assertThat(iterable).containsExactly(1);
  }

  @Test
  void iterableFromEmptyArray() {

    Iterable<Integer> iterable = Iterables.from();

    assertThat(iterable).isNotNull();
    assertThat(iterable).isEmpty();
  }

  @Test
  void iterableFromNullArray() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Iterables.from((Object[]) null))
      .withMessage("Array is required")
      .withNoCause();
  }

  @Test
  void iterableFromCache() {

    Cache<?, Integer> mockCache = mockCache("MockCache", 1, 2, 3);

    Iterable<Integer> iterable = Iterables.from(mockCache);

    assertThat(iterable).isNotNull();
    assertThat(iterable).containsExactly(1, 2, 3);

    verify(mockCache, times(1)).stream();
    verify(mockCache, times(1)).spliterator();
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  void iterableFromSingleEntryCache() {

    Cache<?, Integer> mockCache = mockCache("MockCache", 1);

    Iterable<Integer> iterable = Iterables.from(mockCache);

    assertThat(iterable).isNotNull();
    assertThat(iterable).containsExactly(1);

    verify(mockCache, times(1)).stream();
    verify(mockCache, times(1)).spliterator();
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  @SuppressWarnings("unchecked")
  void iterableFromEmptyCache() {

    Cache<?, Integer> mockCache = mockCache("MockCache");

    Iterable<Integer> iterable = Iterables.from(mockCache);

    assertThat(iterable).isNotNull();
    assertThat(iterable).isEmpty();

    verify(mockCache, times(1)).stream();
    verify(mockCache, times(1)).spliterator();
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  void iterableFromNullCache() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Iterables.from((Cache<?, ?>) null))
      .withMessage("Cache is required")
      .withNoCause();
  }

  @Test
  void iterableFromMap() {

    Map<?, Integer> map = MapBuilder.<Object, Integer>newHashMap()
      .put("A", 1)
      .put("B", 2)
      .put("C", 3)
      .build();

    Iterable<Integer> iterable = Iterables.from(map);

    assertThat(iterable).isNotNull();
    assertThat(iterable).containsExactlyInAnyOrder(1, 2, 3);
  }

  @Test
  void iterableFromSingletonMap() {

    Iterable<Integer> iterable = Iterables.from(Collections.singletonMap("A", 1));

    assertThat(iterable).isNotNull();
    assertThat(iterable).containsExactly(1);
  }

  @Test
  void iterableFromEmptyMap() {

    Iterable<Integer> iterable = Iterables.from(Collections.emptyMap());

    assertThat(iterable).isNotNull();
    assertThat(iterable).isEmpty();
  }

  @Test
  void iterableFromNullMap() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Iterables.from((Map<?, ?>) null))
      .withMessage("Map is required")
      .withNoCause();
  }

  @Test
  void iterableFromTable() {

    Table mockTable = mockTable("MockTable", 3);

    Iterable<Row> iterable = Iterables.from(mockTable);

    assertThat(iterable).isNotNull();
    assertThat(StreamUtils.stream(iterable).map(Row::index).toList()).containsExactly(0, 1, 2);
  }

  @Test
  void iterableFromSingleRowTable() {

    Table mockTable = mockTable("MockTable", 1);

    Iterable<Row> iterable = Iterables.from(mockTable);

    assertThat(iterable).isNotNull();
    assertThat(StreamUtils.stream(iterable).map(Row::index).toList()).containsExactly(0);
  }

  @Test
  void iterableFromEmptyTable() {

    Table mockTable = mockTable("MockTable", 0);

    Iterable<Row> iterable = Iterables.from(mockTable);

    assertThat(iterable).isNotNull();
    assertThat(iterable).isEmpty();
  }

  @Test
  void iterableFromNullTable() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Iterables.from((Table) null))
      .withMessage("Table is required")
      .withNoCause();
  }

  @Test
  void nullSafeIterableWithNonNullIterable() {

    Iterable<?> mockIterable = mock(Iterable.class);

    assertThat(Iterables.nullSafeIterable(mockIterable)).isSameAs(mockIterable);

    verifyNoInteractions(mockIterable);
  }

  @Test
  void nullSafeIterableWithNullIterable() {

    Iterable<?> iterable = Iterables.nullSafeIterable(null);

    assertThat(iterable).isNotNull();
    assertThat(iterable).isEmpty();
  }
}
