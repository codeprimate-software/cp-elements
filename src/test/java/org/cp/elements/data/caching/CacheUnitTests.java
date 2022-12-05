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
package org.cp.elements.data.caching;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.withSettings;

import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.Id;
import org.cp.elements.test.annotation.SubjectUnderTest;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.MapBuilder;

import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.mockito.quality.Strictness;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * Unit Tests for {@link Cache}.
 *
 * @author John Blum
 * @see java.util.Map
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.data.caching.AbstractCache
 * @see org.cp.elements.data.caching.Cache
 * @see org.cp.elements.lang.Identifiable
 * @see org.cp.elements.util.MapBuilder
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
@SuppressWarnings({ "rawtypes", "unchecked" })
public class CacheUnitTests {

  @Mock
  @SubjectUnderTest
  private Cache cache;

  @Before
  public void resetMockCache() {
    Mockito.reset(this.cache);
  }

  @SuppressWarnings("unchecked")
  private <KEY extends Comparable<KEY>, VALUE> Cache.Entry<KEY, VALUE> mockCacheEntry(KEY key, VALUE value) {

    Cache.Entry<KEY, VALUE> mockCacheEntry = mock(Cache.Entry.class, withSettings().strictness(Strictness.LENIENT));

    doReturn(key).when(mockCacheEntry).getKey();
    doReturn(value).when(mockCacheEntry).getValue();
    doReturn(mockCacheEntry).when(mockCacheEntry).materialize();

    return mockCacheEntry;
  }

  @SuppressWarnings("unchecked")
  private <ID extends Comparable<ID>> Identifiable<ID> mockIdentifiable(ID id) {

    Identifiable<ID> mockIdentifiable = mock(Identifiable.class);

    doReturn(id).when(mockIdentifiable).getId();

    return mockIdentifiable;
  }

  @Test
  public void getCacheLockReturnsNullByDefault() {

    doCallRealMethod().when(this.cache).getLock();

    assertThat(this.cache.getLock()).isNull();

    verify(this.cache, times(1)).getLock();
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void isEmptyReturnsTrueWhenSizeIsZero() {

    doReturn(0).when(this.cache).size();
    doCallRealMethod().when(this.cache).isEmpty();

    assertThat(this.cache.isEmpty()).isTrue();

    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).isEmpty();
    verify(this.cache, times(1)).size();
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void isEmptyReturnsFalseWhenSizeIsNotZero() {

    doReturn(1).doReturn(-1).when(this.cache).size();
    doCallRealMethod().when(this.cache).isEmpty();

    assertThat(this.cache.isEmpty()).isFalse();
    assertThat(this.cache.isEmpty()).isFalse();

    verify(this.cache, times(2)).getLock();
    verify(this.cache, times(2)).isEmpty();
    verify(this.cache, times(2)).size();
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void clearCache() {

    doReturn(CollectionUtils.asSet("KeyOne", "KeyTwo", "KeyThree")).when(this.cache).keys();
    doCallRealMethod().when(this.cache).clear();

    this.cache.clear();

    verify(this.cache, times(1)).clear();
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).keys();
    verify(this.cache, times(1))
      .evictAll(eq(CollectionUtils.asSet("KeyOne", "KeyTwo", "KeyThree")));

    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void clearEmptyCache() {

    doReturn(Collections.emptySet()).when(this.cache).keys();
    doCallRealMethod().when(this.cache).clear();

    this.cache.clear();

    verify(this.cache, times(1)).clear();
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).keys();
    verify(this.cache, times(1)).evictAll(eq(Collections.emptySet()));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void containsAllWithArrayWhenAllKeysArePresentReturnsTrue() {

    doReturn(true).when(this.cache).contains(any());
    doCallRealMethod().when(this.cache).containsAll(any(Comparable.class));

    assertThat(this.cache.containsAll("KeyOne", "KeyTwo", "KeyThree")).isTrue();

    verify(this.cache, times(1))
      .containsAll(eq("KeyOne"), eq("KeyTwo"), eq("KeyThree"));

    verify(this.cache, times(1)).getLock();

    Arrays.asList("KeyOne", "KeyTwo", "KeyThree").forEach(key ->
      verify(this.cache, times(1)).contains(eq(key)));

    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void containsAllWithArrayWhenJustOneKeyIsNotPresentReturnsFalse() {

    doReturn(true, false, true).when(this.cache).contains(any());
    doCallRealMethod().when(this.cache).containsAll(any(Comparable.class));

    assertThat(this.cache.containsAll("KeyOne", "KeyTwo", "KeyThree")).isFalse();

    verify(this.cache, times(1))
      .containsAll(eq("KeyOne"), eq("KeyTwo"), eq("KeyThree"));

    verify(this.cache, times(1)).getLock();

    Arrays.asList("KeyOne", "KeyTwo").forEach(key ->
      verify(this.cache, times(1)).contains(eq(key)));

    verify(this.cache, never()).contains(eq("KeyThree"));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void containsAllWithEmptyArrayReturnsFalse() {

    doCallRealMethod().when(this.cache).containsAll(any(Comparable.class));

    assertThat(this.cache.containsAll()).isFalse();

    verify(this.cache, times(1)).containsAll();
    verify(this.cache, never()).contains(any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void containsAllWithNullArrayIsNullSafeAndReturnsFalse() {

    doCallRealMethod().when(this.cache).containsAll(ArgumentMatchers.<Comparable[]>any());

    assertThat(this.cache.containsAll((Comparable[]) null)).isFalse();

    verify(this.cache, times(1)).containsAll(ArgumentMatchers.<Comparable[]>isNull());
    verify(this.cache, never()).contains(any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void containsAllWithIterableWhenAllKeysArePresentReturnsTrue() {

    doReturn(true).when(this.cache).contains(any());
    doCallRealMethod().when(this.cache).containsAll(any(Iterable.class));

    assertThat(this.cache.containsAll(Arrays.asList("KeyOne", "KeyTwo", "KeyThree"))).isTrue();

    verify(this.cache, times(1)).containsAll(eq(Arrays.asList("KeyOne", "KeyTwo", "KeyThree")));
    verify(this.cache, times(1)).getLock();

    Arrays.asList("KeyOne", "KeyTwo", "KeyThree").forEach(key ->
      verify(this.cache, times(1)).contains(eq(key)));

    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void containsAllWithIterableWhenJustOneKeyIsNotPresentReturnsFalse() {

    doReturn(true, false, true).when(this.cache).contains(any());
    doCallRealMethod().when(this.cache).containsAll(any(Iterable.class));

    assertThat(this.cache.containsAll(Arrays.asList("KeyOne", "KeyTwo", "KeyThree"))).isFalse();

    verify(this.cache, times(1)).containsAll(eq(Arrays.asList("KeyOne", "KeyTwo", "KeyThree")));
    verify(this.cache, times(1)).getLock();

    Arrays.asList("KeyOne", "KeyTwo").forEach(key ->
      verify(this.cache, times(1)).contains(eq(key)));

    verify(this.cache, never()).contains(eq("KeyThree"));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void containsAllWithEmptyIterableReturnsFalse() {

    doCallRealMethod().when(this.cache).containsAll(any(Iterable.class));

    assertThat(this.cache.containsAll(Collections::emptyIterator)).isFalse();

    verify(this.cache, times(1)).containsAll(isA(Iterable.class));
    verify(this.cache, never()).contains(any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void containsAllWithNullIterableIsNullSafeAndReturnsFalse() {

    doCallRealMethod().when(this.cache).containsAll(ArgumentMatchers.<Iterable>any());

    assertThat(this.cache.containsAll((Iterable) null)).isFalse();

    verify(this.cache, times(1)).containsAll(ArgumentMatchers.<Iterable>isNull());
    verify(this.cache, never()).contains(any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void containsAnyWithArrayWhenJustOneKeyIsPresentReturnsTrue() {

    doReturn(false, true, false).when(this.cache).contains(any());
    doCallRealMethod().when(this.cache).containsAny(any(Comparable.class));

    assertThat(this.cache.containsAny("KeyOne", "KeyTwo", "KeyThree")).isTrue();

    verify(this.cache, times(1))
      .containsAny(eq("KeyOne"), eq("KeyTwo"), eq("KeyThree"));

    verify(this.cache, times(1)).getLock();

    Arrays.asList("KeyOne", "KeyTwo").forEach(key ->
      verify(this.cache, times(1)).contains(eq(key)));

    verify(this.cache, never()).contains(eq("KeyThree"));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void containsAnyWithArrayWhenNoKeysArePresentReturnsFalse() {

    doReturn(false).when(this.cache).contains(any());
    doCallRealMethod().when(this.cache).containsAny(any(Comparable.class));

    assertThat(this.cache.containsAny("KeyOne", "KeyTwo", "KeyThree")).isFalse();

    verify(this.cache, times(1))
      .containsAny(eq("KeyOne"), eq("KeyTwo"), eq("KeyThree"));

    verify(this.cache, times(1)).getLock();

    Arrays.asList("KeyOne", "KeyTwo", "KeyThree").forEach(key ->
      verify(this.cache, times(1)).contains(eq(key)));

    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void containsAnyWithEmptyArrayReturnsFalse() {

    doCallRealMethod().when(this.cache).containsAny(any(Comparable.class));

    assertThat(this.cache.containsAny()).isFalse();

    verify(this.cache, times(1)).containsAny();
    verify(this.cache, never()).contains(any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void containsAnyWithNullArrayIsNullSafeAndReturnsFalse() {

    doCallRealMethod().when(this.cache).containsAny(ArgumentMatchers.<Comparable[]>any());

    assertThat(this.cache.containsAny((Comparable[]) null)).isFalse();

    verify(this.cache, times(1)).containsAny(ArgumentMatchers.<Comparable[]>isNull());
    verify(this.cache, never()).contains(any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void containsAnyWithIterableWhenJustOneKeyIsPresentReturnsTrue() {

    doReturn(false, true, false).when(this.cache).contains(any());
    doCallRealMethod().when(this.cache).containsAny(any(Iterable.class));

    assertThat(this.cache.containsAny(Arrays.asList("KeyOne", "KeyTwo", "KeyThree"))).isTrue();

    verify(this.cache, times(1)).containsAny(eq(Arrays.asList("KeyOne", "KeyTwo", "KeyThree")));
    verify(this.cache, times(1)).getLock();

    Arrays.asList("KeyOne", "KeyTwo").forEach(key ->
      verify(this.cache, times(1)).contains(eq(key)));

    verify(this.cache, never()).containsAny(eq("KeyThree"));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void containsAnyWithIterableWhenNoKeysArePresentReturnsFalse() {

    doReturn(false).when(this.cache).contains(any());
    doCallRealMethod().when(this.cache).containsAny(any(Iterable.class));

    assertThat(this.cache.containsAny(Arrays.asList("KeyOne", "KeyTwo", "KeyThree"))).isFalse();

    verify(this.cache, times(1)).containsAny(eq(Arrays.asList("KeyOne", "KeyTwo", "KeyThree")));
    verify(this.cache, times(1)).getLock();

    Arrays.asList("KeyOne", "KeyTwo", "KeyThree").forEach(key ->
      verify(this.cache, times(1)).contains(eq(key)));

    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void containsAnyWithEmptyIterableReturnsFalse() {

    doCallRealMethod().when(this.cache).containsAny(any(Iterable.class));

    assertThat(this.cache.containsAny(Collections::emptyIterator)).isFalse();

    verify(this.cache, times(1)).containsAny(isA(Iterable.class));
    verify(this.cache, never()).contains(any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void containsAnyWithNullIterableIsNullSafeAndReturnsFalse() {

    doCallRealMethod().when(this.cache).containsAny(ArgumentMatchers.<Iterable>any());

    assertThat(this.cache.containsAny((Iterable) null)).isFalse();

    verify(this.cache, times(1)).containsAny(ArgumentMatchers.<Iterable>isNull());
    verify(this.cache, never()).contains(any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void evictAllWithArray() {

    doCallRealMethod().when(this.cache).evictAll(any(Comparable.class));

    this.cache.evictAll("KeyOne", "KeyTwo", "KeyThree");

    verify(this.cache, times(1))
      .evictAll(eq("KeyOne"), eq("KeyTwo"), eq("KeyThree"));

    verify(this.cache, times(1)).getLock();

    Arrays.asList("KeyOne", "KeyTwo", "KeyThree").forEach(key ->
      verify(this.cache, times(1)).evict(eq(key)));

    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void evictAllWithEmptyArray() {

    doCallRealMethod().when(this.cache).evictAll(any(Comparable.class));

    this.cache.evictAll();

    verify(this.cache, times(1)).evictAll();
    verify(this.cache, never()).evict(any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void evictAllWithNullArray() {

    doCallRealMethod().when(this.cache).evictAll(ArgumentMatchers.<Comparable[]>any());

    this.cache.evictAll((Comparable[]) null);

    verify(this.cache, times(1)).evictAll(ArgumentMatchers.<Comparable[]>isNull());
    verify(this.cache, never()).evict(any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void evictAllWithIterable() {

    doCallRealMethod().when(this.cache).evictAll(any(Iterable.class));

    this.cache.evictAll(Arrays.asList("KeyOne", "KeyTwo", "KeyThree"));

    verify(this.cache, times(1)).evictAll(eq(Arrays.asList("KeyOne", "KeyTwo", "KeyThree")));
    verify(this.cache, times(1)).getLock();

    Arrays.asList("KeyOne", "KeyTwo", "KeyThree").forEach(key ->
      verify(this.cache, times(1)).evict(eq(key)));

    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void evictAllWithEmptyIterable() {

    doCallRealMethod().when(this.cache).evictAll(any(Iterable.class));

    this.cache.evictAll(Collections::emptyIterator);

    verify(this.cache, times(1)).evictAll(isA(Iterable.class));
    verify(this.cache, never()).evict(any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void evictAllWithNullIterable() {

    doCallRealMethod().when(this.cache).evictAll(ArgumentMatchers.<Iterable>any());

    this.cache.evictAll((Iterable) null);

    verify(this.cache, times(1)).evictAll(ArgumentMatchers.<Iterable>isNull());
    verify(this.cache, never()).evict(any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void fromMapIsCorrect() {

    Map map = MapBuilder.newHashMap()
      .put(1, "one")
      .put(2, "two")
      .put(4, null)
      .put(3, "three")
      .put(null, null)
      .build();

    doCallRealMethod().when(this.cache).from(any(Map.class));

    this.cache.from(map);

    verify(this.cache, times(1)).from(eq(map));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(4)).put(isA(Cache.Entry.class));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void fromEmptyMapIsCorrect() {

    this.cache.from(Collections.emptyMap());

    verify(this.cache, times(1)).from(eq(Collections.emptyMap()));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void fromNullMapIsCorrect() {

    this.cache.from(null);

    verify(this.cache, times(1)).from(isNull());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void getAllWithArrayReturnsList() {

    doReturn("A", "B", "C").when(this.cache).get(any());
    doCallRealMethod().when(this.cache).getAll(any(Comparable.class));

    assertThat(this.cache.getAll(1, 2, 3)).containsExactly("A", "B", "C");

    verify(this.cache, times(1)).getAll(eq(1), eq(2), eq(3));
    verify(this.cache, times(1)).getLock();

    Stream.of(1, 2, 3).forEach(key -> verify(this.cache, times(1)).get(eq(key)));

    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void getAllWithArrayHavingNullKeysReturnsListContainingNullValues() {

    doReturn("A", null, "C").when(this.cache).get(any());
    doCallRealMethod().when(this.cache).getAll(ArgumentMatchers.<Comparable>any());

    assertThat(this.cache.getAll(1, null, 3)).containsExactly("A", null, "C");

    verify(this.cache, times(1)).getAll(eq(1), isNull(), eq(3));
    verify(this.cache, times(1)).getLock();

    Stream.of(1, null, 3).forEach(key -> verify(this.cache, times(1)).get(eq(key)));

    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void getAllWithEmptyArrayReturnsEmptyList() {

    doCallRealMethod().when(this.cache).getAll(any(Comparable.class));

    assertThat(this.cache.getAll()).isEmpty();

    verify(this.cache, times(1)).getAll();
    verify(this.cache, never()).get(any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void getAllWithNullArrayIsNullSafeReturnsEmptyList() {

    doCallRealMethod().when(this.cache).getAll(ArgumentMatchers.<Comparable[]>any());

    assertThat(this.cache.getAll((Comparable[]) null)).isEmpty();

    verify(this.cache, times(1)).getAll(ArgumentMatchers.<Comparable[]>isNull());
    verify(this.cache, never()).get(any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void getAllWithIterableReturnsList() {

    doReturn("A", "B", "C").when(this.cache).get(any());
    doCallRealMethod().when(this.cache).getAll(any(Iterable.class));

    assertThat(this.cache.getAll(Arrays.asList(1, 2, 3))).containsExactly("A", "B", "C");

    verify(this.cache, times(1)).getAll(eq(Arrays.asList(1, 2, 3)));
    verify(this.cache, times(1)).getLock();

    Stream.of(1, 2, 3).forEach(key -> verify(this.cache, times(1)).get(eq(key)));

    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void getAllWithIterableHavingNullKeysReturnsListContainingNullValues() {

    doReturn("A", null, "C").when(this.cache).get(any());
    doCallRealMethod().when(this.cache).getAll(any(Iterable.class));

    assertThat(this.cache.getAll(Arrays.asList(1, null, 3))).containsExactly("A", null, "C");

    verify(this.cache, times(1)).getAll(eq(Arrays.asList(1, null, 3)));
    verify(this.cache, times(1)).getLock();

    Stream.of(1, null, 3).forEach(key -> verify(this.cache, times(1)).get(eq(key)));

    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void getAllWithEmptyIterableReturnsEmptyList() {

    doCallRealMethod().when(this.cache).getAll(any(Iterable.class));

    assertThat(this.cache.getAll(Collections::emptyIterator)).isEmpty();

    verify(this.cache, times(1)).getAll(isA(Iterable.class));
    verify(this.cache, never()).get(any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void getAllWithNullIterableIsNullSafeReturnsEmptyList() {

    doCallRealMethod().when(this.cache).getAll(ArgumentMatchers.<Iterable>any());

    assertThat(this.cache.getAll((Iterable) null)).isEmpty();

    verify(this.cache, times(1)).getAll(ArgumentMatchers.<Iterable>isNull());
    verify(this.cache, never()).get(any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void getAndEvictWithKey() {

    doReturn("A").when(this.cache).get(any());
    doCallRealMethod().when(this.cache).getAndEvict(any(Comparable.class));

    assertThat(this.cache.getAndEvict(1)).isEqualTo("A");

    verify(this.cache, times(1)).getAndEvict(eq(1));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).get(eq(1));
    verify(this.cache, times(1)).evict(eq(1));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void getAndEvictWithKeyAndExpectedValueDoesEviction() {

    doReturn("A").when(this.cache).get(any());
    doCallRealMethod().when(this.cache).getAndEvict(any(Comparable.class), any());

    assertThat(this.cache.getAndEvict(1, "A")).isEqualTo("A");

    verify(this.cache, times(1)).getAndEvict(eq(1), eq("A"));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).get(eq(1));
    verify(this.cache, times(1)).evict(eq(1));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void getAndEvictWithKeyAndExpectedValueDoesNotEvict() {

    doReturn("A").when(this.cache).get(any());
    doCallRealMethod().when(this.cache).getAndEvict(any(Comparable.class), any());

    assertThat(this.cache.getAndEvict(1, "B")).isEqualTo("A");

    verify(this.cache, times(1)).getAndEvict(eq(1), eq("B"));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).get(eq(1));
    verify(this.cache, never()).evict(any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void getAndPutIsCorrect() {

    doReturn("A").when(this.cache).get(any());
    doCallRealMethod().when(this.cache).getAndPut(any(), any());

    assertThat(this.cache.getAndPut(1, "B")).isEqualTo("A");

    verify(this.cache, times(1)).getAndPut(eq(1), eq("B"));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).get(eq(1));
    verify(this.cache, times(1)).put(eq(1), eq("B"));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void getAndReplaceWithKeyWhenContainsKey() {

    doReturn(true).when(this.cache).contains(any());
    doReturn("A").when(this.cache).get(any());
    doCallRealMethod().when(this.cache).getAndReplace(any(), any());

    assertThat(this.cache.getAndReplace(1, "B")).isEqualTo("A");

    verify(this.cache, times(1)).getAndReplace(eq(1), eq("B"));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).contains(eq(1));
    verify(this.cache, times(1)).get(eq(1));
    verify(this.cache, times(1)).put(eq(1), eq("B"));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void getAndReplaceWithKeyWhenKeyNotPresent() {

    doReturn(false).when(this.cache).contains(any());
    doCallRealMethod().when(this.cache).getAndReplace(any(), any());

    assertThat(this.cache.getAndReplace(1, "B")).isNull();

    verify(this.cache, times(1)).getAndReplace(eq(1), eq("B"));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).contains(eq(1));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void getAndReplaceWithKeyAndExistingValueWhenContainsKeyAndValuesMatch() {

    doReturn(true).when(this.cache).contains(any());
    doReturn("A").when(this.cache).get(any());
    doCallRealMethod().when(this.cache).getAndReplace(any(), any(), any());

    assertThat(this.cache.getAndReplace(1, "A", "B")).isEqualTo("A");

    verify(this.cache, times(1)).getAndReplace(eq(1), eq("A"), eq("B"));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).contains(eq(1));
    verify(this.cache, times(1)).get(eq(1));
    verify(this.cache, times(1)).put(eq(1), eq("B"));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void getAndReplaceWithKeyAndExistingValueWhenKeyNotPresent() {

    doReturn(false).when(this.cache).contains(any());
    doCallRealMethod().when(this.cache).getAndReplace(any(), any(), any());

    assertThat(this.cache.getAndReplace(1, "A", "B")).isNull();

    verify(this.cache, times(1)).getAndReplace(eq(1), eq("A"), eq("B"));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).contains(eq(1));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void getAndReplaceWithKeyAndExistingValueWhenValuesDoNotMatch() {

    doReturn(true).when(this.cache).contains(any());
    doReturn("C").when(this.cache).get(any());
    doCallRealMethod().when(this.cache).getAndReplace(any(), any(), any());

    assertThat(this.cache.getAndReplace(1, "A", "B")).isEqualTo("C");

    verify(this.cache, times(1)).getAndReplace(eq(1), eq("A"), eq("B"));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).contains(eq(1));
    verify(this.cache, times(1)).get(eq(1));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void getCacheEntryForKey() {

    AtomicReference<Object> value = new AtomicReference<>("A");

    doReturn(true).when(this.cache).contains(eq("mockKey"));
    doAnswer(invocation -> value.get()).when(this.cache).get(eq("mockKey"));
    doCallRealMethod().when(this.cache).getEntry(any());

    Cache.Entry<?, ?> cacheEntry = this.cache.getEntry("mockKey");

    assertThat(cacheEntry).isNotNull();
    assertThat(cacheEntry.getKey()).isEqualTo("mockKey");
    assertThat(cacheEntry.getSource()).isSameAs(this.cache);
    assertThat(cacheEntry.getValue()).isEqualTo("A");

    value.set("B");

    assertThat(cacheEntry.getValue()).isEqualTo("B");

    verify(this.cache, times(1)).getEntry(eq("mockKey"));
    verify(this.cache, times(4)).contains(eq("mockKey"));
    verify(this.cache, times(2)).get(eq("mockKey"));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void getEntryForNonExistingKey() {

    doReturn(false).when(this.cache).contains(any());
    doCallRealMethod().when(this.cache).getEntry(any());

    assertThat(this.cache.getEntry("testKey")).isNull();

    verify(this.cache, times(1)).getEntry(eq("testKey"));
    verify(this.cache, times(1)).contains(eq("testKey"));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void getEntryOperationsAfterDetached() {

    doReturn("MockCache").when(this.cache).getName();
    doReturn(true, true, false).when(this.cache).contains(eq("mockKey"));
    doReturn("A", (Object) null).when(this.cache).get(eq("mockKey"));
    doCallRealMethod().when(this.cache).getEntry(any());

    Cache.Entry<?, ?> cacheEntry = this.cache.getEntry("mockKey");

    assertThat(cacheEntry).isNotNull();
    assertThat(cacheEntry.getKey()).isEqualTo("mockKey");
    assertThat(cacheEntry.getValue()).isEqualTo("A");

    assertThatIllegalStateException()
      .isThrownBy(cacheEntry::getValue)
      .withMessage("Cache [MockCache] no longer contains key [mockKey]")
      .withNoCause();

    verify(this.cache, times(1)).getEntry(eq("mockKey"));
    verify(this.cache, times(3)).contains(eq("mockKey"));
    verify(this.cache, times(1)).get(eq("mockKey"));
    verify(this.cache, times(1)).getName();
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void putCacheEntryIsCorrect() {

    Cache.Entry<?, ?> mockCacheEntry = mockCacheEntry(1, "A");

    doCallRealMethod().when(this.cache).put(any(Cache.Entry.class));

    this.cache.put(mockCacheEntry);

    verify(this.cache, times(1)).put(eq(mockCacheEntry));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).put(eq(1), eq("A"));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  @SuppressWarnings("all")
  public void putNullCacheEntry() {

    doCallRealMethod().when(this.cache).put(ArgumentMatchers.<Cache.Entry<?, ?>>any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> this.cache.put((Cache.Entry<?, ?>) null))
      .withMessage("The Cache.Entry to put in this cache is required")
      .withNoCause();

    verify(this.cache, times(1)).put(ArgumentMatchers.<Cache.Entry<?, ?>>isNull());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void putEntity() {

    Identifiable<Integer> mockEntity = mockIdentifiable(1);

    doCallRealMethod().when(this.cache).put(any(Identifiable.class));

    this.cache.put(mockEntity);

    verify(this.cache, times(1)).put(eq(mockEntity));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).put(eq(1), eq(mockEntity));
    verify(mockEntity, times(2)).getId();
    verifyNoMoreInteractions(this.cache, mockEntity);
  }

  @Test
  public void putEntityWithNullIdThrowsIllegalArgumentException() {

    Identifiable<Integer> mockEntity = mockIdentifiable(null);

    doCallRealMethod().when(this.cache).put(any(Identifiable.class));

    assertThatIllegalArgumentException()
      .isThrownBy(() -> this.cache.put(mockEntity))
      .withMessage("ID of entity to cache is required")
      .withNoCause();

    verify(this.cache, times(1)).put(eq(mockEntity));
    verify(mockEntity, times(1)).getId();
    verifyNoMoreInteractions(this.cache, mockEntity);
  }

  @Test
  public void putNullEntityThrowsIllegalArgumentException() {

    doCallRealMethod().when(this.cache).put(ArgumentMatchers.<Identifiable<?>>any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> this.cache.put((Identifiable<?>) null))
      .withMessage("Entity to cache is required")
      .withNoCause();

    verify(this.cache, times(1)).put(ArgumentMatchers.<Identifiable<?>>isNull());
    verify(this.cache, never()).put(any(), any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void putAllWithArray() {

    Person jonDoe = Person.newPerson().named("Jon", "Doe").identifiedBy(1L);
    Person janeDoe = Person.newPerson().named("Jane", "Doe").identifiedBy(2L);
    Person lanDoe = Person.newPerson().named("Lan", "Doe").identifiedBy(3L);

    doCallRealMethod().when(this.cache).putAll(any(Identifiable.class));

    this.cache.putAll(jonDoe, janeDoe, lanDoe);

    verify(this.cache, times(1)).putAll(eq(jonDoe), eq(janeDoe), eq(lanDoe));
    verify(this.cache, times(1)).getLock();

    Arrays.asList(jonDoe, janeDoe, lanDoe).forEach(person ->
      verify(this.cache, times(1)).put(eq(person)));

    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void putAllWithArrayContainingNullEntityThrowsIllegalArgumentException() {

    Person jonDoe = Person.newPerson().named("Jon", "Doe").identifiedBy(1L);
    Person janeDoe = Person.newPerson().named("Jane", "Doe").identifiedBy(2L);

    doCallRealMethod().when(this.cache).putAll(ArgumentMatchers.<Identifiable<?>[]>any());

    this.cache.putAll(jonDoe, null, janeDoe);

    verify(this.cache, times(1)).putAll(eq(jonDoe), isNull(), eq(janeDoe));
    verify(this.cache, times(1)).getLock();

    Arrays.asList(jonDoe, janeDoe).forEach(person ->
      verify(this.cache, times(1)).put(eq(person)));

    verify(this.cache, never()).put(ArgumentMatchers.<Identifiable<?>>isNull());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void putAllWithArrayContainingEntityWithNullIdThrowsIllegalArgumentException() {

    Person jonDoe = Person.newPerson().named("Jon", "Doe");
    Person janeDoe = Person.newPerson().named("Jane", "Doe").identifiedBy(2L);

    doCallRealMethod().when(this.cache).putAll(ArgumentMatchers.<Identifiable<?>>any());
    doCallRealMethod().when(this.cache).put(ArgumentMatchers.<Identifiable<?>>any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> this.cache.putAll(jonDoe, janeDoe))
      .withMessage("ID of entity to cache is required")
      .withNoCause();

    verify(this.cache, times(1)).putAll(eq(jonDoe), eq(janeDoe));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).put(eq(jonDoe));
    verify(this.cache, never()).put(any(), any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void putAllWithEmptyArray() {

    doCallRealMethod().when(this.cache).putAll(any(Identifiable.class));

    this.cache.putAll();

    verify(this.cache, times(1)).putAll();
    verify(this.cache, never()).put(any(Identifiable.class));
    verify(this.cache, never()).put(any(), any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void putAllWithNullArray() {

    doCallRealMethod().when(this.cache).putAll(ArgumentMatchers.<Identifiable<?>[]>any());

    this.cache.putAll((Identifiable[]) null);

    verify(this.cache, times(1)).putAll(ArgumentMatchers.<Identifiable<?>[]>isNull());
    verify(this.cache, never()).put(any(Identifiable.class));
    verify(this.cache, never()).put(any(), any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void putAllWithIterable() {

    Person cookieDoe = Person.newPerson().named("Cookie", "Doe").identifiedBy(1L);
    Person lanDoe = Person.newPerson().named("Lan", "Doe").identifiedBy(2L);
    Person sourDoe = Person.newPerson().named("Sour", "Doe").identifiedBy(3L);

    doCallRealMethod().when(this.cache).putAll(any(Iterable.class));

    this.cache.putAll(Arrays.asList(cookieDoe, lanDoe, sourDoe));

    verify(this.cache, times(1)).putAll(eq(Arrays.asList(cookieDoe, lanDoe, sourDoe)));
    verify(this.cache, times(1)).getLock();

    Arrays.asList(cookieDoe, lanDoe, sourDoe).forEach(person ->
      verify(this.cache, times(1)).put(eq(person)));

    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void putAllWithIterableContainingNullEntitiesThrowsIllegalArgumentException() {

    Person jonDoe = Person.newPerson().named("Jon", "Doe").identifiedBy(1L);
    Person janeDoe = Person.newPerson().named("Jane", "Doe").identifiedBy(2L);

    doCallRealMethod().when(this.cache).putAll(any(Iterable.class));

    this.cache.putAll(Arrays.asList(jonDoe, null, janeDoe));

    verify(this.cache, times(1)).putAll(Arrays.asList(jonDoe, null, janeDoe));
    verify(this.cache, times(1)).getLock();

    Arrays.asList(jonDoe, janeDoe).forEach(person ->
      verify(this.cache, times(1)).put(eq(person)));

    verify(this.cache, never()).put(ArgumentMatchers.<Identifiable<?>>isNull());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void putAllWithIterableContainingEntityWithNullIdThrowsIllegalArgumentException() {

    Person jonDoe = Person.newPerson().named("Jon", "Doe");
    Person janeDoe = Person.newPerson().named("Jane", "Doe").identifiedBy(2L);

    doCallRealMethod().when(this.cache).putAll(isA(Iterable.class));
    doCallRealMethod().when(this.cache).put(isA(Identifiable.class));

    assertThatIllegalArgumentException()
      .isThrownBy(() -> this.cache.putAll(Arrays.asList(jonDoe, janeDoe)))
      .withMessage("ID of entity to cache is required")
      .withNoCause();

    verify(this.cache, times(1)).putAll(eq(Arrays.asList(jonDoe, janeDoe)));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).put(eq(jonDoe));
    verify(this.cache, never()).put(any(), any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void putAllWithEmptyIterable() {

    doCallRealMethod().when(this.cache).putAll(any(Iterable.class));

    this.cache.putAll(Collections::emptyIterator);

    verify(this.cache, times(1)).putAll(isA(Iterable.class));
    verify(this.cache, never()).put(any(Identifiable.class));
    verify(this.cache, never()).put(any(), any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void putAllWithNullIterable() {

    doCallRealMethod().when(this.cache).putAll(ArgumentMatchers.<Iterable<Identifiable<?>>>any());

    this.cache.putAll((Iterable<Identifiable<?>>) null);

    verify(this.cache, times(1)).putAll(ArgumentMatchers.<Iterable<Identifiable<?>>>isNull());
    verify(this.cache, never()).put(any(Identifiable.class));
    verify(this.cache, never()).put(any(), any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void putIfAbsentWithKeyValue() {

    doReturn(false).when(this.cache).contains(any());
    doCallRealMethod().when(this.cache).putIfAbsent(any(), any());

    assertThat(this.cache.putIfAbsent(1, "test")).isNull();

    verify(this.cache, times(1)).putIfAbsent(eq(1), eq("test"));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).contains(eq(1));
    verify(this.cache, times(1)).put(eq(1), eq("test"));
    verify(this.cache, never()).get(any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void putIfAbsentWithKeyValueUsingExistingKey() {

    doReturn(true).when(this.cache).contains(any());
    doReturn("existingValue").when(this.cache).get(any());
    doCallRealMethod().when(this.cache).putIfAbsent(any(), any());

    assertThat(this.cache.putIfAbsent(1, "test")).isEqualTo("existingValue");

    verify(this.cache, times(1)).putIfAbsent(eq(1), eq("test"));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).contains(eq(1));
    verify(this.cache, times(1)).get(eq(1));
    verify(this.cache, never()).put(any(), any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void putIfAbsentWithKeyValueUsingNullKeyThrowsIllegalArgumentException() {

    doCallRealMethod().when(this.cache).putIfAbsent(any(), any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> this.cache.putIfAbsent(null, "test"))
      .withMessage("Key is required")
      .withNoCause();

    verify(this.cache, times(1)).putIfAbsent(eq(null), eq("test"));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void putIfAbsentWithEntity() {

    doReturn(false).when(this.cache).contains(any());
    doCallRealMethod().when(this.cache).putIfAbsent(any(Identifiable.class));

    Identifiable<Integer> mockEntity = mockIdentifiable(1);

    assertThat(this.cache.putIfAbsent(mockEntity)).isNull();

    verify(this.cache, times(1)).putIfAbsent(eq(mockEntity));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).contains(eq(1));
    verify(this.cache, times(1)).put(eq(1), eq(mockEntity));
    verify(this.cache, never()).get(anyLong());
    verify(mockEntity, times(1)).getId();
    verifyNoMoreInteractions(this.cache, mockEntity);
  }

  @Test
  public void putIfAbsentWithExistingEntity() {

    Identifiable<Integer> mockEntity = mockIdentifiable(1);

    Person bobDoe = Person.newPerson().named("Bob", "Doe").identifiedBy(1L);

    doReturn(true).when(this.cache).contains(any());
    doReturn(mockEntity).when(this.cache).get(eq(1L));
    doCallRealMethod().when(this.cache).putIfAbsent(any(Identifiable.class));

    assertThat(this.cache.putIfAbsent(bobDoe)).isEqualTo(mockEntity);

    verify(this.cache, times(1)).putIfAbsent(eq(bobDoe));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).contains(eq(1L));
    verify(this.cache, times(1)).get(eq(1L));
    verify(this.cache, never()).put(any(), any());
    verifyNoMoreInteractions(this.cache);
    verifyNoInteractions(mockEntity);
  }

  @Test
  public void putIfAbsentWithEntityHavingNullId() {

    Identifiable<Integer> mockEntity = mockIdentifiable(null);

    doCallRealMethod().when(this.cache).putIfAbsent(any(Identifiable.class));

    assertThatIllegalArgumentException()
      .isThrownBy(() -> this.cache.putIfAbsent(mockEntity))
      .withMessage("ID of the entity to cache is required")
      .withNoCause();

    verify(this.cache, times(1)).putIfAbsent(eq(mockEntity));
    verify(mockEntity, times(1)).getId();
    verifyNoMoreInteractions(this.cache, mockEntity);
  }

  @Test
  public void putIfAbsentWithNullEntity() {

    doCallRealMethod().when(this.cache).putIfAbsent(any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> this.cache.putIfAbsent(null))
      .withMessage("Entity to cache is required")
      .withNoCause();

    verify(this.cache, times(1)).putIfAbsent(isNull());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void putIfPresentWithKeyValue() {

    doReturn(true).when(this.cache).contains(any());
    doReturn("existingValue").when(this.cache).get(any());
    doCallRealMethod().when(this.cache).putIfPresent(any(), any());

    assertThat(this.cache.putIfPresent(1, "test")).isEqualTo("existingValue");

    verify(this.cache, times(1)).putIfPresent(eq(1), eq("test"));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).contains(eq(1));
    verify(this.cache, times(1)).get(eq(1));
    verify(this.cache, times(1)).put(eq(1), eq("test"));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void putIfPresentWithKeyValueUsingNonExistingKey() {

    doReturn(false).when(this.cache).contains(any());
    doCallRealMethod().when(this.cache).putIfPresent(any(), any());

    assertThat(this.cache.putIfPresent(1, "test")).isNull();

    verify(this.cache, times(1)).putIfPresent(eq(1), eq("test"));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).contains(eq(1));
    verify(this.cache, never()).get(any());
    verify(this.cache, never()).put(any(), any());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void putIfPresentWithKeyValueUsingNullKeyThrowsIllegalArgumentException() {

    doCallRealMethod().when(this.cache).putIfPresent(any(), any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> this.cache.putIfPresent(null, "test"))
      .withMessage("Key is required")
      .withNoCause();

    verify(this.cache, times(1)).putIfPresent(isNull(), eq("test"));
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void putIfPresentWithEntity() {

    Identifiable<Integer> mockEntity = mockIdentifiable(1);

    Person bobDoe = Person.newPerson().named("Bob", "Doe").identifiedBy(1L);

    doReturn(true).when(this.cache).contains(any());
    doReturn(mockEntity).when(this.cache).get(eq(1L));
    doCallRealMethod().when(this.cache).putIfPresent(any(Identifiable.class));

    assertThat(this.cache.putIfPresent(bobDoe)).isEqualTo(mockEntity);

    verify(this.cache, times(1)).putIfPresent(eq(bobDoe));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).contains(eq(1L));
    verify(this.cache, times(1)).get(eq(1L));
    verify(this.cache, times(1)).put(eq(1L), eq(bobDoe));
    verifyNoMoreInteractions(this.cache);
    verifyNoInteractions(mockEntity);
  }

  @Test
  public void putIfPresentWithEntityHavingNullId() {

    Identifiable<Integer> mockEntity = mockIdentifiable(null);

    doReturn(false).when(this.cache).contains(isNull());
    doCallRealMethod().when(this.cache).putIfPresent(any(Identifiable.class));

    assertThat(this.cache.putIfPresent(mockEntity)).isNull();

    verify(this.cache, times(1)).putIfPresent(eq(mockEntity));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).contains(isNull());
    verify(mockEntity, times(1)).getId();
    verifyNoMoreInteractions(this.cache, mockEntity);
  }

  @Test
  public void putIfPresentWithNonExistingEntity() {

    Identifiable<Integer> mockEntity = mockIdentifiable(1);

    doReturn(false).when(this.cache).contains(any());
    doCallRealMethod().when(this.cache).putIfPresent(any(Identifiable.class));

    assertThat(this.cache.putIfPresent(mockEntity)).isNull();

    verify(this.cache, times(1)).putIfPresent(eq(mockEntity));
    verify(this.cache, times(1)).getLock();
    verify(this.cache, times(1)).contains(eq(1));
    verify(mockEntity, times(1)).getId();
    verifyNoMoreInteractions(this.cache, mockEntity);
  }

  @Test
  public void putIfPresentWithNullEntity() {

    doCallRealMethod().when(this.cache).putIfPresent(any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> this.cache.putIfPresent(null))
      .withMessage("Entity to cache is required")
      .withNoCause();

    verify(this.cache, times(1)).putIfPresent(isNull());
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void sizeReturnsDefaultSize() {

    doCallRealMethod().when(this.cache).size();

    assertThat(this.cache.size()).isEqualTo(Cache.DEFAULT_SIZE);

    verify(this.cache, times(1)).size();
    verifyNoMoreInteractions(this.cache);
  }

  @Test
  public void toMap() {

    doReturn(CollectionUtils.asSet(1, 2)).when(this.cache).keys();
    doReturn("A").when(this.cache).get(eq(1));
    doReturn("B").when(this.cache).get(eq(2));
    doCallRealMethod().when(this.cache).toMap();

    Map<Integer, String> map = this.cache.toMap();

    assertThat(map).isNotNull();
    assertThat(map).hasSize(2);
    assertThat(map).containsEntry(1, "A");
    assertThat(map).containsEntry(2, "B");
  }

  @Test
  public void toMapFromEmptyCache() {

    doReturn(Collections.emptySet()).when(this.cache).keys();
    doCallRealMethod().when(this.cache).toMap();

    Map<Integer, String> map = this.cache.toMap();

    assertThat(map).isNotNull();
    assertThat(map).isEmpty();
  }

  @Getter
  @SuppressWarnings("all")
  @NoArgsConstructor(staticName = "newPerson")
  static class Person implements Identifiable<Long> {

    @Id
    @Setter
    private Long id;

    private String firstName;
    private String lastName;

    String getName() {
      return String.format("%1$s %2$s", getFirstName(), getLastName());
    }

    Person named(String firstName, String lastName) {

      this.firstName = firstName;
      this.lastName = lastName;

      return this;
    }

    @Override
    public boolean equals(Object obj) {

      if (this == obj) {
        return true;
      }

      if (!(obj instanceof Person)) {
        return false;
      }

      Person that = (Person) obj;

      return ObjectUtils.equals(this.getId(), that.getId())
        && ObjectUtils.equals(this.getName(), that.getName());
    }

    @Override
    public int hashCode() {
      return ObjectUtils.hashCodeOf(getId(), getName());
    }

    @Override
    public String toString() {
      return String.format("%s(%d)", getName(), getId());
    }
  }
}
