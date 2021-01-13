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
package org.cp.elements.data.caching.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.data.caching.support.CacheToMapAdapter.CacheEntry;
import static org.cp.elements.util.CollectionUtils.asSet;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.atMost;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import org.cp.elements.data.caching.Cache;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.MapBuilder;
import org.cp.elements.util.MapUtils;
import org.junit.Test;

/**
 * Unit Tests for {@link CacheToMapAdapter}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.caching.support.CacheToMapAdapter
 * @since 1.0.0
 */
@SuppressWarnings({ "rawtypes", "unchecked" })
public class CacheToMapAdapterTests {

  @Test
  public void constructCacheToMapAdapterWithCache() {

    Cache mockCache = mock(Cache.class);

    CacheToMapAdapter adapter = new CacheToMapAdapter(mockCache);

    assertThat(adapter).isNotNull();
    assertThat(adapter.getCache()).isEqualTo(mockCache);
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructCacheToMapAdapterWithNullCache() {

    try {
      new CacheToMapAdapter(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Cache is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void clearClearsCache() {

    Cache mockCache = mock(Cache.class);

    CacheToMapAdapter.of(mockCache).clear();

    verify(mockCache, times(1)).clear();
  }

  @Test
  public void compute() {

    Cache<Integer, Object> mockCache = mock(Cache.class);

    assertThat(CacheToMapAdapter.of(mockCache).compute(2, (key, value) -> key * key)).isEqualTo(4);

    verify(mockCache, times(1)).put(eq(2), eq(4));
  }

  @Test
  public void computeIfAbsentWhenAbsent() {

    Cache<Integer, Object> mockCache = mock(Cache.class);

    when(mockCache.get(any())).thenReturn(null);

    assertThat(CacheToMapAdapter.of(mockCache).computeIfAbsent(2, key -> key * key)).isEqualTo(4);

    verify(mockCache, times(2)).get(eq(2));
    verify(mockCache, times(1)).put(eq(2), eq(4));
  }

  @Test
  public void computeIfAbsentWhenPresent() {

    Cache<Integer, Object> mockCache = mock(Cache.class);

    when(mockCache.get(any())).thenReturn(0);

    assertThat(CacheToMapAdapter.of(mockCache).computeIfAbsent(2, key -> key * key)).isEqualTo(0);

    verify(mockCache, times(1)).get(eq(2));
    verify(mockCache, never()).put(any(), any());
  }

  @Test
  public void computeIfPresentWhenAbsent() {

    Cache<Integer, Object> mockCache = mock(Cache.class);

    when(mockCache.get(any())).thenReturn(null);

    assertThat(CacheToMapAdapter.of(mockCache).computeIfPresent(2, (key, value) -> key * key)).isNull();

    verify(mockCache, times(1)).get(eq(2));
    verify(mockCache, never()).put(any(), any());
  }

  @Test
  public void computeIfPresentWhenPresent() {

    Cache<Integer, Object> mockCache = mock(Cache.class);

    when(mockCache.get(any())).thenReturn(0L);

    assertThat(CacheToMapAdapter.of(mockCache).computeIfPresent(2, (key, value) -> key * key)).isEqualTo(4);

    verify(mockCache, times(2)).get(eq(2));
    verify(mockCache, times(1)).put(eq(2), eq(4));
  }

  @Test
  public void containsKeyReturnsTrueForExistingKey() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.contains(any())).thenReturn(true);

    assertThat(CacheToMapAdapter.of(mockCache).containsKey("key")).isTrue();

    verify(mockCache, times(1)).contains(eq("key"));
  }

  @Test
  public void containsKeyReturnsFalseForNonExistingKey() {

    Cache mockCache = mock(Cache.class);

    assertThat(CacheToMapAdapter.of(mockCache).containsKey("key")).isFalse();

    verify(mockCache, times(1)).contains(eq("key"));
  }

  @Test
  public void containsValueReturnsTrueForExistingValue() {
    assertThat(CacheToMapAdapter.of(MapToCacheAdapter.of(Collections.singletonMap("key", "value")))
      .containsValue("value")).isTrue();
  }

  @Test
  public void containsValueReturnsFalseForNonExistingValue() {
    assertThat(CacheToMapAdapter.of(MapToCacheAdapter.of(Collections.<Comparable, Object>emptyMap()))
      .containsValue("value")).isFalse();
  }

  @Test
  public void entrySetIsCorrect() {

    Map map = MapBuilder.newHashMap()
      .put(1, "one")
      .put(2, "two")
      .build();

    Map cacheMap = CacheToMapAdapter.of(MapToCacheAdapter.of(map));

    assertThat(cacheMap).isNotNull();
    assertThat(cacheMap).hasSize(map.size());

    Set entrySet = cacheMap.entrySet();

    assertThat(entrySet).isNotNull();
    assertThat(entrySet).hasSize(map.size());
    assertThat(entrySet).containsExactlyInAnyOrder(MapUtils.newMapEntry(1, "one"), MapUtils.newMapEntry(2, "two"));
  }

  @Test(expected = NoSuchElementException.class)
  public void entrySetNextWithNoMoreCacheEntriesThrowsNoSuchElementException() {

    Map cacheMap = CacheToMapAdapter.of(MapToCacheAdapter.of(
      Collections.<Comparable, Object>singletonMap("key", "value")));

    assertThat(cacheMap).isNotNull();
    assertThat(cacheMap).hasSize(1);

    Set entrySet = cacheMap.entrySet();

    assertThat(entrySet).isNotNull();
    assertThat(entrySet).hasSize(1);

    Iterator<Map.Entry> entrySetIterator = entrySet.iterator();

    assertThat(entrySetIterator.hasNext()).isTrue();
    assertThat(entrySetIterator.next().getValue()).isEqualTo("value");
    assertThat(entrySetIterator.hasNext()).isFalse();

    try {
      entrySetIterator.next();
    }
    catch (NoSuchElementException expected) {

      assertThat(expected).hasMessage("No more cache entries");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void equalsIsCorrect() {

    Map mapOne = Collections.singletonMap(1, "test");
    Map mapTwo = Collections.singletonMap(1, "test");

    assertThat(mapOne).isNotSameAs(mapTwo);
    assertThat(mapOne).isEqualTo(mapTwo);
    assertThat(CacheToMapAdapter.of(MapToCacheAdapter.of(mapOne))).isEqualTo(mapTwo);
  }

  @Test
  public void forEachIsCorrect() {

    Map map = MapBuilder.newHashMap()
      .put("one", 1)
      .put("two", 2)
      .build();

    AtomicInteger sum = new AtomicInteger(0);

    CacheToMapAdapter.of(MapToCacheAdapter.of(map)).forEach((key, value) -> sum.addAndGet((int) value));

    assertThat(sum.get()).isEqualTo(3);
  }

  @Test
  public void getWithExistingKeyReturnsValue() {
    assertThat(CacheToMapAdapter.of(MapToCacheAdapter.of(Collections.singletonMap("key", "value"))).get("key"))
      .isEqualTo("value");
  }

  @Test
  public void getWithNonExistingKeyReturnsNull() {
    assertThat(CacheToMapAdapter.of(MapToCacheAdapter.of(Collections.<Comparable, Object>emptyMap())).get("key"))
      .isNull();
  }

  @Test
  public void getOrDefaultWithExistingKeyReturnsValue() {
    assertThat(CacheToMapAdapter.of(MapToCacheAdapter.of(Collections.singletonMap("key", "value")))
      .getOrDefault("key", "default")).isEqualTo("value");
  }

  @Test
  public void getOrDefaultWithNonExistingKeyReturnsDefaultValue() {
    assertThat(CacheToMapAdapter.of(MapToCacheAdapter.of(Collections.<Comparable, Object>emptyMap()))
      .getOrDefault("key", "default")).isEqualTo("default");
  }

  @Test
  public void hashCodeIsCorrect() {

    Map map = Collections.singletonMap("key", "value");

    Cache cache = MapToCacheAdapter.of(map);

    Map cacheMap = CacheToMapAdapter.of(cache);

    assertThat(cacheMap.hashCode()).isNotZero();
    assertThat(cacheMap.hashCode()).isNotEqualTo(cache.hashCode());
    assertThat(cacheMap.hashCode()).isNotEqualTo(map.hashCode());
    assertThat(cache.hashCode()).isNotEqualTo(map.hashCode());
  }

  @Test
  public void isEmptyReturnsTrueWhenCacheIsEmpty() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.isEmpty()).thenReturn(true);

    assertThat(CacheToMapAdapter.of(mockCache).isEmpty()).isTrue();

    verify(mockCache, times(1)).isEmpty();
  }

  @Test
  public void isEmptyReturnsFalseWhenCacheIsNotEmpty() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.isEmpty()).thenReturn(false);

    assertThat(CacheToMapAdapter.of(mockCache).isEmpty()).isFalse();

    verify(mockCache, times(1)).isEmpty();
  }

  @Test
  public void keySetReturnsAllCacheKeys() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.keys()).thenReturn(asSet(1, 2, 3));

    assertThat(CacheToMapAdapter.of(mockCache).keySet()).containsExactlyInAnyOrder(1, 2, 3);

    verify(mockCache, times(1)).keys();
  }

  @Test
  public void mergeWithExistingKeyMapsSuppliedValue() {

    Cache<String, Integer> mockCache = mock(Cache.class);

    when(mockCache.get(any())).thenReturn(1);

    assertThat(CacheToMapAdapter.of(mockCache).merge("sum", 2, Integer::sum)).isEqualTo(3);

    verify(mockCache, times(2)).get(eq("sum"));
    verify(mockCache, times(1)).put(eq("sum"), eq(3));
  }

  @Test
  public void mergeWithNonExistingKeyMapsSuppliedValue() {

    Cache<String, String> mockCache = mock(Cache.class);

    when(mockCache.get(any())).thenReturn(null);

    assertThat(CacheToMapAdapter.of(mockCache).merge("key", "test",
      (oldValue, newValue) -> newValue.toUpperCase())).isEqualTo("test");

    verify(mockCache, times(2)).get(eq("key"));
    verify(mockCache, times(1)).put(eq("key"), eq("test"));
  }

  @Test
  public void mergeWithNonExistingKeyRemovesMapping() {

    Cache<String, String> mockCache = mock(Cache.class);

    when(mockCache.get(any())).thenReturn("test");

    assertThat(CacheToMapAdapter.of(mockCache).merge("key", "trash", (oldValue, newValue) -> null)).isNull();

    verify(mockCache, times(2)).get(eq("key"));
    verify(mockCache, times(1)).evict(eq("key"));
  }

  @Test
  public void putCallsCachePutAndReturnsOldValue() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.get(any())).thenReturn("oldValue");

    assertThat(CacheToMapAdapter.of(mockCache).put("key", "newValue")).isEqualTo("oldValue");

    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, times(1)).put(eq("key"), eq("newValue"));
  }

  @Test
  public void putAllCallsCacheFrom() {

    Cache mockCache = mock(Cache.class);

    Map mockMap = mock(Map.class);

    CacheToMapAdapter.of(mockCache).putAll(mockMap);

    verify(mockCache, times(1)).from(eq(mockMap));
  }

  @Test
  public void putIfAbsentCallsCacheGetAndPutIfAbsent() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.get(any())).thenReturn("test");

    assertThat(CacheToMapAdapter.of(mockCache).putIfAbsent("key", "value")).isEqualTo("test");

    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, times(1)).putIfAbsent(eq("key"), eq("value"));
  }

  @Test
  public void removeCallsCacheGetAndEvict() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.get(any())).thenReturn("test");

    assertThat(CacheToMapAdapter.of(mockCache).remove("key")).isEqualTo("test");

    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, times(1)).evict(eq("key"));
  }

  @Test
  public void removeWithExistingKeyAndEqualValue() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.contains(any())).thenReturn(true);
    when(mockCache.get(any())).thenReturn("test");

    assertThat(CacheToMapAdapter.of(mockCache).remove("key", "test")).isTrue();

    verify(mockCache, atMost(1)).contains(eq("key"));
    verify(mockCache, times(2)).get(eq("key"));
    verify(mockCache, times(1)).evict(eq("key"));
  }

  @Test
  public void removeWithExistingKeyAndUnequalValue() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.contains(any())).thenReturn(true);
    when(mockCache.get(any())).thenReturn("value");

    assertThat(CacheToMapAdapter.of(mockCache).remove("key", "test")).isFalse();

    verify(mockCache, atMost(1)).contains(eq("key"));
    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, never()).evict(any());
  }

  @Test
  public void removeWithNonExistingKeyValue() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.contains(any())).thenReturn(false);

    assertThat(CacheToMapAdapter.of(mockCache).remove("key", null)).isFalse();

    verify(mockCache, times(1)).contains(eq("key"));
    verify(mockCache, atMost(1)).get(eq("key"));
    verify(mockCache, never()).evict(any());
  }

  @Test
  public void replaceWithExistingKey() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.contains(any())).thenReturn(true);
    when(mockCache.get(any())).thenReturn("test");

    assertThat(CacheToMapAdapter.of(mockCache).replace("key", "value")).isEqualTo("test");

    verify(mockCache, atMost(1)).contains(eq("key"));
    verify(mockCache, times(2)).get(eq("key"));
    verify(mockCache, times(1)).put(eq("key"), eq("value"));
  }

  @Test
  public void replaceWithNonExistingKey() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.contains(any())).thenReturn(false);
    when(mockCache.get(any())).thenReturn(null);

    assertThat(CacheToMapAdapter.of(mockCache).replace("key", "value")).isNull();

    verify(mockCache, times(1)).contains(eq("key"));
    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, never()).put(any(), any());
  }

  @Test
  public void replaceWithExistingKeyAndEqualValue() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.contains(any())).thenReturn(true);
    when(mockCache.get(any())).thenReturn("test");

    assertThat(CacheToMapAdapter.of(mockCache).replace("key", "test", "value")).isTrue();

    verify(mockCache, atMost(1)).contains(eq("key"));
    verify(mockCache, times(2)).get(eq("key"));
    verify(mockCache, times(1)).put(eq("key"), eq("value"));
  }

  @Test
  public void replaceWithExistingKeyAndUnequalValue() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.contains(any())).thenReturn(true);
    when(mockCache.get(any())).thenReturn("test");

    assertThat(CacheToMapAdapter.of(mockCache).replace("key", "tset", "value")).isFalse();

    verify(mockCache, atMost(1)).contains(eq("key"));
    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, never()).put(any(), any());
  }

  @Test
  public void replaceWithNonExistingKeyAndValue() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.contains(any())).thenReturn(false);
    when(mockCache.get(any())).thenReturn(null);

    assertThat(CacheToMapAdapter.of(mockCache).replace("key", null, "value")).isFalse();

    verify(mockCache, times(1)).contains(eq("key"));
    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, never()).put(any(), any());
  }

  @Test
  public void replaceAllIsCorrect() {

    Map<Integer, Integer> map = MapBuilder.<Integer, Integer>newHashMap()
      .put(1, 1)
      .put(2, 2)
      .build();

    CacheToMapAdapter.of(MapToCacheAdapter.of(map)).replaceAll((key, oldValue) -> key + key);

    assertThat(map).hasSize(2);
    assertThat(map).containsEntry(1, 2);
    assertThat(map).containsEntry(2, 4);
  }

  @Test
  public void sizeIsEqualToCacheSize() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.size()).thenReturn(101);

    assertThat(CacheToMapAdapter.of(mockCache)).hasSize(101);

    verify(mockCache, times(1)).size();
  }

  @Test
  public void valuesReturnsAllCacheValues() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.iterator()).thenReturn(ArrayUtils.asIterator("one", "two", "three"));
    when(mockCache.size()).thenReturn(3);

    Map<Object, Object> map = CacheToMapAdapter.of(mockCache);

    assertThat(map).isNotNull();

    Collection<Object> mapValues = map.values();

    assertThat(mapValues).isNotNull();
    assertThat(mapValues).hasSize(3);
    assertThat(mapValues).containsExactlyInAnyOrder("one", "two", "three");

    verify(mockCache, times(1)).iterator();
    verify(mockCache, atLeastOnce()).size();
  }

  @Test
  public void constructCacheEntry() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.contains(eq("key"))).thenReturn(true);
    when(mockCache.get(eq("key"))).thenReturn("value");

    CacheEntry cacheEntry = CacheEntry.of("key", mockCache);

    assertThat(cacheEntry).isNotNull();
    assertThat(cacheEntry.getKey()).isEqualTo("key");
    assertThat(cacheEntry.getValue()).isEqualTo("value");

    verify(mockCache, times(1)).contains(eq("key"));
    verify(mockCache, times(1)).get(eq("key"));
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructCacheEntryWithNullKey() {

    Cache mockCache = mock(Cache.class);

    try {
      CacheEntry.of(null, mockCache);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Key is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyNoInteractions(mockCache);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructCacheEntryWithNullCache() {

    try {
      CacheEntry.of("key", null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Cache is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalStateException.class)
  public void constructCacheEntryWithNonExistingCacheKey() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.contains(any(Comparable.class))).thenReturn(false);
    when(mockCache.getName()).thenReturn("TestCache");

    try {
      CacheEntry.of("key", mockCache);
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("Key [key] is not contained in Cache [TestCache]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockCache, times(1)).contains(eq("key"));
      verify(mockCache, times(1)).getName();
    }
  }

  @Test
  public void cacheEntrySetValue() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.contains(eq("key"))).thenReturn(true);
    when(mockCache.get(eq("key"))).thenReturn("value");

    assertThat(CacheEntry.of("key", mockCache).setValue("test")).isEqualTo("value");

    verify(mockCache, times(1)).contains(eq("key"));
    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, times(1)).put(eq("key"), eq("test"));
  }

  @Test
  public void cacheEntryIsEqualToMapEntry() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.contains(any(Comparable.class))).thenReturn(true);

    assertThat(CacheEntry.of("key", mockCache)).isEqualTo(MapUtils.newMapEntry("key", "test"));
  }

  @Test
  public void cacheEntryHashCode() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.contains(any(Comparable.class))).thenReturn(true);

    CacheEntry cacheEntry = CacheEntry.of("key", mockCache);

    assertThat(cacheEntry.hashCode()).isNotZero();
    assertThat(cacheEntry.hashCode()).isNotEqualTo("key".hashCode());
  }

  @Test
  public void cacheEntryToString() {

    Cache mockCache = mock(Cache.class);

    when(mockCache.contains(any(Comparable.class))).thenReturn(true);
    when(mockCache.get(eq("key"))).thenReturn("value");

    assertThat(CacheEntry.of("key", mockCache).toString()).isEqualTo("key = value");

    verify(mockCache, times(1)).contains(eq("key"));
    verify(mockCache, times(1)).get(eq("key"));
  }
}
