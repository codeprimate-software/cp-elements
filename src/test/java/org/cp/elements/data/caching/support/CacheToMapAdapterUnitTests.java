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
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.IntStream;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.caching.Cache;
import org.cp.elements.lang.Sourced;
import org.cp.elements.test.annotation.IntegrationTest;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.MapBuilder;
import org.cp.elements.util.stream.StreamUtils;

import org.assertj.core.api.InstanceOfAssertFactories;

/**
 * Unit Tests for {@link CacheToMapAdapter}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.caching.support.CacheToMapAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unchecked")
public class CacheToMapAdapterUnitTests {

  @Test
  public void constructCacheToMapAdapterWithCache() {

    Cache<?, ?> mockCache = mock(Cache.class);

    CacheToMapAdapter<?, ?> adapter = new CacheToMapAdapter<>(mockCache);

    assertThat(adapter).isNotNull();
    assertThat(adapter.getCache()).isSameAs(mockCache);

    verifyNoInteractions(mockCache);
  }

  @Test
  public void constructCacheToMapAdapterWithNullCache() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new CacheToMapAdapter<>(null))
      .withMessage("Cache is required")
      .withNoCause();
  }

  @Test
  public void ofCacheIsCorrect() {

    Cache<?, ?> mockCache = mock(Cache.class);

    Map<?, ?> map = CacheToMapAdapter.of(mockCache);

    assertThat(map).isNotNull();

    verifyNoInteractions(mockCache);
  }

  @Test
  public void mapClearCallsCacheClear() {

    Cache<?, ?> mockCache = mock(Cache.class);

    CacheToMapAdapter.of(mockCache).clear();

    verify(mockCache, times(1)).clear();
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void mapContainsKeyCallsCacheContains() {

    Cache<String, Object> mockCache = mock(Cache.class);

    doReturn(true).when(mockCache).contains(eq("testKey"));

    assertThat(CacheToMapAdapter.of(mockCache).containsKey("testKey")).isTrue();
    assertThat(CacheToMapAdapter.of(mockCache).containsKey("mockKey")).isFalse();

    verify(mockCache, times(2)).contains(anyString());
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void mapContainsKeyWithNullKeyIsNullSafeReturnsFalse() {

    Cache<String, Object> mockCache = mock(Cache.class);

    assertThat(CacheToMapAdapter.of(mockCache).containsKey(null)).isFalse();

    verifyNoInteractions(mockCache);
  }

  @Test
  @SuppressWarnings("all")
  public void mapEntrySetIsCorrect() {

    Cache<Integer, Object> mockCache = mock(Cache.class);

    Set<Integer> keys = CollectionUtils.asSet(1, 2, 3);

    doAnswer(invocation -> keys.contains(invocation.getArgument(0))).when(mockCache).contains(anyInt());
    doReturn(keys).when(mockCache).keys();
    doReturn(3L).when(mockCache).size();

    Set<Map.Entry<Integer, Object>> mapEntrySet = CacheToMapAdapter.of(mockCache).entrySet();

    assertThat(mapEntrySet).isNotNull();
    assertThat(mapEntrySet.size()).isEqualTo(3);

    verify(mockCache, times(1)).size();
    verifyNoMoreInteractions(mockCache);

    Iterator<Map.Entry<Integer, Object>> mapEntryIterator = mapEntrySet.iterator();

    assertThat(mapEntryIterator).isNotNull();
    assertThat(mapEntryIterator).hasNext();

    AtomicInteger count = new AtomicInteger(0);

    StreamUtils.stream(mapEntryIterator).filter(Objects::nonNull).forEach(mapEntry -> {

      assertThat(mapEntry).isInstanceOf(Sourced.class);

      assertThat(mapEntry)
        .asInstanceOf(InstanceOfAssertFactories.type(Sourced.class))
        .extracting(Sourced::getSource)
        .isSameAs(mockCache);

      assertThat(mapEntry.getKey()).isEqualTo(count.incrementAndGet());
    });

    assertThat(count.get()).isEqualTo(3);

    verify(mockCache, times(1)).keys();

    IntStream.range(1, 4).forEach(key ->
      verify(mockCache, times(1)).contains(eq(key)));

    verifyNoMoreInteractions(mockCache);
  }

  @Test
  @IntegrationTest
  public void mapEntrySetIteratorNextWithNoMoreCacheEntries() {

    Map<String, Object> cacheMap =
      CacheToMapAdapter.of(MapToCacheAdapter.of(Collections.singletonMap("key", "value")));

    assertThat(cacheMap).isNotNull();
    assertThat(cacheMap).hasSize(1);

    Set<Map.Entry<String, Object>> entrySet = cacheMap.entrySet();

    assertThat(entrySet).isNotNull();
    assertThat(entrySet).hasSize(1);

    Iterator<Map.Entry<String, Object>> entrySetIterator = entrySet.iterator();

    assertThat(entrySetIterator).isNotNull();
    assertThat(entrySetIterator).hasNext();

    Map.Entry<String, Object> entry = entrySetIterator.next();

    assertThat(entry).isNotNull();
    assertThat(entry.getKey()).isEqualTo("key");
    assertThat(entry.getValue()).isEqualTo("value");

    assertThat(entrySetIterator).isExhausted();

    assertThatExceptionOfType(NoSuchElementException.class)
      .isThrownBy(entrySetIterator::next)
      .withMessage("No more cache entries")
      .withNoCause();
  }

  @Test
  @IntegrationTest
  public void mapEntrySetIteratorRemoveIsNotSupported() {

    Map<String, Object> cacheMap =
      CacheToMapAdapter.of(MapToCacheAdapter.of(Collections.singletonMap("key", "value")));

    assertThat(cacheMap).isNotNull();
    assertThat(cacheMap).hasSize(1);

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> cacheMap.entrySet().iterator().remove())
      .withNoCause();
  }

  @Test
  public void mapGetCallsCacheGet() {

    Cache<String, Object> mockCache = mock(Cache.class);

    doReturn("mockValue").when(mockCache).get(any());

    assertThat(CacheToMapAdapter.of(mockCache).get("testKey")).isEqualTo("mockValue");

    verify(mockCache, times(1)).get(eq("testKey"));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void mapIsEmptyCallsCacheIsEmpty() {

    Cache<?, ?> mockCache = mock(Cache.class);

    doReturn(false).when(mockCache).isEmpty();

    assertThat(CacheToMapAdapter.of(mockCache).isEmpty()).isFalse();

    verify(mockCache, times(1)).isEmpty();
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void mapKeySetCallsCacheKeys() {

    Cache<Integer, Object> mockCache = mock(Cache.class);

    doReturn(CollectionUtils.asSet(1, 2, 3)).when(mockCache).keys();

    assertThat(CacheToMapAdapter.of(mockCache).keySet()).containsExactlyInAnyOrder(1, 2, 3);

    verify(mockCache, times(1)).keys();
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void mapPutCallsCacheGetAndPut() {

    Cache<String, Object> mockCache = mock(Cache.class);

    doReturn("testValue").when(mockCache).getAndPut(any(), any());

    assertThat(CacheToMapAdapter.of(mockCache).put("testKey", "mockValue")).isEqualTo("testValue");

    verify(mockCache, times(1)).getAndPut(eq("testKey"), eq("mockValue"));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void mapPutAllCallsCacheFromMap() {

    Map<Integer, Object> mockMap = mock(Map.class);
    Cache<Integer, Object> mockCache = mock(Cache.class);

    CacheToMapAdapter.of(mockCache).putAll(mockMap);

    verify(mockCache, times(1)).from(eq(mockMap));
    verifyNoMoreInteractions(mockCache);
    verifyNoInteractions(mockMap);
  }

  @Test
  public void mapPutIfAbsentCallsCachePutIfAbsent() {

    Cache<Integer, Object> mockCache = mock(Cache.class);

    doReturn("testValue").when(mockCache).putIfAbsent(any(), any());

    assertThat(CacheToMapAdapter.of(mockCache).putIfAbsent(1, "mockValue")).isEqualTo("testValue");

    verify(mockCache, times(1)).putIfAbsent(eq(1), eq("mockValue"));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void mapRemoveCallsCacheGetAndEvict() {

    Cache<Integer, Object> mockCache = mock(Cache.class);

    doReturn("testValue").when(mockCache).getAndEvict(any());

    assertThat(CacheToMapAdapter.of(mockCache).remove(1)).isEqualTo("testValue");

    verify(mockCache, times(1)).getAndEvict(eq(1));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void mapSizeCallsCacheSize() {

    Cache<?, ?> mockCache = mock(Cache.class);

    doReturn(64L).when(mockCache).size();

    assertThat(CacheToMapAdapter.of(mockCache).size()).isEqualTo(64);

    verify(mockCache, times(1)).size();
    verifyNoMoreInteractions(mockCache);
  }

  // Additional (default) Map Operations

  @Test
  public void compute() {

    Cache<String, Integer> mockCache = mock(Cache.class);

    doReturn(4).when(mockCache).get(any());

    assertThat(CacheToMapAdapter.of(mockCache)
      .compute("key", (key, value) -> value != null ? value * value : 0)).isEqualTo(16);

    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, times(1)).getAndPut(eq("key"), eq(16));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void computeIfAbsentWhenAbsent() {

    Cache<Integer, Object> mockCache = mock(Cache.class);

    doReturn(null).when(mockCache).get(any());

    assertThat(CacheToMapAdapter.of(mockCache).computeIfAbsent(2, key -> key * key)).isEqualTo(4);

    verify(mockCache, times(1)).get(eq(2));
    verify(mockCache, times(1)).getAndPut(eq(2), eq(4));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void computeIfAbsentWhenPresent() {

    Cache<Integer, Object> mockCache = mock(Cache.class);

    doReturn(0).when(mockCache).get(any());

    assertThat(CacheToMapAdapter.of(mockCache).computeIfAbsent(2, key -> key * key)).isEqualTo(0);

    verify(mockCache, times(1)).get(eq(2));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void computeIfPresentWhenAbsent() {

    Cache<Integer, Object> mockCache = mock(Cache.class);

    doReturn(null).when(mockCache).get(any());

    assertThat(CacheToMapAdapter.of(mockCache).computeIfPresent(2, (key, value) -> key * key)).isNull();

    verify(mockCache, times(1)).get(eq(2));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void computeIfPresentWhenPresent() {

    Cache<Integer, Object> mockCache = mock(Cache.class);

    doReturn(0).when(mockCache).get(any());

    assertThat(CacheToMapAdapter.of(mockCache).computeIfPresent(2, (key, value) -> key * key)).isEqualTo(4);

    verify(mockCache, times(1)).get(eq(2));
    verify(mockCache, times(1)).getAndPut(eq(2), eq(4));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  @IntegrationTest
  public void containsValueForExistingValueReturnsTrue() {

    assertThat(CacheToMapAdapter.of(MapToCacheAdapter.of(Collections.singletonMap("key", "mockValue")))
      .containsValue("mockValue")).isTrue();
  }

  @Test
  @IntegrationTest
  public void containsValueForNonExistingValueReturnsFalse() {

    assertThat(CacheToMapAdapter.of(MapToCacheAdapter.of(Collections.singletonMap("key", "testValue")))
      .containsValue("mockValue")).isFalse();

    assertThat(CacheToMapAdapter.of(MapToCacheAdapter.of(Collections.<String, Object>emptyMap()))
      .containsValue("mockValue")).isFalse();
  }

  @Test
  @IntegrationTest
  public void equalsIsCorrect() {

    Map<Integer, Object> mapOne = Collections.singletonMap(1, "test");
    Map<Integer, Object> mapTwo = Collections.singletonMap(1, "test");

    assertThat(mapOne).isEqualTo(mapTwo);
    assertThat(mapOne).isNotSameAs(mapTwo);
    assertThat(CacheToMapAdapter.of(MapToCacheAdapter.of(mapOne))).isEqualTo(mapTwo);
  }

  @Test
  @IntegrationTest
  public void forEachIsCorrect() {

    Map<Integer, Object> map = MapBuilder.<Integer, Object>newHashMap()
      .put(1, "A")
      .put(2, "B")
      .build();

    AtomicInteger sum = new AtomicInteger(0);

    CacheToMapAdapter.of(MapToCacheAdapter.of(map)).forEach((key, value) -> sum.addAndGet(key));

    assertThat(sum.get()).isEqualTo(3);
  }

  @Test
  @IntegrationTest
  public void getOrDefaultWithExistingKeyReturnsValue() {

    assertThat(CacheToMapAdapter.of(MapToCacheAdapter.of(Collections.singletonMap("key", "value")))
      .getOrDefault("key", "default")).isEqualTo("value");
  }

  @Test
  @IntegrationTest
  public void getOrDefaultWithNonExistingKeyReturnsDefaultValue() {

    assertThat(CacheToMapAdapter.of(MapToCacheAdapter.of(Collections.singletonMap("key", null)))
      .getOrDefault("nonExistingKey", "default")).isEqualTo("default");

    assertThat(CacheToMapAdapter.of(MapToCacheAdapter.of(Collections.<String, Object>emptyMap()))
      .getOrDefault("key", "default")).isEqualTo("default");
  }

  @Test
  @IntegrationTest
  public void hashCodeIsCorrect() {

    Map<String, Object> map = Collections.singletonMap("key", "value");

    Cache<String, Object> cache = MapToCacheAdapter.of(map);

    Map<String, Object> cacheMap = CacheToMapAdapter.of(cache);

    assertThat(cacheMap.hashCode()).isNotZero();
    assertThat(cacheMap.hashCode()).isNotEqualTo(cache.hashCode());
    assertThat(cacheMap.hashCode()).isNotEqualTo(map.hashCode());
    assertThat(cache.hashCode()).isNotEqualTo(map.hashCode());
  }

  @Test
  @IntegrationTest
  public void mergeWithExistingKeyMapsSuppliedValue() {

    Cache<String, Integer> mockCache = mock(Cache.class);

    doReturn(1).when(mockCache).get(any());

    assertThat(CacheToMapAdapter.of(mockCache).merge("sum", 2, Integer::sum)).isEqualTo(3);

    verify(mockCache, times(1)).get(eq("sum"));
    verify(mockCache, times(1)).getAndPut(eq("sum"), eq(3));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  @IntegrationTest
  public void mergeWithExistingKeyAndResultOfNullRemovesMapping() {

    Cache<String, String> mockCache = mock(Cache.class);

    doReturn("old").when(mockCache).get(any());

    assertThat(CacheToMapAdapter.of(mockCache).merge("key", "new", (oldValue, newValue) -> null)).isNull();

    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, times(1)).getAndEvict(eq("key"));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  @IntegrationTest
  public void mergeWithNonExistingKeyMapsSuppliedValue() {

    Cache<String, String> mockCache = mock(Cache.class);

    doReturn(null).when(mockCache).get(any());

    assertThat(CacheToMapAdapter.of(mockCache).merge("key", "test",
      (oldValue, newValue) -> newValue.toUpperCase())).isEqualTo("test");

    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, times(1)).getAndPut(eq("key"), eq("test"));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void removeWithNonExistingKeyValue() {

    Cache<String, Object> mockCache = mock(Cache.class);

    doReturn(null).when(mockCache).get(any());
    doReturn(false).when(mockCache).contains(any());

    assertThat(CacheToMapAdapter.of(mockCache).remove("key", null)).isFalse();

    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, times(1)).contains(eq("key"));
    verify(mockCache, never()).getAndEvict(any());
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void removeWithExistingKeyAndEqualValue() {

    Cache<String, Object> mockCache = mock(Cache.class);

    doReturn("test").when(mockCache).get(any());

    assertThat(CacheToMapAdapter.of(mockCache).remove("key", "test")).isTrue();

    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, never()).contains(any());
    verify(mockCache, times(1)).getAndEvict(eq("key"));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void removeWithExistingKeyAndUnequalValue() {

    Cache<String, Object> mockCache = mock(Cache.class);

    doReturn("mock").when(mockCache).get(any());

    assertThat(CacheToMapAdapter.of(mockCache).remove("key", "test")).isFalse();

    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, never()).contains(any());
    verify(mockCache, never()).getAndEvict(any());
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void replaceWithExistingKeyAndValue() {

    Cache<String, Object> mockCache = mock(Cache.class);

    doReturn("test").when(mockCache).get(any());
    doReturn("test").when(mockCache).getAndPut(any(), any());

    assertThat(CacheToMapAdapter.of(mockCache).replace("key", "mock")).isEqualTo("test");

    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, never()).contains(any());
    verify(mockCache, times(1)).getAndPut(eq("key"), eq("mock"));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void replaceWithExistingKeyAndNullValue() {

    Cache<String, Object> mockCache = mock(Cache.class);

    doReturn(null).when(mockCache).get(any());
    doReturn(true).when(mockCache).contains(any());
    doReturn("mock").when(mockCache).getAndPut(any(), any());

    assertThat(CacheToMapAdapter.of(mockCache).replace("key", "test")).isEqualTo("mock");

    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, times(1)).contains(eq("key"));
    verify(mockCache, times(1)).getAndPut(eq("key"), eq("test"));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void replaceWithNonExistingKey() {

    Cache<String, Object> mockCache = mock(Cache.class);

    doReturn(null).when(mockCache).get(any());
    doReturn(false).when(mockCache).contains(any());

    assertThat(CacheToMapAdapter.of(mockCache).replace("key", "test")).isNull();

    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, times(1)).contains(eq("key"));
    verify(mockCache, never()).getAndPut(any(), any());
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void replaceWithExistingKeyAndEqualValue() {

    Cache<String, Object> mockCache = mock(Cache.class);

    doReturn("test").when(mockCache).get(any());

    assertThat(CacheToMapAdapter.of(mockCache).replace("key", "test", "mock")).isTrue();

    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, never()).contains(any());
    verify(mockCache, times(1)).getAndPut(eq("key"), eq("mock"));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void replaceWithExistingKeyAndUnequalValue() {

    Cache<String, Object> mockCache = mock(Cache.class);

    doReturn("test").when(mockCache).get(any());

    assertThat(CacheToMapAdapter.of(mockCache).replace("key", "tst", "mock")).isFalse();

    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, never()).contains(any());
    verify(mockCache, never()).getAndPut(any(), any());
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void replaceWithExistingKeyMappedToNull() {

    Cache<String, Object> mockCache = mock(Cache.class);

    doReturn(null).when(mockCache).get(any());
    doReturn(true).when(mockCache).contains(any());

    assertThat(CacheToMapAdapter.of(mockCache).replace("key", null, "mock")).isTrue();

    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, times(1)).contains(eq("key"));
    verify(mockCache, times(1)).getAndPut(eq("key"), eq("mock"));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void replaceWithNonExistingKeyAndValue() {

    Cache<String, Object> mockCache = mock(Cache.class);

    doReturn(null).when(mockCache).get(any());
    doReturn(false).when(mockCache).contains(any());

    assertThat(CacheToMapAdapter.of(mockCache).replace("key", null, "test")).isFalse();

    verify(mockCache, times(1)).get(eq("key"));
    verify(mockCache, times(1)).contains(eq("key"));
    verify(mockCache, never()).getAndPut(any(), any());
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  @IntegrationTest
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
  public void valuesReturnsAllCacheValues() {

    Cache<Integer, Object> mockCache = mock(Cache.class);

    Map<Integer, Object> map = MapBuilder.<Integer, Object>newHashMap()
      .put(1, "A")
      .put(2, "B")
      .put(3, "C")
      .build();

    doReturn(map.keySet()).when(mockCache).keys();
    doReturn(Integer.valueOf(map.size()).longValue()).when(mockCache).size();

    doAnswer(invocation -> map.containsKey(invocation.getArgument(0, Integer.class)))
      .when(mockCache).contains(any());

    doAnswer(invocation -> map.get(invocation.getArgument(0, Integer.class)))
      .when(mockCache).get(any());

    Map<Integer, Object> cacheMap = CacheToMapAdapter.of(mockCache);

    assertThat(cacheMap).isNotNull();
    assertThat(cacheMap).hasSize(map.size());

    Collection<Object> mapValues = cacheMap.values();

    assertThat(mapValues).isNotNull();
    assertThat(mapValues).hasSize(map.size());
    assertThat(mapValues).containsExactlyInAnyOrder("A", "B", "C");

    verify(mockCache, times(1)).keys();
    verify(mockCache, times(3)).size();

    map.keySet().forEach(key -> {
      verify(mockCache, times(1)).contains(eq(key));
      verify(mockCache, times(1)).get(eq(key));
    });

    verifyNoMoreInteractions(mockCache);
  }
}
