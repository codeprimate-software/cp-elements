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
package org.cp.elements.data.caching.provider;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.ArgumentMatchers.isNotNull;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Arrays;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentMap;
import java.util.function.BiFunction;

import org.junit.Before;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;

import org.cp.elements.test.annotation.SubjectUnderTest;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.MapBuilder;

import org.mockito.Mock;
import org.mockito.Mock.Strictness;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * Unit Tests for {@link ConcurrentMapCache}.
 *
 * @author John Blum
 * @see java.util.Map
 * @see java.util.concurrent.ConcurrentMap
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @see org.cp.elements.data.caching.provider.ConcurrentMapCache
 * @since 1.0.0
 */
@SuppressWarnings({ "rawtypes", "unchecked" })
@RunWith(MockitoJUnitRunner.class)
public class ConcurrentMapCacheUnitTests {

  @Mock(strictness = Strictness.LENIENT)
  private ConcurrentMap mockMap;

  @Spy
  @SubjectUnderTest
  private ConcurrentMapCache concurrentMapCache;

  @Before
  public void newConcurrentMapCache() {
    doReturn(this.mockMap).when(this.concurrentMapCache).getConcurrentMap();
  }

  @SuppressWarnings("unchecked")
  private <K extends Comparable<K>, V> Map.Entry<K, V> mockMapEntry(K key, V value) {

    Map.Entry<K, V> mockMapEntry = mock(Map.Entry.class);

    doReturn(key).when(mockMapEntry).getKey();
    doReturn(value).when(mockMapEntry).getValue();

    return mockMapEntry;
  }

  @Test
  public void lockIsNull() {

    assertThat(this.concurrentMapCache.getLock()).isNull();

    this.concurrentMapCache.setLock(new Object());

    assertThat(this.concurrentMapCache.getLock()).isNull();

    verifyNoInteractions(this.mockMap);
  }

  @Test
  public void isEmptyCallsMapIsEmpty() {

    doReturn(this.mockMap).when(this.concurrentMapCache).getConcurrentMap();
    doReturn(false).when(this.mockMap).isEmpty();

    assertThat(this.concurrentMapCache.isEmpty()).isFalse();

    verify(this.concurrentMapCache, times(1)).getConcurrentMap();
    verify(this.mockMap, times(1)).isEmpty();
    verifyNoMoreInteractions(this.mockMap);
  }

  @Test
  public void clearCallsMapClear() {

    this.concurrentMapCache.clear();

    verify(this.concurrentMapCache, times(1)).getConcurrentMap();
    verify(this.mockMap, times(1)).clear();
    verifyNoMoreInteractions(this.mockMap);
  }

  @Test
  public void containsCallsMapContainsKey() {

    doReturn(true).when(this.mockMap).containsKey(isNotNull());

    assertThat(this.concurrentMapCache.contains(1)).isTrue();
    assertThat(this.concurrentMapCache.contains("mockKey")).isTrue();
    assertThat(this.concurrentMapCache.contains("testKey")).isTrue();

    verify(this.concurrentMapCache, times(3)).getConcurrentMap();
    verify(this.mockMap, times(1)).containsKey(eq(1));
    verify(this.mockMap, times(1)).containsKey(eq("mockKey"));
    verify(this.mockMap, times(1)).containsKey(eq("testKey"));
    verifyNoMoreInteractions(this.mockMap);
  }

  @Test
  public void containsNonExistingKeyReturnsFalse() {

    doReturn(false).when(this.mockMap).containsKey(any());

    assertThat(this.concurrentMapCache.contains(0)).isFalse();
    assertThat(this.concurrentMapCache.contains("nonExistingKey")).isFalse();

    verify(this.concurrentMapCache, times(2)).getConcurrentMap();
    verify(this.mockMap, times(1)).containsKey(eq(0));
    verify(this.mockMap, times(1)).containsKey(eq("nonExistingKey"));
    verifyNoMoreInteractions(this.mockMap);
  }

  @Test
  public void containsNullKeyIsNullSafeReturnsFalse() {

    assertThat(this.concurrentMapCache.contains(null)).isFalse();

    verifyNoInteractions(this.mockMap);
  }

  @Test
  public void evictCallsMapRemove() {

    this.concurrentMapCache.evict("testKey");

    verify(this.concurrentMapCache, times(1)).getConcurrentMap();
    verify(this.mockMap, times(1)).remove(eq("testKey"));
    verifyNoMoreInteractions(this.mockMap);
  }

  @Test
  public void evictWithNullKeyIsNullSafe() {

    this.concurrentMapCache.evict(null);

    verifyNoInteractions(this.mockMap);
  }

  @Test
  public void fromMapIsCorrect() {

    Map mockMap = mock(Map.class);

    doReturn(false).when(mockMap).isEmpty();
    doReturn(CollectionUtils.asSet(mockMapEntry(1, "A"), mockMapEntry(2, "B")))
      .when(mockMap).entrySet();

    this.concurrentMapCache.from(mockMap);

    verify(this.concurrentMapCache, times(1)).getConcurrentMap();
    verify(mockMap, times(1)).isEmpty();
    verify(mockMap, times(1)).entrySet();
    //verify(this.mockMap, times(1)).putAll(eq(mockMap));
    verify(this.mockMap, times(1))
      .putAll(eq(MapBuilder.newHashMap().put(1, "A").put(2, "B").build()));
    verifyNoMoreInteractions(this.mockMap, mockMap);
  }

  @Test
  public void fromMapWithNullEntriesNullKeysAndNullValuesIsNullSafe() {

    Map mockMap = mock(Map.class);

    doReturn(false).when(mockMap).isEmpty();
    doReturn(CollectionUtils.asSet(mockMapEntry(1, "A"), null, mockMapEntry(null, "B"), null, null,
      mockMapEntry(3, null))).when(mockMap).entrySet();

    this.concurrentMapCache.from(mockMap);

    verify(this.concurrentMapCache, times(1)).getConcurrentMap();
    verify(mockMap, times(1)).isEmpty();
    verify(mockMap, times(1)).entrySet();
    verify(this.mockMap, times(1)).putAll(eq(MapBuilder.newHashMap().put(1, "A").build()));
    verifyNoMoreInteractions(this.mockMap, mockMap);
  }

  @Test
  public void fromEmptyMapIsCorrect() {

    Map mockMap = mock(Map.class);

    doReturn(true).when(mockMap).isEmpty();

    this.concurrentMapCache.from(mockMap);

    verify(mockMap, times(1)).isEmpty();
    verify(mockMap, never()).entrySet();
    verifyNoMoreInteractions(mockMap);
    verifyNoInteractions(this.mockMap);
  }

  @Test
  public void fromNullMapIsNullSafe() {

    this.concurrentMapCache.from(null);

    verifyNoInteractions(this.mockMap);
  }

  @Test
  public void getCallsMapGet() {

    doReturn("A").when(this.mockMap).get(any());

    assertThat(this.concurrentMapCache.get("testKey")).isEqualTo("A");

    verify(this.concurrentMapCache, times(1)).getConcurrentMap();
    verify(this.mockMap, times(1)).get(eq("testKey"));
    verifyNoMoreInteractions(this.mockMap);
  }

  @Test
  public void getCallsMapGetWithNonExistingKeyIsSafeReturnsNull() {

    doReturn("A").when(this.mockMap).get(eq("testKey"));

    assertThat(this.concurrentMapCache.get("mockKey")).isNull();

    verify(this.concurrentMapCache, times(1)).getConcurrentMap();
    verify(this.mockMap, times(1)).get(eq("mockKey"));
    verifyNoMoreInteractions(this.mockMap);
  }


  @Test
  public void getWithNullKeyIsNullSafeReturnsNull() {

    assertThat(this.concurrentMapCache.get(null)).isNull();

    verifyNoInteractions(this.mockMap);
  }

  @Test
  public void getNameWhenNamed() {

    Arrays.asList("TestCache", "  ", "", null, "MockCache").forEach(name -> {
      assertThat(this.concurrentMapCache.named(name)).isSameAs(this.concurrentMapCache);
      assertThat(this.concurrentMapCache.getName()).isEqualTo(name);
    });

    verifyNoInteractions(this.mockMap);
  }

  @Test
  public void getNameWhenUnsetReturnsNull() {

    assertThat(this.concurrentMapCache.getName()).isNull();

    verifyNoInteractions(this.mockMap);
  }

  @Test
  public void keysFromEmptyCacheCallsMapKeySet() {

    Set<Comparable<?>> keys = this.concurrentMapCache.keys();

    assertThat(keys).isNotNull();
    assertThat(keys).isEmpty();

    verify(this.concurrentMapCache, times(1)).getConcurrentMap();
    verify(this.mockMap, times(1)).keySet();
    verifyNoMoreInteractions(this.mockMap);
  }

  @Test
  public void keysFromNonEmptyCacheCallsMapKeySet() {

    doReturn(CollectionUtils.asSet(1, 2)).when(this.mockMap).keySet();

    assertThat(this.concurrentMapCache.keys()).containsExactlyInAnyOrder(1, 2);

    verify(this.concurrentMapCache, times(1)).getConcurrentMap();
    verify(this.mockMap, times(1)).keySet();
    verifyNoMoreInteractions(this.mockMap);
  }

  @Test
  public void putCallsMapPut() {

    this.concurrentMapCache.put(1, "A");
    this.concurrentMapCache.put(2, "B");

    verify(this.concurrentMapCache, times(2)).getConcurrentMap();
    verify(this.mockMap, times(1)).put(eq(1), eq("A"));
    verify(this.mockMap, times(1)).put(eq(2), eq("B"));
    verifyNoMoreInteractions(this.mockMap);
  }

  @Test
  public void putWithNullKey() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> this.concurrentMapCache.put(null, "value"))
      .withMessage("Key is required")
      .withNoCause();

    verifyNoInteractions(this.mockMap);
  }

  @Test
  public void putWithNullValue() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> this.concurrentMapCache.put("key", null))
      .withMessage("Value is required")
      .withNoCause();

    verifyNoInteractions(this.mockMap);
  }

  @Test
  public void putIfAbsentCallsMapPutIfAbsent() {

    this.concurrentMapCache.putIfAbsent(1, "A");

    verify(this.concurrentMapCache, times(1)).getConcurrentMap();
    verify(this.mockMap, times(1)).putIfAbsent(eq(1), eq("A"));
    verifyNoMoreInteractions(this.mockMap);
  }

  @Test
  public void putIfAbsentWithNullKey() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> this.concurrentMapCache.putIfAbsent(null, "value"))
      .withMessage("Key is required")
      .withNoCause();

    verifyNoInteractions(this.mockMap);
  }

  @Test
  public void putIfAbsentWithNullValue() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> this.concurrentMapCache.putIfAbsent("key", null))
      .withMessage("Value is required")
      .withNoCause();

    verifyNoInteractions(this.mockMap);
  }

  @Test
  public void putIfPresentCallsMapComputeIfPresent() {

    doAnswer(invocation -> {

      Object key = invocation.getArgument(0);
      BiFunction function = invocation.getArgument(1);

      return function.apply(key, "mock");

    }).when(this.mockMap).computeIfPresent(any(), isA(BiFunction.class));

    assertThat(this.concurrentMapCache.putIfPresent(1, "test")).isEqualTo("mock");

    verify(this.concurrentMapCache, times(1)).getConcurrentMap();
    verify(this.mockMap, times(1)).computeIfPresent(eq(1), isNotNull());
    verifyNoMoreInteractions(this.mockMap);
  }
  @Test
  public void putIfPresentWithNullKeyReturnsNull() {

    assertThat(this.concurrentMapCache.putIfPresent(null, "value")).isNull();

    verifyNoInteractions(this.mockMap);
  }

  @Test
  public void putIfPresentWithNullValueThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> this.concurrentMapCache.putIfPresent("key", null))
      .withMessage("Value is required")
      .withNoCause();

    verifyNoInteractions(this.mockMap);
  }

  @Test
  public void sizeCallsMapSize() {

    doReturn(69).when(this.mockMap).size();

    assertThat(this.concurrentMapCache.size()).isEqualTo(69L);

    verify(this.mockMap, times(1)).size();
    verifyNoMoreInteractions(this.mockMap);
  }

  @Test
  public void toMapReturnsConcurrentMap() {

    assertThat(this.concurrentMapCache.toMap()).isNotNull();

    verify(this.concurrentMapCache, times(1)).getConcurrentMap();
    verifyNoInteractions(this.mockMap);
  }

  @Test
  public void toMapWhenCacheIsPopulated() {

    doReturn(MapBuilder.newConcurrentMap()
        .put(1, "A")
        .put(2, "B")
        .put(3, "C")
        .build())
      .when(this.concurrentMapCache).getConcurrentMap();

    Map map = this.concurrentMapCache.toMap();

    assertThat(map).isNotNull();
    assertThat(map).hasSize(3);
    assertThat(map).containsEntry(1, "A");
    assertThat(map).containsEntry(2, "B");
    assertThat(map).containsEntry(3, "C");
  }

  @Test
  public void concurrentCachePutThenGetIsSuccessful() throws Throwable {
    TestFramework.runOnce(new CachePutThenCacheGetConcurrentTestCase());
  }

  @SuppressWarnings("unused")
  public static class CachePutThenCacheGetConcurrentTestCase extends MultithreadedTestCase {

    private ConcurrentMapCache concurrentMapCache;

    @Override
    public void initialize() {

      super.initialize();

      this.concurrentMapCache = new ConcurrentMapCache();

      assertThat(this.concurrentMapCache).isEmpty();
    }

    public void threadOne() {

      Thread.currentThread().setName("Cache Put Thread");

      this.concurrentMapCache.put("key", "test");
    }

    public void threadTwo() {

      Thread.currentThread().setName("Cache Get Thread");

      waitForTick(1);

      assertThat(this.concurrentMapCache.get("key")).isEqualTo("test");
    }

    @Override
    public void finish() {
      this.concurrentMapCache.clear();
      this.concurrentMapCache = null;
    }
  }
}
