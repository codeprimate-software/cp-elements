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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.concurrent.atomic.AtomicReference;

import org.junit.Test;

import org.cp.elements.data.caching.Cache;
import org.cp.elements.data.caching.support.CacheToMapAdapter.CacheMapEntry;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.util.MapUtils;

/**
 * Unit Tests for {@link CacheToMapAdapter.CacheMapEntry}
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.caching.support.CacheToMapAdapter.CacheMapEntry
 * @since 1.0.0
 */
@SuppressWarnings("unchecked")
public class CacheMapEntryUnitTests {

  @Test
  public void constructNewCacheMapEntry() {

    Cache<String, Object> mockCache = mock(Cache.class);

    doReturn(true).when(mockCache).contains(eq("key"));
    doReturn("value").when(mockCache).get(eq("key"));

    CacheMapEntry<String, Object> cacheMapEntry = new CacheMapEntry<>(mockCache, "key");

    assertThat(cacheMapEntry).isNotNull();
    assertThat(cacheMapEntry.getSource()).isEqualTo(mockCache);
    assertThat(cacheMapEntry.getKey()).isEqualTo("key");
    assertThat(cacheMapEntry.getValue()).isEqualTo("value");

    verify(mockCache, times(1)).contains(eq("key"));
    verify(mockCache, times(1)).get(eq("key"));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void constructNewCacheMapEntryWithNullCache() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new CacheMapEntry<>(null, "key"))
      .withMessage("Cache is required")
      .withNoCause();
  }

  @Test
  public void constructNewCacheMapEntryWithNullKey() {

    Cache<?, ?> mockCache = mock(Cache.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new CacheMapEntry<>(mockCache, null))
      .withMessage("Key is required")
      .withNoCause();

    verifyNoInteractions(mockCache);
  }

  @Test
  public void constructNewCacheMapEntryWithNonExistingCacheKey() {

    Cache<String, ?> mockCache = mock(Cache.class);

    doReturn(false).when(mockCache).contains(any());
    doReturn("TestCache").when(mockCache).getName();

    assertThatIllegalStateException()
      .isThrownBy(() -> new CacheMapEntry<>(mockCache, "nonExistingKey"))
      .withMessage("Key [nonExistingKey] is not contained in Cache [TestCache]")
      .withNoCause();

    verify(mockCache, times(1)).contains(eq("nonExistingKey"));
    verify(mockCache, times(1)).getName();
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void cacheMapEntryOf() {

    Cache<String, Object> mockCache = mock(Cache.class);

    doReturn(true).when(mockCache).contains(eq("key"));
    doReturn("value").when(mockCache).get(eq("key"));

    CacheMapEntry<String, Object> cacheMapEntry = CacheMapEntry.of(mockCache, "key");

    assertThat(cacheMapEntry).isNotNull();
    assertThat(cacheMapEntry.getSource()).isSameAs(mockCache);
    assertThat(cacheMapEntry.getKey()).isEqualTo("key");
    assertThat(cacheMapEntry.getValue()).isEqualTo("value");

    verify(mockCache, times(1)).contains(eq("key"));
    verify(mockCache, times(1)).get(eq("key"));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void cacheMapEntryGetValue() {

    AtomicReference<Object> cacheValue = new AtomicReference<>("A");

    Cache<Integer, Object> mockCache = mock(Cache.class);

    doAnswer(invocation -> ObjectUtils.equals(1, invocation.getArgument(0)) ? cacheValue.get() : null)
      .when(mockCache).get(anyInt());

    doReturn(true).when(mockCache).contains(eq(1));

    CacheMapEntry<Integer, Object> cacheMapEntry = CacheMapEntry.of(mockCache, 1);

    assertThat(cacheMapEntry).isNotNull();
    assertThat(cacheMapEntry.getSource()).isEqualTo(mockCache);
    assertThat(cacheMapEntry.getKey()).isEqualTo(1);
    assertThat(cacheMapEntry.getValue()).isEqualTo("A");

    cacheValue.set("B");

    assertThat(cacheMapEntry.getValue()).isEqualTo("B");

    verify(mockCache, times(1)).contains(eq(1));
    verify(mockCache, times(2)).get(eq(1));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void cacheMapEntrySetValue() {

    Cache<Integer, Object> mockCache = mock(Cache.class);

    doReturn(true).when(mockCache).contains(eq(1));
    doReturn("test").when(mockCache).getAndPut(any(), any());

    assertThat(CacheMapEntry.of(mockCache, 1).setValue("mock")).isEqualTo("test");

    verify(mockCache, times(1)).contains(eq(1));
    verify(mockCache, times(1)).getAndPut(eq(1), eq("mock"));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void cacheMapEntryComparedToMapEntry() {

    Cache<String, Object> mockCache = mock(Cache.class);

    doReturn(true).when(mockCache).contains(any());

    CacheMapEntry<String, Object> cacheMapEntry = CacheMapEntry.of(mockCache, "testKey");

    assertThat(cacheMapEntry).isNotNull();
    assertThat(cacheMapEntry.getKey()).isEqualTo("testKey");

    assertThat(cacheMapEntry.compareTo(MapUtils.newMapEntry("mockKey", "value"))).isGreaterThan(0);
    assertThat(cacheMapEntry.compareTo(MapUtils.newMapEntry("zipKey", "A"))).isLessThan(0);
    assertThat(cacheMapEntry.compareTo(MapUtils.newMapEntry("testKey", null))).isZero();

    verify(mockCache, times(1)).contains(eq("testKey"));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void cacheMapEntryIsEqualToMapEntry() {

    Cache<String, Object> mockCache = mock(Cache.class);

    doReturn(true).when(mockCache).contains(any());

    assertThat(CacheMapEntry.of(mockCache, "key")).isEqualTo(MapUtils.newMapEntry("key", "test"));

    verify(mockCache, times(1)).contains(eq("key"));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void cacheMapEntryHashCode() {

    Cache<String, Object> mockCache = mock(Cache.class);

    doReturn(true).when(mockCache).contains(any());

    CacheMapEntry<String, Object> cacheMapEntry = CacheMapEntry.of(mockCache, "key");

    assertThat(cacheMapEntry.hashCode()).isNotZero();
    assertThat(cacheMapEntry.hashCode()).isNotEqualTo("key".hashCode());

    verify(mockCache, times(1)).contains(eq("key"));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void cacheMapEntryToString() {

    Cache<String, Object> mockCache = mock(Cache.class);

    doReturn(true).when(mockCache).contains(any());
    doReturn("value").when(mockCache).get(eq("key"));

    assertThat(CacheMapEntry.of(mockCache, "key").toString()).isEqualTo("key = value");

    verify(mockCache, times(1)).contains(eq("key"));
    verify(mockCache, times(1)).get(eq("key"));
    verifyNoMoreInteractions(mockCache);
  }
}
