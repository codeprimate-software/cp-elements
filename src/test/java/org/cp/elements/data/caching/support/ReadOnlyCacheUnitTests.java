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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Collections;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.caching.Cache;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.test.annotation.SubjectUnderTest;

/**
 * Unit Tests for {@link ReadOnlyCache}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.data.caching.support.ReadOnlyCache
 * @since 1.0.0
 */
@SuppressWarnings({ "rawtypes", "unchecked" })
public class ReadOnlyCacheUnitTests {

  @SubjectUnderTest
  private final ReadOnlyCache cache = spy(new ReadOnlyCache() { });

  @Test
  public void clearThrowsUnsupportedOperationException() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(this.cache::clear)
      .withMessage("Clear is not supported")
      .withNoCause();
  }

  @Test
  public void evictThrowsUnsupportedOperationException() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.cache.evict("key"))
      .withMessage("Eviction is not supported")
      .withNoCause();
  }

  @Test
  public void fromMapThrowsUnsupportedOperationException() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.cache.from(Collections.emptyMap()))
      .withMessage("From Map is not supported")
      .withNoCause();
  }

  @Test
  public void getReturnsNull() {
    assertThat(this.cache.get("key")).isNull();
  }

  @Test
  public void getAndEvictThrowsUnsupportedOperationException() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.cache.getAndEvict("key"))
      .withMessage("Get and Evict is not supported")
      .withNoCause();
  }

  @Test
  public void getAndEvictWithExpectedValueThrowsUnsupportedOperationException() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.cache.getAndEvict("key", "expectedValue"))
      .withMessage("Get and Evict is not supported")
      .withNoCause();
  }

  @Test
  public void getAndPutThrowsUnsupportedOperationException() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.cache.getAndPut("key", "newValue"))
      .withMessage("Get and Put is not supported")
      .withNoCause();
  }

  @Test
  public void getAndReplaceThrowsUnsupportedOperationException() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.cache.getAndReplace("key", "newValue"))
      .withMessage("Get and Replace is not supported")
      .withNoCause();
  }

  @Test
  public void getAndReplaceWithExpectedValueThrowsUnsupportedOperationException() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.cache.getAndReplace("key", "expectedValue", "newValue"))
      .withMessage("Get and Replace is not supported")
      .withNoCause();
  }

  @Test
  public void getEntryReturnsWrappedCacheEntry() {

    Cache<?, ?> mockCache = mock(Cache.class);

    Cache.Entry<?, ?> mockCacheEntry = mock(Cache.Entry.class);

    doReturn("testKey").when(mockCacheEntry).getKey();
    doReturn(mockCache).when(mockCacheEntry).getSource();
    doReturn("mockValue").when(mockCacheEntry).getValue();
    doReturn(mockCacheEntry).when(mockCacheEntry).materialize();
    doReturn(mockCacheEntry).when(this.cache).doGetEntry(any());

    Cache.Entry<?, ?> cacheEntry = this.cache.getEntry("testKey");

    assertThat(cacheEntry).isNotNull();
    assertThat(cacheEntry).isNotSameAs(mockCacheEntry);
    assertThat(cacheEntry.getKey()).isEqualTo("testKey");
    assertThat(cacheEntry.getSource()).isEqualTo(mockCache);
    assertThat(cacheEntry.getValue()).isEqualTo("mockValue");
    assertThat(cacheEntry.materialize()).isEqualTo(mockCacheEntry);

    verify(mockCacheEntry, times(1)).getKey();
    verify(mockCacheEntry, times(1)).getSource();
    verify(mockCacheEntry, times(1)).getValue();
    verify(mockCacheEntry, times(1)).materialize();
    verifyNoMoreInteractions(mockCacheEntry);
    verifyNoInteractions(mockCache);
  }

  @Test
  public void getEntrySetValueThrowsUnsupportedOperationException() {

    Cache.Entry<?, ?> mockCacheEntry = mock(Cache.Entry.class);

    doReturn("mockKey").when(mockCacheEntry).getKey();
    doReturn(mockCacheEntry).when(this.cache).doGetEntry(any());

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.cache.getEntry("mockKey").setValue("test"))
      .withMessage("Setting the value of Cache.Entry(mockKey) is not supported")
      .withNoCause();

    verify(mockCacheEntry, times(1)).getKey();
    verifyNoMoreInteractions(mockCacheEntry);
  }

  @Test
  public void getEntryWhenKeyIsNotPresentReturnsNull() {

    doReturn(null).when(this.cache).doGetEntry(any());

    assertThat(this.cache.getEntry("testKey")).isNull();
  }

  @Test
  public void iteratorRemoveThrowsUnsupportedOperationExceptionReturnsEmptyIterator() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.cache.iterator().remove())
      .withNoCause();
  }

  @Test
  public void putThrowsUnsupportedOperationException() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.cache.put("key", "value"))
      .withMessage("Put is not supported")
      .withNoCause();
  }

  @Test
  public void putCacheEntryThrowsUnsupportedOperationException() {

    Cache.Entry<?, ?> mockCacheEntry = mock(Cache.Entry.class);

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.cache.put(mockCacheEntry))
      .withMessage("Put is not supported")
      .withNoCause();

    verifyNoInteractions(mockCacheEntry);
  }

  @Test
  public void putEntityThrowsUnsupportedOperationException() {

    Identifiable<?> mockEntity = mock(Identifiable.class);

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.cache.put(mockEntity))
      .withMessage("Put is not supported")
      .withNoCause();

    verifyNoInteractions(mockEntity);
  }

  @Test
  public void putIfAbsentThrowsUnsupportedOperationException() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.cache.putIfAbsent("key", "value"))
      .withMessage("Put is not supported")
      .withNoCause();
  }

  @Test
  public void putIfAbsentWithEntityThrowsUnsupportedOperationException() {

    Identifiable<?> mockEntity = mock(Identifiable.class);

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.cache.putIfAbsent(mockEntity))
      .withMessage("Put is not supported")
      .withNoCause();

    verifyNoInteractions(mockEntity);
  }

  @Test
  public void putIfPresentThrowsUnsupportedOperationException() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.cache.putIfPresent("key", "value"))
      .withMessage("Put is not supported")
      .withNoCause();
  }

  @Test
  public void putIfPresentWithEntityThrowsUnsupportedOperationException() {

    Identifiable<?> mockEntity = mock(Identifiable.class);

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.cache.putIfPresent(mockEntity))
      .withMessage("Put is not supported")
      .withNoCause();

    verifyNoInteractions(mockEntity);
  }
}
