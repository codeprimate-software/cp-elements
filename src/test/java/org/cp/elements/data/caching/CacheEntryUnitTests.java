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
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Test;

/**
 * Unit Tests for {@link Cache.Entry}.
 *
 * @author John Blum
 * @see java.util.Map
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.caching.Cache
 * @see org.cp.elements.data.caching.Cache.Entry
 * @since 1.0.0
 */
public class CacheEntryUnitTests {

  @Test
  public void copiesCacheEntry() {

    AtomicInteger value = new AtomicInteger(1);

    Cache<?, ?> mockCache = mock(Cache.class);

    Cache.Entry<?, ?> mockCacheEntry = mock(Cache.Entry.class);

    doReturn("testKey").when(mockCacheEntry).getKey();
    doReturn(mockCache).when(mockCacheEntry).getSource();
    doAnswer(invocation -> value.updateAndGet(it -> it * 2)).when(mockCacheEntry).getValue();

    Cache.Entry<?, ?> cacheEntryCopy = Cache.Entry.copy(mockCacheEntry);

    assertThat(cacheEntryCopy).isNotNull();
    assertThat(cacheEntryCopy.getKey()).isEqualTo("testKey");
    assertThat(cacheEntryCopy.getKey()).isEqualTo("testKey");
    assertThat(cacheEntryCopy.getSource()).isEqualTo(mockCache);
    assertThat(cacheEntryCopy.getValue()).isEqualTo(2);
    assertThat(mockCacheEntry.getValue()).isEqualTo(4);
    assertThat(mockCacheEntry.getValue()).isEqualTo(8);
    assertThat(mockCacheEntry.getValue()).isEqualTo(16);
    assertThat(cacheEntryCopy.getValue()).isEqualTo(2);
    assertThat(cacheEntryCopy.materialize()).isSameAs(cacheEntryCopy);

    verify(mockCacheEntry, times(2)).getKey();
    verify(mockCacheEntry, times(1)).getSource();
    verify(mockCacheEntry, times(4)).getValue();
    verifyNoMoreInteractions(mockCacheEntry);
  }

  @Test
  public void copyNullCacheEntry() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Cache.Entry.copy(null))
      .withMessage("Cache.Entry to copy is required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void fromMapEntry() {

    Map.Entry<String, ?> mockMapEntry = mock(Map.Entry.class);

    doReturn("mockKey").when(mockMapEntry).getKey();
    doReturn(1, 2, 4).when(mockMapEntry).getValue();

    Cache.Entry<String, ?> cacheEntry = Cache.Entry.from(mockMapEntry);

    assertThat(cacheEntry).isNotNull();
    assertThat(cacheEntry.getKey()).isEqualTo("mockKey");
    assertThat(cacheEntry.getKey()).isEqualTo("mockKey");
    assertThat(cacheEntry.getSource()).isNull();
    assertThat(cacheEntry.getValue()).isEqualTo(1);
    assertThat(mockMapEntry.getValue()).isEqualTo(2);
    assertThat(mockMapEntry.getValue()).isEqualTo(4);
    assertThat(cacheEntry.getValue()).isEqualTo(1);
    assertThat(cacheEntry.materialize()).isSameAs(cacheEntry);

    verify(mockMapEntry, times(2)).getKey();
    verify(mockMapEntry, times(3)).getValue();
    verifyNoMoreInteractions(mockMapEntry);
  }

  @Test
  public void fromNullMapEntry() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Cache.Entry.from(null))
      .withMessage("Map.Entry to convert is required")
      .withNoCause();
  }

  @Test
  public void getDefaultSourceThrowsCacheNotFoundException() {

    Cache.Entry<?, ?> mockCacheEntry = mock(Cache.Entry.class);

    doCallRealMethod().when(mockCacheEntry).getSource();

    assertThatExceptionOfType(CacheNotFoundException.class)
      .isThrownBy(mockCacheEntry::getSource)
      .withMessage("Cache cannot be determined")
      .withNoCause();

    verify(mockCacheEntry, times(1)).getSource();
    verifyNoMoreInteractions(mockCacheEntry);
  }

  @Test
  public void getDefaultValueIsNull() {

    Cache.Entry<?, ?> mockCacheEntry = mock(Cache.Entry.class);

    doCallRealMethod().when(mockCacheEntry).getValue();

    assertThat(mockCacheEntry.getValue()).isNull();

    verify(mockCacheEntry, times(1)).getValue();
    verifyNoMoreInteractions(mockCacheEntry);
  }

  @Test
  public void defaultMaterializeThrowsUnsupportedOperationException() {

    Cache.Entry<?, ?> mockCacheEntry = mock(Cache.Entry.class);

    doCallRealMethod().when(mockCacheEntry).materialize();

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(mockCacheEntry::materialize)
      .withMessage("Cache.Entry cannot be materialized")
      .withNoCause();

    verify(mockCacheEntry, times(1)).materialize();
    verifyNoMoreInteractions(mockCacheEntry);
  }
}
