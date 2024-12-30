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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.withSettings;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.IntStream;

import org.junit.jupiter.api.Test;

import org.mockito.quality.Strictness;

/**
 * Unit Tests for {@link Cache.Entry}.
 *
 * @author John Blum
 * @see java.util.Map
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.caching.Cache
 * @see org.cp.elements.data.caching.Cache.Entry
 * @since 1.0.0
 */
public class CacheEntryUnitTests {

  @SuppressWarnings("unchecked")
  private <KEY extends Comparable<KEY>, VALUE> Cache.Entry<KEY, VALUE> mockCacheEntry(KEY key, VALUE value) {

    Cache.Entry<KEY, VALUE> mockCacheEntry = mock(Cache.Entry.class, withSettings().strictness(Strictness.LENIENT));

    doReturn(key).when(mockCacheEntry).getKey();
    doReturn(value).when(mockCacheEntry).getValue();

    return mockCacheEntry;
  }

  @Test
  public void copyCacheEntry() {

    AtomicInteger value = new AtomicInteger(1);

    Cache<?, ?> mockCache = mock(Cache.class);

    Cache.Entry<?, ?> mockCacheEntry = mockCacheEntry("testKey", null);

    doReturn(mockCache).when(mockCacheEntry).getSource();
    doAnswer(invocation -> value.updateAndGet(it -> it * 2)).when(mockCacheEntry).getValue();

    Cache.Entry<?, ?> cacheEntryCopy = Cache.Entry.copy(mockCacheEntry);

    assertThat(cacheEntryCopy).isNotNull();
    assertThat(cacheEntryCopy).isNotSameAs(mockCacheEntry);
    assertThat(cacheEntryCopy.getKey()).isEqualTo("testKey");
    assertThat(cacheEntryCopy.getKey()).isEqualTo("testKey");
    assertThat(cacheEntryCopy.getSource()).isEqualTo(mockCache);
    assertThat(cacheEntryCopy.getValue()).isEqualTo(2);
    assertThat(mockCacheEntry.getValue()).isEqualTo(4);
    assertThat(mockCacheEntry.getValue()).isEqualTo(8);
    assertThat(mockCacheEntry.getValue()).isEqualTo(16);
    assertThat(mockCacheEntry.getValue()).isEqualTo(32);
    assertThat(mockCacheEntry.getValue()).isEqualTo(64);
    assertThat(cacheEntryCopy.getValue()).isEqualTo(2);
    assertThat(cacheEntryCopy.materialize()).isSameAs(cacheEntryCopy);

    verify(mockCacheEntry, times(1)).getKey();
    verify(mockCacheEntry, times(1)).getSource();
    verify(mockCacheEntry, times(6)).getValue();
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
  public void setValueOnCacheEntryCopyThrowsUnsupportedOperationException() {

    Cache.Entry<?, ?> mockCacheEntry = mockCacheEntry("testKey", null);
    Cache.Entry<?, ?> cacheEntryCopy = Cache.Entry.copy(mockCacheEntry);

    assertThat(cacheEntryCopy).isNotNull();
    assertThat(cacheEntryCopy).isNotSameAs(mockCacheEntry);

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> cacheEntryCopy.setValue(null))
      .withMessage("Value for Cache.Entry(testKey) copy cannot be set")
      .withNoCause();

    verify(mockCacheEntry, times(1)).getKey();
    verify(mockCacheEntry, times(1)).getValue();
    verify(mockCacheEntry, times(1)).getSource();
    verifyNoMoreInteractions(mockCacheEntry);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void fromMapEntry() {

    Map.Entry<String, ?> mockMapEntry = mock(Map.Entry.class);

    doReturn("MockKey").when(mockMapEntry).getKey();
    doReturn(1, 2, 4).when(mockMapEntry).getValue();

    Cache.Entry<String, ?> cacheEntry = Cache.Entry.from(mockMapEntry);

    assertThat(cacheEntry).isNotNull();
    assertThat(cacheEntry).isNotSameAs(mockMapEntry);
    assertThat(cacheEntry.getKey()).isEqualTo("MockKey");
    assertThat(cacheEntry.getKey()).isEqualTo("MockKey");
    assertThat(cacheEntry.getSource()).isNull();
    assertThat(cacheEntry.getValue()).isEqualTo(1);
    assertThat(mockMapEntry.getValue()).isEqualTo(2);
    assertThat(mockMapEntry.getValue()).isEqualTo(4);
    assertThat(cacheEntry.getValue()).isEqualTo(1);
    assertThat(cacheEntry.materialize()).isSameAs(cacheEntry);

    verify(mockMapEntry, times(1)).getKey();
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
  @SuppressWarnings("unchecked")
  public void setValueOnCacheEntrySourcedFromMapThrowsUnsupportedOperationException() {

    Map.Entry<String, Object> mockMapEntry = mock(Map.Entry.class);

    doReturn("mockKey").when(mockMapEntry).getKey();

    Cache.Entry<String, Object> cacheEntry = Cache.Entry.from(mockMapEntry);

    assertThat(cacheEntry).isNotNull();
    assertThat(cacheEntry).isNotSameAs(mockMapEntry);

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> cacheEntry.setValue(null))
      .withMessage("Value for Cache.Entry(mockKey) sourced from Map.Entry cannot be set")
      .withNoCause();

    verify(mockMapEntry, times(1)).getKey();
    verify(mockMapEntry, times(1)).getValue();
    verifyNoMoreInteractions(mockMapEntry);
  }

  @Test
  public void defaultGetSourceThrowsCacheNotFoundException() {

    Cache.Entry<?, ?> mockCacheEntry = mockCacheEntry(null, null);

    doCallRealMethod().when(mockCacheEntry).getSource();

    assertThatExceptionOfType(CacheNotFoundException.class)
      .isThrownBy(mockCacheEntry::getSource)
      .withMessage("Cache cannot be determined")
      .withNoCause();

    verify(mockCacheEntry, times(1)).getSource();
    verifyNoMoreInteractions(mockCacheEntry);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void defaultGetValueReturnsValueFromCacheGet() {

    Cache<String, Object> mockCache = mock(Cache.class);

    Cache.Entry<String, Object> mockCacheEntry = mockCacheEntry("mockKey", null);

    doReturn("test").when(mockCache).get(any());
    doReturn(mockCache).when(mockCacheEntry).getSource();
    doCallRealMethod().when(mockCacheEntry).getValue();

    assertThat(mockCacheEntry.getValue()).isEqualTo("test");

    verify(mockCache, times(1)).get(eq("mockKey"));
    verify(mockCacheEntry, times(1)).getValue();
    verify(mockCacheEntry, times(1)).getSource();
    verify(mockCacheEntry, times(1)).getKey();
    verifyNoMoreInteractions(mockCache, mockCacheEntry);
  }

  @Test
  public void defaultGetValueWithDefaultValueReturnsCacheEntryValue() {

    Cache.Entry<Integer, Object> mockCacheEntry = mockCacheEntry(null, "test");

    doCallRealMethod().when(mockCacheEntry).getValue(any());

    assertThat(mockCacheEntry.getValue("mock")).isEqualTo("test");

    verify(mockCacheEntry, times(1)).getValue(eq("mock"));
    verify(mockCacheEntry, times(1)).getValue();
    verifyNoMoreInteractions(mockCacheEntry);
  }

  @Test
  public void defaultGetValueWithDefaultValueReturnsDefaultValue() {

    Cache.Entry<Integer, Object> mockCacheEntry = mockCacheEntry(null, null);

    doCallRealMethod().when(mockCacheEntry).getValue(any());

    assertThat(mockCacheEntry.getValue("mock")).isEqualTo("mock");

    verify(mockCacheEntry, times(1)).getValue(eq("mock"));
    verify(mockCacheEntry, times(1)).getValue();
    verifyNoMoreInteractions(mockCacheEntry);
  }

  @Test
  public void defaultGetOptionalValue() {

    Cache.Entry<Integer, Object> mockCacheEntry = mockCacheEntry(null, "test");

    doCallRealMethod().when(mockCacheEntry).getOptionalValue();

    Optional<Object> optionalValue = mockCacheEntry.getOptionalValue();

    assertThat(optionalValue).isNotNull();
    assertThat(optionalValue).isPresent();
    assertThat(optionalValue).hasValue("test");

    verify(mockCacheEntry, times(1)).getOptionalValue();
    verify(mockCacheEntry, times(1)).getValue();
    verifyNoMoreInteractions(mockCacheEntry);
  }

  @Test
  public void defaultGetOptionalValueIsEmtpyWhenCacheEntryValueIsNull() {

    Cache.Entry<Integer, Object> mockCacheEntry = mockCacheEntry(null, null);

    doCallRealMethod().when(mockCacheEntry).getOptionalValue();

    Optional<Object> optionalValue = mockCacheEntry.getOptionalValue();

    assertThat(optionalValue).isNotNull();
    assertThat(optionalValue).isNotPresent();

    verify(mockCacheEntry, times(1)).getOptionalValue();
    verify(mockCacheEntry, times(1)).getValue();
    verifyNoMoreInteractions(mockCacheEntry);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void defaultSetValueSetsKeyWithCachePut() {

    Cache<String, Object> mockCache = mock(Cache.class);

    Cache.Entry<String, Object> mockCacheEntry = mockCacheEntry("testKey", null);

    doReturn(mockCache).when(mockCacheEntry).getSource();
    doCallRealMethod().when(mockCacheEntry).setValue(any());

    mockCacheEntry.setValue("mockValue");

    verify(mockCache, times(1)).put(eq("testKey"), eq("mockValue"));
    verify(mockCacheEntry, times(1)).setValue(eq("mockValue"));
    verify(mockCacheEntry, times(1)).getSource();
    verify(mockCacheEntry, times(1)).getKey();
    verifyNoMoreInteractions(mockCache, mockCacheEntry);
  }

  @Test
  @SuppressWarnings({ "all" })
  public void defaultCompareTo() {

    Cache.Entry<String, Object> mockCacheEntryOne = mockCacheEntry("mockKey", null);
    Cache.Entry<String, Object> mockCacheEntryTwo = mockCacheEntry("testKey", null);

    doCallRealMethod().when(mockCacheEntryOne).compareTo(any());
    doCallRealMethod().when(mockCacheEntryTwo).compareTo(any());

    assertThat(mockCacheEntryOne.compareTo(mockCacheEntryOne)).isZero();
    assertThat(mockCacheEntryOne.compareTo(mockCacheEntryTwo)).isLessThan(0);
    assertThat(mockCacheEntryTwo.compareTo(mockCacheEntryOne)).isGreaterThan(0);

    verify(mockCacheEntryOne, times(1)).compareTo(eq(mockCacheEntryOne));
    verify(mockCacheEntryOne, times(1)).compareTo(eq(mockCacheEntryTwo));
    verify(mockCacheEntryOne, times(4)).getKey();
    verify(mockCacheEntryTwo, times(1)).compareTo(eq(mockCacheEntryOne));
    verify(mockCacheEntryTwo, times(2)).getKey();
    verifyNoMoreInteractions(mockCacheEntryOne, mockCacheEntryTwo);
  }

  @Test
  public void defaultMaterializeCopiesCacheEntry() {

    Cache<?, ?> mockCache = mock(Cache.class);

    Cache.Entry<?, ?> mockCacheEntry = mockCacheEntry("TestKey", "MockValue");

    doReturn(mockCache).when(mockCacheEntry).getSource();
    doCallRealMethod().when(mockCacheEntry).materialize();

    Cache.Entry<?, ?> materializedCacheEntry = mockCacheEntry.materialize();

    assertThat(materializedCacheEntry).isNotNull();
    assertThat(materializedCacheEntry).isNotSameAs(mockCacheEntry);

    IntStream.range(0, 10).forEach(count -> {
      assertThat(materializedCacheEntry.getSource()).isSameAs(mockCache);
      assertThat(materializedCacheEntry.getKey()).isEqualTo("TestKey");
      assertThat(materializedCacheEntry.getValue()).isEqualTo("MockValue");
    });

    verify(mockCacheEntry, times(1)).materialize();
    verify(mockCacheEntry, times(1)).getSource();
    verify(mockCacheEntry, times(1)).getKey();
    verify(mockCacheEntry, times(1)).getValue();
    verifyNoMoreInteractions(mockCacheEntry);
    verifyNoInteractions(mockCache);
  }
}
