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
import static org.mockito.ArgumentMatchers.any;
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

import org.cp.elements.data.caching.AbstractCache.AbstractEntry;
import org.cp.elements.data.caching.AbstractCache.AttachedCacheEntry;
import org.cp.elements.data.caching.AbstractCache.SimpleCacheEntry;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Unit Tests for {@link AbstractCache.AbstractEntry}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.caching.AbstractCache.AbstractEntry
 * @since 1.0.0
 */
public class AbstractCacheEntryUnitTests {

  @Test
  @SuppressWarnings("unchecked")
  public void constructAbstractEntry() {

    Cache<String, Object> mockCache = mock(Cache.class);

    AbstractEntry<String, Object> cacheEntry = new TestEntry<>(mockCache, "testKey");

    assertThat(cacheEntry).isNotNull();
    assertThat(cacheEntry.getKey()).isEqualTo("testKey");
    assertThat(cacheEntry.getSource()).isSameAs(mockCache);

    verifyNoInteractions(mockCache);
  }

  @Test
  public void constructAbstractEntryWithNullCache() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new TestEntry<>(null, "mockKey"))
      .withMessage("Cache used as the source of this Entry is required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void constructAbstractEntryWithNullKey() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new TestEntry<String, Object>(mock(Cache.class), null))
      .withMessage("Key is required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void entryIsAttached() {

    Cache<Integer, String> mockCache = mock(Cache.class);

    AtomicReference<String> cacheEntryValue = new AtomicReference<>("A");

    doAnswer(invocation -> ObjectUtils.equals(invocation.getArgument(0), 1) ? cacheEntryValue.get() : null)
      .when(mockCache).get(any());

    AbstractEntry<Integer, String> cacheEntry = new TestEntry<>(mockCache, 1);

    assertThat(cacheEntry.getSource()).isSameAs(mockCache);
    assertThat(cacheEntry.getKey()).isEqualTo(1);
    assertThat(cacheEntry.getValue()).isEqualTo("A");

    cacheEntryValue.set("B");

    assertThat(cacheEntry.getValue()).isEqualTo("B");

    verify(mockCache, times(2)).get(eq(1));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void materializingAbstractEntryReturnsCopy() {

    Cache<Integer, String> mockCache = mock(Cache.class);

    doReturn("A", "B", "C", "D").when(mockCache).get(any());

    AbstractEntry<Integer, String> cacheEntry = new TestEntry<>(mockCache, 1);

    assertThat(cacheEntry).isNotNull();
    assertThat(cacheEntry.getSource()).isSameAs(mockCache);
    assertThat(cacheEntry.getKey()).isEqualTo(1);
    assertThat(cacheEntry.getValue()).isEqualTo("A");
    assertThat(cacheEntry.getValue()).isEqualTo("B");

    Cache.Entry<Integer, String> cacheEntryCopy = cacheEntry.materialize();

    assertThat(cacheEntryCopy).isNotNull();
    assertThat(cacheEntryCopy).isNotSameAs(cacheEntry);
    assertThat(cacheEntryCopy.getSource()).isSameAs(mockCache);
    assertThat(cacheEntryCopy.getKey()).isEqualTo(1);
    assertThat(cacheEntryCopy.getValue()).isEqualTo("C");
    assertThat(cacheEntryCopy.getValue()).isEqualTo("C");
    assertThat(cacheEntryCopy.materialize()).isSameAs(cacheEntryCopy);
    assertThat(cacheEntry.getValue()).isEqualTo("D");
    assertThat(cacheEntryCopy.getValue()).isEqualTo("C");

    verify(mockCache, times(4)).get(eq(1));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void fromCacheAndKeyToAttachedCacheEntry() {

    Cache<Integer, String> mockCache = mock(Cache.class);

    doReturn("A", "B").when(mockCache).get(any());

    AttachedCacheEntry<Integer, String> cacheEntry = AttachedCacheEntry.from(mockCache, 1);

    assertThat(cacheEntry).isNotNull();
    assertThat(cacheEntry.getSource()).isSameAs(mockCache);
    assertThat(cacheEntry.getKey()).isEqualTo(1);
    assertThat(cacheEntry.getValue()).isEqualTo("A");
    assertThat(cacheEntry.getValue()).isEqualTo("B");

    verify(mockCache, times(2)).get(eq(1));
    verifyNoMoreInteractions(mockCache);
  }

  @Test
  public void fromKeyAndValueToSimpleCacheEntry() {

    SimpleCacheEntry<Integer, String> cacheEntry = SimpleCacheEntry.of(1, "test");

    assertThat(cacheEntry).isNotNull();
    assertThat(cacheEntry.getSource()).isNull();
    assertThat(cacheEntry.getKey()).isEqualTo(1);
    assertThat(cacheEntry.getValue()).isEqualTo("test");
    assertThat(cacheEntry.getValue()).isEqualTo("test");
    assertThat(cacheEntry.materialize()).isSameAs(cacheEntry);
  }

  @Test
  public void fromNullKeyAndValueToSimpleCacheEntry() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SimpleCacheEntry.of(null, "test"))
      .withMessage("Key is required")
      .withNoCause();
  }

  protected static class TestEntry<KEY extends Comparable<KEY>, VALUE> extends AbstractCache.AbstractEntry<KEY, VALUE> {

    protected TestEntry(@NotNull Cache<KEY, VALUE> cache, @NotNull KEY key) {
      super(cache, key);
    }
  }
}
