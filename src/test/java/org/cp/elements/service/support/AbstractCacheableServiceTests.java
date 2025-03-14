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
package org.cp.elements.service.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Optional;
import java.util.function.Supplier;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cp.elements.data.caching.Cache;
import org.cp.elements.data.caching.provider.ConcurrentMapCache;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Unit Tests for {@link AbstractCacheableService}.
 *
 * @author John Blum
 * @see org.cp.elements.service.support.AbstractCacheableService
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
@SuppressWarnings({ "rawtypes", "unchecked" })
class AbstractCacheableServiceTests {

  private AbstractCacheableService cacheableService;

  @Mock
  private Cache mockCache;

  @BeforeEach
  public void setup() {
    this.cacheableService = spy(new TestCacheableService());
  }

  @Test
  void getCacheReturnsConcurrentMapCache() {
    assertThat(this.cacheableService.getCache()).isPresent();
    assertThat(this.cacheableService.getCache().orElse(null)).isInstanceOf(ConcurrentMapCache.class);
  }

  @Test
  void isCacheEnabledWithCacheReturnsTrue() {
    doReturn(Optional.of(this.mockCache)).when(this.cacheableService).getCache();
    assertThat(this.cacheableService.isCachingEnabled()).isTrue();
    verifyNoInteractions(this.mockCache);
  }

  @Test
  void isCacheEnabledWithNoCacheReturnsFalse() {
    doReturn(Optional.empty()).when(this.cacheableService).getCache();
    assertThat(this.cacheableService.isCachingEnabled()).isFalse();
  }

  @Test
  void isCacheHitWhenCachedReturnsTrue() {
    assertThat(this.cacheableService.isCacheHit()).isTrue();
    assertThat(this.cacheableService.isCacheHit()).isTrue();
  }

  @Test
  void isCacheHitWhenNotCachedReturnsFalse() {
    assertThat(this.cacheableService.setCacheMiss()).isTrue();
    assertThat(this.cacheableService.isCacheHit()).isFalse();
  }

  @Test
  void isCacheHitWhenNotCachedReturnsFalseThenResetsWhenQueriedAndReturnsTrue() {
    assertThat(this.cacheableService.setCacheMiss()).isTrue();
    assertThat(this.cacheableService.isCacheHit()).isFalse();
    assertThat(this.cacheableService.isCacheHit()).isTrue();
  }

  @Test
  void isCacheMissWhenCachedReturnsFalse() {
    assertThat(this.cacheableService.isCacheMiss()).isFalse();
    assertThat(this.cacheableService.isCacheMiss()).isFalse();
  }

  @Test
  void isCacheMissWhenNotCachedReturnsTrue() {
    assertThat(this.cacheableService.setCacheMiss()).isTrue();
    assertThat(this.cacheableService.isCacheMiss()).isTrue();
  }

  @Test
  void isCacheMissWhenNotCachedReturnsTrueThenResetsWhenQueriedAndReturnsFalse() {
    assertThat(this.cacheableService.setCacheMiss()).isTrue();
    assertThat(this.cacheableService.isCacheMiss()).isTrue();
    assertThat(this.cacheableService.isCacheMiss()).isFalse();
  }

  @Test
  void setCacheMissTwiceReturnsTrueThenFalse() {
    assertThat(this.cacheableService.setCacheMiss()).isTrue();
    assertThat(this.cacheableService.setCacheMiss()).isFalse();
  }

  @Test
  void withCachingResultsInCacheHit() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    doReturn(Optional.of(this.mockCache)).when(this.cacheableService).getCache();
    doReturn("testValue").when(this.mockCache).get(eq("mockKey"));

    assertThat(this.cacheableService.withCaching("mockKey", mockSupplier)).isEqualTo("testValue");

    verify(this.cacheableService, times(1)).withCaching(eq("mockKey"), eq(mockSupplier));
    verify(this.cacheableService, times(1)).getCache();
    verify(this.cacheableService, never()).setCacheMiss();
    verify(this.mockCache, times(1)).get(eq("mockKey"));
    verifyNoMoreInteractions(this.cacheableService, this.mockCache);
    verifyNoInteractions(mockSupplier);
  }

  @Test
  void withCachingResultsInCacheMiss() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    doReturn(Optional.of(this.mockCache)).when(this.cacheableService).getCache();
    doReturn(null).when(this.mockCache).get(eq("mockKey"));
    doReturn("TEST").when(mockSupplier).get();

    assertThat(this.cacheableService.withCaching("mockKey", mockSupplier)).isEqualTo("TEST");

    verify(this.cacheableService, times(1)).withCaching(eq("mockKey"), eq(mockSupplier));
    verify(this.cacheableService, times(1)).getCache();
    verify(this.cacheableService, times(1)).setCacheMiss();
    verify(this.mockCache, times(1)).get(eq("mockKey"));
    verify(this.mockCache, times(1)).put(eq("mockKey"), eq("TEST"));
    verifyNoMoreInteractions(this.cacheableService, this.mockCache, mockSupplier);
  }

  @Test
  void withCachingWhenNoCacheInvokesCacheLoader() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    doReturn(Optional.empty()).when(this.cacheableService).getCache();
    doReturn("MOCK").when(mockSupplier).get();

    assertThat(this.cacheableService.withCaching("testKey", mockSupplier)).isEqualTo("MOCK");

    verify(this.cacheableService, times(1)).withCaching(eq("testKey"), eq(mockSupplier));
    verify(this.cacheableService, times(1)).getCache();
    verify(this.cacheableService, never()).setCacheMiss();
    verifyNoMoreInteractions(this.cacheableService, mockSupplier);
  }

  static class TestCacheableService extends AbstractCacheableService { }

}
