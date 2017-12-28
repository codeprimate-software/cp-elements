/*
 * Copyright 2016 Author or Authors.
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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.function.Supplier;

import org.cp.elements.data.caching.Cache;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * Unit tests for {@link CachingTemplate}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.caching.support.CachingTemplate
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
@SuppressWarnings("unchecked")
public class CachingTemplateTests {

  @Mock
  private Cache mockCache;

  @Test
  public void constructWithNonNullCache() {

    CachingTemplate template = new CachingTemplate(this.mockCache);

    assertThat(template).isNotNull();
    assertThat(template.getCache()).isEqualTo(this.mockCache);
    assertThat(template.getLock()).isNotNull();
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructWithNullCache() {

    try {
      new CachingTemplate(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Cache is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void usingReadWriteLock() {

    ReadWriteLock mockLock = mock(ReadWriteLock.class);

    CachingTemplate template = CachingTemplate.with(this.mockCache).using(mockLock);

    assertThat(template).isNotNull();
    assertThat(template.getCache()).isEqualTo(this.mockCache);
    assertThat(template.getLock()).isEqualTo(mockLock);
  }

  @Test
  public void withCachingIsSuccessful() {

    Supplier<Object> mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenReturn("testOne", "testTwo");
    when(this.mockCache.get(any())).thenReturn(null, "testOne");

    CachingTemplate template = CachingTemplate.with(this.mockCache);

    template.withCaching(1L, mockSupplier);
    template.withCaching(1L, mockSupplier);

    verify(this.mockCache, times(2)).get(eq(1L));
    verify(mockSupplier, times(1)).get();
  }

  @Test(expected = IllegalArgumentException.class)
  public void withCachingUsingNullKey() {

    Supplier<Object> mockSupplier = mock(Supplier.class);

    try {
      CachingTemplate.with(this.mockCache).withCaching(null, mockSupplier);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Key is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyZeroInteractions(this.mockCache);
      verifyZeroInteractions(mockSupplier);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void withCachingUsingNullSupplier() {

    try {
      CachingTemplate.with(this.mockCache).withCaching("key", null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Supplier is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyZeroInteractions(this.mockCache);
    }
  }

  @Test
  public void withCachingWhenSupplierReturnsNull() {

    Supplier<Object> mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenReturn(null);

    CachingTemplate template = CachingTemplate.with(this.mockCache);

    assertThat(template).isNotNull();
    assertThat(template.withCaching(1L, mockSupplier)).isNull();

    verify(this.mockCache, never()).put(any(Comparable.class), any());
    verify(mockSupplier, times(1)).get();
  }

  @Test(expected = RuntimeException.class)
  public void withCachingWhenSupplierThrowsException() {

    Supplier<Object> mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenThrow(new RuntimeException("test"));

    try {
      CachingTemplate.with(this.mockCache).withCaching(1L, mockSupplier);
    }
    catch (RuntimeException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(this.mockCache, never()).put(any(Comparable.class), any());
      verify(mockSupplier, times(1)).get();
    }
  }

  @Test
  public void withCacheClear() {

    Supplier<Object> mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenReturn("test");

    CachingTemplate template = CachingTemplate.with(this.mockCache);

    assertThat(template).isNotNull();
    assertThat(template.withCacheClear(mockSupplier)).isEqualTo("test");

    verify(this.mockCache, times(1)).clear();
    verify(mockSupplier, times(1)).get();
  }

  @Test(expected = IllegalArgumentException.class)
  public void withCacheClearUsingNullSupplier() {

    try {
      CachingTemplate.with(this.mockCache).withCacheClear(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Supplier is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyZeroInteractions(this.mockCache);
    }
  }

  @Test(expected = RuntimeException.class)
  public void withCacheClearWhenSupplierThrowsException() {

    Supplier<Object> mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenThrow(new RuntimeException("test"));

    try {
      CachingTemplate.with(this.mockCache).withCacheClear(mockSupplier);
    }
    catch (RuntimeException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyZeroInteractions(this.mockCache);
    }
  }

  @Test
  public void withCacheEvict() {

    Supplier<Object> mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenReturn("test");

    CachingTemplate template = CachingTemplate.with(this.mockCache);

    assertThat(template).isNotNull();
    assertThat(template.withCacheEvict(1L, mockSupplier)).isEqualTo("test");

    verify(this.mockCache, times(1)).evict(1L);
    verify(mockSupplier, times(1)).get();
  }

  @Test(expected = IllegalArgumentException.class)
  public void withCacheEvictUsingNullSupplier() {

    try {
      CachingTemplate.with(this.mockCache).withCacheEvict(null, null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Supplier is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyZeroInteractions(this.mockCache);
    }
  }

  @Test(expected = RuntimeException.class)
  public void withCacheEvictWhenSupplierThrowsException() {

    Supplier<Object> mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenThrow(new RuntimeException("test"));

    try {
      CachingTemplate.with(this.mockCache).withCacheEvict(1L, mockSupplier);
    }
    catch (RuntimeException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyZeroInteractions(this.mockCache);
    }
  }

  @Test
  public void withCachePut() {

    Supplier<Object> mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenReturn("test");

    CachingTemplate template = CachingTemplate.with(this.mockCache);

    assertThat(template).isNotNull();
    assertThat(template.withCachePut(1L, mockSupplier)).isEqualTo("test");

    verify(this.mockCache, times(1)).put(eq(1L), eq("test"));
    verify(mockSupplier, times(1)).get();
  }

  @Test(expected = IllegalArgumentException.class)
  public void withCachePutUsingNullKey() {

    Supplier<Object> mockSupplier = mock(Supplier.class);

    try {
      CachingTemplate.with(this.mockCache).withCachePut(null, mockSupplier);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Key is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyZeroInteractions(this.mockCache);
      verifyZeroInteractions(mockSupplier);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void withCachePutUsingNullSupplier() {

    try {
      CachingTemplate.with(this.mockCache).withCachePut("key", null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Supplier is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyZeroInteractions(this.mockCache);
    }
  }

  @Test
  public void withCachePutWhenSupplierReturnsNull() {

    Supplier<Object> mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenReturn(null);

    CachingTemplate template = CachingTemplate.with(this.mockCache);

    assertThat(template).isNotNull();
    assertThat(template.withCachePut(1L, mockSupplier)).isNull();

    verify(this.mockCache, never()).put(any(Comparable.class), any());
    verify(mockSupplier, times(1)).get();
  }

  @Test(expected = RuntimeException.class)
  public void withCachePutWhenSupplierThrowsException() {

    Supplier<Object> mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenThrow(new RuntimeException("test"));

    try {
      CachingTemplate.with(this.mockCache).withCachePut(1L, mockSupplier);
    }
    catch (RuntimeException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyZeroInteractions(this.mockCache);
    }
  }

  @Test
  public void concurrentCachePutAndCacheGetOperationsIsSuccessful() throws Throwable {
    TestFramework.runOnce(new CacheGetWhileCachePutIsWritingToCacheTestCase());
  }

  @SuppressWarnings("unused")
  public static class CacheGetWhileCachePutIsWritingToCacheTestCase extends MultithreadedTestCase {

    private CachingTemplate template;

    @Override
    public void initialize() {

      Map map = new HashMap<>();

      Cache cache = spy(new MapToCacheAdapter(map));

      doAnswer(invocation -> {
        waitForTick(2);
        map.put(invocation.getArgument(0), invocation.getArgument(1));
        return null;
      }).when(cache).put(any(Comparable.class), any());

      this.template = CachingTemplate.with(cache);
    }

    public void threadOne() {

      Thread.currentThread().setName("Get Thread");

      waitForTick(1);

      assertThat(this.template.withCaching(1L, () -> "GET")).isEqualTo("PUT");
    }

    public void threadTwo() {

      Thread.currentThread().setName("Put Thread");

      this.template.withCachePut(1L, () -> "PUT");
    }

    @Override
    protected void finalize() throws Throwable {
      this.template.getCache().clear();
    }
  }
}
