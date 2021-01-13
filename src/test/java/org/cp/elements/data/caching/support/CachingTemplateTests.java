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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;
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
 * Unit Tests for {@link CachingTemplate}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @see org.cp.elements.data.caching.support.CachingTemplate
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
@SuppressWarnings({ "rawtypes", "unchecked" })
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
  public void withCacheUsingReadWriteLock() {

    ReadWriteLock mockLock = mock(ReadWriteLock.class);

    CachingTemplate template = CachingTemplate.with(this.mockCache).using(mockLock);

    assertThat(template).isNotNull();
    assertThat(template.getCache()).isEqualTo(this.mockCache);
    assertThat(template.getLock()).isEqualTo(mockLock);
  }

  @Test
  public void withCaching() {

    Supplier mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenReturn("testOne", "testTwo", "testThree");
    when(this.mockCache.get(any())).thenReturn(null, "testOne");

    CachingTemplate template = CachingTemplate.with(this.mockCache);

    assertThat(template).isNotNull();
    assertThat(template.withCaching(1, mockSupplier)).isEqualTo("testOne");
    assertThat(template.withCaching(1, mockSupplier)).isEqualTo("testOne");
    assertThat(template.withCaching(1, mockSupplier)).isEqualTo("testOne");

    verify(mockSupplier, times(1)).get();
    verify(this.mockCache, times(3)).get(eq(1));
  }

  @Test(expected = IllegalArgumentException.class)
  public void withCachingUsingNullKey() {

    Supplier mockSupplier = mock(Supplier.class);

    try {
      CachingTemplate.with(this.mockCache).withCaching(null, mockSupplier);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Key is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyNoInteractions(mockSupplier);
      verifyNoInteractions(this.mockCache);
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
      verifyNoInteractions(this.mockCache);
    }
  }

  @Test
  public void withCachingWhenSupplierReturnsNull() {

    Supplier mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenReturn(null);

    CachingTemplate template = CachingTemplate.with(this.mockCache);

    assertThat(template).isNotNull();
    assertThat(template.withCaching("key", mockSupplier)).isNull();

    verify(mockSupplier, times(1)).get();
    verify(this.mockCache, never()).put(any(), any());
  }

  @Test(expected = RuntimeException.class)
  public void withCachingWhenSupplierThrowsException() {

    Supplier mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenThrow(new RuntimeException("test"));

    try {
      CachingTemplate.with(this.mockCache).withCaching("key", mockSupplier);
    }
    catch (RuntimeException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockSupplier, times(1)).get();
      verify(this.mockCache, never()).put(any(), any());
    }
  }

  @Test
  public void withCacheClear() {

    Supplier mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenReturn("test");

    CachingTemplate template = CachingTemplate.with(this.mockCache);

    assertThat(template).isNotNull();
    assertThat(template.withCacheClear(mockSupplier)).isEqualTo("test");

    verify(mockSupplier, times(1)).get();
    verify(this.mockCache, times(1)).clear();
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
      verifyNoInteractions(this.mockCache);
    }
  }

  @Test
  public void withCacheClearWhenSupplierReturnsNull() {

    Supplier mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenReturn(null);

    assertThat(CachingTemplate.with(this.mockCache).withCacheClear(mockSupplier)).isNull();

    verify(mockSupplier, times(1)).get();
    verify(this.mockCache, times(1)).clear();
  }

  @Test(expected = RuntimeException.class)
  public void withCacheClearWhenSupplierThrowsException() {

    Supplier mockSupplier = mock(Supplier.class);

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
      verify(mockSupplier, times(1)).get();
      verifyNoInteractions(this.mockCache);
    }
  }

  @Test
  public void withCacheEvict() {

    Supplier mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenReturn("test");

    CachingTemplate template = CachingTemplate.with(this.mockCache);

    assertThat(template).isNotNull();
    assertThat(template.withCacheEvict("key", mockSupplier)).isEqualTo("test");

    verify(mockSupplier, times(1)).get();
    verify(this.mockCache, times(1)).evict("key");
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
      verifyNoInteractions(this.mockCache);
    }
  }

  @Test
  public void withCacheEvictWhenSupplierReturnsNullValue() {

    Supplier mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenReturn(null);

    assertThat(CachingTemplate.with(this.mockCache).withCacheEvict("key", mockSupplier)).isNull();

    verify(mockSupplier, times(1)).get();
    verify(this.mockCache, times(1)).evict(eq("key"));
  }

  @Test(expected = RuntimeException.class)
  public void withCacheEvictWhenSupplierThrowsException() {

    Supplier mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenThrow(new RuntimeException("test"));

    try {
      CachingTemplate.with(this.mockCache).withCacheEvict("key", mockSupplier);
    }
    catch (RuntimeException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockSupplier, times(1)).get();
      verifyNoInteractions(this.mockCache);
    }
  }

  @Test
  public void withCachePut() {

    Supplier mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenReturn("test");

    CachingTemplate template = CachingTemplate.with(this.mockCache);

    assertThat(template).isNotNull();
    assertThat(template.withCachePut("key", mockSupplier)).isEqualTo("test");

    verify(mockSupplier, times(1)).get();
    verify(this.mockCache, times(1)).put(eq("key"), eq("test"));
  }

  @Test(expected = IllegalArgumentException.class)
  public void withCachePutUsingNullKey() {

    Supplier mockSupplier = mock(Supplier.class);

    try {
      CachingTemplate.with(this.mockCache).withCachePut(null, mockSupplier);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Key is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyNoInteractions(mockSupplier);
      verifyNoInteractions(this.mockCache);
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
      verifyNoInteractions(this.mockCache);
    }
  }

  @Test
  public void withCachePutWhenSupplierReturnsNull() {

    Supplier mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenReturn(null);

    CachingTemplate template = CachingTemplate.with(this.mockCache);

    assertThat(template).isNotNull();
    assertThat(template.withCachePut("key", mockSupplier)).isNull();

    verify(mockSupplier, times(1)).get();
    verify(this.mockCache, never()).put(any(), any());
  }

  @Test(expected = RuntimeException.class)
  public void withCachePutWhenSupplierThrowsException() {

    Supplier mockSupplier = mock(Supplier.class);

    when(mockSupplier.get()).thenThrow(new RuntimeException("test"));

    try {
      CachingTemplate.with(this.mockCache).withCachePut("key", mockSupplier);
    }
    catch (RuntimeException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockSupplier, times(1)).get();
      verifyNoInteractions(this.mockCache);
    }
  }

  @Test
  public void concurrentCacheGetWhileCachePutIsSuccessful() throws Throwable {
    TestFramework.runOnce(new CacheGetWhileCachePutIsWritingToCacheConcurrentTestCase());
  }

  @Test
  public void concurrentCachePutWhileCacheGetIsSuccessfull() throws Throwable {
    TestFramework.runOnce(new CachePutWhileCacheGetIsReadingFromCacheConcurrentTestCase());
  }

  @SuppressWarnings("unused")
  public static class CacheGetWhileCachePutIsWritingToCacheConcurrentTestCase extends MultithreadedTestCase {

    private CachingTemplate template;

    @Override
    public void initialize() {

      super.initialize();

      Map map = new HashMap<>();

      Cache cache = spy(new MapToCacheAdapter(map));

      doAnswer(invocation -> {
        waitForTick(2);
        map.put(invocation.getArgument(0), invocation.getArgument(1));
        return null;
      }).when(cache).put(any(), any());

      this.template = CachingTemplate.with(cache);
    }

    public void threadOne() {

      Thread.currentThread().setName("Cache Get Thread");

      waitForTick(1);

      assertThat(this.template.withCaching(1, () -> "GET")).isEqualTo("PUT");
    }

    public void threadTwo() {

      Thread.currentThread().setName("Cache Put Thread");

      this.template.withCachePut(1, () -> "PUT");
    }

    @Override
    public void finish() {

      super.finish();

      this.template.getCache().clear();
    }
  }

  @SuppressWarnings("unused")
  public static class CachePutWhileCacheGetIsReadingFromCacheConcurrentTestCase extends MultithreadedTestCase {

    private CachingTemplate template;

    private List<String> threadOrder = new CopyOnWriteArrayList<>();

    @Override
    public void initialize() {

      super.initialize();

      AtomicInteger tick = new AtomicInteger(3);

      Map map = new HashMap<>();

      map.put("key", "value");

      Cache cache = spy(new MapToCacheAdapter(map));

      doAnswer(invocation -> {
        waitForTick(tick.getAndIncrement());
        this.threadOrder.add(Thread.currentThread().getName());
        return map.get(invocation.getArgument(0));
      }).when(cache).get(any());

      doAnswer(invocation -> {
        tick.incrementAndGet();
        waitForTick(tick.getAndIncrement());
        this.threadOrder.add(Thread.currentThread().getName());
        map.put(invocation.getArgument(0), invocation.getArgument(1));
        return "value";
      }).when(cache).put(any(), any());

      this.template = CachingTemplate.with(cache);
    }

    public void threadOne() {

      Thread.currentThread().setName("Cache Get Thread One");

      assertTick(0);

      assertThat(this.template.withCaching("key", () -> "ONE")).isEqualTo("value");

      assertTick(3);
    }

    public void threadTwo() {

      Thread.currentThread().setName("Cache Get Thread Two");

      waitForTick(1);

      assertThat(this.template.withCaching("key", () -> "TWO")).isEqualTo("value");

      assertTick(4);
    }

    public void threadThree() {

      Thread.currentThread().setName("Cache Put Thread");

      waitForTick(2);

      assertThat(this.template.withCachePut("key", () -> "test")).isEqualTo("test");

      assertTick(6);
    }

    public void threadFour() {

      Thread.currentThread().setName("Cache Get Thread Three");

      waitForTick(5);

      assertThat(this.template.withCaching("key", () -> "THREE")).isEqualTo("test");

      assertTick(7);
    }

    @Override
    public void finish() {

      super.finish();

      this.template.getCache().clear();

      assertThat(this.threadOrder).containsExactly("Cache Get Thread One", "Cache Get Thread Two", "Cache Put Thread",
        "Cache Get Thread Three");
    }
  }
}
