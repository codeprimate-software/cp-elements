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

package org.cp.elements.data.caching.provider;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.cp.elements.util.MapBuilder;
import org.junit.Before;
import org.junit.Test;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * Unit tests for {@link ConcurrentMapCache}.
 *
 * @author John Blum
 * @see java.util.Map
 * @see org.junit.Test
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @see org.cp.elements.data.caching.provider.ConcurrentMapCache
 * @since 1.0.0
 */
@SuppressWarnings("unchecked")
public class ConcurrentMapCacheTests {

  private ConcurrentMapCache concurrentMapCache;

  @Before
  public void setup() {
    this.concurrentMapCache = new ConcurrentMapCache();
  }

  @Test
  public void isEmptyReturnsTrueWhenEmpty() {
    assertThat(this.concurrentMapCache.isEmpty()).isTrue();
  }

  @Test
  public void isEmptyReturnsFalseWhenNotEmpty() {

    this.concurrentMapCache.put("key", "value");

    assertThat(this.concurrentMapCache.isEmpty()).isFalse();
  }

  @Test
  public void clearEmptyCacheIsSuccessful() {

    assertThat(this.concurrentMapCache.isEmpty()).isTrue();

    this.concurrentMapCache.clear();

    assertThat(this.concurrentMapCache.isEmpty()).isTrue();
  }

  @Test
  public void clearNonEmptyCacheIsSuccessful() {

    this.concurrentMapCache.put(1, "one");
    this.concurrentMapCache.put(2, "two");

    assertThat(this.concurrentMapCache.isEmpty()).isFalse();

    this.concurrentMapCache.clear();

    assertThat(this.concurrentMapCache.isEmpty()).isTrue();
  }

  @Test
  public void containsExistingKeyReturnsTrue() {

    this.concurrentMapCache.put("key", "value");

    assertThat(this.concurrentMapCache.contains("key")).isTrue();
  }

  @Test
  public void containsNonExistingKeyReturnsFalse() {

    this.concurrentMapCache.put(1, "one");

    assertThat(this.concurrentMapCache.contains(-1)).isFalse();
    assertThat(this.concurrentMapCache.contains(1L)).isFalse();
    assertThat(this.concurrentMapCache.contains("one")).isFalse();
    assertThat(this.concurrentMapCache.contains(2)).isFalse();
    assertThat(this.concurrentMapCache.contains("test")).isFalse();
  }

  @Test
  public void containsNullKeyReturnsFalse() {
    assertThat(this.concurrentMapCache.contains(null)).isFalse();
  }

  @Test
  public void evictExistingKeyIsSuccessful() {

    this.concurrentMapCache.put(1, "one");
    this.concurrentMapCache.put(2, "two");

    assertThat(this.concurrentMapCache.isEmpty()).isFalse();
    assertThat(this.concurrentMapCache.contains(1)).isTrue();
    assertThat(this.concurrentMapCache.contains(2)).isTrue();

    this.concurrentMapCache.evict(1);

    assertThat(this.concurrentMapCache.isEmpty()).isFalse();
    assertThat(this.concurrentMapCache.contains(1)).isFalse();
    assertThat(this.concurrentMapCache.contains(2)).isTrue();
  }

  @Test
  public void evictNonExistingKeyIsSafe() {

    assertThat(this.concurrentMapCache.contains("key")).isFalse();

    this.concurrentMapCache.evict("key");
  }

  @Test
  public void evictNullKeyIsSuccessful() {
    this.concurrentMapCache.evict(null);
  }

  @Test
  public void fromMap() {

    Map map = MapBuilder.newHashMap()
      .put(1, "one")
      .put(2, "two")
      .put(3, "three")
      .build();

    assertThat(this.concurrentMapCache.isEmpty()).isTrue();

    this.concurrentMapCache.from(map);

    assertThat(this.concurrentMapCache.isEmpty()).isFalse();
    assertThat(this.concurrentMapCache).hasSize(3);
    assertThat(this.concurrentMapCache.keys()).containsExactlyInAnyOrder(1, 2, 3);
    assertThat(this.concurrentMapCache).containsExactlyInAnyOrder("one", "two", "three");
  }

  @Test
  public void fromEmptyMap() {

    assertThat(this.concurrentMapCache).hasSize(0);

    this.concurrentMapCache.from(Collections.emptyMap());

    assertThat(this.concurrentMapCache).hasSize(0);
  }

  @Test
  public void fromNullMap() {

    this.concurrentMapCache.from(null);

    assertThat(this.concurrentMapCache.isEmpty()).isTrue();
  }

  @Test
  public void getWithExistingKeyReturnsValue() {

    this.concurrentMapCache.put("key", "value");

    assertThat(this.concurrentMapCache.get("key")).isEqualTo("value");
  }

  @Test
  public void getWithNonExistingKeyReturnsNull() {
    assertThat(this.concurrentMapCache.get("key")).isNull();
  }

  @Test
  public void getWithNullKeyReturnsNull() {
    assertThat(this.concurrentMapCache.get(null)).isNull();
  }

  @Test
  public void getNameWhenNamed() {
    assertThat(this.concurrentMapCache.named("TestCache")).isSameAs(this.concurrentMapCache);
    assertThat(this.concurrentMapCache.getName()).isEqualTo("TestCache");
  }

  @Test
  public void getNameWhenUnsetReturnsNull() {
    assertThat(this.concurrentMapCache.getName()).isNull();
  }

  @Test
  public void iteratorForEmptyCache() {

    Iterator<?> valueIterator = this.concurrentMapCache.iterator();

    assertThat(valueIterator).isNotNull();
    assertThat(valueIterator).isEmpty();
  }

  @Test
  public void iteratorForNonEmptyCache() {

    this.concurrentMapCache.put(1, "one");
    this.concurrentMapCache.put(2, "two");

    assertThat(this.concurrentMapCache.iterator()).containsExactlyInAnyOrder("one", "two");
  }

  @Test
  public void keysForEmptyCache() {

    Set<Comparable<?>> keys = this.concurrentMapCache.keys();

    assertThat(keys).isNotNull();
    assertThat(keys).isEmpty();
  }

  @Test
  public void keysForNonEmptyCache() {

    this.concurrentMapCache.put(1, "one");
    this.concurrentMapCache.put(2, "two");

    assertThat(this.concurrentMapCache.keys()).containsExactlyInAnyOrder(1, 2);
  }

  @Test
  public void putIsSuccessful() {

    assertThat(this.concurrentMapCache.isEmpty()).isTrue();

    this.concurrentMapCache.put(1, "one");
    this.concurrentMapCache.put(2, "two");

    assertThat(this.concurrentMapCache).hasSize(2);
    assertThat(this.concurrentMapCache.contains(1)).isTrue();
    assertThat(this.concurrentMapCache.get(1)).isEqualTo("one");
    assertThat(this.concurrentMapCache.contains(2)).isTrue();
    assertThat(this.concurrentMapCache.get(2)).isEqualTo("two");
  }

  @Test(expected = IllegalArgumentException.class)
  public void putWithNullKeyThrowsIllegalArgumentException() {

    try {
      this.concurrentMapCache.put(null, "value");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Key is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(this.concurrentMapCache.isEmpty()).isTrue();
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void putWithNullValueThrowsIllegalArgumentException() {

    try {
      this.concurrentMapCache.put("key", null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Value is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(this.concurrentMapCache.isEmpty()).isTrue();
    }
  }

  @Test
  public void putIfAbsentWhenAbsent() {

    assertThat(this.concurrentMapCache.contains("key")).isFalse();

    this.concurrentMapCache.putIfAbsent("key", "value");

    assertThat(this.concurrentMapCache.get("key")).isEqualTo("value");
  }

  @Test
  public void putIfAbsentWhenPresent() {

    this.concurrentMapCache.put("key", "value");

    assertThat(this.concurrentMapCache.contains("key")).isTrue();

    this.concurrentMapCache.putIfAbsent("key", "test");

    assertThat(this.concurrentMapCache.get("key")).isEqualTo("value");
  }

  @Test(expected = IllegalArgumentException.class)
  public void putIfAbsentWithNullKeyThrowsIllegalArgumentException() {

    try {
      this.concurrentMapCache.putIfAbsent(null, "value");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Key is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(this.concurrentMapCache.isEmpty()).isTrue();
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void putIfAbsentWithNullValueThrowsIllegalArgumentException() {

    try {
      this.concurrentMapCache.putIfAbsent("key", null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Value is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(this.concurrentMapCache.isEmpty()).isTrue();
    }
  }

  @Test
  public void putIfPresentWhenAbsent() {

    assertThat(this.concurrentMapCache.contains("key")).isFalse();

    this.concurrentMapCache.putIfPresent("key", "value");

    assertThat(this.concurrentMapCache.get("key")).isNull();
  }

  @Test
  public void putIfPresentWhenPresent() {

    this.concurrentMapCache.put("key", "value");

    assertThat(this.concurrentMapCache.contains("key")).isTrue();

    this.concurrentMapCache.putIfPresent("key", "test");

    assertThat(this.concurrentMapCache.get("key")).isEqualTo("test");
  }

  @Test
  public void putIfPresentWithNullKey() {

    this.concurrentMapCache.putIfPresent(null, "value");

    assertThat(this.concurrentMapCache.isEmpty()).isTrue();
  }

  @Test(expected = IllegalArgumentException.class)
  public void putIfPresentWithNullValueThrowsIllegalArgumentException() {

    this.concurrentMapCache.put("key", "value");

    try {
      this.concurrentMapCache.putIfPresent("key", null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Value is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(this.concurrentMapCache.get("key")).isEqualTo("value");
    }
  }

  @Test
  public void sizeIsZeroWhenEmpty() {
    assertThat(this.concurrentMapCache.size()).isZero();
  }

  @Test
  public void sizeIsOneWhenCacheContainsSingleEntry() {

    this.concurrentMapCache.from(Collections.singletonMap("key", "value"));

    assertThat(this.concurrentMapCache.size()).isEqualTo(1);
  }

  @Test
  public void sizeIsNotZero() {

    this.concurrentMapCache.put(1, "one");
    this.concurrentMapCache.put(2, "two");
    this.concurrentMapCache.put(3, "three");

    assertThat(this.concurrentMapCache.size()).isEqualTo(3);
  }

  @Test
  public void toMapWhenCacheIsEmpty() {
    assertThat(this.concurrentMapCache.toMap()).isEmpty();
  }

  @Test
  public void toMapWhenCacheIsPopulated() {

    this.concurrentMapCache.put(1, "one");
    this.concurrentMapCache.put(2, "two");
    this.concurrentMapCache.put(3, "three");

    Map map = this.concurrentMapCache.toMap();

    assertThat(map).isNotNull();
    assertThat(map).hasSize(this.concurrentMapCache.size());
    assertThat(map).containsEntry(1, "one");
    assertThat(map).containsEntry(2, "two");
    assertThat(map).containsEntry(3, "three");
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
