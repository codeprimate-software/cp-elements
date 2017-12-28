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

/**
 * Unit tests for {@link ConcurrentMapCache}.
 *
 * @author John Blum
 * @see java.util.Map
 * @see org.junit.Test
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

    this.concurrentMapCache.put(1L, "one");

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

    this.concurrentMapCache.put(1L, "one");
    this.concurrentMapCache.put(2L, "two");

    assertThat(this.concurrentMapCache.isEmpty()).isFalse();

    this.concurrentMapCache.clear();

    assertThat(this.concurrentMapCache.isEmpty()).isTrue();
  }

  @Test
  public void containsExistingKeyReturnsTrue() {

    this.concurrentMapCache.put(1L, "one");

    assertThat(this.concurrentMapCache.contains(1L)).isTrue();
  }

  @Test
  public void containsNonExistingKeyReturnsFalse() {

    this.concurrentMapCache.put(1L, "one");

    assertThat(this.concurrentMapCache.contains(-1L)).isFalse();
    assertThat(this.concurrentMapCache.contains(1)).isFalse();
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

    this.concurrentMapCache.put(1L, "one");
    this.concurrentMapCache.put(2L, "two");

    assertThat(this.concurrentMapCache.isEmpty()).isFalse();
    assertThat(this.concurrentMapCache.contains(1L)).isTrue();
    assertThat(this.concurrentMapCache.contains(2L)).isTrue();

    this.concurrentMapCache.evict(1L);

    assertThat(this.concurrentMapCache.isEmpty()).isFalse();
    assertThat(this.concurrentMapCache.contains(1L)).isFalse();
    assertThat(this.concurrentMapCache.contains(2L)).isTrue();
  }

  @Test
  public void evictNonExistingKeyIsSafe() {

    assertThat(this.concurrentMapCache.contains(1L)).isFalse();

    this.concurrentMapCache.evict(1L);
  }

  @Test
  public void evictNullKeyIsSuccessful() {
    this.concurrentMapCache.evict(null);
  }

  @Test
  public void fromMapIsSuccessful() {

    Map<Long, String> map = MapBuilder.<Long, String>newHashMap()
      .put(1L, "one")
      .put(2L, "two")
      .put(3L, "three")
      .build();

    assertThat(this.concurrentMapCache.isEmpty()).isTrue();

    this.concurrentMapCache.from(map);

    assertThat(this.concurrentMapCache.isEmpty()).isFalse();
    assertThat(this.concurrentMapCache.keys()).containsExactlyInAnyOrder(1L, 2L, 3L);
    assertThat(this.concurrentMapCache.iterator()).containsExactlyInAnyOrder("one", "two", "three");
  }

  @Test
  public void fromNullMapIsNullSafe() {

    this.concurrentMapCache.from(null);

    assertThat(this.concurrentMapCache.isEmpty()).isTrue();
  }

  @Test
  public void getWithExistingKeyReturnsValue() {

    this.concurrentMapCache.put(1L, "one");

    assertThat(this.concurrentMapCache.get(1L)).isEqualTo("one");
  }

  @Test
  public void getWithNonExistingKeyReturnsNull() {
    assertThat(this.concurrentMapCache.get(1L)).isNull();
  }

  @Test
  public void getWithNullKeyReturnsNull() {
    assertThat(this.concurrentMapCache.get(null)).isNull();
  }

  @Test
  public void getNameWhenUnsetReturnsNull() {
    assertThat(this.concurrentMapCache.getName()).isNull();
  }

  @Test
  public void getNameWhenNamed() {

    assertThat(this.concurrentMapCache.named("TestCache")).isSameAs(this.concurrentMapCache);
    assertThat(this.concurrentMapCache.getName()).isEqualTo("TestCache");
  }

  @Test
  public void iteratorForEmptyCache() {

    Iterator<?> valueIterator = this.concurrentMapCache.iterator();

    assertThat(valueIterator).isNotNull();
    assertThat(valueIterator).isEmpty();
  }

  @Test
  public void iteratorForNonEmptyCache() {

    this.concurrentMapCache.put(1L, "one");
    this.concurrentMapCache.put(2L, "two");

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

    this.concurrentMapCache.put(1L, "one");
    this.concurrentMapCache.put(2L, "two");

    assertThat(this.concurrentMapCache.keys()).containsExactlyInAnyOrder(1L, 2L);
  }

  @Test
  public void putIsSuccessful() {

    assertThat(this.concurrentMapCache.isEmpty()).isTrue();

    this.concurrentMapCache.put(1L, "one");
    this.concurrentMapCache.put(2L, "two");

    assertThat(this.concurrentMapCache.contains(1L)).isTrue();
    assertThat(this.concurrentMapCache.get(1L)).isEqualTo("one");
    assertThat(this.concurrentMapCache.contains(2L)).isTrue();
    assertThat(this.concurrentMapCache.get(2L)).isEqualTo("two");
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

    assertThat(this.concurrentMapCache.contains(1L)).isFalse();

    this.concurrentMapCache.putIfAbsent(1L, "one");

    assertThat(this.concurrentMapCache.get(1L)).isEqualTo("one");
  }

  @Test
  public void putIfAbsentWhenPresent() {

    this.concurrentMapCache.put(1L, "one");

    assertThat(this.concurrentMapCache.contains(1L)).isTrue();

    this.concurrentMapCache.putIfAbsent(1L, "two");

    assertThat(this.concurrentMapCache.get(1L)).isEqualTo("one");
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

    assertThat(this.concurrentMapCache.contains(1L)).isFalse();

    this.concurrentMapCache.putIfPresent(1L, "one");

    assertThat(this.concurrentMapCache.get(1L)).isNull();
  }

  @Test
  public void putIfPresentWhenPresent() {

    this.concurrentMapCache.put(1L, "one");

    assertThat(this.concurrentMapCache.contains(1L)).isTrue();

    this.concurrentMapCache.putIfPresent(1L, "two");

    assertThat(this.concurrentMapCache.get(1L)).isEqualTo("two");
  }

  @Test
  public void putIfPresentWithNullKey() {

    this.concurrentMapCache.putIfPresent(null, "value");

    assertThat(this.concurrentMapCache.isEmpty()).isTrue();
  }

  @Test(expected = IllegalArgumentException.class)
  public void putIfPresentWithNullValueThrowsIllegalArgumentException() {

    this.concurrentMapCache.put(1L, "one");

    try {
      this.concurrentMapCache.putIfPresent(1L, null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Value is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(this.concurrentMapCache.get(1L)).isEqualTo("one");
    }
  }

  @Test
  public void sizeIsZeroWhenEmpty() {
    assertThat(this.concurrentMapCache.size()).isZero();
  }

  @Test
  public void sizeIsOneWhenCacheContainsSingleEntry() {

    this.concurrentMapCache.from(Collections.singletonMap(1L, "one"));

    assertThat(this.concurrentMapCache.size()).isEqualTo(1);
  }

  @Test
  public void sizeIsNotZero() {

    this.concurrentMapCache.put(1L, "one");
    this.concurrentMapCache.put(2L, "two");
    this.concurrentMapCache.put(3L, "three");

    assertThat(this.concurrentMapCache.size()).isEqualTo(3);
  }

  @Test
  public void toMapWhenCacheIsEmpty() {
    assertThat(this.concurrentMapCache.toMap()).isEmpty();
  }

  @Test
  public void toMapWhenCacheIsPopulated() {

    this.concurrentMapCache.put(1L, "one");
    this.concurrentMapCache.put(2L, "two");
    this.concurrentMapCache.put(3L, "three");

    Map<Long, String> map = this.concurrentMapCache.toMap();

    assertThat(map).isNotNull();
    assertThat(map).hasSize(this.concurrentMapCache.size());
    assertThat(map).containsEntry(1L, "one");
    assertThat(map).containsEntry(2L, "two");
    assertThat(map).containsEntry(3L, "three");
  }
}
