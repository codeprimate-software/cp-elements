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
import java.util.Map;

import org.cp.elements.util.MapBuilder;
import org.junit.Before;
import org.junit.Test;

/**
 * Unit tests for {@link ConcurrentMapCache}.
 *
 * @author John Blum
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
  public void getWithExistingKeyReturnsValue() {

    this.concurrentMapCache.put(1L, "one");

    assertThat(this.concurrentMapCache.get(1L)).isEqualTo("one");
  }

  @Test
  public void getWithNonExistingKeyReturnsNull() {
    assertThat(this.concurrentMapCache.get(1L)).isNull();
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
