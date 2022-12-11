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
package org.cp.elements.data.caching.provider;

import static org.assertj.core.api.Assertions.assertThat;

import java.text.StringCharacterIterator;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.junit.Test;

import org.cp.elements.data.caching.Cache;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.util.MapBuilder;
import org.cp.elements.util.stream.StreamUtils;

/**
 * Integration Tests for {@link ConcurrentMapCache}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.data.caching.provider.ConcurrentMapCache
 * @since 1.0.0
 */
public class ConcurrentMapCacheIntegrationTests {

  @Test
  public void isEmptyAndSizeMatchAndAreCorrect() {

    ConcurrentMapCache<Integer, String> cache = new ConcurrentMapCache<>();

    assertThat(cache.isEmpty()).isTrue();
    assertThat(cache.size()).isZero();
    assertThat(cache.getConcurrentMap().put(1, "test")).isNull();
    assertThat(cache.isEmpty()).isFalse();
    assertThat(cache.size()).isOne();
    assertThat(cache.getConcurrentMap().remove(1, "mock")).isFalse();
    assertThat(cache.isEmpty()).isFalse();
    assertThat(cache.size()).isOne();
    assertThat(cache.getConcurrentMap().remove(1)).isEqualTo("test");
    assertThat(cache.isEmpty()).isTrue();
    assertThat(cache.size()).isZero();
  }

  @Test
  public void clearIsCorrect() {

    String alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    ConcurrentMapCache<Integer, Character> cache = new ConcurrentMapCache<>();

    AtomicInteger key = new AtomicInteger(0);

    StringUtils.toIterator(new StringCharacterIterator(alphabet))
      .forEachRemaining(letter -> cache.put(key.getAndIncrement(), letter));

    assertThat(cache).isNotEmpty();
    assertThat(cache).hasSize(alphabet.length());

    cache.clear();

    assertThat(cache).isEmpty();
    assertThat(cache).hasSize(0);
  }

  @Test
  public void containsIsCorrect() {

    String alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    ConcurrentMapCache<Integer, Character> cache = new ConcurrentMapCache<>();

    AtomicInteger key = new AtomicInteger(0);

    StringUtils.toIterator(new StringCharacterIterator(alphabet))
      .forEachRemaining(letter -> cache.put(key.getAndIncrement(), letter));

    IntStream.range(0, alphabet.length()).forEach(theKey -> assertThat(cache.contains(theKey)).isTrue());

    assertThat(cache.containsAll(0, 12, 13, 25)).isTrue();
    assertThat(cache.containsAll(0, 13, 26)).isFalse();
    assertThat(cache.containsAny(-1, null, 13, 26)).isTrue();
    assertThat(cache.containsAny(-1, null, 26)).isFalse();
  }

  @Test
  public void getPutAndEvictAreCorrect() {

    ConcurrentMapCache<String, String> cache = new ConcurrentMapCache<>();

    assertThat(cache.get("testKey")).isNull();

    cache.put("testKey", "A");

    assertThat(cache.get("mockKey")).isNull();
    assertThat(cache.get("testKey")).isEqualTo("A");

    cache.put("mockKey", "B");

    assertThat(cache.get("mockKey")).isEqualTo("B");
    assertThat(cache.get("testKey")).isEqualTo("A");

    cache.evict("nonExistingKey");

    assertThat(cache.get("mockKey")).isEqualTo("B");
    assertThat(cache.get("testKey")).isEqualTo("A");

    cache.evict("testKey");

    assertThat(cache.get("mockKey")).isEqualTo("B");
    assertThat(cache.get("testKey")).isNull();
  }

  @Test
  public void fromMapIsCorrect() {

    ConcurrentMapCache<Integer, String> cache = new ConcurrentMapCache<>();

    Map<Integer, String> map = MapBuilder.<Integer, String>newHashMap()
      .put(1, "A")
      .put(2, "B")
      .put(3, "C")
      .build();

    assertThat(cache.isEmpty()).isTrue();

    cache.from(map);

    assertThat(cache).isNotEmpty();
    assertThat(cache).hasSize(3);
    assertThat(cache.keys()).containsExactlyInAnyOrder(1, 2, 3);
    assertThat(cache.getAll(1, 2, 3)).containsExactlyInAnyOrder("A", "B", "C");
  }

  @Test
  public void fromEmptyMapIsCorrect() {

    ConcurrentMapCache<?, ?> cache = new ConcurrentMapCache<>();

    assertThat(cache).isEmpty();

    cache.from(Collections.emptyMap());

    assertThat(cache).isEmpty();
  }

  @Test
  public void fromNullMapIsNullSafe() {

    ConcurrentMapCache<?, ?> cache = new ConcurrentMapCache<>();

    assertThat(cache).isEmpty();

    cache.from(null);

    assertThat(cache).isEmpty();
  }

  @Test
  public void iteratorForEmptyCache() {

    ConcurrentMapCache<?, ?> cache = new ConcurrentMapCache<>();

    assertThat(cache).isEmpty();

    Iterator<?> cacheEntries = cache.iterator();

    assertThat(cacheEntries).isNotNull();
    assertThat(cacheEntries).isExhausted();
  }

  @Test
  public void iteratorForNonEmptyCache() {

    ConcurrentMapCache<Integer, String> cache = new ConcurrentMapCache<>();

    cache.put(1, "A");
    cache.put(2, "B");

    assertThat(cache).isNotEmpty();

    Iterator<Cache.Entry<Integer, String>> cacheEntryIterator = cache.iterator();

    assertThat(cacheEntryIterator).isNotNull();

    Set<String> values = StreamUtils.stream(cacheEntryIterator)
      .map(Cache.Entry::getValue)
      .collect(Collectors.toSet());

    assertThat(values).containsExactlyInAnyOrder("A", "B");
  }

  @Test
  public void putIfAbsentWhenAbsent() {

    ConcurrentMapCache<Integer, String> cache = new ConcurrentMapCache<>();

    assertThat(cache.contains(1)).isFalse();
    assertThat(cache.putIfAbsent(1, "test")).isNull();
    assertThat(cache.get(1)).isEqualTo("test");
  }

  @Test
  public void putIfAbsentWhenPresent() {

    ConcurrentMapCache<Integer, String> cache = new ConcurrentMapCache<>();

    cache.put(1, "test");

    assertThat(cache.contains(1)).isTrue();
    assertThat(cache.putIfAbsent(1, "mock")).isEqualTo("test");
    assertThat(cache.get(1)).isEqualTo("test");
  }

  @Test
  public void putIfPresentWhenAbsent() {

    ConcurrentMapCache<Integer, String> cache = new ConcurrentMapCache<>();

    assertThat(cache.contains(1)).isFalse();
    assertThat(cache.putIfPresent(1, "test")).isNull();
    assertThat(cache.get(1)).isNull();
  }

  @Test
  public void putIfPresentWhenPresent() {

    ConcurrentMapCache<Integer, String> cache = new ConcurrentMapCache<>();

    cache.put(1, "test");

    assertThat(cache.contains(1)).isTrue();
    assertThat(cache.putIfPresent(1, "mock")).isEqualTo("test");
    assertThat(cache.get(1)).isEqualTo("mock");
  }

  @Test
  public void sizeWhenCacheContainsSingleEntryIsOne() {

    ConcurrentMapCache<String, String> cache = new ConcurrentMapCache<>();

    cache.from(Collections.singletonMap("testKey", "mockValue"));

    assertThat(cache.size()).isOne();
  }

  @Test
  public void sizeIsNotZero() {

    ConcurrentMapCache<Integer, String> cache = new ConcurrentMapCache<>();

    cache.from(MapBuilder.<Integer, String>newHashMap()
      .put(1, "A")
      .put(2, "B")
      .put(3, "C")
      .build());

    assertThat(cache.size()).isEqualTo(3);
  }

  @Test
  public void toMapIsCorrect() {

    String alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    MapBuilder<Integer, Character> mapBuilder = MapBuilder.newHashMap();

    ConcurrentMapCache<Integer, Character> cache = new ConcurrentMapCache<>();

    AtomicInteger key = new AtomicInteger(0);

    StringUtils.toIterator(new StringCharacterIterator(alphabet)).forEachRemaining(letter -> {
      mapBuilder.put(key.get(), letter);
      cache.put(key.getAndIncrement(), letter);
    });

    Map<Integer, Character> map = cache.toMap();

    assertThat(map).isNotNull();
    assertThat(map).hasSize(alphabet.length());
    assertThat(map).isEqualTo(mapBuilder.build());
  }
}
