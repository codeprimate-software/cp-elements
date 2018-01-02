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
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.cp.elements.util.MapBuilder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link MapToCacheAdapter}.
 *
 * @author John Blum
 * @see java.util.Map
 * @see org.junit.Test
 * @see org.cp.elements.data.caching.support.MapToCacheAdapter
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
@SuppressWarnings("unchecked")
public class MapToCacheAdapterTests {

  @Mock
  private Map map;

  @Test
  public void constructMapToCacheAdapterWithMap() {

    MapToCacheAdapter adapter = new MapToCacheAdapter(this.map);

    assertThat(adapter).isNotNull();
    assertThat(adapter.getMap()).isEqualTo(this.map);
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructMapToCacheAdapterWithNullMap() {

    try {
      new MapToCacheAdapter(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Map is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void isEmptyWithEmptyMapReturnsTrue() {
    assertThat(MapToCacheAdapter.of(Collections.<Comparable, Object>emptyMap()).isEmpty()).isTrue();
  }

  @Test
  public void isEmptyWithNonEmptyMapReturnsFalse() {
    assertThat(MapToCacheAdapter.of(Collections.singletonMap("key", "value")).isEmpty()).isFalse();
  }

  @Test
  public void clearClearsMap() {

    MapToCacheAdapter.of(this.map).clear();

    verify(this.map, times(1)).clear();
  }

  @Test
  public void containsExistingKeyReturnsTrue() {
    assertThat(MapToCacheAdapter.of(Collections.singletonMap("key", "value")).contains("key")).isTrue();
  }

  @Test
  public void containsNonExistingKeyReturnsFalse() {

    Map map = Collections.singletonMap(1, "test");

    MapToCacheAdapter adapter = MapToCacheAdapter.of(map);

    assertThat(adapter).isNotNull();
    assertThat(adapter.getMap()).isEqualTo(map);
    assertThat(adapter.contains(-1)).isFalse();
    assertThat(adapter.contains(0)).isFalse();
    assertThat(adapter.contains(1L)).isFalse();
    assertThat(adapter.contains(2)).isFalse();
    assertThat(adapter.contains("1")).isFalse();
    assertThat(adapter.contains("test")).isFalse();
  }

  @Test
  public void containsNullKeyReturnsFalse() {
    assertThat(MapToCacheAdapter.of(Collections.singletonMap("null", "value")).contains(null)).isFalse();
  }

  @Test
  public void evictWithExistingKey() {

    Map map = MapBuilder.newHashMap()
      .put(1, "one")
      .put(2, "two")
      .build();

    assertThat(map).containsOnlyKeys(1, 2);

    MapToCacheAdapter.of(map).evict(1);

    assertThat(map).containsOnlyKeys(2);
  }

  @Test
  public void evictWithNonExistingKey() {

    MapToCacheAdapter.of(this.map).evict("key");

    verify(this.map, times(1)).remove(eq("key"));
  }

  @Test
  public void evictWithNullKey() {

    MapToCacheAdapter.of(this.map).evict(null);

    verifyZeroInteractions(this.map);
  }

  @Test
  public void fromMap() {

    Map map = MapBuilder.newHashMap()
      .put(1, "one")
      .put(2, "two")
      .build();

    MapToCacheAdapter.of(this.map).from(map);

    verify(this.map, times(1)).putAll(eq(map));
  }

  @Test
  public void fromEmptyMap() {

    Map map = Collections.emptyMap();

    MapToCacheAdapter.of(this.map).from(map);

    verify(this.map, times(1)).putAll(eq(map));
  }

  @Test
  public void fromNullMap() {

    MapToCacheAdapter.of(this.map).from(null);

    verify(this.map, times(1)).putAll(eq(Collections.emptyMap()));
  }

  @Test
  public void getWithExistingKeyReturnsValue() {
    assertThat(MapToCacheAdapter.of(Collections.singletonMap("key", "value")).get("key")).isEqualTo("value");
  }

  @Test
  public void getWithNonExistingKeyReturnsNull() {
    assertThat(MapToCacheAdapter.of(Collections.singletonMap("key", "value")).get("nonExistingKey")).isNull();
  }

  @Test
  public void getWithNullKeyReturnsNull() {
    assertThat(MapToCacheAdapter.of(Collections.singletonMap("key", "value")).get(null)).isNull();
  }

  @Test
  public void iteratorWithMap() {

    Map map = MapBuilder.newHashMap()
      .put(1, "one")
      .put(2, "two")
      .build();

    Iterator values = MapToCacheAdapter.of(map).iterator();

    assertThat(values).isNotNull();
    assertThat(values).containsExactlyInAnyOrder("one", "two");
  }

  @Test
  public void iteratorWithEmptyMap() {
    assertThat(MapToCacheAdapter.of(Collections.<Comparable, Object>emptyMap()).iterator()).isEmpty();
  }

  @Test
  public void keysWithMap() {

    Map map = MapBuilder.newHashMap()
      .put(1, "one")
      .put(2, "two")
      .build();

    Set keys = MapToCacheAdapter.of(map).keys();

    assertThat(keys).isNotNull();
    assertThat(keys).containsExactlyInAnyOrder(1, 2);
  }

  @Test
  public void keysWithEmptyMap() {
    assertThat(MapToCacheAdapter.of(Collections.<Comparable, Object>emptyMap()).keys()).isEmpty();
  }

  @Test
  public void putCallsMapPut() {

    MapToCacheAdapter.of(this.map).put("key", "value");

    verify(this.map, times(1)).put(eq("key"), eq("value"));
  }

  @Test(expected = IllegalArgumentException.class)
  public void putWithNullKeyThrowsIllegalArgumentException() {

    try {
      MapToCacheAdapter.of(this.map).put(null, "value");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Key is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyZeroInteractions(this.map);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void putWithNullValueThrowsIllegalArgumentException() {

    try {
      MapToCacheAdapter.of(this.map).put("key", null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Value is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyZeroInteractions(this.map);
    }
  }

  @Test
  public void putIfAbsentCallsMapPutIfAbsent() {

    MapToCacheAdapter.of(this.map).putIfAbsent("key", "value");

    verify(this.map, times(1)).putIfAbsent(eq("key"), eq("value"));
  }

  @Test(expected = IllegalArgumentException.class)
  public void putIfAbsentWithNullKeyThrowsIllegalArgumentException() {

    try {
      MapToCacheAdapter.of(this.map).putIfAbsent(null, "value");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Key is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyZeroInteractions(this.map);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void putIfAbsentWithNullValueThrowsIllegalArgumentException() {

    try {
      MapToCacheAdapter.of(this.map).putIfAbsent("key", null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Value is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyZeroInteractions(this.map);
    }
  }

  @Test
  public void putIfPresentCallsMapComputeIfPresent() {

    MapToCacheAdapter.of(this.map).putIfPresent("key", "value");

    verify(this.map, times(1)).computeIfPresent(eq("key"), any());
  }

  @Test
  public void putIfPresentWithNullKeyIgnoresCall() {

    MapToCacheAdapter.of(this.map).putIfPresent(null, "value");

    verify(this.map, never()).computeIfPresent(any(), any());
  }

  @Test(expected = IllegalArgumentException.class)
  public void putIfPresentWithNullValueThrowsIllegalArgumentException() {

    try {
      MapToCacheAdapter.of(this.map).putIfPresent("key", null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Value is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyZeroInteractions(this.map);
    }
  }

  @Test
  public void sizeIsEqualToMapSize() {

    when(this.map.size()).thenReturn(100);

    assertThat(MapToCacheAdapter.of(this.map).size()).isEqualTo(100);

    verify(this.map, times(1)).size();
  }

  @Test
  public void toMap() {

    when(this.map.get("key")).thenReturn("value");

    assertThat(MapToCacheAdapter.of(this.map).toMap().get("key")).isEqualTo("value");

    verify(this.map, times(1)).get(eq("key"));
  }
}
