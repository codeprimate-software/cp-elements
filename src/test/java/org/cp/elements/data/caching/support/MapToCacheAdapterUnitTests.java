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
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Collections;
import java.util.Map;
import java.util.function.BiFunction;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.cp.elements.test.annotation.IntegrationTest;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.MapBuilder;

import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit Tests for {@link MapToCacheAdapter}.
 *
 * @author John Blum
 * @see java.util.Map
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.data.caching.support.MapToCacheAdapter
 * @see org.cp.elements.test.annotation.IntegrationTest
 * @see org.cp.elements.util.MapBuilder
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
@SuppressWarnings({ "rawtypes", "unchecked" })
public class MapToCacheAdapterUnitTests {

  @Mock
  private Map map;

  @Test
  public void constructNewMapToCacheAdapterWithMap() {

    MapToCacheAdapter adapter = new MapToCacheAdapter(this.map);

    assertThat(adapter).isNotNull();
    assertThat(adapter.getMap()).isSameAs(this.map);

    verifyNoInteractions(this.map);
  }

  @Test
  public void constructNewMapToCacheAdapterWithNullMap() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new MapToCacheAdapter(null))
      .withMessage("Map is required")
      .withNoCause();
  }

  @Test
  public void mapToCacheAdapterOfMap() {

    MapToCacheAdapter<?, ?> adapter = MapToCacheAdapter.of(this.map);

    assertThat(adapter).isNotNull();
    assertThat(adapter.getMap()).isSameAs(this.map);

    verifyNoInteractions(this.map);
  }

  @Test
  public void isEmptyCallsMapIsEmpty() {

    doReturn(false).when(this.map).isEmpty();

    assertThat(MapToCacheAdapter.of(this.map).isEmpty()).isFalse();

    verify(this.map, times(1)).isEmpty();
    verifyNoMoreInteractions(this.map);
  }

  @Test
  public void clearCallMapClear() {

    MapToCacheAdapter.of(this.map).clear();

    verify(this.map, times(1)).clear();
    verifyNoMoreInteractions(this.map);
  }

  @Test
  public void containsCallsMapContainsKey() {

    doReturn(true).when(this.map).containsKey(eq("testKey"));

    assertThat(MapToCacheAdapter.of(this.map).contains("testKey")).isTrue();
    assertThat(MapToCacheAdapter.of(this.map).contains("mockKey")).isFalse();

    verify(this.map, times(1)).containsKey(eq("testKey"));
    verify(this.map, times(1)).containsKey(eq("mockKey"));
    verifyNoMoreInteractions(this.map);
  }

  @Test
  @IntegrationTest
  public void containsExistingKeyReturnsTrue() {
    assertThat(MapToCacheAdapter.of(Collections.singletonMap(1, "test")).contains(1)).isTrue();
  }

  @Test
  @IntegrationTest
  public void containsNonExistingKeyReturnsTrue() {
    assertThat(MapToCacheAdapter.of(Collections.singletonMap(1, "test")).contains(2)).isFalse();
  }

  @Test
  public void containsNullKeyIsNullSafeReturnsFalse() {

    assertThat(MapToCacheAdapter.of(this.map).contains(null)).isFalse();

    verifyNoInteractions(this.map);
  }

  @Test
  public void evictCallsMapRemove() {

    MapToCacheAdapter.of(this.map).evict("testKey");

    verify(this.map, times(1)).remove(eq("testKey"));
    verifyNoMoreInteractions(this.map);
  }

  @Test
  @IntegrationTest
  public void evictWithExistingKey() {

    Map map = MapBuilder.newHashMap()
      .put(1, "A")
      .put(2, "B")
      .build();

    assertThat(map).containsOnlyKeys(1, 2);

    MapToCacheAdapter.of(map).evict(1);

    assertThat(map).containsOnlyKeys(2);
  }

  @Test
  public void evictWithNullKeyIsNullSafe() {

    MapToCacheAdapter.of(this.map).evict(null);

    verifyNoInteractions(this.map);
  }

  @Test
  @IntegrationTest
  public void fromMap() {

    Map map = MapBuilder.newHashMap()
      .put(1, "A")
      .put(2, "B")
      .build();

    MapToCacheAdapter.of(this.map).from(map);

    verify(this.map, times(1)).putAll(eq(map));
  }

  @Test
  @IntegrationTest
  public void fromMapWithNullEntriesIsNullSafe() {

    Map map = MapBuilder.newHashMap()
      .put(1, "A")
      .put(null, "B")
      .put(3, null)
      .put(null, null)
      .build();

    MapToCacheAdapter.of(this.map).from(map);

    verify(this.map, times(1)).putAll(eq(Collections.singletonMap(1, "A")));
    verifyNoMoreInteractions(this.map);
  }

  @Test
  public void fromNullMapIsNullSafe() {

    MapToCacheAdapter.of(this.map).from(null);

    verify(this.map, times(1)).putAll(eq(Collections.emptyMap()));
    verifyNoMoreInteractions(this.map);
  }

  @Test
  public void getCallsMapGet() {

    doReturn("test").when(this.map).get(any());

    assertThat(MapToCacheAdapter.of(this.map).get(1)).isEqualTo("test");

    verify(this.map, times(1)).get(eq(1));
    verifyNoMoreInteractions(this.map);
  }

  @Test
  @IntegrationTest
  public void getWithExistingKeyReturnsValue() {
    assertThat(MapToCacheAdapter.of(Collections.singletonMap("key", "value")).get("key")).isEqualTo("value");
  }

  @Test
  @IntegrationTest
  public void getWithNonExistingKeyReturnsNull() {
    assertThat(MapToCacheAdapter.of(Collections.singletonMap("key", "value")).get("nonExistingKey")).isNull();
  }

  @Test
  public void getWithNullKeyIsNullSafeReturnsNull() {

    assertThat(MapToCacheAdapter.of(this.map).get(null)).isNull();

    verifyNoInteractions(this.map);
  }

  @Test
  public void keysCallsMapKeysSet() {

    doReturn(CollectionUtils.asSet(1, 2, 3)).when(this.map).keySet();

    assertThat(MapToCacheAdapter.of(this.map).keys()).containsExactlyInAnyOrder(1, 2, 3);

    verify(this.map, times(1)).keySet();
    verifyNoMoreInteractions(this.map);
  }

  @Test
  public void putCallsMapPut() {

    MapToCacheAdapter.of(this.map).put("key", "value");

    verify(this.map, times(1)).put(eq("key"), eq("value"));
    verifyNoMoreInteractions(this.map);
  }

  @Test
  public void putWithNullKey() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> MapToCacheAdapter.of(this.map).put(null, "value"))
      .withMessage("Key is required")
      .withNoCause();

    verifyNoInteractions(this.map);
  }

  @Test
  public void putWithNullValue() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> MapToCacheAdapter.of(this.map).put("key", null))
      .withMessage("Value is required")
      .withNoCause();

    verifyNoInteractions(this.map);
  }

  @Test
  public void putIfAbsentCallsMapPutIfAbsent() {

    doCallRealMethod().when(this.map).putIfAbsent(any(), any());

    assertThat(MapToCacheAdapter.of(this.map).putIfAbsent("key", "value")).isNull();

    verify(this.map, times(1)).putIfAbsent(eq("key"), eq("value"));
    verify(this.map, times(1)).get(eq("key"));
    verify(this.map, times(1)).put(eq("key"), eq("value"));
    verifyNoMoreInteractions(this.map);
  }

  @Test
  public void putIfAbsentCallsMapPutIfAbsentReturnsExistingValue() {

    doReturn("existingValue").when(this.map).get(any());
    doCallRealMethod().when(this.map).putIfAbsent(any(), any());

    assertThat(MapToCacheAdapter.of(this.map).putIfAbsent("key", "value")).isEqualTo("existingValue");

    verify(this.map, times(1)).putIfAbsent(eq("key"), eq("value"));
    verify(this.map, times(1)).get(eq("key"));
    verify(this.map, never()).put(any(), any());
    verifyNoMoreInteractions(this.map);
  }

  @Test
  public void putIfAbsentWithNullKey() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> MapToCacheAdapter.of(this.map).putIfAbsent(null, "value"))
      .withMessage("Key is required")
      .withNoCause();

    verifyNoInteractions(this.map);
  }

  @Test
  public void putIfAbsentWithNullValue() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> MapToCacheAdapter.of(this.map).putIfAbsent("key", null))
      .withMessage("Value is required")
      .withNoCause();

    verifyNoInteractions(this.map);
  }

  @Test
  public void putIfPresentCallsMapComputeIfPresent() {

    doReturn(null).when(this.map).get(any());
    doCallRealMethod().when(this.map).computeIfPresent(any(), any(BiFunction.class));

    assertThat(MapToCacheAdapter.of(this.map).putIfPresent("key", "value")).isNull();

    verify(this.map, times(1)).computeIfPresent(eq("key"), any());
    verify(this.map, times(1)).get(eq("key"));
    verify(this.map, never()).put(any(), any());
    verify(this.map, never()).remove(any());
    verifyNoMoreInteractions(this.map);
  }

  @Test
  public void putIfPresentCallsMapComputeIfPresentReturnsExistingValue() {

    doReturn("existingValue").when(this.map).get(any());
    doCallRealMethod().when(this.map).computeIfPresent(any(), any(BiFunction.class));

    assertThat(MapToCacheAdapter.of(this.map).putIfPresent("key", "value")).isEqualTo("existingValue");

    verify(this.map, times(1)).computeIfPresent(eq("key"), any());
    verify(this.map, times(1)).get(eq("key"));
    verify(this.map, times(1)).put(eq("key"), eq("value"));
    verifyNoMoreInteractions(this.map);
  }

  @Test
  public void putIfPresentWithNullKey() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> MapToCacheAdapter.of(this.map).putIfPresent(null, "value"))
      .withMessage("Key is required")
      .withNoCause();

    verifyNoInteractions(this.map);
  }

  @Test
  public void putIfPresentWithNullValue() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> MapToCacheAdapter.of(this.map).putIfPresent("key", null))
      .withMessage("Value is required")
      .withNoCause();

    verifyNoInteractions(this.map);
  }

  @Test
  public void sizeIsEqualToMapSize() {

    doReturn(101).when(this.map).size();

    assertThat(MapToCacheAdapter.of(this.map).size()).isEqualTo(101L);

    verify(this.map, times(1)).size();
    verifyNoMoreInteractions(this.map);
  }

  @Test
  public void toMap() {

    doReturn("value").when(this.map).get("key");

    Map map = MapToCacheAdapter.of(this.map).toMap();

    assertThat(map).isNotNull();
    assertThat(map.get("key")).isEqualTo("value");

    verify(this.map, times(1)).get(eq("key"));
    verifyNoMoreInteractions(this.map);
  }

  @Test
  public void toMapIsUnmodifiable() {

    Map map = MapToCacheAdapter.of(this.map).toMap();

    assertThat(map).isNotNull();

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> map.put(1, "test"));

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> map.remove(1, "test"));

    verifyNoInteractions(this.map);
  }
}
