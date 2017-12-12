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

package org.cp.elements.util;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.HashMap;
import java.util.Map;
import java.util.SortedMap;
import java.util.concurrent.ConcurrentMap;

import org.junit.Test;

/**
 * Unit tests for {@link MapBuilder}.
 *
 * @author John Blum
 * @see java.util.Map
 * @see org.junit.Test
 * @see org.cp.elements.util.MapBuilder
 * @since 1.0.0
 */
public class MapBuilderTests {

  @Test
  public void newConcurrentMapIsCorrect() {

    MapBuilder<?, ?> mapBuilder = MapBuilder.newConcurrentMap();

    assertThat(mapBuilder).isNotNull();
    assertThat(mapBuilder.getMap()).isInstanceOf(ConcurrentMap.class);
    assertThat(mapBuilder.getMap()).isEmpty();
  }

  @Test
  public void newHashMapIsCorrect() {

    MapBuilder<?, ?> mapBuilder = MapBuilder.newHashMap();

    assertThat(mapBuilder).isNotNull();
    assertThat(mapBuilder.getMap()).isInstanceOf(HashMap.class);
    assertThat(mapBuilder.getMap()).isEmpty();
  }

  @Test
  public void newSortedMapIsCorrect() {

    MapBuilder<?, ?> mapBuilder = MapBuilder.newSortedMap();

    assertThat(mapBuilder).isNotNull();
    assertThat(mapBuilder.getMap()).isInstanceOf(SortedMap.class);
    assertThat(mapBuilder.getMap()).isEmpty();
  }

  @Test
  public void putOneEntryAndBuildIsCorrect() {

    Map<String, Object> map = MapBuilder.<String, Object>newHashMap()
      .put("one", 1)
      .build();

    assertThat(map).isNotNull();
    assertThat(map).hasSize(1);
    assertThat(map).containsEntry("one", 1);
  }

  @Test
  public void putTwoEntriesAndBuildIsCorrect() {

    Map<String, Object> map = MapBuilder.<String, Object>newHashMap()
      .put("one", 1)
      .put("two", 2)
      .build();

    assertThat(map).isNotNull();
    assertThat(map).hasSize(2);
    assertThat(map).containsEntry("one", 1);
    assertThat(map).containsEntry("two", 2);
  }

  @Test
  public void putAllFromMapAndBuildIsCorrect() {

    Map<String, Object> source = new HashMap<>();

    source.put("one", 1);
    source.put("two", 2);
    source.put("three", 3);

    Map<String, Object> target = MapBuilder.<String, Object>newHashMap()
      .putAll(source)
      .build();

    assertThat(target).isNotNull();
    assertThat(target).hasSize(source.size());
    assertThat(target).containsAllEntriesOf(source);
  }

  @Test
  public void putIfAbsentAndBuildIsCorrect() {

    Map<String, Object> map = MapBuilder.<String, Object>newHashMap()
      .putIfAbsent("one", 1)
      .putIfAbsent("one", 3)
      .putIfAbsent("two", 2)
      .build();

    assertThat(map).isNotNull();
    assertThat(map).hasSize(2);
    assertThat(map).containsEntry("one", 1);
    assertThat(map).containsEntry("two", 2);
  }

  @Test
  public void putsSortedIsCorrect() {

    Map<Integer, String> map = MapBuilder.<Integer, String>newSortedMap()
      .put(3, "three")
      .put(1, "one")
      .put(2, "two")
      .build();

    assertThat(map).isNotNull();
    assertThat(map).hasSize(3);
    assertThat(map).containsEntry(1, "one");
    assertThat(map).containsEntry(2, "two");
    assertThat(map).containsEntry(3, "three");

    int expectedKey = 0;

    for (Integer key : map.keySet()) {
      assertThat(key).isEqualTo(++expectedKey);
    }
  }
}
