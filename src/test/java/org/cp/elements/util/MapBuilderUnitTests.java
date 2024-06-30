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
package org.cp.elements.util;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.lang.ThrowableAssertions.assertThatUnsupportedOperationException;

import java.util.HashMap;
import java.util.Map;
import java.util.SortedMap;
import java.util.concurrent.ConcurrentMap;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link MapBuilder}.
 *
 * @author John Blum
 * @see java.util.Map
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.util.MapBuilder
 * @since 1.0.0
 */
public class MapBuilderUnitTests {

  @Test
  void newConcurrentMapIsCorrect() {

    MapBuilder<?, ?> mapBuilder = MapBuilder.newConcurrentMap();

    assertThat(mapBuilder).isNotNull();
    assertThat(mapBuilder.getMap()).isInstanceOf(ConcurrentMap.class);
    assertThat(mapBuilder.getMap()).isEmpty();
  }

  @Test
  void newHashMapIsCorrect() {

    MapBuilder<?, ?> mapBuilder = MapBuilder.newHashMap();

    assertThat(mapBuilder).isNotNull();
    assertThat(mapBuilder.getMap()).isInstanceOf(HashMap.class);
    assertThat(mapBuilder.getMap()).isEmpty();
  }

  @Test
  void newSortedMapIsCorrect() {

    MapBuilder<?, ?> mapBuilder = MapBuilder.newSortedMap();

    assertThat(mapBuilder).isNotNull();
    assertThat(mapBuilder.getMap()).isInstanceOf(SortedMap.class);
    assertThat(mapBuilder.getMap()).isEmpty();
  }

  @Test
  void putSingleEntryAndBuildMap() {

    Map<String, Object> map = MapBuilder.<String, Object>newHashMap()
      .put("A", 1)
      .build();

    assertThat(map).isNotNull();
    assertThat(map).hasSize(1);
    assertThat(map).containsEntry("A", 1);
  }

  @Test
  void putTwoEntriesAndBuildMap() {

    Map<String, Object> map = MapBuilder.<String, Object>newHashMap()
      .put("A", 1)
      .put("B", 2)
      .build();

    assertThat(map).isNotNull();
    assertThat(map).hasSize(2);
    assertThat(map).containsEntry("A", 1);
    assertThat(map).containsEntry("B", 2);
  }

  @Test
  void putAllFromMapAndBuildNewMap() {

    Map<String, Object> source = new HashMap<>();

    source.put("A", 1);
    source.put("B", 2);
    source.put("C", 3);

    Map<String, Object> target = MapBuilder.<String, Object>newHashMap()
      .putAll(source)
      .build();

    assertThat(target).isNotNull().isNotSameAs(source);
    assertThat(target).hasSize(source.size());
    assertThat(target).containsAllEntriesOf(source);
  }

  @Test
  void putIfAbsentAndBuildMap() {

    Map<String, Object> map = MapBuilder.<String, Object>newHashMap()
      .putIfAbsent("A", 1)
      .putIfAbsent("A", 3)
      .putIfAbsent("B", 2)
      .build();

    assertThat(map).isNotNull();
    assertThat(map).hasSize(2);
    assertThat(map).containsEntry("A", 1);
    assertThat(map).containsEntry("B", 2);
  }

  @Test
  void putAndSortEntriesThenBuildMap() {

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

  @Test
  void buildUnmodifiableMap() {

    MapBuilder<Object, Object> mapBuilder = MapBuilder.newHashMap()
      .put("A", 1);

    MapBuilder<Object, Object> unmodifiableMapBuilder = mapBuilder.makeUnmodifiable();

    assertThat(unmodifiableMapBuilder).isNotNull().isNotSameAs(mapBuilder);

    Map<Object, Object> map = unmodifiableMapBuilder.build();

    assertThat(map).isNotNull();
    assertThat(map).hasSize(1);
    assertThat(map).isUnmodifiable();
  }

  @Test
  void buildUnmodifiableMapAndMutateDuringBuild() {

    assertThatUnsupportedOperationException()
      .isThrownBy(args -> MapBuilder.newHashMap().makeUnmodifiable().put("A", 1).build())
      .withNoCause();
  }
}
