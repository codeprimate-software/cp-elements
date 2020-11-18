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
import static org.mockito.Mockito.mock;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

import org.cp.elements.lang.Constants;
import org.cp.elements.lang.FilteringTransformer;
import org.cp.elements.lang.NumberUtils;
import org.junit.Test;

/**
 * Unit Tests for {@link MapUtils}.
 *
 * @author John J. Blum
 * @see java.util.Collections
 * @see java.util.Map
 * @see org.junit.Test
 * @see org.cp.elements.util.MapUtils
 * @since 1.0.0
 */
public class MapUtilsTests {

  @Test
  public void countMapReturnsSize() {

    Map<Object, Object> map = new HashMap<>();

    map.put("one", 1);
    map.put("two", 2);

    assertThat(MapUtils.count(map)).isEqualTo(map.size());
  }

  @Test
  public void countMapWithInitialCapacityReturnsZero() {
    assertThat(MapUtils.count(new HashMap<>(10))).isEqualTo(0);
  }

  @Test
  public void countEmptyMapReturnsZero() {
    assertThat(MapUtils.count(Collections.emptyMap())).isEqualTo(0);
  }

  @Test
  public void countNullMapReturnsZero() {
    assertThat(MapUtils.count(null)).isEqualTo(0);
  }

  @Test
  public void countSingleEntryMapReturnsOne() {
    assertThat(MapUtils.count(Collections.singletonMap("one", 1))).isEqualTo(1);
  }

  @Test
  public void countMapWithFilter() {

    Map<String, Integer> map = new HashMap<>();

    map.put("one", 1);
    map.put("two", 2);
    map.put("three", 3);
    map.put("four", 4);
    map.put("five", 5);
    map.put("six", 6);
    map.put("seven", 7);
    map.put("eight", 8);
    map.put("nine", 9);

    assertThat(MapUtils.count(map, (entry) -> NumberUtils.isEven(entry.getValue()))).isEqualTo(4L);
    assertThat(MapUtils.count(map, (entry) -> NumberUtils.isOdd(entry.getValue()))).isEqualTo(5L);
  }

  @Test
  public void countMapWithFilterAcceptsAll() {

    Map<String, Integer> map = new HashMap<>();

    map.put("one", 1);
    map.put("two", 2);

    assertThat(MapUtils.count(map, (entry) -> true)).isEqualTo((long) map.size());
  }

  @Test
  public void countMapWithFilterRejectsAll() {

    Map<String, Integer> map = new HashMap<>();

    map.put("one", 1);
    map.put("two", 2);

    assertThat(MapUtils.count(map, (entry) -> false)).isEqualTo(0L);
  }

  @Test
  public void countEmptyMapWithFilter() {
    assertThat(MapUtils.count(Collections.emptyMap(), (entry) -> true)).isEqualTo(0L);
  }

  @Test
  public void countNullMapWithFilter() {
    assertThat(MapUtils.count(null, (entry) -> true)).isEqualTo(0L);
  }

  @Test(expected = IllegalArgumentException.class)
  public void countNonNullMapWithNullFilter() {

    try {
      MapUtils.count(Collections.emptyMap(), null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Filter is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void filter() {

    Map<String, Integer> map = new HashMap<>(3);

    map.put("one", 1);
    map.put("two", 2);
    map.put("three", 3);

    Map<String, Integer> evenMap = MapUtils.filter(map, (entry) -> NumberUtils.isEven(entry.getValue()));

    assertThat(evenMap).isNotNull();
    assertThat(evenMap).isNotSameAs(map);
    assertThat(evenMap.size()).isEqualTo(1);
    assertThat(evenMap.containsKey("one")).isFalse();
    assertThat(evenMap.containsKey("two")).isTrue();
    assertThat(evenMap.containsKey("three")).isFalse();
    assertThat(evenMap.get("two")).isEqualTo(2);

    Map<String, Integer> oddMap = MapUtils.filter(map, (entry) -> NumberUtils.isOdd(entry.getValue()));

    assertThat(map).isNotNull();
    assertThat(oddMap).isNotSameAs(map);
    assertThat(oddMap.size()).isEqualTo(2);
    assertThat(oddMap.containsKey("one")).isTrue();
    assertThat(oddMap.containsKey("two")).isFalse();
    assertThat(oddMap.containsKey("three")).isTrue();
    assertThat(oddMap.get("one")).isEqualTo(1);
    assertThat(oddMap.get("three")).isEqualTo(3);
  }

  @Test
  public void filterAcceptsAll() {

    Map<String, Integer> map = new HashMap<>(2);

    map.put("one", 1);
    map.put("two", 2);

    Map<String, Integer> resultMap = MapUtils.filter(map, (entry) -> true);

    assertThat(resultMap).isNotNull();
    assertThat(resultMap).isNotSameAs(map);
    assertThat(resultMap.size()).isEqualTo(map.size());
    assertThat(resultMap.containsKey("one")).isTrue();
    assertThat(resultMap.containsKey("two")).isTrue();
    assertThat(resultMap.get("one")).isEqualTo(1);
    assertThat(resultMap.get("two")).isEqualTo(2);
  }

  @Test
  public void filterRejectsAll() {

    Map<String, Integer> map = new HashMap<>(2);

    map.put("one", 1);
    map.put("two", 2);

    Map<String, Integer> resultMap = MapUtils.filter(map, (entry) -> false);

    assertThat(resultMap).isNotNull();
    assertThat(resultMap).isNotSameAs(map);
    assertThat(resultMap.isEmpty()).isTrue();
  }

  @Test
  public void filterEmptyMap() {

    Map<Object, Object> emptyMap = Collections.emptyMap();
    Map<Object, Object> resultMap = MapUtils.filter(emptyMap, (entry) -> true);

    assertThat(resultMap).isNotNull();
    assertThat(resultMap).isNotSameAs(emptyMap);
    assertThat(resultMap.isEmpty()).isTrue();
  }

  @Test(expected = IllegalArgumentException.class)
  public void filterWithNullFilter() {

    try {
      MapUtils.filter(Collections.emptyMap(), null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Filter is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void filterWithNullMap() {

    try {
      MapUtils.filter(null, (entry) -> true);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Map is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void filterAndTransform() {

    Map<String, Integer> map = new HashMap<>(3);

    map.put("one", 1);
    map.put("two", 2);
    map.put("three", 3);

    Map<String, Integer> resultMap = MapUtils.filterAndTransform(map,
      new FilteringTransformer<Map.Entry<String, Integer>>() {
        @Override
        public boolean accept(Map.Entry<String, Integer> entry) {
          return NumberUtils.isEven(entry.getValue());
        }

        @Override
        public Map.Entry<String, Integer> transform(Map.Entry<String, Integer> entry) {
          entry.setValue(Double.valueOf(Math.pow(entry.getValue(), 2)).intValue());
          return entry;
        }
      });

    assertThat(resultMap).isNotNull();
    assertThat(resultMap).isNotSameAs(map);
    assertThat(resultMap.size()).isEqualTo(1);
    assertThat(resultMap.containsKey("one")).isFalse();
    assertThat(resultMap.containsKey("two")).isTrue();
    assertThat(resultMap.containsKey("three")).isFalse();
    assertThat(resultMap.get("two")).isEqualTo(4);

    resultMap = MapUtils.filterAndTransform(map, new FilteringTransformer<Map.Entry<String, Integer>>() {
      @Override
      public boolean accept(Map.Entry<String, Integer> entry) {
        return NumberUtils.isOdd(entry.getValue());
      }

      @Override
      public Map.Entry<String, Integer> transform(Map.Entry<String, Integer> entry) {
        entry.setValue(Double.valueOf(Math.pow(entry.getValue(), 2)).intValue());
        return entry;
      }
    });

    assertThat(resultMap).isNotNull();
    assertThat(resultMap).isNotSameAs(map);
    assertThat(resultMap.size()).isEqualTo(2);
    assertThat(resultMap.containsKey("one")).isTrue();
    assertThat(resultMap.containsKey("two")).isFalse();
    assertThat(resultMap.containsKey("three")).isTrue();
    assertThat(resultMap.get("one")).isEqualTo(1);
    assertThat(resultMap.get("three")).isEqualTo(9);
  }

  @Test
  public void filterAndTransformAcceptsAll() {

    Map<String, String> map = Collections.singletonMap("key", "test");

    Map<String, String> resultMap = MapUtils.filterAndTransform(map,
      new FilteringTransformer<Map.Entry<String, String>>() {
        @Override
        public boolean accept(Map.Entry<String, String> entry) {
          return true;
        }

        @Override
        public Map.Entry<String, String> transform(Map.Entry<String, String> entry) {
          return new Map.Entry<String, String>() {
            @Override
            public String getKey() {
              return entry.getKey();
            }

            @Override
            public String getValue() {
              return entry.getValue().toUpperCase();
            }

            @Override
            public String setValue(final String value) {
              throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
            }
          };
        }
      });

    assertThat(resultMap).isNotNull();
    assertThat(resultMap).isNotSameAs(map);
    assertThat(resultMap.size()).isEqualTo(1);
    assertThat(resultMap.containsKey("key")).isTrue();
    assertThat(resultMap.get("key")).isEqualTo("TEST");
  }

  @Test
  public void filterAndTransformRejectsAll() {

    Map<String, String> map = Collections.singletonMap("key", "test");

    Map<String, String> resultMap = MapUtils.filterAndTransform(map,
      new FilteringTransformer<Map.Entry<String, String>>() {
        @Override
        public boolean accept(Map.Entry<String, String> entry) {
          return false;
        }

        @Override
        public Map.Entry<String, String> transform(Map.Entry<String, String> entry) {
          return entry;
        }
      });

    assertThat(resultMap).isNotNull();
    assertThat(resultMap).isNotSameAs(map);
    assertThat(resultMap.isEmpty()).isTrue();
  }

  @Test
  public void filterAndTransformEmptyMap() {

    Map<Object, Object> emptyMap = Collections.emptyMap();

    Map<Object, Object> resultMap = MapUtils.filterAndTransform(emptyMap, new FilteringTransformer<Map.Entry<Object, Object>>() {
      @Override
      public boolean accept(Map.Entry<Object, Object> entry) {
        return true;
      }

      @Override
      public Map.Entry<Object, Object> transform(Map.Entry<Object, Object> entry) {
        return entry;
      }
    });

    assertThat(resultMap).isNotNull();
    assertThat(resultMap).isNotSameAs(emptyMap);
    assertThat(resultMap.isEmpty()).isTrue();
  }

  @Test(expected = IllegalArgumentException.class)
  public void filterAndTransformWithNullFilter() {

    try {
      MapUtils.filterAndTransform(Collections.emptyMap(), null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("FilteringTransformer is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @SuppressWarnings("unchecked")
  @Test(expected = IllegalArgumentException.class)
  public void filterAndTransformWithNullMap() {

    try {
      MapUtils.filterAndTransform(null, mock(FilteringTransformer.class));
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Map is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void findAll() {

    Map<String, String> map = new HashMap<>(3);

    map.put("one", "test");
    map.put("two", "testing");
    map.put("three", "tested");

    Map<String, String> resultMap = MapUtils.findAll(map, (entry) -> "test".equals(entry.getValue()));

    assertThat(resultMap).isNotNull();
    assertThat(resultMap).isNotSameAs(map);
    assertThat(resultMap.size()).isEqualTo(1);
    assertThat(resultMap.containsKey("one")).isTrue();
    assertThat(resultMap.containsKey("two")).isFalse();
    assertThat(resultMap.containsKey("three")).isFalse();
    assertThat(resultMap.get("one")).isEqualTo("test");

    resultMap = MapUtils.findAll(map, (entry) -> entry.getValue().endsWith("ing") || entry.getValue().endsWith("ed"));

    assertThat(resultMap).isNotNull();
    assertThat(resultMap).isNotSameAs(map);
    assertThat(resultMap.size()).isEqualTo(2);
    assertThat(resultMap.containsKey("one")).isFalse();
    assertThat(resultMap.containsKey("two")).isTrue();
    assertThat(resultMap.containsKey("three")).isTrue();
    assertThat(resultMap.get("two")).isEqualTo("testing");
    assertThat(resultMap.get("three")).isEqualTo("tested");
  }

  @Test
  public void findAllAcceptsAll() {

    Map<String, String> map = Collections.singletonMap("one", "test");
    Map<String, String> resultMap = MapUtils.findAll(map, (entry) -> true);

    assertThat(resultMap).isNotNull();
    assertThat(resultMap).isNotSameAs(map);
    assertThat(resultMap.size()).isEqualTo(1);
    assertThat(resultMap.containsKey("one")).isTrue();
    assertThat(resultMap.get("one")).isEqualTo("test");
  }

  @Test
  public void findAllRejectsAll() {

    Map<String, String> map = Collections.singletonMap("one", "test");
    Map<String, String> resultMap = MapUtils.findAll(map, (entry) -> false);

    assertThat(resultMap).isNotNull();
    assertThat(resultMap).isNotSameAs(map);
    assertThat(resultMap.isEmpty()).isTrue();
  }

  @Test
  public void findAllWithEmptyMap() {

    Map<String, String> emptyMap = Collections.emptyMap();
    Map<String, String> resultMap = MapUtils.findAll(emptyMap, (entry) -> true);

    assertThat(resultMap).isNotNull();
    assertThat(resultMap).isNotSameAs(emptyMap);
    assertThat(resultMap.isEmpty()).isTrue();
  }

  @Test(expected = IllegalArgumentException.class)
  public void findAllWithNullFilter() {

    try {
      MapUtils.findAll(Collections.emptyMap(), null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Filter is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void findAllWithNullMap() {

    try {
      MapUtils.findAll(null, (entry) -> true);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Map is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void fromAssociativeArray() {

    String[] associativeArray = { "keyOne=valueOne", "keyTwo=valueTwo" };

    Map<String, String> map = MapUtils.fromAssociativeArray(associativeArray);

    assertThat(map).isNotNull();
    assertThat(map.size()).isEqualTo(associativeArray.length);
    assertThat(map.containsKey("keyOne")).isTrue();
    assertThat(map.get("keyOne")).isEqualTo("valueOne");
    assertThat(map.containsKey("keyTwo")).isTrue();
    assertThat(map.get("keyTwo")).isEqualTo("valueTwo");
  }

  @Test
  public void fromEmptyAssociativeArray() {

    Map<String, String> map = MapUtils.fromAssociativeArray(new String[0]);

    assertThat(map).isNotNull();
    assertThat(map.isEmpty()).isTrue();
  }

  @Test
  public void fromSingleEntryAssociativeArray() {

    Map<String, String> map = MapUtils.fromAssociativeArray(new String[] { "key=value" });

    assertThat(map).isNotNull();
    assertThat(map.size()).isEqualTo(1);
    assertThat(map.containsKey("key")).isTrue();
    assertThat(map.get("key")).isEqualTo("value");
  }

  @Test(expected = IllegalArgumentException.class)
  public void fromInvalidAssociateArray() {

    String[] associativeArray = { "keyOne=valueOne", "", "keyThree" };

    try {
      MapUtils.fromAssociativeArray(associativeArray);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Entry [] at index [1] must be specified");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void fromInvalidEntryInAssociateArray() {

    String[] associativeArray = { "keyOne=valueOne", "keyTwo=valueTwo", "keyThree" };

    try {
      MapUtils.fromAssociativeArray(associativeArray);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Entry [keyThree] at index [2] must have both a key and a value");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("all")
  public void isEmptyMapWithEmptyMap() {

    assertThat(MapUtils.isEmpty(new HashMap<>(10))).isTrue();
    assertThat(MapUtils.isEmpty(Collections.emptyMap())).isTrue();
    assertThat(MapUtils.isEmpty(null)).isTrue();
  }

  @Test
  public void isEmptyWithNonEmptyMap() {

    Map<String, Integer> map = new HashMap<>();

    map.put("one", 1);
    map.put("two", 2);

    assertThat(MapUtils.isEmpty(map)).isFalse();
    assertThat(MapUtils.isEmpty(Collections.singletonMap("one", 1))).isFalse();
    assertThat(MapUtils.isEmpty(Collections.singletonMap(null, null))).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void isNotEmptyMapWithEmptyMap() {

    assertThat(MapUtils.isNotEmpty(new HashMap<>(10))).isFalse();
    assertThat(MapUtils.isNotEmpty(Collections.emptyMap())).isFalse();
    assertThat(MapUtils.isNotEmpty(null)).isFalse();
  }

  @Test
  public void isNotEmptyMapWithNonEmptyMap() {
    Map<String, Integer> map = new HashMap<>();

    map.put("one", 1);
    map.put("two", 2);

    assertThat(MapUtils.isNotEmpty(map)).isTrue();
    assertThat(MapUtils.isNotEmpty(Collections.singletonMap("one", 1))).isTrue();
    assertThat(MapUtils.isNotEmpty(Collections.singletonMap(null, null))).isTrue();
  }

  @Test
  public void isSizeOneWithNullMapIsFalse() {
    assertThat(MapUtils.isSizeOne(null)).isFalse();
  }

  @Test
  public void isSizeOneWithEmptyMapIsFalse() {
    assertThat(MapUtils.isSizeOne(Collections.emptyMap())).isFalse();
  }

  @Test
  public void isSizeOneWithMapHavingOneEntryIsTrue() {
    assertThat(MapUtils.isSizeOne(Collections.singletonMap("one", 1))).isTrue();
  }

  @Test
  public void isSizeOneWithMapHavingTwoEntriesIsFalse() {

    Map<Object, Object> map = new HashMap<>();

    map.put("one", 1);
    map.put("two", 2);

    assertThat(MapUtils.isSizeOne(map)).isFalse();
  }

  @Test
  public void isSizeXWithMapOfSizeXIsTrue() {

    assertThat(MapUtils.isSize(Collections.singletonMap("key", "value"), 1)).isTrue();

    Map<?, ?> twoEntryMap = MapBuilder.newHashMap()
      .put("keyOne", "valueOne")
      .put("keyTwo", "valueTwo")
      .build();

    assertThat(MapUtils.isSize(twoEntryMap, 2)).isTrue();
  }

  @Test
  public void isSizeXWithMapOfSizeYIsFalse() {
    assertThat(MapUtils.isSize(Collections.singletonMap("key", "value"), 2)).isFalse();
  }

  @Test
  public void isSizeXWithEmptyMapIsFalse() {
    assertThat(MapUtils.isSize(Collections.emptyMap(), 1)).isFalse();
  }

  @Test
  public void isSizeZeroWithEmptyMapIsTrue() {
    assertThat(MapUtils.isSize(Collections.emptyMap(), 0)).isTrue();
  }

  @Test
  public void isSizeXWithNullMapIsFalse() {
    assertThat(MapUtils.isSize(null, 1)).isFalse();
  }

  @Test
  public void isSizeZeroWithNullMapIsFalse() {
    assertThat(MapUtils.isSize(null, 0)).isTrue();
  }

  @Test
  public void newMapEntry() {

    Map.Entry<Object, Object> mapEntry = MapUtils.newMapEntry("TestKey", "TestValue");

    assertThat(mapEntry).isNotNull();
    assertThat(mapEntry.getKey()).isEqualTo("TestKey");
    assertThat(mapEntry.getValue()).isEqualTo("TestValue");
  }

  @Test(expected = UnsupportedOperationException.class)
  public void newMapEntryIsImmutable() {

    Map.Entry<Object, Object> mapEntry = MapUtils.newMapEntry("aKey", "aValue");

    assertThat(mapEntry).isNotNull();
    assertThat(mapEntry.getKey()).isEqualTo("aKey");
    assertThat(mapEntry.getValue()).isEqualTo("aValue");

    try {
      mapEntry.setValue("junk");
    }
    catch (UnsupportedOperationException expected) {

      assertThat(expected.getMessage()).isEqualTo(Constants.OPERATION_NOT_SUPPORTED);
      assertThat(expected.getCause()).isNull();

      throw expected;
    }
  }

  @Test
  public void nullSafeMapWithMap() {

    Map<Object, Object> map = Collections.singletonMap("one", 1);

    assertThat(MapUtils.nullSafeMap(map)).isSameAs(map);
  }

  @Test
  public void nullSafeMapWithEmptyMap() {

    Map<Object, Object> emptyMap = Collections.emptyMap();

    assertThat(MapUtils.nullSafeMap(emptyMap)).isSameAs(emptyMap);
  }

  @Test
  public void nullSafeMapWithNullMap() {

    Map<?, ?> map = MapUtils.nullSafeMap(null);

    assertThat(map).isNotNull();
    assertThat(map.isEmpty()).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfMapReturnsSize() {

    Map<Object, Object> map = new HashMap<>();

    map.put("one", 1);
    map.put("two", 2);

    assertThat(MapUtils.size(map)).isEqualTo(map.size());

    map = new HashMap<>();
    map.put("one", 1);
    map.put("one", 2);

    assertThat(MapUtils.size(map)).isEqualTo(1);
  }

  @Test
  public void sizeOfMapWithInitialCapacityReturnsZero() {
    assertThat(MapUtils.size(new HashMap<>(10))).isEqualTo(0);
  }

  @Test
  public void sizeOfEmptyMapReturnsZero() {
    assertThat(MapUtils.size(Collections.emptyMap())).isEqualTo(0);
  }

  @Test
  public void sizeOfNullMapReturnsZero() {
    assertThat(MapUtils.size(null)).isEqualTo(0);
  }

  @Test
  public void sizeOfSingleEntryMapReturnsOne() {
    assertThat(MapUtils.size(Collections.singletonMap("one", 1))).isEqualTo(1);
  }

  @Test
  public void toAssociativeArrayFromMap() {

    Map<String, String> map = new TreeMap<>();

    map.put("keyOne", "valueOne");
    map.put("keyTwo", "valueTwo");
    map.put("keyZero", "valueZero");

    String[] associativeArray = MapUtils.toAssociativeArray(map);

    assertThat(associativeArray).isNotNull();
    assertThat(associativeArray.length).isEqualTo(3);
    assertThat(Arrays.toString(associativeArray)).isEqualTo("[keyOne=valueOne, keyTwo=valueTwo, keyZero=valueZero]");
  }

  @Test
  public void toAssociativeArrayFromSingleElementMap() {

    Map<String, String> map = Collections.singletonMap("keyOne", "valueOne");

    String[] associativeArray = MapUtils.toAssociativeArray(map);

    assertThat(associativeArray).isNotNull();
    assertThat(associativeArray.length).isEqualTo(1);
    assertThat(Arrays.toString(associativeArray)).isEqualTo("[keyOne=valueOne]");
  }

  @Test
  public void toAssociativeArrayFromEmptyMap() {

    Map<String, String> map = Collections.emptyMap();

    String[] associativeArray = MapUtils.toAssociativeArray(map);

    assertThat(associativeArray).isNotNull();
    assertThat(associativeArray.length).isEqualTo(0);
    assertThat(Arrays.toString(associativeArray)).isEqualTo("[]");
  }

  @Test
  public void toAssociativeArrayFromNullMap() {

    String[] associativeArray = MapUtils.toAssociativeArray(null);

    assertThat(associativeArray).isNotNull();
    assertThat(associativeArray.length).isEqualTo(0);
    assertThat(Arrays.toString(associativeArray)).isEqualTo("[]");
  }

  @Test
  public void toStringWithEmptyMap() {
    assertThat(MapUtils.toString(Collections.emptyMap())).isEqualTo("[]");
  }

  @Test
  public void toStringWithNonEmptyMap() {

    Map<String, Integer> map = new HashMap<>(3);

    map.put("one", 1);
    map.put("two", 2);
    map.put("three", 3);

    String mapString = MapUtils.toString(map);

    String expected = "["
      .concat("\n\t").concat("one = 1,")
      .concat("\n\t").concat("three = 3,")
      .concat("\n\t").concat("two = 2")
      .concat("\n]");

    assertThat(mapString).isEqualTo(expected);
  }

  @Test
  public void toStringWithNullMap() {
    assertThat(MapUtils.toString(null)).isEqualTo("[]");
  }

  @Test
  public void toStringWithSingleEntryMap() {
    assertThat(MapUtils.toString(Collections.singletonMap("one", 1))).isEqualTo("[\n\tone = 1\n]");
  }

  @Test
  public void transform() {

    Map<Integer, String> map = new HashMap<>(3);

    map.put(1, "test");
    map.put(2, "testing");
    map.put(3, "tested");

    Map<Integer, String> resultMap = MapUtils.transform(map, String::toUpperCase);

    assertThat(resultMap).isNotNull();
    assertThat(resultMap).isNotSameAs(map);
    assertThat(resultMap.size()).isEqualTo(3);
    assertThat(resultMap.containsKey(1)).isTrue();
    assertThat(resultMap.containsKey(2)).isTrue();
    assertThat(resultMap.containsKey(3)).isTrue();
    assertThat(resultMap.get(1)).isEqualTo("TEST");
    assertThat(resultMap.get(2)).isEqualTo("TESTING");
    assertThat(resultMap.get(3)).isEqualTo("TESTED");
  }

  @Test
  public void transformEmptyMap() {

    Map<Object, Object> emptyMap = Collections.emptyMap();
    Map<Object, Object> resultMap =  MapUtils.transform(emptyMap, (value) -> null);

    assertThat(resultMap).isNotNull();
    assertThat(resultMap).isNotSameAs(emptyMap);
    assertThat(resultMap.isEmpty()).isTrue();
  }

  @Test(expected = IllegalArgumentException.class)
  public void transformWithNullMap() {

    try {
      MapUtils.transform(null, (value) -> null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Map is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void transformWithNullTransformer() {

    try {
      MapUtils.transform(Collections.emptyMap(), null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Transformer is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }
}
