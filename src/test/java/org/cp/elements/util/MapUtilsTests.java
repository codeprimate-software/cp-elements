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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

import org.cp.elements.lang.Constants;
import org.cp.elements.lang.FilteringTransformer;
import org.cp.elements.lang.NumberUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for {@link MapUtils}.
 *
 * @author John J. Blum
 * @see java.util.Collections
 * @see java.util.Map
 * @see org.junit.Test
 * @see org.junit.Rule
 * @see org.junit.rules.ExpectedException
 * @see org.cp.elements.util.MapUtils
 * @since 1.0.0
 */
public class MapUtilsTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Test
  public void countMapReturnsSize() {

    Map<Object, Object> map = new HashMap<>();

    map.put("one", 1);
    map.put("two", 2);

    assertThat(MapUtils.count(map), is(equalTo(map.size())));
  }

  @Test
  public void countMapWithInitialCapacityReturnsZero() {
    assertThat(MapUtils.count(new HashMap<>(10)), is(equalTo(0)));
  }

  @Test
  public void countEmptyMapReturnsZero() {
    assertThat(MapUtils.count(Collections.emptyMap()), is(equalTo(0)));
  }

  @Test
  public void countNullMapReturnsZero() {
    assertThat(MapUtils.count(null), is(equalTo(0)));
  }

  @Test
  public void countSingleEntryMapReturnsOne() {
    assertThat(MapUtils.count(Collections.singletonMap("one", 1)), is(equalTo(1)));
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

    assertThat(MapUtils.count(map, (entry) -> NumberUtils.isEven(entry.getValue())), is(equalTo(4)));
    assertThat(MapUtils.count(map, (entry) -> NumberUtils.isOdd(entry.getValue())), is(equalTo(5)));
  }

  @Test
  public void countMapWithFilterAcceptsAll() {

    Map<String, Integer> map = new HashMap<>();

    map.put("one", 1);
    map.put("two", 2);

    assertThat(MapUtils.count(map, (entry) -> true), is(equalTo(map.size())));
  }

  @Test
  public void countMapWithFilterRejectsAll() {

    Map<String, Integer> map = new HashMap<>();

    map.put("one", 1);
    map.put("two", 2);

    assertThat(MapUtils.count(map, (entry) -> false), is(equalTo(0)));
  }

  @Test
  public void countEmptyMapWithFilter() {
    assertThat(MapUtils.count(Collections.emptyMap(), (entry) -> true), is(equalTo(0)));
  }

  @Test
  public void countNullMapWithFilter() {
    assertThat(MapUtils.count(null, (entry) -> true), is(equalTo(0)));
  }

  @Test
  public void countNonNullMapWithNullFilter() {

    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Filter cannot be null");

    MapUtils.count(Collections.emptyMap(), null);
  }

  @Test
  public void filter() {
    Map<String, Integer> map = new HashMap<>(3);

    map.put("one", 1);
    map.put("two", 2);
    map.put("three", 3);

    Map<String, Integer> evenMap = MapUtils.filter(map, (entry) -> NumberUtils.isEven(entry.getValue()));

    assertThat(evenMap, is(notNullValue(Map.class)));
    assertThat(evenMap, is(not(sameInstance(map))));
    assertThat(evenMap.size(), is(equalTo(1)));
    assertThat(evenMap.containsKey("one"), is(false));
    assertThat(evenMap.containsKey("two"), is(true));
    assertThat(evenMap.containsKey("three"), is(false));
    assertThat(evenMap.get("two"), is(equalTo(2)));

    Map<String, Integer> oddMap = MapUtils.filter(map, (entry) -> NumberUtils.isOdd(entry.getValue()));

    assertThat(map, is(notNullValue(Map.class)));
    assertThat(oddMap, is(not(sameInstance(map))));
    assertThat(oddMap.size(), is(equalTo(2)));
    assertThat(oddMap.containsKey("one"), is(true));
    assertThat(oddMap.containsKey("two"), is(false));
    assertThat(oddMap.containsKey("three"), is(true));
    assertThat(oddMap.get("one"), is(equalTo(1)));
    assertThat(oddMap.get("three"), is(equalTo(3)));
  }

  @Test
  public void filterAcceptsAll() {
    Map<String, Integer> map = new HashMap<>(2);

    map.put("one", 1);
    map.put("two", 2);

    Map<String, Integer> resultMap = MapUtils.filter(map, (entry) -> true);

    assertThat(resultMap, is(notNullValue(Map.class)));
    assertThat(resultMap, is(not(sameInstance(map))));
    assertThat(resultMap.size(), is(equalTo(map.size())));
    assertThat(resultMap.containsKey("one"), is(true));
    assertThat(resultMap.containsKey("two"), is(true));
    assertThat(resultMap.get("one"), is(equalTo(1)));
    assertThat(resultMap.get("two"), is(equalTo(2)));
  }

  @Test
  public void filterRejectsAll() {
    Map<String, Integer> map = new HashMap<>(2);

    map.put("one", 1);
    map.put("two", 2);

    Map<String, Integer> resultMap = MapUtils.filter(map, (entry) -> false);

    assertThat(resultMap, is(notNullValue(Map.class)));
    assertThat(resultMap, is(not(sameInstance(map))));
    assertThat(resultMap.isEmpty(), is(true));
  }

  @Test
  public void filterEmptyMap() {
    Map<Object, Object> emptyMap = Collections.emptyMap();
    Map<Object, Object> resultMap = MapUtils.filter(emptyMap, (entry) -> true);

    assertThat(resultMap, is(notNullValue(Map.class)));
    assertThat(resultMap, is(not(sameInstance(emptyMap))));
    assertThat(resultMap.isEmpty(), is(true));
  }

  @Test
  public void filterWithNullFilter() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Filter cannot be null");

    MapUtils.filter(Collections.emptyMap(), null);
  }

  @Test
  public void filterWithNullMap() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Map cannot be null");

    MapUtils.filter(null, (entry) -> true);
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

    assertThat(resultMap, is(notNullValue(Map.class)));
    assertThat(resultMap, is(not(sameInstance(map))));
    assertThat(resultMap.size(), is(equalTo(1)));
    assertThat(resultMap.containsKey("one"), is(false));
    assertThat(resultMap.containsKey("two"), is(true));
    assertThat(resultMap.containsKey("three"), is(false));
    assertThat(resultMap.get("two"), is(equalTo(4)));

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

    assertThat(resultMap, is(notNullValue(Map.class)));
    assertThat(resultMap, is(not(sameInstance(map))));
    assertThat(resultMap.size(), is(equalTo(2)));
    assertThat(resultMap.containsKey("one"), is(true));
    assertThat(resultMap.containsKey("two"), is(false));
    assertThat(resultMap.containsKey("three"), is(true));
    assertThat(resultMap.get("one"), is(equalTo(1)));
    assertThat(resultMap.get("three"), is(equalTo(9)));
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

    assertThat(resultMap, is(notNullValue(Map.class)));
    assertThat(resultMap, is(not(sameInstance(map))));
    assertThat(resultMap.size(), is(equalTo(1)));
    assertThat(resultMap.containsKey("key"), is(true));
    assertThat(resultMap.get("key"), is(equalTo("TEST")));
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

    assertThat(resultMap, is(notNullValue(Map.class)));
    assertThat(resultMap, is(not(sameInstance(map))));
    assertThat(resultMap.isEmpty(), is(true));
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

    assertThat(resultMap, is(notNullValue(Map.class)));
    assertThat(resultMap, is(not(sameInstance(emptyMap))));
    assertThat(resultMap.isEmpty(), is(true));
  }

  @Test
  public void filterAndTransformWithNullFilter() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("FilteringTransformer cannot be null");

    MapUtils.filterAndTransform(Collections.emptyMap(), null);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void filterAndTransformWithNullMap() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Map cannot be null");

    MapUtils.filterAndTransform(null, mock(FilteringTransformer.class));
  }

  @Test
  public void findAll() {
    Map<String, String> map = new HashMap<>(3);

    map.put("one", "test");
    map.put("two", "testing");
    map.put("three", "tested");

    Map<String, String> resultMap = MapUtils.findAll(map, (entry) -> "test".equals(entry.getValue()));

    assertThat(resultMap, is(notNullValue(Map.class)));
    assertThat(resultMap, is(not(sameInstance(map))));
    assertThat(resultMap.size(), is(equalTo(1)));
    assertThat(resultMap.containsKey("one"), is(true));
    assertThat(resultMap.containsKey("two"), is(false));
    assertThat(resultMap.containsKey("three"), is(false));
    assertThat(resultMap.get("one"), is(equalTo("test")));

    resultMap = MapUtils.findAll(map, (entry) -> entry.getValue().endsWith("ing") || entry.getValue().endsWith("ed"));

    assertThat(resultMap, is(notNullValue(Map.class)));
    assertThat(resultMap, is(not(sameInstance(map))));
    assertThat(resultMap.size(), is(equalTo(2)));
    assertThat(resultMap.containsKey("one"), is(false));
    assertThat(resultMap.containsKey("two"), is(true));
    assertThat(resultMap.containsKey("three"), is(true));
    assertThat(resultMap.get("two"), is(equalTo("testing")));
    assertThat(resultMap.get("three"), is(equalTo("tested")));
  }

  @Test
  public void findAllAcceptsAll() {
    Map<String, String> map = Collections.singletonMap("one", "test");
    Map<String, String> resultMap = MapUtils.findAll(map, (entry) -> true);

    assertThat(resultMap, is(notNullValue(Map.class)));
    assertThat(resultMap, is(not(sameInstance(map))));
    assertThat(resultMap.size(), is(equalTo(1)));
    assertThat(resultMap.containsKey("one"), is(true));
    assertThat(resultMap.get("one"), is(equalTo("test")));
  }

  @Test
  public void findAllRejectsAll() {
    Map<String, String> map = Collections.singletonMap("one", "test");
    Map<String, String> resultMap = MapUtils.findAll(map, (entry) -> false);

    assertThat(resultMap, is(notNullValue(Map.class)));
    assertThat(resultMap, is(not(sameInstance(map))));
    assertThat(resultMap.isEmpty(), is(true));
  }

  @Test
  public void findAllWithEmptyMap() {
    Map<String, String> emptyMap = Collections.emptyMap();
    Map<String, String> resultMap = MapUtils.findAll(emptyMap, (entry) -> true);

    assertThat(resultMap, is(notNullValue(Map.class)));
    assertThat(resultMap, is(not(sameInstance(emptyMap))));
    assertThat(resultMap.isEmpty(), is(true));
  }

  @Test
  public void findAllWithNullFilter() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Filter cannot be null");

    MapUtils.findAll(Collections.emptyMap(), null);
  }

  @Test
  public void findAllWithNullMap() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Map cannot be null");

    MapUtils.findAll(null, (entry) -> true);
  }

  @Test
  public void fromAssociativeArray() {
    String[] associativeArray = { "keyOne=valueOne", "keyTwo=valueTwo" };

    Map<String, String> map = MapUtils.fromAssociativeArray(associativeArray);

    assertThat(map, is(notNullValue(Map.class)));
    assertThat(map.size(), is(equalTo(associativeArray.length)));
    assertThat(map.containsKey("keyOne"), is(true));
    assertThat(map.get("keyOne"), is(equalTo("valueOne")));
    assertThat(map.containsKey("keyTwo"), is(true));
    assertThat(map.get("keyTwo"), is(equalTo("valueTwo")));
  }

  @Test
  public void fromEmptyAssociativeArray() {
    Map<String, String> map = MapUtils.fromAssociativeArray(new String[0]);

    assertThat(map, is(notNullValue(Map.class)));
    assertThat(map.isEmpty(), is(true));
  }

  @Test
  public void fromSingleEntryAssociativeArray() {
    Map<String, String> map = MapUtils.fromAssociativeArray(new String[] { "key=value" });

    assertThat(map, is(notNullValue(Map.class)));
    assertThat(map.size(), is(equalTo(1)));
    assertThat(map.containsKey("key"), is(true));
    assertThat(map.get("key"), is(equalTo("value")));
  }

  @Test
  public void fromInvalidAssociateArray() {
    String[] associativeArray = { "keyOne=valueOne", "", "keyThree" };

    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Entry [] at index [1] must be specified");

    MapUtils.fromAssociativeArray(associativeArray);
  }

  @Test
  public void fromInvalidEntryInAssociateArray() {
    String[] associativeArray = { "keyOne=valueOne", "keyTwo=valueTwo", "keyThree" };

    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Entry [keyThree] at index [2] must have both a key and a value");

    MapUtils.fromAssociativeArray(associativeArray);
  }

  @Test
  public void isEmptyMapWithEmptyMap() {
    assertThat(MapUtils.isEmpty(new HashMap<>(10)), is(true));
    assertThat(MapUtils.isEmpty(Collections.emptyMap()), is(true));
    assertThat(MapUtils.isEmpty(null), is(true));
  }

  @Test
  public void isEmptyWithNonEmptyMap() {
    Map<String, Integer> map = new HashMap<>();

    map.put("one", 1);
    map.put("two", 2);

    assertThat(MapUtils.isEmpty(map), is(false));
    assertThat(MapUtils.isEmpty(Collections.singletonMap("one", 1)), is(false));
    assertThat(MapUtils.isEmpty(Collections.singletonMap(null, null)), is(false));
  }

  @Test
  public void isNotEmptyMapWithEmptyMap() {
    assertThat(MapUtils.isNotEmpty(new HashMap<>(10)), is(false));
    assertThat(MapUtils.isNotEmpty(Collections.emptyMap()), is(false));
    assertThat(MapUtils.isNotEmpty(null), is(false));
  }

  @Test
  public void isNotEmptyMapWithNonEmptyMap() {
    Map<String, Integer> map = new HashMap<>();

    map.put("one", 1);
    map.put("two", 2);

    assertThat(MapUtils.isNotEmpty(map), is(true));
    assertThat(MapUtils.isNotEmpty(Collections.singletonMap("one", 1)), is(true));
    assertThat(MapUtils.isNotEmpty(Collections.singletonMap(null, null)), is(true));
  }

  @Test
  public void isSizeOneWithNullMapIsFalse() {
    assertThat(MapUtils.isSizeOne(null), is(false));
  }

  @Test
  public void isSizeOneWithEmptyMapIsFalse() {
    assertThat(MapUtils.isSizeOne(Collections.emptyMap()), is(false));
  }

  @Test
  public void isSizeOneWithMapHavingOneEntryIsTrue() {
    assertThat(MapUtils.isSizeOne(Collections.singletonMap("one", 1)), is(true));
  }

  @Test
  public void isSizeOneWithMapHavingTwoEntriesIsFalse() {

    Map<Object, Object> map = new HashMap<>();

    map.put("one", 1);
    map.put("two", 2);

    assertThat(MapUtils.isSizeOne(map), is(false));
  }

  @Test
  public void nullSafeMapWithMap() {
    Map<Object, Object> map = Collections.singletonMap("one", 1);

    assertThat(MapUtils.nullSafeMap(map), is(sameInstance(map)));
  }

  @Test
  public void nullSafeMapWithEmptyMap() {
    Map<Object, Object> emptyMap = Collections.emptyMap();

    assertThat(MapUtils.nullSafeMap(emptyMap), is(sameInstance(emptyMap)));
  }

  @Test
  public void nullSafeMapWithNullMap() {
    Map<?, ?> map = MapUtils.nullSafeMap(null);

    assertThat(map, is(notNullValue(Map.class)));
    assertThat(map.isEmpty(), is(true));
  }

  @Test
  public void sizeOfMapReturnsSize() {
    Map<Object, Object> map = new HashMap<>();

    map.put("one", 1);
    map.put("two", 2);

    assertThat(MapUtils.size(map), is(equalTo(map.size())));

    map = new HashMap<>();
    map.put("one", 1);
    map.put("one", 2);

    assertThat(MapUtils.size(map), is(equalTo(1)));
  }

  @Test
  public void sizeOfMapWithInitialCapacityReturnsZero() {
    assertThat(MapUtils.size(new HashMap<>(10)), is(equalTo(0)));
  }

  @Test
  public void sizeOfEmptyMapReturnsZero() {
    assertThat(MapUtils.size(Collections.emptyMap()), is(equalTo(0)));
  }

  @Test
  public void sizeOfNullMapReturnsZero() {
    assertThat(MapUtils.size(null), is(equalTo(0)));
  }

  @Test
  public void sizeOfSingleEntryMapReturnsOne() {
    assertThat(MapUtils.size(Collections.singletonMap("one", 1)), is(equalTo(1)));
  }

  @Test
  public void toAssociativeArrayFromMap() {
    Map<String, String> map = new TreeMap<>();

    map.put("keyOne", "valueOne");
    map.put("keyTwo", "valueTwo");
    map.put("keyZero", "valueZero");

    String[] associativeArray = MapUtils.toAssociativeArray(map);

    assertThat(associativeArray, is(notNullValue(String[].class)));
    assertThat(associativeArray.length, is(equalTo(3)));
    assertThat(Arrays.toString(associativeArray), is(equalTo("[keyOne=valueOne, keyTwo=valueTwo, keyZero=valueZero]")));
  }

  @Test
  public void toAssociativeArrayFromSingleElementMap() {
    Map<String, String> map = Collections.singletonMap("keyOne", "valueOne");
    String[] associativeArray = MapUtils.toAssociativeArray(map);

    assertThat(associativeArray, is(notNullValue(String[].class)));
    assertThat(associativeArray.length, is(equalTo(1)));
    assertThat(Arrays.toString(associativeArray), is(equalTo("[keyOne=valueOne]")));
  }

  @Test
  public void toAssociativeArrayFromEmptyMap() {
    Map<String, String> map = Collections.emptyMap();
    String[] associativeArray = MapUtils.toAssociativeArray(map);

    assertThat(associativeArray, is(notNullValue(String[].class)));
    assertThat(associativeArray.length, is(equalTo(0)));
    assertThat(Arrays.toString(associativeArray), is(equalTo("[]")));
  }

  @Test
  public void toAssociativeArrayFromNullMap() {
    String[] associativeArray = MapUtils.toAssociativeArray(null);

    assertThat(associativeArray, is(notNullValue(String[].class)));
    assertThat(associativeArray.length, is(equalTo(0)));
    assertThat(Arrays.toString(associativeArray), is(equalTo("[]")));
  }

  @Test
  public void toStringWithEmptyMap() {
    assertThat(MapUtils.toString(Collections.emptyMap()), is(equalTo("[]")));
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

    assertThat(mapString, is(equalTo(expected)));
  }

  @Test
  public void toStringWithNullMap() {
    assertThat(MapUtils.toString(null), is(equalTo("[]")));
  }

  @Test
  public void toStringWithSingleEntryMap() {
    assertThat(MapUtils.toString(Collections.singletonMap("one", 1)), is(equalTo("[\n\tone = 1\n]")));
  }

  @Test
  public void transform() {
    Map<Integer, String> map = new HashMap<>(3);

    map.put(1, "test");
    map.put(2, "testing");
    map.put(3, "tested");

    Map<Integer, String> resultMap = MapUtils.transform(map, String::toUpperCase);

    assertThat(resultMap, is(notNullValue(Map.class)));
    assertThat(resultMap, is(not(sameInstance(map))));
    assertThat(resultMap.size(), is(equalTo(3)));
    assertThat(resultMap.containsKey(1), is(true));
    assertThat(resultMap.containsKey(2), is(true));
    assertThat(resultMap.containsKey(3), is(true));
    assertThat(resultMap.get(1), is(equalTo("TEST")));
    assertThat(resultMap.get(2), is(equalTo("TESTING")));
    assertThat(resultMap.get(3), is(equalTo("TESTED")));
  }

  @Test
  public void transformEmptyMap() {
    Map<Object, Object> emptyMap = Collections.emptyMap();
    Map<Object, Object> resultMap =  MapUtils.transform(emptyMap, (value) -> null);

    assertThat(resultMap, is(notNullValue(Map.class)));
    assertThat(resultMap, is(not(sameInstance(emptyMap))));
    assertThat(resultMap.isEmpty(), is(true));
  }

  @Test
  public void transformWithNullMap() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Map cannot be null");

    MapUtils.transform(null, (value) -> null);
  }

  @Test
  public void transformWithNullTransformer() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Transformer cannot be null");

    MapUtils.transform(Collections.emptyMap(), null);
  }
}
