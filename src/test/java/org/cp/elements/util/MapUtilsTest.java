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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.Transformer;
import org.cp.elements.lang.support.DefaultFilter;
import org.junit.Test;

/**
 * The MapUtilsTest class is a test suite of test cases testing the contract and functionality of the MapUtils class.
 *
 * @author John J. Blum
 * @see java.util.Map
 * @see org.cp.elements.util.MapUtils
 * @see org.junit.Test
 * @since 1.0.0
 */
public class MapUtilsTest {

  @Test
  public void testCount() {
    Map<String, String> map = new HashMap<>(6);

    map.put("test", "testing");
    map.put(null, null);
    map.put("testing", "tested");
    map.put("null", null);
    map.put("tested", "test");
    map.put("nil", "null");

    assertNotNull(map);
    assertFalse(map.isEmpty());
    assertEquals(6, map.size());

    assertEquals(3, MapUtils.count(map, (Map.Entry<String, String> entry) ->
      StringUtils.contains(entry.getKey(), "test") || StringUtils.contains(entry.getValue(), "test")));
  }

  @Test
  public void testCountReturnsSize() {
    Map<String, String> map = new HashMap<>(1);

    map.put("key", "value");

    assertNotNull(map);
    assertEquals(map.size(), MapUtils.count(map, new DefaultFilter<>(true)));
  }

  @Test
  public void testCountReturnsZero() {
    Map<String, String> map = new HashMap<>(1);

    map.put("key", "value");

    assertNotNull(map);
    assertEquals(0, MapUtils.count(map, new DefaultFilter<>(false)));
  }

  @Test(expected = NullPointerException.class)
  public void testCountWithNullMap() {
    MapUtils.count(null, new DefaultFilter<>(true));
  }

  @Test(expected = NullPointerException.class)
  public void testCountWithNullFilter() {
    MapUtils.count(Collections.emptyMap(), null);
  }

  @Test
  public void testEmptyMap() {
    Map<String, String> expectedMap = new HashMap<>(1);

    expectedMap.put("key", "value");

    assertFalse(expectedMap.isEmpty());

    Map<?, ?> actualMap = MapUtils.emptyMap(expectedMap);

    assertSame(expectedMap, actualMap);
    assertFalse(actualMap.isEmpty());
  }

  @Test
  public void testEmptyMapWithEmptyMap() {
    Map<String, String> expectedMap = new HashMap<>(0);

    assertTrue(expectedMap.isEmpty());

    Map<?, ?> actualMap = MapUtils.emptyMap(expectedMap);

    assertSame(expectedMap, actualMap);
    assertTrue(actualMap.isEmpty());
  }

  @Test
  public void testEmptyMapWithNullMap() {
    Map<?, ?> actualMap = MapUtils.emptyMap(null);

    assertNotNull(actualMap);
    assertTrue(actualMap.isEmpty());
  }

  @Test
  public void testFilter() {
    Map<String, String> map = new HashMap<>(6);

    map.put("test", "testing");
    map.put(null, null);
    map.put("testing", "tested");
    map.put("null", null);
    map.put("tested", "test");
    map.put("nil", "null");

    assertNotNull(map);
    assertFalse(map.isEmpty());
    assertEquals(6, map.size());

    Map<String, String> filteredMap = MapUtils.filter(map, (Map.Entry<String, String> entry) ->
      entry.getKey() != null && entry.getValue() != null);

    assertSame(map, filteredMap);
    assertFalse(map.isEmpty());
    assertEquals(4, map.size());
  }

  @Test(expected = NullPointerException.class)
  public void testFilterWithNullMap() {
    MapUtils.filter(null, new DefaultFilter<>(true));
  }

  @Test(expected = NullPointerException.class)
  public void testFilterWithNullFilter() {
    MapUtils.filter(Collections.emptyMap(), null);
  }

  @Test
  public void testFind() {
    Map<String, String> map = new HashMap<>(6);

    map.put("test", "testing");
    map.put(null, null);
    map.put("testing", "tested");
    map.put("null", null);
    map.put("tested", "test");
    map.put("nil", "null");

    assertNotNull(map);
    assertFalse(map.isEmpty());
    assertEquals(6, map.size());

    Map<String, String> resultMap = MapUtils.find(map, (Map.Entry<String, String> entry) ->
      StringUtils.contains(entry.getKey(), "test"));

    assertNotNull(resultMap);
    assertNotSame(map, resultMap);
    assertFalse(resultMap.isEmpty());
    assertEquals(3, resultMap.size());
    assertTrue(resultMap.keySet().containsAll(Arrays.asList("test", "testing", "tested")));
  }

  @Test(expected = NullPointerException.class)
  public void testFindWithNullMap() {
    MapUtils.find(null, new DefaultFilter<>(true));
  }

  @Test(expected = NullPointerException.class)
  public void testFindWithNullFilter() {
    MapUtils.find(Collections.emptyMap(), null);
  }

  @Test
  public void testIsEmptyMap() {
    assertTrue(MapUtils.isEmpty(null));
    assertTrue(MapUtils.isEmpty(Collections.emptyMap()));
    assertFalse(MapUtils.isEmpty(Collections.singletonMap("mySingleKey", "mySingleValue")));
    assertFalse(MapUtils.isEmpty(Collections.singletonMap(null, null)));
  }

  @Test
  public void testSizeOfMap() {
    assertEquals(0, MapUtils.size(null));
    assertEquals(0, MapUtils.size(Collections.emptyMap()));
    assertEquals(1, MapUtils.size(Collections.singletonMap("mySingleKey", "mySingleValue")));
    assertEquals(1, MapUtils.size(Collections.singletonMap(null, null)));
  }

  @Test
  public void testTransform() {
    Map<Integer, String> map = new HashMap<>(3);

    map.put(0, "zero");
    map.put(1, "one");
    map.put(2, "two");

    Transformer<String> transformer = String::toUpperCase;

    Map<Integer, String > actualMap = MapUtils.transform(map, transformer);

    assertSame(map, actualMap);
    assertFalse(actualMap.isEmpty());
    assertEquals(3, actualMap.size());
    assertEquals("ZERO", map.get(0));
    assertEquals("ONE", map.get(1));
    assertEquals("TWO", map.get(2));
  }

  @Test
  public void testTransformEmptyMap() {
    Map emptyMap = Collections.emptyMap();

    assertTrue(emptyMap.isEmpty());
    assertSame(emptyMap, MapUtils.transform(emptyMap, (value) -> null));
    assertTrue(emptyMap.isEmpty());
  }

  @Test(expected = NullPointerException.class)
  public void testTransformNullMap() {
    MapUtils.transform(null, (value) -> null);
  }

  @Test(expected = NullPointerException.class)
  public void testTransformWithNullTransformer() {
    MapUtils.transform(Collections.emptyMap(), null);
  }

}
