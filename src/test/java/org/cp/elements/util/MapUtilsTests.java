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
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.cp.elements.lang.Constants;
import org.cp.elements.lang.NumberUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * The MapUtilsTests class is a test suite of test cases testing the contract and functionality
 * of the {@link MapUtils} class.
 *
 * @author John J. Blum
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
    fail(Constants.NOT_IMPLEMENTED);
  }

  @Test
  public void filterAndTransform() {
    fail(Constants.NOT_IMPLEMENTED);
  }

  @Test
  public void findAll() {
    fail(Constants.NOT_IMPLEMENTED);
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
  public void transform() {
    fail(Constants.NOT_IMPLEMENTED);
  }
}
