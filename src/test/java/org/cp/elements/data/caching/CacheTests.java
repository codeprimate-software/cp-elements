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

package org.cp.elements.data.caching;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.Map;

import org.cp.elements.util.MapBuilder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * The CacheTests class...
 *
 * @author John Blum
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
@SuppressWarnings("unchecked")
public class CacheTests {

  @Mock
  private Cache cache;

  @Test
  public void isEmptyReturnsTrueWhenSizeIsZero() {

    when(this.cache.isEmpty()).thenCallRealMethod();
    when(this.cache.size()).thenReturn(0);

    assertThat(this.cache.isEmpty()).isTrue();

    verify(this.cache, times(1)).size();
  }

  @Test
  public void isEmptyReturnsFalseWhenSizeIsNotZero() {

    when(this.cache.isEmpty()).thenCallRealMethod();
    when(this.cache.size()).thenReturn(1);

    assertThat(this.cache.isEmpty()).isFalse();

    verify(this.cache, times(1)).size();
  }

  @Test
  public void containsAllWithArrayReturnsTrueWhenAllKeysArePresent() {

    when(this.cache.containsAll(any(Comparable.class))).thenCallRealMethod();
    when(this.cache.contains(any())).thenReturn(true);

    assertThat(this.cache.containsAll("KeyOne", "KeyTwo", "KeyThree")).isTrue();
  }

  @Test
  public void containsAllWithArrayReturnsFalseWhenJustOneKeyIsNotPresent() {

    when(this.cache.containsAll(any(Comparable.class))).thenCallRealMethod();
    when(this.cache.contains(any())).thenReturn(true, false, true);

    assertThat(this.cache.containsAll("KeyOne", "KeyTwo", "KeyThree")).isFalse();
  }

  @Test
  public void containsAllWithEmptyArrayReturnsTrue() {
    when(this.cache.containsAll(any(Comparable.class))).thenCallRealMethod();
    assertThat(this.cache.containsAll()).isTrue();
  }

  @Test
  public void containsAllWithNullArrayIsNullSafeAndReturnsFalse() {
    assertThat(this.cache.containsAll((Comparable[]) null)).isFalse();
  }

  @Test
  public void containsAllWithIterableReturnsTrueWhenAllKeysArePresent() {

    when(this.cache.containsAll(any(Iterable.class))).thenCallRealMethod();
    when(this.cache.contains(any())).thenReturn(true);

    assertThat(this.cache.containsAll(Arrays.asList("KeyOne", "KeyTwo", "KeyThree"))).isTrue();
  }

  @Test
  public void containsAllWithIterableReturnsFalseWhenJustOneKeyIsNotPresent() {

    when(this.cache.containsAll(any(Iterable.class))).thenCallRealMethod();
    when(this.cache.contains(any())).thenReturn(true, false, true);

    assertThat(this.cache.containsAll(Arrays.asList("KeyOne", "KeyTwo", "KeyThree"))).isFalse();
  }

  @Test
  public void containsAllWithEmptyIterableReturnsTrue() {
    when(this.cache.containsAll(any(Iterable.class))).thenCallRealMethod();
    assertThat(this.cache.containsAll(Collections::emptyIterator)).isTrue();
  }

  @Test
  public void containsAllWithNullIterableIsNullSafeReturnsFalse() {
    assertThat(this.cache.containsAll((Iterable) null)).isFalse();
  }

  @Test
  public void containsAnyWithArrayReturnsTrueWhenJustOneKeyIsPresent() {

    when(this.cache.containsAny(any(Comparable.class))).thenCallRealMethod();
    when(this.cache.contains(any())).thenReturn(true, false, false);

    assertThat(this.cache.containsAny("KeyOne", "KeyTwo", "KeyThree")).isTrue();
  }

  @Test
  public void containsAnyWithArrayReturnsFalseWhenNoKeysArePresent() {

    when(this.cache.containsAny(any(Comparable.class))).thenCallRealMethod();
    when(this.cache.contains(any())).thenReturn(false);

    assertThat(this.cache.containsAny("KeyOne", "KeyTwo", "KeyThree")).isFalse();
  }

  @Test
  public void containsAnyWithEmptyArrayReturnsFalse() {
    when(this.cache.containsAny(any(Comparable.class))).thenCallRealMethod();
    assertThat(this.cache.containsAny()).isFalse();
  }

  @Test
  public void containsAnyWithNullArrayIsNullSafeReturnsFalse() {
    assertThat(this.cache.containsAny((Comparable[]) null)).isFalse();
  }

  @Test
  public void containsAnyWithIterableReturnsTrueWhenJustOneKeyIsPresent() {

    when(this.cache.containsAny(any(Iterable.class))).thenCallRealMethod();
    when(this.cache.contains(any())).thenReturn(true, false, false);

    assertThat(this.cache.containsAny(Arrays.asList("KeyOne", "KeyTwo", "KeyThree"))).isTrue();
  }

  @Test
  public void containsAnyWithIterableReturnsFalseWhenNoKeysArePresent() {

    when(this.cache.containsAny(any(Iterable.class))).thenCallRealMethod();
    when(this.cache.contains(any())).thenReturn(false);

    assertThat(this.cache.containsAny(Arrays.asList("KeyOne", "KeyTwo", "KeyThree"))).isFalse();
  }

  @Test
  public void containsAnyWithEmptyIterableReturnsFalse() {
    when(this.cache.containsAny(any(Iterable.class))).thenCallRealMethod();
    assertThat(this.cache.containsAny(Collections::emptyIterator)).isFalse();
  }

  @Test
  public void containsAnyWithNullIterableIsNullSafeReturnsFalse() {
    assertThat(this.cache.containsAny((Iterable) null)).isFalse();
  }

  @Test
  public void fromMapCallsPut() {

    Map<Integer, String> map = MapBuilder.<Integer, String>newHashMap()
      .put(1, "one")
      .put(2, "two")
      .put(3, "three")
      .build();

    doCallRealMethod().when(this.cache).from(any(Map.class));

    this.cache.from(map);

    verify(this.cache, times(1)).put(eq(1), eq("one"));
    verify(this.cache, times(1)).put(eq(2), eq("two"));
    verify(this.cache, times(1)).put(eq(3), eq("three"));
  }
}
