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
import static org.cp.elements.util.CollectionUtils.asSet;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.Map;

import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.Id;
import org.cp.elements.util.MapBuilder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Unit tests for {@link Cache}.
 *
 * @author John Blum
 * @see java.util.Map
 * @see lombok
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.caching.AbstractCache
 * @see org.cp.elements.data.caching.Cache
 * @see org.cp.elements.lang.Identifiable
 * @see org.cp.elements.util.MapBuilder
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
@SuppressWarnings("unchecked")
public class CacheTests {

  @Mock
  private Cache cache;

  @Test
  public void isEmptyReturnsTrueWhenSizeIsZero() {

    when(this.cache.size()).thenReturn(0);
    when(this.cache.isEmpty()).thenCallRealMethod();

    assertThat(this.cache.isEmpty()).isTrue();

    verify(this.cache, times(1)).size();
  }

  @Test
  public void isEmptyReturnsFalseWhenSizeIsNotZero() {

    when(this.cache.size()).thenReturn(1);
    when(this.cache.isEmpty()).thenCallRealMethod();

    assertThat(this.cache.isEmpty()).isFalse();

    verify(this.cache, times(1)).size();
  }

  @Test
  public void clearCache() {

    when(this.cache.keys()).thenReturn(asSet("KeyOne", "KeyTwo", "KeyThree"));

    doCallRealMethod().when(this.cache).evictAll(any(Iterable.class));
    doCallRealMethod().when(this.cache).clear();

    this.cache.clear();

    verify(this.cache, times(1)).evictAll(eq(asSet("KeyOne", "KeyTwo", "KeyThree")));
    verify(this.cache, times(1)).evict(eq("KeyOne"));
    verify(this.cache, times(1)).evict(eq("KeyTwo"));
    verify(this.cache, times(1)).evict(eq("KeyThree"));
  }

  @Test
  public void clearEmptyCache() {

    when(this.cache.keys()).thenReturn(Collections.emptySet());

    doCallRealMethod().when(this.cache).evictAll(any(Iterable.class));
    doCallRealMethod().when(this.cache).clear();

    this.cache.clear();

    verify(this.cache, times(1)).evictAll(eq(Collections.emptySet()));
    verify(this.cache, never()).evict(any());
  }

  @Test
  public void containsAllWithArrayReturnsTrueWhenAllKeysArePresent() {

    when(this.cache.contains(any())).thenReturn(true);
    when(this.cache.containsAll(any(Comparable.class))).thenCallRealMethod();

    assertThat(this.cache.containsAll("KeyOne", "KeyTwo", "KeyThree")).isTrue();

    verify(this.cache, times(3)).contains(any());
  }

  @Test
  public void containsAllWithArrayReturnsFalseWhenJustOneKeyIsNotPresent() {

    when(this.cache.contains(any())).thenReturn(true, false, true);
    when(this.cache.containsAll(any(Comparable.class))).thenCallRealMethod();

    assertThat(this.cache.containsAll("KeyOne", "KeyTwo", "KeyThree")).isFalse();

    verify(this.cache, times(2)).contains(any());
  }

  @Test
  public void containsAllWithEmptyArrayReturnsFalse() {

    when(this.cache.containsAll(any(Comparable.class))).thenCallRealMethod();

    assertThat(this.cache.containsAll()).isFalse();

    verify(this.cache, never()).contains(any());
  }

  @Test
  public void containsAllWithNullArrayIsNullSafeAndReturnsFalse() {

    assertThat(this.cache.containsAll((Comparable[]) null)).isFalse();

    verify(this.cache, never()).contains(any(Comparable.class));
  }

  @Test
  public void containsAllWithIterableReturnsTrueWhenAllKeysArePresent() {

    when(this.cache.contains(any())).thenReturn(true);
    when(this.cache.containsAll(any(Iterable.class))).thenCallRealMethod();

    assertThat(this.cache.containsAll(Arrays.asList("KeyOne", "KeyTwo", "KeyThree"))).isTrue();

    verify(this.cache, times(3)).contains(any());
  }

  @Test
  public void containsAllWithIterableReturnsFalseWhenJustOneKeyIsNotPresent() {

    when(this.cache.contains(any())).thenReturn(true, false, true);
    when(this.cache.containsAll(any(Iterable.class))).thenCallRealMethod();

    assertThat(this.cache.containsAll(Arrays.asList("KeyOne", "KeyTwo", "KeyThree"))).isFalse();

    verify(this.cache, times(2)).contains(any());
  }

  @Test
  public void containsAllWithEmptyIterableReturnsFalse() {

    when(this.cache.containsAll(any(Iterable.class))).thenCallRealMethod();

    assertThat(this.cache.containsAll(Collections::emptyIterator)).isFalse();

    verify(this.cache, never()).contains(any());
  }

  @Test
  public void containsAllWithNullIterableIsNullSafeAndReturnsFalse() {

    assertThat(this.cache.containsAll((Iterable) null)).isFalse();

    verify(this.cache, never()).contains(any());
  }

  @Test
  public void containsAnyWithArrayReturnsTrueWhenJustOneKeyIsPresent() {

    when(this.cache.contains(any())).thenReturn(false, true, false);
    when(this.cache.containsAny(any(Comparable.class))).thenCallRealMethod();

    assertThat(this.cache.containsAny("KeyOne", "KeyTwo", "KeyThree")).isTrue();

    verify(this.cache, times(2)).contains(any());
  }

  @Test
  public void containsAnyWithArrayReturnsFalseWhenNoKeysArePresent() {

    when(this.cache.contains(any())).thenReturn(false);
    when(this.cache.containsAny(any(Comparable.class))).thenCallRealMethod();

    assertThat(this.cache.containsAny("KeyOne", "KeyTwo", "KeyThree")).isFalse();

    verify(this.cache, times(3)).contains(any());
  }

  @Test
  public void containsAnyWithEmptyArrayReturnsFalse() {

    when(this.cache.containsAny(any(Comparable.class))).thenCallRealMethod();

    assertThat(this.cache.containsAny()).isFalse();

    verify(this.cache, never()).contains(any());
  }

  @Test
  public void containsAnyWithNullArrayIsNullSafeAndReturnsFalse() {

    assertThat(this.cache.containsAny((Comparable[]) null)).isFalse();

    verify(this.cache, never()).contains(any());
  }

  @Test
  public void containsAnyWithIterableReturnsTrueWhenJustOneKeyIsPresent() {

    when(this.cache.contains(any())).thenReturn(false, true, false);
    when(this.cache.containsAny(any(Iterable.class))).thenCallRealMethod();

    assertThat(this.cache.containsAny(Arrays.asList("KeyOne", "KeyTwo", "KeyThree"))).isTrue();

    verify(this.cache, times(2)).contains(any());
  }

  @Test
  public void containsAnyWithIterableReturnsFalseWhenNoKeysArePresent() {

    when(this.cache.contains(any())).thenReturn(false);
    when(this.cache.containsAny(any(Iterable.class))).thenCallRealMethod();

    assertThat(this.cache.containsAny(Arrays.asList("KeyOne", "KeyTwo", "KeyThree"))).isFalse();

    verify(this.cache, times(3)).contains(any());
  }

  @Test
  public void containsAnyWithEmptyIterableReturnsFalse() {

    when(this.cache.containsAny(any(Iterable.class))).thenCallRealMethod();

    assertThat(this.cache.containsAny(Collections::emptyIterator)).isFalse();

    verify(this.cache, never()).contains(any());
  }

  @Test
  public void containsAnyWithNullIterableIsNullSafeAndReturnsFalse() {

    assertThat(this.cache.containsAny((Iterable) null)).isFalse();

    verify(this.cache, never()).contains(any());
  }

  @Test
  public void evictAllWithArray() {

    doCallRealMethod().when(this.cache).evictAll(any(Comparable.class));

    this.cache.evictAll("KeyOne", "KeyTwo", "KeyThree");

    verify(this.cache, times(3)).evict(any());
  }

  @Test
  public void evictAllWithEmptyArray() {

    doCallRealMethod().when(this.cache).evictAll(any(Comparable.class));

    this.cache.evictAll();

    verify(this.cache, never()).evict(any());
  }

  @Test
  public void evictAllWithNullArray() {

    this.cache.evictAll((Comparable[]) null);

    verify(this.cache, never()).evict(any());
  }

  @Test
  public void evictAllWithIterable() {

    doCallRealMethod().when(this.cache).evictAll(any(Iterable.class));

    this.cache.evictAll(Arrays.asList("KeyOne", "KeyTwo", "KeyThree"));

    verify(this.cache, times(3)).evict(any());
  }

  @Test
  public void evictAllWithEmptyIterable() {

    doCallRealMethod().when(this.cache).evictAll(any(Iterable.class));

    this.cache.evictAll(Collections::emptyIterator);

    verify(this.cache, never()).evict(any());
  }

  @Test
  public void evictAllWithNullIterable() {

    this.cache.evictAll((Iterable) null);

    verify(this.cache, never()).evict(any());
  }

  @Test
  public void fromMapCallsPut() {

    Map map = MapBuilder.newHashMap()
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

  @Test
  public void getAllWithArrayReturnsList() {

    when(this.cache.get(any())).thenReturn("one", "two", "three");
    when(this.cache.getAll(any(Comparable.class))).thenCallRealMethod();

    assertThat(this.cache.getAll(1, 2, 3)).containsExactly("one", "two", "three");

    verify(this.cache, times(3)).get(any());
  }

  @Test
  public void getAllWithArrayReturnsListContainingNulls() {

    when(this.cache.get(any())).thenReturn("one", null, "three");
    when(this.cache.getAll(any(Comparable.class))).thenCallRealMethod();

    assertThat(this.cache.getAll(1, 2, 3)).containsExactly("one", null, "three");

    verify(this.cache, times(3)).get(any());
  }

  @Test
  public void getAllWithEmptyArrayReturnsEmptyList() {

    when(this.cache.getAll(any(Comparable.class))).thenCallRealMethod();

    assertThat(this.cache.getAll()).isEmpty();

    verify(this.cache, never()).get(any());
  }

  @Test
  public void getAllWithNullArrayIsNullSafeReturnsEmptyList() {

    assertThat(this.cache.getAll((Comparable[]) null)).isEmpty();

    verify(this.cache, never()).get(any());
  }

  @Test
  public void getAllWithIterableReturnsList() {

    when(this.cache.get(any())).thenReturn("one", "two", "three");
    when(this.cache.getAll(any(Iterable.class))).thenCallRealMethod();

    assertThat(this.cache.getAll(Arrays.asList(1, 2, 3))).containsExactly("one", "two", "three");

    verify(this.cache, times(3)).get(any());
  }

  @Test
  public void getAllWithIterableReturnsListContainingNulls() {

    when(this.cache.get(any())).thenReturn("one", null, "three");
    when(this.cache.getAll(any(Iterable.class))).thenCallRealMethod();

    assertThat(this.cache.getAll(Arrays.asList(1, 2, 3))).containsExactly("one", null, "three");

    verify(this.cache, times(3)).get(any());
  }

  @Test
  public void getAllWithEmptyIterableReturnsEmptyList() {

    when(this.cache.getAll(any(Iterable.class))).thenCallRealMethod();

    assertThat(this.cache.getAll(Collections::emptyIterator)).isEmpty();

    verify(this.cache, never()).get(any());
  }

  @Test
  public void getAllWithNullIterableIsNullSafeReturnsEmptyList() {

    assertThat(this.cache.getAll((Iterable) null)).isEmpty();

    verify(this.cache, never()).get(any());
  }

  @Test
  public void putEntity() {

    doCallRealMethod().when(this.cache).put(any(Identifiable.class));

    Person person = Person.newPerson().identifiedBy(1L);

    this.cache.put(person);

    verify(this.cache, times(1)).put(eq(1L), eq(person));
  }

  @Test(expected = IllegalArgumentException.class)
  public void putEntityWithNullIdThrowsIllegalArgumentException() {

    doCallRealMethod().when(this.cache).put(any(Identifiable.class));

    try {
      this.cache.put(Person.newPerson());
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Entity ID is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(this.cache, never()).put(any(), any());
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void putNullEntityThrowsIllegalArgumentException() {

    try {
      new TestCache().put(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Entity is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(this.cache, never()).put(any(), any());
    }
  }

  @Test
  public void putAllWithArray() {

    Person jonDoe = Person.newPerson().named("Jon", "Doe").identifiedBy(1L);
    Person janeDoe = Person.newPerson().named("Jane", "Doe").identifiedBy(2L);
    Person pieDoe = Person.newPerson().named("Jane", "Doe").identifiedBy(3L);

    doCallRealMethod().when(this.cache).put(any(Identifiable.class));
    doCallRealMethod().when(this.cache).putAll(any(Identifiable.class));

    this.cache.putAll(jonDoe, janeDoe, pieDoe);

    verify(this.cache, times(1)).put(eq(1L), eq(jonDoe));
    verify(this.cache, times(1)).put(eq(2L), eq(janeDoe));
    verify(this.cache, times(1)).put(eq(3L), eq(pieDoe));
  }

  @Test(expected = IllegalArgumentException.class)
  public void putAllWithArrayContainingNullsThrowsIllegalArgumentException() {

    Person jonDoe = Person.newPerson().named("Jon", "Doe").identifiedBy(1L);
    Person janeDoe = Person.newPerson().named("Jane", "Doe").identifiedBy(2L);

    TestCache testCache = spy(new TestCache());

    doNothing().when(testCache).put(any(), any());

    try {
      testCache.putAll(jonDoe, null, janeDoe);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Entity is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(testCache, times(1)).put(eq(1L), eq(jonDoe));
      verify(testCache, times(1)).put(isNull());
      verify(testCache, never()).put(eq(2L), eq(janeDoe));
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void putAllWithArrayContainingEntityWithNullIdThrowsIllegalArgumentException() {

    Person jonDoe = Person.newPerson().named("Jon", "Doe");
    Person janeDoe = Person.newPerson().named("Jane", "Doe").identifiedBy(2L);

    TestCache testCache = spy(new TestCache());

    try {
      testCache.putAll(jonDoe, janeDoe);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Entity ID is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(testCache, never()).put(any(), any());
    }
  }

  @Test
  public void putAllWithEmptyArray() {

    doCallRealMethod().when(this.cache).putAll(any(Identifiable.class));

    this.cache.putAll();

    verify(this.cache, never()).put(any(Identifiable.class));
    verify(this.cache, never()).put(any(), any());
  }

  @Test
  public void putAllWithNullArray() {

    TestCache testCache = spy(new TestCache());

    testCache.putAll((Identifiable[]) null);

    verify(testCache, never()).put(any(Identifiable.class));
    verify(testCache, never()).put(any(), any());
  }

  @Test
  public void putAllWithIterable() {

    Person cookieDoe = Person.newPerson().named("Cookie", "Doe").identifiedBy(1L);
    Person joeDoe = Person.newPerson().named("Joe", "Doe").identifiedBy(2L);
    Person sourDoe = Person.newPerson().named("Sour", "Doe").identifiedBy(3L);

    doCallRealMethod().when(this.cache).put(any(Identifiable.class));
    doCallRealMethod().when(this.cache).putAll(any(Iterable.class));

    this.cache.putAll(Arrays.asList(cookieDoe, joeDoe, sourDoe));

    verify(this.cache, times(1)).put(eq(1L), eq(cookieDoe));
    verify(this.cache, times(1)).put(eq(2L), eq(joeDoe));
    verify(this.cache, times(1)).put(eq(3L), eq(sourDoe));
  }

  @Test(expected = IllegalArgumentException.class)
  public void putAllWithIterableContainingNullsThrowsIllegalArgumentException() {

    Person jonDoe = Person.newPerson().named("Jon", "Doe").identifiedBy(1L);
    Person janeDoe = Person.newPerson().named("Jane", "Doe").identifiedBy(2L);

    TestCache testCache = spy(new TestCache());

    doNothing().when(testCache).put(any(), any());

    try {
      testCache.putAll(Arrays.asList(jonDoe, null, janeDoe));
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Entity is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(testCache, times(1)).put(eq(1L), eq(jonDoe));
      verify(testCache, times(1)).put(isNull());
      verify(testCache, never()).put(eq(2L), eq(janeDoe));
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void putAllWithIterableContainingEntityWithNullIdThrowsIllegalArgumentException() {

    Person jonDoe = Person.newPerson().named("Jon", "Doe");
    Person janeDoe = Person.newPerson().named("Jane", "Doe").identifiedBy(2L);

    TestCache testCache = spy(new TestCache());

    try {
      testCache.putAll(Arrays.asList(jonDoe, janeDoe));
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Entity ID is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(testCache, never()).put(any(), any());
    }
  }

  @Test
  public void putAllWithEmptyIterable() {

    doCallRealMethod().when(this.cache).putAll(any(Iterable.class));

    this.cache.putAll(Collections::emptyIterator);

    verify(this.cache, never()).put(any(Identifiable.class));
    verify(this.cache, never()).put(any(), any());
  }

  @Test
  public void putAllWithNullIterable() {

    TestCache testCache = spy(new TestCache());

    testCache.putAll((Iterable) null);

    verify(testCache, never()).put(any(Identifiable.class));
    verify(testCache, never()).put(any(), any());
  }

  @Test
  public void putIfAbsentWithKeyValue() {

    when(this.cache.contains(any())).thenReturn(false);
    doCallRealMethod().when(this.cache).putIfAbsent(any(), any());

    this.cache.putIfAbsent(1, "test");

    verify(this.cache, times(1)).contains(eq(1));
    verify(this.cache, never()).get(any());
    verify(this.cache, times(1)).put(eq(1), eq("test"));
  }

  @Test
  public void putIfAbsentWithKeyValueUsingExistingKey() {

    when(this.cache.contains(any())).thenReturn(true);
    when(this.cache.get(any())).thenReturn("existingValue");
    doCallRealMethod().when(this.cache).putIfAbsent(any(), any());

    assertThat(this.cache.putIfAbsent(1, "test")).isEqualTo("existingValue");

    verify(this.cache, times(1)).contains(eq(1));
    verify(this.cache, times(1)).get(eq(1));
    verify(this.cache, never()).put(any(), any());
  }

  @Test(expected = IllegalArgumentException.class)
  public void putIfAbsentWithKeyValueUsingNullKeyThrowsIllegalArgumentException() {

    doCallRealMethod().when(this.cache).putIfAbsent(any(), any());

    try {
      this.cache.putIfAbsent(null, "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Key is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(this.cache, never()).contains(any());
      verify(this.cache, never()).get(any());
      verify(this.cache, never()).put(any(), any());
    }
  }

  @Test
  public void putIfAbsentWithEntity() {

    when(this.cache.contains(any())).thenReturn(false);
    doCallRealMethod().when(this.cache).putIfAbsent(any(Identifiable.class));

    Person person = Person.newPerson().identifiedBy(1L);

    assertThat(this.cache.putIfAbsent(person)).isNull();

    verify(this.cache, times(1)).contains(eq(1L));
    verify(this.cache, never()).get(anyLong());
    verify(this.cache, times(1)).put(eq(1L), eq(person));
  }

  @Test(expected = IllegalArgumentException.class)
  public void putIfAbsentWithEntityHavingNullId() {

    doCallRealMethod().when(this.cache).putIfAbsent(any(Identifiable.class));

    try {
      this.cache.putIfAbsent(Person.newPerson().named("Jon", "Doe"));
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Entity ID is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(this.cache, never()).contains(any());
      verify(this.cache, never()).get(any());
      verify(this.cache, never()).put(any(), any());
    }
  }

  @Test
  public void putIfAbsentWithExistingEntity() {

    Identifiable<Long> mockIdentifiable = mock(Identifiable.class);

    when(this.cache.contains(any())).thenReturn(true);
    when(this.cache.get(eq(1L))).thenReturn(mockIdentifiable);
    doCallRealMethod().when(this.cache).putIfAbsent(any(Identifiable.class));

    assertThat(this.cache.putIfAbsent(Person.newPerson().identifiedBy(1L))).isEqualTo(mockIdentifiable);

    verify(this.cache, times(1)).contains(eq(1L));
    verify(this.cache, times(1)).get(eq(1L));
    verify(this.cache, never()).put(any(), any());
  }

  @Test
  public void putIfAbsentWithNullEntity() {

    try {
      this.cache.putIfAbsent(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Entity is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(this.cache, never()).contains(any());
      verify(this.cache, never()).get(any());
      verify(this.cache, never()).put(any(), any());
    }
  }

  @Test
  public void putIfPresentWithKeyValue() {

    when(this.cache.contains(any())).thenReturn(true);
    when(this.cache.get(any())).thenReturn("existingValue");
    doCallRealMethod().when(this.cache).putIfPresent(any(), any());

    assertThat(this.cache.putIfPresent(1, "test")).isEqualTo("existingValue");

    verify(this.cache, times(1)).contains(eq(1));
    verify(this.cache, times(1)).get(any());
    verify(this.cache, times(1)).put(eq(1), eq("test"));
  }

  @Test
  public void putIfPresentWithKeyValueUsingNonExistingKey() {

    when(this.cache.contains(any())).thenReturn(false);
    doCallRealMethod().when(this.cache).putIfPresent(any(), any());

    assertThat(this.cache.putIfPresent(1, "test")).isNull();

    verify(this.cache, times(1)).contains(eq(1));
    verify(this.cache, never()).get(any());
    verify(this.cache, never()).put(any(), any());
  }

  @Test(expected = IllegalArgumentException.class)
  public void putIfPresentWithKeyValueUsingNullKey() {

    doCallRealMethod().when(this.cache).putIfPresent(any(), any());

    try {
      this.cache.putIfPresent(null, "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Key is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(this.cache, never()).contains(any());
      verify(this.cache, never()).get(any());
      verify(this.cache, never()).put(any(), any());
    }
  }

  @Test
  public void putIfPresentWithEntity() {

    Identifiable<Long> mockIdentifiable = mock(Identifiable.class);

    when(this.cache.contains(any())).thenReturn(true);
    when(this.cache.get(eq(1L))).thenReturn(mockIdentifiable);
    doCallRealMethod().when(this.cache).putIfPresent(any(Identifiable.class));

    Person person = Person.newPerson().identifiedBy(1L);

    assertThat(this.cache.putIfPresent(person)).isEqualTo(mockIdentifiable);

    verify(this.cache, times(1)).contains(eq(1L));
    verify(this.cache, times(1)).get(eq(1L));
    verify(this.cache, times(1)).put(eq(1L), eq(person));
  }

  @Test
  public void putIfPresentWithEntityHavingNullId() {

    doCallRealMethod().when(this.cache).putIfPresent(any(Identifiable.class));

    assertThat(this.cache.putIfPresent(Person.newPerson().named("Jon", "Doe"))).isNull();

    verify(this.cache, times(1)).contains(isNull());
    verify(this.cache, never()).get(any());
    verify(this.cache, never()).put(any(), any());
  }

  @Test
  public void putIfPresentWithNonExistingEntity() {

    when(this.cache.contains(any())).thenReturn(false);
    doCallRealMethod().when(this.cache).putIfPresent(any(Identifiable.class));

    assertThat(this.cache.putIfPresent(Person.newPerson().identifiedBy(1L))).isNull();

    verify(this.cache, times(1)).contains(eq(1L));
    verify(this.cache, never()).put(any(), any());
  }

  @Test(expected = IllegalArgumentException.class)
  public void putIfPresentWithNullEntity() {

    doCallRealMethod().when(this.cache).putIfPresent(any());

    try {
      this.cache.putIfPresent(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Entity is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(this.cache, never()).contains(any());
      verify(this.cache, never()).get(any());
      verify(this.cache, never()).put(any(), any());
    }
  }

  @Test
  public void toMap() {

    when(this.cache.keys()).thenReturn(asSet(1, 2));
    when(this.cache.get(eq(1))).thenReturn("one");
    when(this.cache.get(eq(2))).thenReturn("two");
    when(this.cache.toMap()).thenCallRealMethod();

    Map<Integer, String> map = this.cache.toMap();

    assertThat(map).isNotNull();
    assertThat(map).hasSize(2);
    assertThat(map).containsEntry(1, "one");
    assertThat(map).containsEntry(2, "two");
  }

  @Test
  public void toMapFromEmptyCache() {

    when(this.cache.keys()).thenReturn(Collections.emptySet());
    when(this.cache.toMap()).thenCallRealMethod();

    Map<Integer, String> map = this.cache.toMap();

    assertThat(map).isNotNull();
    assertThat(map).isEmpty();
  }

  @Data
  @NoArgsConstructor(staticName = "newPerson")
  static class Person implements Identifiable<Long> {

    @Id
    private Long id;

    private String firstName;
    private String lastName;

    String getName() {
      return String.format("%1$s %2$s", getFirstName(), getLastName());
    }

    Person named(String firstName, String lastName) {

      this.firstName = firstName;
      this.lastName = lastName;

      return this;
    }

    @Override
    public boolean equals(Object obj) {

      if (this == obj) {
        return true;
      }

      if (!(obj instanceof Person)) {
        return false;
      }

      Person that = (Person) obj;

      return ObjectUtils.equals(this.getId(), that.getId())
        && ObjectUtils.equals(this.getName(), that.getName());
    }

    @Override
    public int hashCode() {

      int hashValue = 17;

      hashValue = 37 * hashValue + ObjectUtils.hashCode(this.getId());
      hashValue = 37 * hashValue + ObjectUtils.hashCode(this.getName());

      return hashValue;
    }

    @Override
    public String toString() {
      return String.format("%s(%d)", getName(), getId());
    }
  }

  static class TestCache<KEY extends Comparable<KEY>, VALUE> extends AbstractCache<KEY, VALUE> {

  }
}
