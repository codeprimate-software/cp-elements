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
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.Map;

import org.cp.elements.data.caching.support.AbstractCache;
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
 * Unit test for {@link Cache}.
 *
 * @author John Blum
 * @see java.util.Map
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.caching.Cache
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
  public void containsAllWithArrayReturnsTrueWhenAllKeysArePresent() {

    when(this.cache.contains(any())).thenReturn(true);
    when(this.cache.containsAll(any(Comparable.class))).thenCallRealMethod();

    assertThat(this.cache.containsAll("KeyOne", "KeyTwo", "KeyThree")).isTrue();
  }

  @Test
  public void containsAllWithArrayReturnsFalseWhenJustOneKeyIsNotPresent() {

    when(this.cache.contains(any())).thenReturn(true, false, true);
    when(this.cache.containsAll(any(Comparable.class))).thenCallRealMethod();

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

    when(this.cache.contains(any())).thenReturn(true);
    when(this.cache.containsAll(any(Iterable.class))).thenCallRealMethod();

    assertThat(this.cache.containsAll(Arrays.asList("KeyOne", "KeyTwo", "KeyThree"))).isTrue();
  }

  @Test
  public void containsAllWithIterableReturnsFalseWhenJustOneKeyIsNotPresent() {

    when(this.cache.contains(any())).thenReturn(true, false, true);
    when(this.cache.containsAll(any(Iterable.class))).thenCallRealMethod();

    assertThat(this.cache.containsAll(Arrays.asList("KeyOne", "KeyTwo", "KeyThree"))).isFalse();
  }

  @Test
  public void containsAllWithEmptyIterableReturnsTrue() {
    when(this.cache.containsAll(any(Iterable.class))).thenCallRealMethod();
    assertThat(this.cache.containsAll(Collections::emptyIterator)).isTrue();
  }

  @Test
  public void containsAllWithNullIterableIsNullSafeAndReturnsFalse() {
    assertThat(this.cache.containsAll((Iterable) null)).isFalse();
  }

  @Test
  public void containsAnyWithArrayReturnsTrueWhenJustOneKeyIsPresent() {

    when(this.cache.contains(any())).thenReturn(true, false, false);
    when(this.cache.containsAny(any(Comparable.class))).thenCallRealMethod();

    assertThat(this.cache.containsAny("KeyOne", "KeyTwo", "KeyThree")).isTrue();
  }

  @Test
  public void containsAnyWithArrayReturnsFalseWhenNoKeysArePresent() {

    when(this.cache.contains(any())).thenReturn(false);
    when(this.cache.containsAny(any(Comparable.class))).thenCallRealMethod();

    assertThat(this.cache.containsAny("KeyOne", "KeyTwo", "KeyThree")).isFalse();
  }

  @Test
  public void containsAnyWithEmptyArrayReturnsFalse() {
    when(this.cache.containsAny(any(Comparable.class))).thenCallRealMethod();
    assertThat(this.cache.containsAny()).isFalse();
  }

  @Test
  public void containsAnyWithNullArrayIsNullSafeAndReturnsFalse() {
    assertThat(this.cache.containsAny((Comparable[]) null)).isFalse();
  }

  @Test
  public void containsAnyWithIterableReturnsTrueWhenJustOneKeyIsPresent() {

    when(this.cache.contains(any())).thenReturn(true, false, false);
    when(this.cache.containsAny(any(Iterable.class))).thenCallRealMethod();

    assertThat(this.cache.containsAny(Arrays.asList("KeyOne", "KeyTwo", "KeyThree"))).isTrue();
  }

  @Test
  public void containsAnyWithIterableReturnsFalseWhenNoKeysArePresent() {

    when(this.cache.contains(any())).thenReturn(false);
    when(this.cache.containsAny(any(Iterable.class))).thenCallRealMethod();

    assertThat(this.cache.containsAny(Arrays.asList("KeyOne", "KeyTwo", "KeyThree"))).isFalse();
  }

  @Test
  public void containsAnyWithEmptyIterableReturnsFalse() {
    when(this.cache.containsAny(any(Iterable.class))).thenCallRealMethod();
    assertThat(this.cache.containsAny(Collections::emptyIterator)).isFalse();
  }

  @Test
  public void containsAnyWithNullIterableIsNullSafeAndReturnsFalse() {
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

  @Test
  public void getAllWithArrayReturnsListOfValues() {

    when(this.cache.get(any(Integer.class))).thenReturn("one", "two", "three");
    when(this.cache.getAll(any(Comparable.class))).thenCallRealMethod();

    assertThat(this.cache.getAll(1, 2, 3)).containsExactly("one", "two", "three");
  }

  @Test
  public void getAllWithArrayMayReturnListContainingNulls() {

    when(this.cache.get(any(Integer.class))).thenReturn("one", null, "three");
    when(this.cache.getAll(any(Comparable.class))).thenCallRealMethod();

    assertThat(this.cache.getAll(1, 2, 3)).containsExactly("one", null, "three");
  }

  @Test
  public void getAllWithEmptyArrayReturnsEmptyList() {
    when(this.cache.getAll(any(Comparable.class))).thenCallRealMethod();
    assertThat(this.cache.getAll()).isEmpty();
  }

  @Test
  public void getAllWithNullArrayIsNullSafeAndReturnsEmptyList() {
    when(this.cache.getAll(any(Comparable.class))).thenCallRealMethod();
    assertThat(this.cache.getAll()).isEmpty();
  }

  @Test
  public void getAllWithIterableReturnsList() {

    when(this.cache.get(any(Integer.class))).thenReturn("one", "two", "three");
    when(this.cache.getAll(any(Iterable.class))).thenCallRealMethod();

    assertThat(this.cache.getAll(Arrays.asList(1, 2, 3))).containsExactly("one", "two", "three");
  }

  @Test
  public void getAllWithIterableMayReturnListContainingNulls() {

    when(this.cache.get(any(Integer.class))).thenReturn("one", null, "three");
    when(this.cache.getAll(any(Iterable.class))).thenCallRealMethod();

    assertThat(this.cache.getAll(Arrays.asList(1, 2, 3))).containsExactly("one", null, "three");
  }

  @Test
  public void getAllWithEmptyIterableReturnsEmptyList() {
    when(this.cache.getAll(any(Iterable.class))).thenCallRealMethod();
    assertThat(this.cache.getAll(Collections::emptyIterator)).isEmpty();
  }

  @Test
  public void getAllWithNullIterableIsNullSafeReturnsEmptyList() {
    when(this.cache.getAll(any(Iterable.class))).thenCallRealMethod();
    assertThat(this.cache.getAll(Collections::emptyIterator)).isEmpty();
  }

  @Test
  public void putEntity() {

    doCallRealMethod().when(this.cache).put(any(Identifiable.class));

    Person person = Person.newPerson().identifiedBy(1L);

    this.cache.put(person);

    verify(this.cache, times(1)).put(eq(1L), eq(person));
  }

  @Test(expected = IllegalArgumentException.class)
  public void putEntityHavingNullIdThrowsIllegalArgumentException() {

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
      verify(this.cache, never()).put(anyLong(), any(Person.class));
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
      verify(this.cache, never()).put(anyLong(), any(Person.class));
    }
  }

  @Test
  public void putAllWithArray() {

    doCallRealMethod().when(this.cache).put(any(Identifiable.class));
    doCallRealMethod().when(this.cache).putAll(any(Identifiable.class));

    Person jonDoe = Person.newPerson().named("Jon", "Doe").identifiedBy(1L);
    Person janeDoe = Person.newPerson().named("Jane", "Doe").identifiedBy(2L);
    Person pieDoe = Person.newPerson().named("Jane", "Doe").identifiedBy(3L);

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

    doNothing().when(testCache).put(any(), any(Identifiable.class));

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

    doCallRealMethod().when(this.cache).put(any(Identifiable.class));
    doCallRealMethod().when(this.cache).putAll(any(Iterable.class));

    Person cookieDoe = Person.newPerson().named("Cookie", "Doe").identifiedBy(1L);
    Person joeDoe = Person.newPerson().named("Joe", "Doe").identifiedBy(2L);
    Person sourDoe = Person.newPerson().named("Sour", "Doe").identifiedBy(3L);

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

    doNothing().when(testCache).put(any(), any(Identifiable.class));

    try {
      testCache.putAll(Arrays.asList(jonDoe, null, janeDoe));
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Entity is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(testCache, times(2)).put(isA(Identifiable.class));
      verify(testCache, times(1)).put(isNull());
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

    when(this.cache.contains(any(Comparable.class))).thenReturn(false);
    doCallRealMethod().when(this.cache).putIfAbsent(any(), any());

    this.cache.putIfAbsent(1L, "test");

    verify(this.cache, times(1)).contains(eq(1L));
    verify(this.cache, times(1)).put(eq(1L), eq("test"));
  }

  @Test
  public void putIfAbsentWithKeyValueUsingExistingKey() {

    when(this.cache.contains(any(Comparable.class))).thenReturn(true);
    doCallRealMethod().when(this.cache).putIfAbsent(any(), any());

    this.cache.putIfAbsent(1L, "test");

    verify(this.cache, times(1)).contains(eq(1L));
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
      verify(this.cache, never()).put(any(), any());
    }
  }

  @Test
  public void putIfAbsentWithEntity() {

    when(this.cache.contains(any(Comparable.class))).thenReturn(false);
    doCallRealMethod().when(this.cache).putIfAbsent(any(Identifiable.class));

    Person person = Person.newPerson().identifiedBy(1L);

    this.cache.putIfAbsent(person);

    verify(this.cache, times(1)).contains(eq(1L));
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
      verify(this.cache, never()).contains(eq(1L));
      verify(this.cache, never()).put(any(Comparable.class), any());
    }
  }

  @Test
  public void putIfAbsentWithExistingEntity() {

    when(this.cache.contains(any(Comparable.class))).thenReturn(true);
    doCallRealMethod().when(this.cache).putIfAbsent(any(Identifiable.class));

    Person person = Person.newPerson().identifiedBy(1L);

    this.cache.putIfAbsent(person);

    verify(this.cache, times(1)).contains(eq(1L));
    verify(this.cache, never()).put(any(Comparable.class), any());
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
      verify(this.cache, never()).contains(eq(1L));
      verify(this.cache, never()).put(any(Comparable.class), any());
    }
  }

  @Test
  public void putIfPresentWithKeyValue() {

    when(this.cache.contains(any(Comparable.class))).thenReturn(true);
    doCallRealMethod().when(this.cache).putIfPresent(any(Comparable.class), any());

    this.cache.putIfPresent(1L, "test");

    verify(this.cache, times(1)).contains(eq(1L));
    verify(this.cache, times(1)).put(eq(1L), eq("test"));
  }

  @Test
  public void putIfPresentWithKeyValueUsingNonExistingKey() {

    when(this.cache.contains(any(Comparable.class))).thenReturn(false);
    doCallRealMethod().when(this.cache).putIfPresent(any(Comparable.class), any());

    this.cache.putIfPresent(1L, "test");

    verify(this.cache, times(1)).contains(eq(1L));
    verify(this.cache, never()).put(any(Comparable.class), any());
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
      verify(this.cache, never()).contains(any(Comparable.class));
      verify(this.cache, never()).put(any(Comparable.class), any());
    }
  }

  @Test
  public void putIfPresentWithEntity() {

    when(this.cache.contains(any(Comparable.class))).thenReturn(true);
    doCallRealMethod().when(this.cache).putIfPresent(any(Identifiable.class));

    Person person = Person.newPerson().identifiedBy(1L);

    this.cache.putIfPresent(person);

    verify(this.cache, times(1)).contains(eq(1L));
    verify(this.cache, times(1)).put(eq(1L), eq(person));
  }

  @Test
  public void putIfPresentWithEntityHavingNullId() {

    doCallRealMethod().when(this.cache).putIfPresent(any(Identifiable.class));

    this.cache.putIfPresent(Person.newPerson().named("Jon", "Doe"));

    verify(this.cache, times(1)).contains(isNull());
    verify(this.cache, never()).put(any(Comparable.class), any());
  }

  @Test
  public void putIfPresentWithNonExistingEntity() {

    when(this.cache.contains(any(Comparable.class))).thenReturn(false);
    doCallRealMethod().when(this.cache).putIfPresent(any(Identifiable.class));

    Person person = Person.newPerson().identifiedBy(1L);

    this.cache.putIfPresent(person);

    verify(this.cache, times(1)).contains(eq(1L));
    verify(this.cache, never()).put(eq(1L), eq(person));
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
      verify(this.cache, never()).contains(any(Comparable.class));
      verify(this.cache, never()).put(any(Comparable.class), any());
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
