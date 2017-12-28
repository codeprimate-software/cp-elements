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

package org.cp.elements.data.caching.support;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;

/**
 * Unit tests for {@link ReadOnlyCache}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.data.caching.support.ReadOnlyCache
 * @since 1.0.0
 */
@SuppressWarnings("unchecked")
public class ReadOnlyCacheTests {

  private ReadOnlyCache cache = new ReadOnlyCache() { };

  @Test
  public void isEmptyReturnsTrue() {
    assertThat(this.cache.isEmpty()).isTrue();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void clearThrowsUnsupportedOperationException() {

    try {
      this.cache.clear();
    }
    catch (UnsupportedOperationException expected) {

      assertThat(expected).hasMessage("Clear is not supported");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void containsReturnsFalse() {
    assertThat(this.cache.contains("key")).isFalse();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void evictThrowsUnsupportedOperationException() {

    try {
      this.cache.evict("key");
    }
    catch (UnsupportedOperationException expected) {

      assertThat(expected).hasMessage("Eviction is not supported");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void getReturnsNull() {
    assertThat(this.cache.get("key")).isNull();
  }

  @Test
  public void iteratorReturnsEmptyIterator() {
    assertThat(this.cache.iterator()).isEmpty();
  }

  @Test
  public void keysReturnsEmptySet() {
    assertThat(this.cache.keys()).isEmpty();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void putThrowsUnsupportedOperationException() {

    try {
      this.cache.put("key", "value");
    }
    catch (UnsupportedOperationException expected) {

      assertThat(expected).hasMessage("Put is not supported");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void sizeReturnsZero() {
    assertThat(this.cache.size()).isZero();
  }
}
