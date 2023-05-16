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
package org.cp.elements.data.caching;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.lang.ThrowableAssertions.assertThatUnsupportedOperationException;

import java.util.Set;

import org.junit.Before;
import org.junit.jupiter.api.Test;

import org.cp.elements.lang.ThrowableOperation;

/**
 * Unit Tests for {@link AbstractCache}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.data.caching.AbstractCache
 * @since 1.0.0
 */
public class AbstractCacheUnitTests {

  private AbstractCache<Integer, Object> cache;

  @Before
  public void constructNewAbstractCache() {
    this.cache = new TestCache<>();
  }

  @Test
  public void setAndGetLock() {

    Object lock = new Object();

    assertThat(this.cache.getLock()).isNull();

    this.cache.setLock(lock);

    assertThat(this.cache.getLock()).isSameAs(lock);

    this.cache.setLock(null);

    assertThat(this.cache.getLock()).isNull();
  }

  @Test
  public void getNameWhenNamedReturnsCache() {

    assertThat(this.cache.<TestCache<Integer, Object>>named("TestCache")).isSameAs(this.cache);
    assertThat(this.cache.getName()).isEqualTo("TestCache");
  }

  @Test
  public void getNameWhenUnnamedReturnsNull() {
    assertThat(this.cache.getName()).isNull();
  }

  @Test
  public void clearIsNotSupported() {

    assertThatUnsupportedOperationException()
      .isThrownBy(ThrowableOperation.fromRunnable(() -> this.cache.clear()))
      .havingMessage("Clear is not supported")
      .withNoCause();
  }

  @Test
  public void evictionIsNotSupported() {

    assertThatUnsupportedOperationException()
      .isThrownBy(ThrowableOperation.fromRunnable(() -> this.cache.evict(1)))
      .havingMessage("Eviction is not supported")
      .withNoCause();
  }

  @Test
  public void getIsNotSupported() {

    assertThatUnsupportedOperationException()
      .isThrownBy(args -> this.cache.get(1))
      .havingMessage("Get is not supported")
      .withNoCause();
  }

  @Test
  public void keysReturnsEmptySet() {

    Set<?> keys = this.cache.keys();

    assertThat(keys).isNotNull();
    assertThat(keys).isEmpty();
  }

  @Test
  public void putIsNotSupported() {

    assertThatUnsupportedOperationException()
      .isThrownBy(ThrowableOperation.fromRunnable(() -> this.cache.put(1, "test")))
      .havingMessage("Put is not supported")
      .withNoCause();
  }

  static class TestCache<KEY extends Comparable<KEY>, VALUE> extends AbstractCache<KEY, VALUE> { }

}
