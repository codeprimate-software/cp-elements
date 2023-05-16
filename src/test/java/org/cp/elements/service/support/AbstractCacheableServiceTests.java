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

package org.cp.elements.service.support;

import static org.cp.elements.lang.LangExtensions.assertThat;

import org.junit.Before;
import org.junit.jupiter.api.Test;

/**
 * Unit tests for {@link AbstractCacheableService}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.service.support.AbstractCacheableService
 * @since 1.0.0
 */
public class AbstractCacheableServiceTests {

  private AbstractCacheableService cacheableService;

  @Before
  public void setup() {
    this.cacheableService = new TestCacheableService();
  }

  @Test
  public void isCacheHitWhenCachedReturnsTrue() {
    assertThat(this.cacheableService.isCacheHit()).isTrue();
    assertThat(this.cacheableService.isCacheHit()).isTrue();
  }

  @Test
  public void isCacheHitWhenNotCachedReturnsFalse() {
    assertThat(this.cacheableService.setCacheMiss()).isTrue();
    assertThat(this.cacheableService.isCacheHit()).isFalse();
  }

  @Test
  public void isCacheHitWhenNotCachedReturnsFalseThenResetsWhenQueriedAndReturnsTrue() {
    assertThat(this.cacheableService.setCacheMiss()).isTrue();
    assertThat(this.cacheableService.isCacheHit()).isFalse();
    assertThat(this.cacheableService.isCacheHit()).isTrue();
  }

  @Test
  public void isCacheMissWhenCachedReturnsFalse() {
    assertThat(this.cacheableService.isCacheMiss()).isFalse();
    assertThat(this.cacheableService.isCacheMiss()).isFalse();
  }

  @Test
  public void isCacheMissWhenNotCachedReturnsTrue() {
    assertThat(this.cacheableService.setCacheMiss()).isTrue();
    assertThat(this.cacheableService.isCacheMiss()).isTrue();
  }

  @Test
  public void isCacheMissWhenNotCachedReturnsTrueThenResetsWhenQueriedAndReturnsFalse() {
    assertThat(this.cacheableService.setCacheMiss()).isTrue();
    assertThat(this.cacheableService.isCacheMiss()).isTrue();
    assertThat(this.cacheableService.isCacheMiss()).isFalse();
  }

  @Test
  public void setCacheMissTwiceReturnsTrueThenFalse() {
    assertThat(this.cacheableService.setCacheMiss()).isTrue();
    assertThat(this.cacheableService.setCacheMiss()).isFalse();
  }

  static class TestCacheableService extends AbstractCacheableService {
  }
}
