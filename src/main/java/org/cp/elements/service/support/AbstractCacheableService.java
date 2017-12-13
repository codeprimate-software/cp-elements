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

package org.cp.elements.service.support;

import java.util.concurrent.atomic.AtomicBoolean;

/**
 * {@link AbstractCacheableService} is an abstract base class extended by application service classes
 * in order to keep track of the cacheable state of the service's operations.
 *
 * @author John Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AbstractCacheableService {

  private final AtomicBoolean cacheMiss = new AtomicBoolean(false);

  /**
   * Determines whether the cacheable service operation resulted in a cache hit.
   *
   * @return a boolean value indicating whether the cacehable service operation
   * resulted in a cache hit.
   * @see #isCacheMiss()
   */
  public boolean isCacheHit() {
    return !isCacheMiss();
  }

  /**
   * Determines whether the cacheable service operation resulted in a cache miss.
   *
   * @return a boolean value indicating whether the cacehable service operation
   * resulted in a cache miss.
   * @see #isCacheHit()
   */
  public boolean isCacheMiss() {
    return this.cacheMiss.getAndSet(false);
  }

  /**
   * Sets the state of the cacheable service as a cache miss for the current operation.
   *
   * @return a boolean value indicating whether the current cacheable service operation
   * was the first cache miss in the transaction.
   */
  protected boolean setCacheMiss() {
    return this.cacheMiss.compareAndSet(false, true);
  }
}
