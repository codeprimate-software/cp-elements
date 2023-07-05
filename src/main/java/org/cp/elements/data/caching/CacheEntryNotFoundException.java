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

/**
 * {@link CacheEntryException} thrown when a {@link Cache.Entry} cannot be found.
 *
 * @author John Blum
 * @see org.cp.elements.data.caching.CacheEntryException
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public class CacheEntryNotFoundException extends CacheEntryException {

  /**
   * Constructs a new {@link CacheEntryNotFoundException} with no {@link String message}
   * and no probable {@link Throwable cause}.
   */
  public CacheEntryNotFoundException() { }

  /**
   * Constructs a new {@link CacheEntryNotFoundException} with the given {@link String message}
   * describing the exception.
   *
   * @param message {@link String} containing a description of the exception.
   */
  public CacheEntryNotFoundException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link CacheEntryNotFoundException} with the given {@link Throwable cause}
   * as the reason this exception was thrown.
   *
   * @param cause {@link Throwable} used as the underlying reason this exception was thrown.
   */
  public CacheEntryNotFoundException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link CacheEntryNotFoundException} with the given {@link String message} describing the exception
   * along with the given {@link Throwable cause} as the reason this exception was thrown.
   *
   * @param message {@link String} containing a description of the exception.
   * @param cause {@link Throwable} used as the underlying reason this exception was thrown.
   */
  public CacheEntryNotFoundException(String message, Throwable cause) {
    super(message, cause);
  }
}
