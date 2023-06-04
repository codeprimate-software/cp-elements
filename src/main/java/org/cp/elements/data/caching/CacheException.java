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
 * A {@link RuntimeException} implementation defining a general classification of exceptions for caching.
 *
 * @author John Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class CacheException extends RuntimeException {

  /**
   * Default constructor used to construct a new, uninitialized instance of {@link CacheException}
   * with no {@link String message} or {@link Throwable cause}.
   */
  public CacheException() { }

  /**
   * Constructs a new {@link CacheException} initialized with the given {@link String message}
   * to describe this cache error.
   *
   * @param message {@link String} containing a {@literal  description} of this {@link CacheException}.
   */
  public CacheException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link CacheException} initialized with the given {@link Throwable cause}
   * used as the underlying reason for this {@link CacheException}.
   *
   * @param cause {@link Throwable} used as the reason (cause) for this {@link CacheException}.
   */
  public CacheException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link CacheException} initialized with the given {@link String message}
   * to describe this cache error along with the given {@link Throwable cause} used as the underlying reason
   * for this {@link CacheException}.
   *
   * @param message {@link String} containing a {@literal  description} of this {@link CacheException}.
   * @param cause {@link Throwable} used as the reason (cause) for this {@link CacheException}.
   */
  public CacheException(String message, Throwable cause) {
    super(message, cause);
  }
}
