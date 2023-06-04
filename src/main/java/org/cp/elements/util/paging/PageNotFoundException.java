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

package org.cp.elements.util.paging;

/**
 * The {@link PageNotFoundException} class is a {@link RuntimeException} indicating that a {@link Page}
 * could not be found in the {@link Pageable} object.
 *
 * @author John J. Blum
 * @see java.lang.RuntimeException
 * @see org.cp.elements.util.paging.Page
 * @see org.cp.elements.util.paging.Pageable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PageNotFoundException extends RuntimeException {

  /**
   * Default constructor to construct a new, uninitialized instance of {@link PageNotFoundException}.
   */
  public PageNotFoundException() {
  }

  /**
   * Constructs a new {@link PageNotFoundException} initialized with a given {@link String message}
   * to describe the {@link Page} not found error.
   *
   * @param message {@link String} describing the {@link Page} not found error.
   * @see java.lang.String
   */
  public PageNotFoundException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link PageNotFoundException} initialized with the given {@link Throwable}
   * to indicate the cause of the {@link Page} not found error.
   *
   * @param cause {@link Throwable} indicating the cause of the {@link Page} not found error.
   * @see java.lang.Throwable
   */
  public PageNotFoundException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link PageNotFoundException} initialized with a given {@link String message}
   * to describe the {@link Page} not found error and a {@link Throwable} to indicate the cause
   * of the {@link Page} not found error.
   *
   * @param message {@link String} describing the {@link Page} not found error.
   * @param cause {@link Throwable} indicating the cause of the {@link Page} not found error.
   * @see java.lang.String
   * @see java.lang.Throwable
   */
  public PageNotFoundException(String message, Throwable cause) {
    super(message, cause);
  }
}
