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
package org.cp.elements.service;

/**
 * A general {@link RuntimeException} implementation to classify problems with an application service.
 *
 * @author John Blum
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ServiceException extends RuntimeException {

  /**
   * Constructs a new, uninitialized instance of {@link ServiceException}.
   */
  public ServiceException() { }

  /**
   * Constructs a new {@link ServiceException} initialized with the given {@link String message}
   * describing the {@link RuntimeException}.
   *
   * @param message {@link String} containing a description of the {@link RuntimeException}.
   */
  public ServiceException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link ServiceException} initialized with the given {@link Throwable}
   * used as the reason this {@link RuntimeException} was thrown.
   *
   * @param cause {@link Throwable} used as the reason this {@link ServiceUnavailableException} was thrown.
   */
  public ServiceException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link ServiceException} initialized with the given {@link String message}
   * describing the {@link RuntimeException} and {@link Throwable} used as the reason this {@link RuntimeException}
   * was thrown.
   *
   * @param message {@link String} containing a description of the {@link RuntimeException}.
   * @param cause {@link Throwable} used as the reason this {@link RuntimeException} was thrown.
   */
  public ServiceException(String message, Throwable cause) {
    super(message, cause);
  }
}
