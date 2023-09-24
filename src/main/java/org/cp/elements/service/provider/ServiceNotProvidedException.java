/*
 * Copyright 2017-Present Author or Authors.
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
package org.cp.elements.service.provider;

import org.cp.elements.service.ServiceException;

/**
 * {@link ServiceNotProvidedException} thrown when a service is not provided by a service provider.
 *
 * @author John Blum
 * @see org.cp.elements.service.ServiceException
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public class ServiceNotProvidedException extends ServiceException {

  /**
   * Constructs a new {@link ServiceNotProvidedException} with no {@link String message} and no {@link Throwable cause}.
   */
  public ServiceNotProvidedException() { }

  /**
   * Constructs a new {@link ServiceNotProvidedException} initialized with the given {@link String message}
   * describing this {@link ServiceException}.
   *
   * @param message {@link String} containing a {@literal description} of this {@link ServiceNotProvidedException}.
   */
  public ServiceNotProvidedException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link ServiceNotProvidedException} initialized with the given {@link Throwable}
   * used as the {@literal cause} of this {@link ServiceException}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of this {@link ServiceNotProvidedException}.
   */
  public ServiceNotProvidedException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link ServiceNotProvidedException} initialized with the given {@link String message}
   * describing this {@link RuntimeException} and {@link Throwable} used as the {@literal cause}
   * of this {@link ServiceException}.
   *
   * @param message {@link String} containing a {@literal description} of this {@link ServiceNotProvidedException}.
   * @param cause {@link Throwable} used as the {@literal cause} of this {@link ServiceNotProvidedException}.
   */
  public ServiceNotProvidedException(String message, Throwable cause) {
    super(message, cause);
  }
}
