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
package org.cp.elements.security;

/**
 * An {@link AbstractSecurityException} used to indicate a user is not authorized (no privileges),
 * or does not have permission to access the target resource.
 *
 * @author John J. Blum
 * @see AbstractSecurityException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AuthorizationException extends AbstractSecurityException {

  /**
   * Constructs a new {@link AuthorizationException} with no {@link String message}
   * and no {@link Throwable cause}.
   */
  public AuthorizationException() { }

  /**
   * Constructs a new {@link AuthorizationException} initialized with the given {@link String message}
   * describing the security violation.
   *
   * @param message {@link String} used to describe the security violation.
   */
  public AuthorizationException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link AuthorizationException} initialized with the given {@link Throwable cause}
   * used as the reason this security violation was thrown.
   *
   * @param cause {@link Throwable} used as the reason this security violation was thrown.
   */
  public AuthorizationException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link AuthorizationException} initialized with the given {@link String message}
   * describing the security violation along with the given {@link Throwable cause} used as the reason
   * this security violation was thrown.
   *
   * @param message {@link String} used to describe the security violation.
   * @param cause {@link Throwable} used as the reason this security violation was thrown.
   */
  public AuthorizationException(String message, Throwable cause) {
    super(message, cause);
  }
}
