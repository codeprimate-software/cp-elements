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

import java.util.Optional;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * A Java {@link java.lang.SecurityException} and abstract base class for classifying security violations.
 *
 * @author John J. Blum
 * @see java.lang.SecurityException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractSecurityException extends java.lang.SecurityException {

  private Object code;

  /**
   * Constructs a new {@link AbstractSecurityException} with no {@link String message}
   * and no {@link Throwable cause}.
   */
  public AbstractSecurityException() { }

  /**
   * Constructs a new {@link AbstractSecurityException} initialized with the given {@link String message}
   * describing the security violation.
   *
   * @param message {@link String} used to describe the security violation.
   */
  public AbstractSecurityException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link AbstractSecurityException} initialized with the given {@link Throwable cause}
   * used as the reason this security violation was thrown.
   *
   * @param cause {@link Throwable} used as the reason this security violation was thrown.
   */
  public AbstractSecurityException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link AbstractSecurityException} initialized with the given {@link String message}
   * describing the security violation along with the given {@link Throwable cause} used as the reason
   * this security violation was thrown.
   *
   * @param message {@link String} used to describe the security violation.
   * @param cause {@link Throwable} used as the reason this security violation was thrown.
   */
  public AbstractSecurityException(String message, Throwable cause) {
    super(message, cause);
  }

  /**
   * Gets the security code used to represent the security violation in a non-obvious, secure way.
   *
   * @return an {@link Optional} security code representing the security violation in a secure way.
   * @see java.util.Optional
   */
  public Optional<Object> getCode() {
    return Optional.ofNullable(this.code);
  }

  /**
   * Set the security code used to represent the security violation in a non-obvious, secure way.
   *
   * @param code security code used to represent the security violation in a secure way.
   */
  public void setCode(@Nullable Object code) {
    this.code = code;
  }

  /**
   * Builder method used to set the security code used to represent the security violation in a non-obvious, secure way.
   *
   * @param securityCode used to represent the security violation in a secure way.
   * @return this {@link AbstractSecurityException} instance.
   */
  public @NotNull AbstractSecurityException with(@Nullable Object securityCode) {
    setCode(securityCode);
    return this;
  }
}
