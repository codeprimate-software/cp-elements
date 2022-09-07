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
package org.cp.elements.beans;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;

import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link PropertyVetoException} implementation supporting a {@link Throwable cause}.
 *
 * @author John Blum
 * @see java.beans.PropertyChangeEvent
 * @see java.beans.PropertyVetoException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ChainedPropertyVetoException extends PropertyVetoException {

  /**
   * Constructs a new instance of {@link ChainedPropertyVetoException} initialized with
   * the given {@link PropertyChangeEvent} that resulted in this checked {@link Exception}
   * along with a {@link String message} describing the {@link Exception}.
   *
   * @param event {@link PropertyChangeEvent} that resulted in this {@link Exception}.
   * @param message {@link String} containing a description of this {@link Exception}.
   * @see java.beans.PropertyChangeEvent
   */
  public ChainedPropertyVetoException(@Nullable PropertyChangeEvent event, @Nullable String message) {
    this(event, message, null);
  }

  /**
   * Constructs a new instance of {@link ChainedPropertyVetoException} initialized with
   * the given {@link PropertyChangeEvent} that resulted in this checked {@link Exception}
   * along with a {@link String message} describing the {@link Exception} and a {@link Throwable cause}
   * used as the underlying reason this checked {@link Exception} was thrown.
   *
   * @param event {@link PropertyChangeEvent} that resulted in this {@link Exception}.
   * @param message {@link String} containing a description of this {@link Exception}.
   * @param cause {@link Throwable} used as the underlying cause of this {@link Exception}.
   * @see java.beans.PropertyChangeEvent
   */
  public ChainedPropertyVetoException(@Nullable PropertyChangeEvent event, @Nullable String message,
      @Nullable Throwable cause) {

    super(message, event);
    initCause(cause);
  }
}
