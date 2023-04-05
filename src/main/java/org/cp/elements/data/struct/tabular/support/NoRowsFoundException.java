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
package org.cp.elements.data.struct.tabular.support;

import org.cp.elements.data.struct.tabular.Row;
import org.cp.elements.data.struct.tabular.Table;
import org.cp.elements.data.struct.tabular.View;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Java {@link RuntimeException} used to indicate that a {@link Table} or {@link View} contains no {@link Row Rows}.
 *
 * @author John Blum
 * @see java.lang.RuntimeException
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.Table
 * @see org.cp.elements.data.struct.tabular.View
 * @since 1.0.2
 */
@SuppressWarnings("unused")
public class NoRowsFoundException extends RuntimeException {

  /**
   * Constructs a new instance of {@link NoRowsFoundException} with no {@link String message}
   * and no {@link Throwable cause}.
   */
  public NoRowsFoundException() { }

  /**
   * Constructs a new instance of {@link NoRowsFoundException} with the given {@link String message}
   * used to describe the error.
   *
   * @param message {@link String} containing a {@literal description} of this {@link NoRowsFoundException}
   */
  public NoRowsFoundException(@Nullable String message) {
    super(message);
  }

  /**
   * Constructs a new instance of {@link NoRowsFoundException} with the given {@link Throwable cause}
   * used as the underlying reason this {@link NoRowsFoundException} was thrown.
   *
   * @param cause {@link Throwable} used as the reason (cause) for this {@link NoRowsFoundException}
   */
  public NoRowsFoundException(@Nullable Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new instance of {@link NoRowsFoundException} with the given {@link String message}
   * used to describe the error along with an underlying reason this {@link NoRowsFoundException} was thrown.
   *
   * @param message {@link String} containing a {@literal description} of this {@link NoRowsFoundException}
   * @param cause {@link Throwable} used as the reason (cause) for this {@link NoRowsFoundException}
   */
  public NoRowsFoundException(@Nullable String message, @Nullable Throwable cause) {
    super(message, cause);
  }
}
