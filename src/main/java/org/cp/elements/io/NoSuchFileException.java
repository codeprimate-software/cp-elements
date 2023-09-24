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
package org.cp.elements.io;

import java.io.File;

/**
 * Java {@link RuntimeException} thrown when a {@link File} cannot be found.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileNotFoundException
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class NoSuchFileException extends RuntimeException {

  /**
   * Constructs a new {@link NoSuchFileException} having no {@link String message} and no {@link Throwable cause}.
   */
  public NoSuchFileException() { }

  /**
   * Constructs a new {@link NoSuchFileException} initialized with the given {@link String message}
   * describing the missing {@link File}.
   *
   * @param message {@link String} containing a {@literal description} of this {@link RuntimeException}.
   * @see java.lang.String
   */
  public NoSuchFileException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@link NoSuchFileException} initialized with the given {@link Throwable}
   * used as the {@literal cause} of this {@link RuntimeException}.
   *
   * @param cause {@link Throwable} used as the {@literal cause} of this {@link RuntimeException}.
   * @see java.lang.Throwable
   */
  public NoSuchFileException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new {@link NoSuchFileException} initialized with the given {@link String message}
   * describing the missing {@link File} along with the given {@link Throwable} used as the {@literal cause}
   * of this {@link RuntimeException}.
   *
   * @param message {@link String} containing a {@literal description} of this {@link RuntimeException}.
   * @param cause {@link Throwable} used as the {@literal cause} of this {@link RuntimeException}.
   * @see java.lang.Throwable
   * @see java.lang.String
   */
  public NoSuchFileException(String message, Throwable cause) {
    super(message, cause);
  }
}
