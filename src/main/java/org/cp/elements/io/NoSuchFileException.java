/*
 * Copyright 2016 Author or Authors.
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

/**
 * The NoSuchFileException class is a {@link RuntimeException} indicating a missing {@link java.io.File}.
 *
 * @author John J. Blum
 * @see java.io.FileNotFoundException
 * @see java.lang.RuntimeException
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class NoSuchFileException extends RuntimeException {

  /**
   * Constructs an uninitialized instance of the {@link NoSuchFileException} class.
   */
  public NoSuchFileException() {
  }

  /**
   * Constructs an instance of the {@link NoSuchFileException} class initialized with the given message describing
   * the details of the missing {@link java.io.File}.
   *
   * @param message description for the cause of this exception.
   */
  public NoSuchFileException(String message) {
    super(message);
  }

  /**
   * Constructs an instance of the {@link NoSuchFileException} class initialized with the given {@link Throwable}
   * indicating the reason this exception was thrown.
   *
   * @param cause {@link Throwable} indicating the cause of this exception.
   * @see java.lang.Throwable
   */
  public NoSuchFileException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs an instance of the {@link NoSuchFileException} class initialized with the given message describing
   * the details of the missing {@link java.io.File} along with the {@link Throwable} indicating the reason
   * this exception was thrown.
   *
   * @param message description for the cause of this exception.
   * @param cause {@link Throwable} indicating the cause of this exception.
   * @see java.lang.Throwable
   */
  public NoSuchFileException(String message, Throwable cause) {
    super(message, cause);
  }
}
