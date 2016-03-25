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
   * Creates an uninitialized instance of the NoSuchFileException class.
   */
  public NoSuchFileException() {
  }

  /**
   * Creates an instance of the NoSuchFileException class initialized with the given message describing
   * the details of the missing {@link java.io.File}.
   *
   * @param message a String describing the cause of this exception.
   */
  public NoSuchFileException(final String message) {
    super(message);
  }

  /**
   * Creates an instance of the NoSuchFileException class initialized with the given {@link Throwable}
   * indicating the reason this exception was thrown.
   *
   * @param cause a {@link Throwable} indicating the cause of this exception.
   * @see java.lang.Throwable
   */
  public NoSuchFileException(final Throwable cause) {
    super(cause);
  }

  /**
   * Creates an instance of the NoSuchFileException class initialized with the given message describing
   * the details of the missing {@link java.io.File} along with the {@link Throwable} indicating the reason
   * this exception was thrown.
   *
   * @param message a String describing the cause of this exception.
   * @param cause a {@link Throwable} indicating the cause of this exception.
   * @see java.lang.Throwable
   */
  public NoSuchFileException(final String message, final Throwable cause) {
    super(message, cause);
  }

}
