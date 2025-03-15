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
package org.cp.elements.io;

import java.io.PrintWriter;
import java.io.Writer;

/**
 * {@link Writer} implementation that does not perform any IO operations.
 *
 * @author John Blum
 * @see java.io.Writer
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public class NoOpWriter extends Writer {

  /**
   * Factory method used to construct a new {@link NoOpWriter}.
   *
   * @return a new {@link NoOpWriter}.
   */
  public static NoOpWriter create() {
    return new NoOpWriter();
  }

  @Override
  @SuppressWarnings("all")
  public void write(char[] characters, int offset, int len) {

  }

  @Override
  public void flush() {

  }

  @Override
  public void close() {

  }

  /**
   * Returns a {@link PrintWriter} from this {@link Writer}.
   *
   * @return a {@link PrintWriter} from this {@link Writer}.
   * @see java.io.PrintWriter
   */
  public PrintWriter asPrintWriter() {
    return new PrintWriter(this);
  }
}
