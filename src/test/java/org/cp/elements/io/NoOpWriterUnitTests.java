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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.io.PrintWriter;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link NoOpWriter}.
 *
 * @author John Blum
 * @see org.cp.elements.io.NoOpWriter
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
class NoOpWriterUnitTests {

  private NoOpWriter newWriter() {
    try (NoOpWriter writer = NoOpWriter.create()) {
      return writer;
    }
  }

  @Test
  void create() {
    assertThat(NoOpWriter.create()).isNotNull();
  }

  @Test
  void writeDoesNothing() {
    try (NoOpWriter writer = newWriter()) {
      writer.write(new char[] { 'X' }, 0, 1);
    }
  }

  @Test
  void flushDoesNothing() {
    try (NoOpWriter writer = newWriter()) {
      writer.flush();
    }
  }

  @Test
  void closeDoesNothing() {
    newWriter().close();
  }

  @Test
  void asPrintWriter() {

    NoOpWriter noOpWriter;
    PrintWriter printWriter;

    try (NoOpWriter writer = NoOpWriter.create()) {
      noOpWriter = spy(writer);
      printWriter = spy(noOpWriter.asPrintWriter());
    }

    assertThat(printWriter).isNotNull();

    char[] characters = { 'X', 'Y', 'Z' };

    printWriter.write(characters, 2, 10);

    verify(noOpWriter, times(1)).asPrintWriter();
    verify(noOpWriter, times(1)).write(eq(characters), eq(2), eq(10));
  }
}
