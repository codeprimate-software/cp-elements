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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cp.elements.util.ArrayUtils;
import org.mockito.ArgumentMatcher;
import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.Mock.Strictness;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Unit Tests for {@link LoggingWriter}.
 *
 * @author John Blum
 * @see org.cp.elements.io.LoggingWriter
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
@ExtendWith(MockitoExtension.class)
public class LoggingWriterUnitTests {

  @Mock(strictness = Strictness.LENIENT)
  private Logger mockLogger;

  @Test
  void constructNewLoggingWriter() {

    LoggingWriter writer = new LoggingWriter(this.mockLogger, Level.WARNING);

    assertThat(writer).isNotNull();
    assertThat(writer.getLogger()).isEqualTo(this.mockLogger);
    assertThat(writer.getLevel()).isEqualTo(Level.WARNING);
    assertThat(writer.isClosed()).isFalse();

    verifyNoInteractions(this.mockLogger);
  }

  @Test
  void constructNewLoggingWriterWithNullLevel() {

    LoggingWriter writer = new LoggingWriter(this.mockLogger, null);

    assertThat(writer).isNotNull();
    assertThat(writer.getLogger()).isEqualTo(this.mockLogger);
    assertThat(writer.getLevel()).isEqualTo(LoggingWriter.DEFAULT_LEVEL);
    assertThat(writer.isClosed()).isFalse();

    verifyNoInteractions(this.mockLogger);
  }

  @Test
  @SuppressWarnings("all")
  void constructNewLoggingWriterWithNullLogger() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new LoggingWriter(null, Level.SEVERE))
      .withMessage("Logger is required")
      .withNoCause();
  }

  @Test
  void fromLogger() {

    LoggingWriter writer = LoggingWriter.from(this.mockLogger);

    assertThat(writer).isNotNull();
    assertThat(writer.getLogger()).isEqualTo(this.mockLogger);
    assertThat(writer.getLevel()).isEqualTo(LoggingWriter.DEFAULT_LEVEL);
    assertThat(writer.isClosed()).isFalse();

    verifyNoInteractions(this.mockLogger);
  }

  @Test
  void fromLoggerAndLevel() {

    LoggingWriter writer = LoggingWriter.from(this.mockLogger, Level.OFF);

    assertThat(writer).isNotNull();
    assertThat(writer.getLogger()).isEqualTo(this.mockLogger);
    assertThat(writer.getLevel()).isEqualTo(Level.OFF);
    assertThat(writer.isClosed()).isFalse();

    verifyNoInteractions(this.mockLogger);
  }

  @Test
  void isNotClosedCallsClosed() {

    LoggingWriter writer = spy(LoggingWriter.from(this.mockLogger));

    assertThat(writer).isNotNull();
    assertThat(writer.getLogger()).isEqualTo(this.mockLogger);
    assertThat(writer.isNotClosed()).isTrue();

    verify(writer, times(1)).getLogger();
    verify(writer, times(1)).isNotClosed();
    verify(writer, times(1)).isClosed();
    verifyNoInteractions(this.mockLogger);
    verifyNoMoreInteractions(writer);
  }

  @Test
  void getHandlers() {

    Handler mockHandler = mock(Handler.class);

    doReturn(ArrayUtils.asArray(mockHandler)).when(this.mockLogger).getHandlers();

    LoggingWriter writer = LoggingWriter.from(this.mockLogger);

    assertThat(writer).isNotNull();
    assertThat(writer.getLogger()).isEqualTo(this.mockLogger);

    Handler[] handlers = writer.getHandlers();

    assertThat(handlers).isNotNull();
    assertThat(handlers).containsExactly(mockHandler);

    verify(this.mockLogger, times(1)).getHandlers();
    verifyNoMoreInteractions(this.mockLogger);
    verifyNoInteractions(mockHandler);
  }

  @Test
  void getHandlersWhenNull() {

    doReturn(null).when(this.mockLogger).getHandlers();

    LoggingWriter writer = LoggingWriter.from(this.mockLogger);

    assertThat(writer).isNotNull();
    assertThat(writer.getLogger()).isEqualTo(this.mockLogger);

    Handler[] handlers = writer.getHandlers();

    assertThat(handlers).isNotNull();
    assertThat(handlers).isEmpty();

    verify(this.mockLogger, times(1)).getHandlers();
    verifyNoMoreInteractions(this.mockLogger);
  }

  @Test
  void logsMessage() throws IOException {

    doReturn(true).when(this.mockLogger).isLoggable(eq(Level.INFO));

    LoggingWriter writer = LoggingWriter.from(this.mockLogger);

    assertThat(writer).isNotNull();
    assertThat(writer.getLogger()).isEqualTo(this.mockLogger);
    assertThat(writer.getLevel()).isEqualTo(Level.INFO);

    String message = "test";

    writer.write(message.toCharArray(), 0, message.length());

    verify(this.mockLogger, times(1)).isLoggable(eq(Level.INFO));
    verify(this.mockLogger, times(1)).log(eq(Level.INFO), eq(message));
    verifyNoMoreInteractions(this.mockLogger);
  }

  @Test
  void logMessageAtLevel() throws IOException {

    doReturn(false).when(this.mockLogger).isLoggable(any());

    LoggingWriter writer = LoggingWriter.from(this.mockLogger, Level.WARNING);

    assertThat(writer).isNotNull();
    assertThat(writer.getLogger()).isEqualTo(this.mockLogger);
    assertThat(writer.getLevel()).isEqualTo(Level.WARNING);

    String message = "test";

    writer.write(message.toCharArray(), 0, message.length());

    verify(this.mockLogger, times(1)).isLoggable(eq(Level.WARNING));
    verify(this.mockLogger, never()).log(any(Level.class), any(String.class));
    verifyNoMoreInteractions(this.mockLogger);
  }

  @Test
  void logMessageWhenClosed() throws IOException {

    LoggingWriter mockWriter = mock(LoggingWriter.class);

    doReturn(false).when(mockWriter).isNotClosed();
    doCallRealMethod().when(mockWriter).write(any(char[].class), anyInt(), anyInt());

    assertThatIllegalStateException()
      .isThrownBy(() -> mockWriter.write("test".toCharArray(), 0, 10))
      .withMessage("Writer is closed")
      .withNoCause();

    verify(mockWriter, times(1)).write(eq("test".toCharArray()), eq(0), eq(10));
    verify(mockWriter, times(1)).isNotClosed();
    verifyNoMoreInteractions(mockWriter);
  }

  @Test
  void logsMessageWithOffsetAndLength() throws IOException {

    doReturn(true).when(this.mockLogger).isLoggable(eq(Level.INFO));

    LoggingWriter writer = LoggingWriter.from(this.mockLogger);

    assertThat(writer).isNotNull();
    assertThat(writer.getLogger()).isEqualTo(this.mockLogger);
    assertThat(writer.getLevel()).isEqualTo(Level.INFO);

    String message = "whenMocking";

    writer.write(message.toCharArray(), 4, 4);

    verify(this.mockLogger, times(1)).isLoggable(eq(Level.INFO));
    verify(this.mockLogger, times(1)).log(eq(Level.INFO), eq("Mock"));
    verifyNoMoreInteractions(this.mockLogger);
  }

  @Test
  void flush() throws IOException {

    Handler mockHandlerOne = mock(Handler.class);
    Handler mockHandlerTwo = mock(Handler.class);

    doReturn(ArrayUtils.asArray(mockHandlerOne, mockHandlerTwo)).when(this.mockLogger).getHandlers();

    LoggingWriter writer = LoggingWriter.from(this.mockLogger);

    assertThat(writer).isNotNull();
    assertThat(writer.getLogger()).isEqualTo(this.mockLogger);

    writer.flush();

    verify(this.mockLogger, times(1)).getHandlers();
    verify(mockHandlerOne, times(1)).flush();
    verify(mockHandlerTwo, times(1)).flush();
    verifyNoMoreInteractions(this.mockLogger, mockHandlerOne, mockHandlerTwo);
  }

  @Test
  void flushWhenClosed() throws IOException {

    LoggingWriter mockWriter = mock(LoggingWriter.class);

    doReturn(true).when(mockWriter).isClosed();
    doCallRealMethod().when(mockWriter).isNotClosed();
    doCallRealMethod().when(mockWriter).flush();

    assertThatIllegalStateException()
      .isThrownBy(mockWriter::flush)
      .withMessage("Writer is closed")
      .withNoCause();

    verify(mockWriter, times(1)).flush();
    verify(mockWriter, times(1)).isNotClosed();
    verify(mockWriter, times(1)).isClosed();
    verifyNoMoreInteractions(mockWriter);
  }

  @Test
  void close() throws IOException {

    Handler mockHandler = mock(Handler.class);

    doReturn(ArrayUtils.asArray(mockHandler)).when(this.mockLogger).getHandlers();

    LoggingWriter writer = LoggingWriter.from(this.mockLogger);

    assertThat(writer).isNotNull();
    assertThat(writer.getLogger()).isEqualTo(this.mockLogger);
    assertThat(writer.isNotClosed()).isTrue();
    assertThat(writer.isClosed()).isFalse();

    writer.close();

    assertThat(writer.isClosed()).isTrue();
    assertThat(writer.isNotClosed()).isFalse();

    verify(this.mockLogger, times(1)).getHandlers();
    verify(mockHandler, times(1)).close();
    verifyNoMoreInteractions(this.mockLogger, mockHandler);
  }

  @Test
  void closeWhenClosed() throws IOException {

    Handler mockHandler = mock(Handler.class);

    doReturn(ArrayUtils.asArray(mockHandler)).when(this.mockLogger).getHandlers();

    LoggingWriter writer = spy(LoggingWriter.from(this.mockLogger));

    doReturn(true).when(writer).isClosed();

    assertThat(writer).isNotNull();
    assertThat(writer.getLogger()).isEqualTo(this.mockLogger);
    assertThat(writer.isNotClosed()).isFalse();
    assertThat(writer.isClosed()).isTrue();

    writer.close();

    assertThat(writer.isNotClosed()).isFalse();
    assertThat(writer.isClosed()).isTrue();

    verifyNoInteractions(this.mockLogger, mockHandler);
  }

  @Test
  void asPrintWriter() throws IOException {

    LoggingWriter writer = spy(LoggingWriter.from(this.mockLogger));

    assertThat(writer).isNotNull();
    assertThat(writer.getLogger()).isEqualTo(this.mockLogger);

    doReturn(true).when(this.mockLogger).isLoggable(eq(Level.INFO));

    PrintWriter printWriter = writer.asPrintWriter();

    assertThat(printWriter).isNotNull();

    String message = "test";

    printWriter.print(message);

    verify(writer, times(1)).asPrintWriter();
    verify(writer, atLeast(2)).getLogger();
    verify(writer, times(1)).write(ArgumentMatchers.argThat(CharacterArrayArgumentMatcher.from(message)),
      eq(0), eq(message.length()));
    verify(this.mockLogger, times(1)).isLoggable(eq(Level.INFO));
    verify(this.mockLogger, times(1)).log(eq(Level.INFO), eq(message));
    verifyNoMoreInteractions(this.mockLogger);
  }

  static final class CharacterArrayArgumentMatcher implements ArgumentMatcher<char[]> {

    static CharacterArrayArgumentMatcher from(String data) {
      return new CharacterArrayArgumentMatcher(data);
    }

    private final String data;

    CharacterArrayArgumentMatcher(String data) {
      this.data = data;
    }

    @Override
    public boolean matches(char[] characters) {

      if (characters.length >= data.length()) {
        for (int index = 0, size = data.length(); index < size; index++) {
          if (characters[index] != data.charAt(index)) {
            return false;
          }
        }

        return true;
      }

      return false;
    }
  }
}
