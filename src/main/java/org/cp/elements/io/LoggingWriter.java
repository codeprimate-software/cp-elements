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

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.Arrays;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;

/**
 * {@link Writer} implementation based on Java {@link Logger}.
 *
 * @author John Blum
 * @see java.io.Writer
 * @see java.util.logging.Logger
 * @since 2.0.0
 */
public class LoggingWriter extends Writer {

  protected static final Level DEFAULT_LEVEL = Level.INFO;

  /**
   * Factory method used to constructs a new {@link Writer} using the given {@link Logger} for write (IO) operations.
   *
   * @param logger {@link Logger} used by the {@link Writer} for write operations.
   * @return a new {@link Writer}.
   * @throws IllegalArgumentException if {@link Logger} is {@literal null}.
   * @see java.util.logging.Logger
   */
  public static LoggingWriter from(@NotNull Logger logger) {
    return new LoggingWriter(logger, DEFAULT_LEVEL);
  }

  /**
   * Factory method used to constructs a new {@link Writer} using the given {@link Logger} for write (IO) operations.
   *
   * @param logger {@link Logger} used by the {@link Writer} for write operations.
   * @param level {@link Level} in which to log the message.
   * @return a new {@link Writer}.
   * @throws IllegalArgumentException if {@link Logger} is {@literal null}.
   * @see java.util.logging.Logger
   * @see java.util.logging.Level
   */
  public static LoggingWriter from(@NotNull Logger logger, @Nullable Level level) {
    return new LoggingWriter(logger, level);
  }

  private volatile boolean closed;

  private final Logger logger;

  private final Level level;

  /**
   * Constructs a new {@link Writer} using the given {@link Logger} for write (IO) operations.
   *
   * @param logger {@link Logger} used by this {@link Writer} for write operations.
   * @param level {@link Level} in which to log the message.
   * @throws IllegalArgumentException if {@link Logger} is {@literal null}.
   * @see java.util.logging.Logger
   * @see java.util.logging.Level
   */
  public LoggingWriter(@NotNull Logger logger, @Nullable Level level) {
    this.logger = ObjectUtils.requireObject(logger, "Logger is required");
    this.level = level != null ? level : DEFAULT_LEVEL;
  }

  /**
   * Returns the configured {@link Logger} used by this {@link Writer}.
   *
   * @return the configured {@link Logger} used by this {@link Writer}.
   * @see java.util.logging.Logger
   */
  protected Logger getLogger() {
    return logger;
  }

  /**
   * Returns the configured {@link Level} used by the {@link Logger} to log messages.
   *
   * @return the configured {@link Level} used by the {@link Logger} to log messages.
   * @see java.util.logging.Level
   */
  protected Level getLevel() {
    return this.level;
  }

  /**
   * Returns all configured {@link Handler Handlers} of the {@link Logger}.
   *
   * @return all configured {@link Handler Handlers} of the {@link Logger}.
   * @see java.util.logging.Handler
   * @see Logger#getHandlers()
   * @see #getLogger()
   */
  @NullSafe
  protected Handler[] getHandlers() {
    return ArrayUtils.nullSafeArray(getLogger().getHandlers(), Handler.class);
  }

  /**
   * Determines whether this {@link Writer} has been closed.
   *
   * @return whether this {@link Writer} has been closed.
   * @see #isNotClosed()
   * @see #close()
   */
  @SuppressWarnings("all")
  public boolean isClosed() {
    return this.closed;
  }

  /**
   * Determines whether this {@link Writer} has been closed.
   *
   * @return whether this {@link Writer} has been closed.
   * @see #isClosed()
   */
  public boolean isNotClosed() {
    return !isClosed();
  }

  /**
   * Returns a {@link Stream} of configured {@link Handler Handlers} on the {@link Logger}.
   *
   * @return a {@link Stream} of configured {@link Handler Handlers} on the {@link Logger}.
   * @see java.util.logging.Handler
   * @see java.util.stream.Stream
   * @see #getHandlers()
   */
  @NullSafe
  private Stream<Handler> streamHandlers() {
    return Arrays.stream(getHandlers());
  }

  @Override
  @SuppressWarnings("all")
  public void write(@NotNull char[] characterBuffer, int offset, int length) throws IOException {
    Assert.state(isNotClosed(), "Writer is closed");
    log(toMessage(characterBuffer, offset, length));
  }

  private String toMessage(char[] characters, int offset, int length) {
    char[] message = new char[length];
    System.arraycopy(characters, offset, message, 0, length);
    return String.valueOf(message);
  }

  private void log(String message) {

    Logger logger = getLogger();
    Level level = getLevel();

    if (logger.isLoggable(level)) {
      logger.log(level, message);
    }
  }

  @Override
  public void flush() throws IOException {
    Assert.state(isNotClosed(), "Writer is closed");
    streamHandlers().forEach(Handler::flush);
  }

  @Override
  public void close() throws IOException {
    if (isNotClosed()) {
      streamHandlers().forEach(Handler::close);
      this.closed = true;
    }
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
