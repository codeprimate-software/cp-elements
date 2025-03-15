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
package org.cp.elements.jdbc;

import static org.cp.elements.lang.RuntimeExceptionsFactory.newUnsupportedOperationException;

import java.io.PrintWriter;
import java.sql.Connection;
import java.util.function.Supplier;
import java.util.logging.Logger;

import javax.sql.DataSource;

import org.cp.elements.function.ThrowableSupplier;
import org.cp.elements.io.LoggingWriter;
import org.cp.elements.io.NoOpWriter;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.ThrowableOperation;

/**
 * Abstract base class and implementation of the {@link DataSource} interface enabling easy extension and support
 * for {@link DataSource} operations.
 *
 * @author John J. Blum
 * @see java.sql.Connection
 * @see javax.sql.DataSource
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractDataSource implements DataSource {

  protected static final int DEFAULT_LOGIN_TIMEOUT_SECONDS = 30;

  protected static final PrintWriter DEFAULT_LOG_WRITER = defaultLogWriter();

  private static PrintWriter defaultLogWriter() {

    ThrowableOperation<PrintWriter> operation = ThrowableOperation.fromSupplier(loggingPrintWriterSupplier());

    return ObjectUtils.doSafely(operation, noOpPrintWriterSupplier());
  }

  @SuppressWarnings("all")
  private static ThrowableSupplier<PrintWriter> loggingPrintWriterSupplier() {

    Logger logger = Logger.getLogger(AbstractDataSource.class.getName());

    ThrowableSupplier<PrintWriter> loggingPrintWriter = () -> {
      try (LoggingWriter writer = LoggingWriter.from(logger)) {
        return writer.asPrintWriter();
      }
    };

    return loggingPrintWriter;
  }

  private static Supplier<PrintWriter> noOpPrintWriterSupplier() {
    try (NoOpWriter writer = NoOpWriter.create()) {
      return writer::asPrintWriter;
    }
  }

  private int loginTimeout = DEFAULT_LOGIN_TIMEOUT_SECONDS;

  private PrintWriter logWriter = DEFAULT_LOG_WRITER;

  @Override
  public Connection getConnection() {
    throw newUnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }

  @Override
  public Connection getConnection(String username, String password) {
    throw newUnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }

  @Override
  public PrintWriter getLogWriter() {
    return this.logWriter;
  }

  @Override
  public void setLogWriter(PrintWriter logWriter) {
    this.logWriter = logWriter != null ? logWriter : DEFAULT_LOG_WRITER;
  }

  @Override
  public int getLoginTimeout() {
    return this.loginTimeout;
  }

  @Override
  public void setLoginTimeout(int seconds) {
    this.loginTimeout = seconds;
  }

  @Override
  public Logger getParentLogger() {
    return Logger.getLogger(getClass().getName());
  }

  @Override
  public boolean isWrapperFor(Class<?> interfaceType) {
    throw newUnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }

  @Override
  public <T> T unwrap(Class<T> interfaceType) {
    throw newUnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }
}
