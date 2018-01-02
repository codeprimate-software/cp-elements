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

package org.cp.elements.jdbc;

import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.util.logging.Logger;

import javax.sql.DataSource;

import org.cp.elements.lang.Constants;

/**
 * {@link AbstractDataSource} is an abstract based class and implementation of the {@link DataSource} interface
 * enabling easy extension and support for {@link DataSource} operations.
 *
 * @author John J. Blum
 * @see java.sql.Connection
 * @see javax.sql.DataSource
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractDataSource implements DataSource {

  @Override
  public Connection getConnection() throws SQLException {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }

  @Override
  public Connection getConnection(String username, String password) throws SQLException {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }

  @Override
  public PrintWriter getLogWriter() throws SQLException {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }

  @Override
  public void setLogWriter(PrintWriter out) throws SQLException {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }

  @Override
  public int getLoginTimeout() throws SQLException {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }

  @Override
  public void setLoginTimeout(int seconds) throws SQLException {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }

  //@Override
  public Logger getParentLogger() throws SQLFeatureNotSupportedException {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }

  @Override
  public boolean isWrapperFor(Class<?> interfaceType) throws SQLException {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }

  @Override
  public <T> T unwrap(Class<T> interfaceType) throws SQLException {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }
}
