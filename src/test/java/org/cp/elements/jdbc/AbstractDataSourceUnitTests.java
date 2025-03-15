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
package org.cp.elements.jdbc;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.mockito.Mockito.mock;

import java.io.PrintWriter;
import java.util.logging.Logger;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.Constants;
import org.cp.elements.security.model.User;

/**
 * Unit Tests for {@link AbstractDataSource}.
 *
 * @author John Blum
 * @see org.cp.elements.jdbc.AbstractDataSource
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
class AbstractDataSourceUnitTests {

  private final AbstractDataSource dataSource = new TestDataSource();

  @Test
  void getConnectionIsUnsupported() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(this.dataSource::getConnection)
      .withMessage(Constants.OPERATION_NOT_SUPPORTED)
      .withNoCause();
  }

  @Test
  void getConnectionWithUsernamePasswordNotSupported() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.dataSource.getConnection("testUser", "secret"))
      .withMessage(Constants.OPERATION_NOT_SUPPORTED)
      .withNoCause();
  }

  @Test
  void setAndGetLogWriter() {

    PrintWriter mockLogWriter = mock(PrintWriter.class);

    assertThat(this.dataSource.getLogWriter()).isEqualTo(AbstractDataSource.DEFAULT_LOG_WRITER);

    this.dataSource.setLogWriter(mockLogWriter);

    assertThat(this.dataSource.getLogWriter()).isEqualTo(mockLogWriter);

    this.dataSource.setLogWriter(null);

    assertThat(this.dataSource.getLogWriter()).isEqualTo(AbstractDataSource.DEFAULT_LOG_WRITER);
  }

  @Test
  void setAndGetLoginTimeout() {

    assertThat(this.dataSource.getLoginTimeout()).isEqualTo(AbstractDataSource.DEFAULT_LOGIN_TIMEOUT_SECONDS);

    this.dataSource.setLoginTimeout(600);

    assertThat(this.dataSource.getLoginTimeout()).isEqualTo(600);

    this.dataSource.setLoginTimeout(Integer.MAX_VALUE);

    assertThat(this.dataSource.getLoginTimeout()).isEqualTo(Integer.MAX_VALUE);

    this.dataSource.setLoginTimeout(Integer.MIN_VALUE);

    assertThat(this.dataSource.getLoginTimeout()).isEqualTo(Integer.MIN_VALUE);
  }

  @Test
  void getParentLogger() {

    Logger logger = this.dataSource.getParentLogger();

    assertThat(logger).isNotNull();
    assertThat(logger.getName()).isEqualTo(this.dataSource.getClass().getName());
  }

  @Test
  void isWrappedForIsUnsupported() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.dataSource.isWrapperFor(User.class))
      .withMessage(Constants.OPERATION_NOT_SUPPORTED)
      .withNoCause();
  }

  @Test
  void unwrapIsUnsupported() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.dataSource.unwrap(User.class))
      .withMessage(Constants.OPERATION_NOT_SUPPORTED)
      .withNoCause();
  }

  static final class TestDataSource extends AbstractDataSource {

  }
}
