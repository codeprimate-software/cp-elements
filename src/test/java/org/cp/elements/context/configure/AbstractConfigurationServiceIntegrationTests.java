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
package org.cp.elements.context.configure;

import static org.assertj.core.api.Assertions.assertThat;

import java.net.URI;
import java.util.Properties;
import java.util.function.Supplier;

import org.junit.Test;

import org.cp.elements.beans.annotation.Required;
import org.cp.elements.context.configure.annotation.ConfigurationProperties;
import org.cp.elements.context.configure.support.PropertiesConfiguration;
import org.cp.elements.util.PropertiesBuilder;

/**
 * Integration Tests for {@link AbstractConfigurationService}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.context.configure.AbstractConfigurationService
 * @since 1.0.0
 */
public class AbstractConfigurationServiceIntegrationTests {

  @Test
  public void proxyIsCorrect() {

    Properties jdbcProperties = PropertiesBuilder.newInstance()
      .set("jdbc.driver-class-name", "x.y.z.jdbc.Driver")
      .set("jdbc.url", "jdbc:mockdb://localhost:1234/testdb")
      .set("jdbc.username", "test")
      .set("jdbc.password", "s3cr3t")
      .build();

    Configuration configuration = new PropertiesConfiguration(jdbcProperties);

    AbstractConfigurationService configurationService = new TestConfigurationService();

    assertThat(configurationService.register(configuration)).isTrue();
    assertThat(configurationService).containsExactly(configuration);
    assertThat(configuration).containsExactlyInAnyOrder("jdbc.driver-class-name", "jdbc.url",
      "jdbc.username", "jdbc.password");

    JdbcConfiguration jdbcConfiguration = configurationService.proxy(JdbcConfiguration.class);

    assertThat(jdbcConfiguration).isNotNull();
    assertThat(jdbcConfiguration.getDriverClassName()).isEqualTo(jdbcProperties.getProperty("jdbc.driver-class-name"));
    assertThat(jdbcConfiguration.getUrl()).isEqualTo(URI.create("jdbc:mockdb://localhost:1234/testdb"));
    assertThat(jdbcConfiguration.getUsername()).isEqualTo(jdbcProperties.getProperty("jdbc.username"));
    assertThat(jdbcConfiguration.getPassword()).isEqualTo(jdbcProperties.getProperty("jdbc.password"));
  }

  @Test
  public void proxyForExtensionIsCorrect() {

    Properties jdbcProperties = PropertiesBuilder.newInstance()
      .set("jdbc.driver-class-name", "x.y.z.jdbc.Driver")
      .set("jdbc.url", "jdbc:mockdb://localhost:1234/testdb")
      .set("jdbc.username", "test")
      .set("jdbc.password", "s3cr3t")
      .set("jdbc.connection.auto-commit", "false")
      .set("jdbc.connection.max", "100")
      //.set("jdbc.connection.min", "10")
      .build();

    Configuration configuration = new PropertiesConfiguration(jdbcProperties);

    AbstractConfigurationService configurationService = new TestConfigurationService();

    assertThat(configurationService.register(configuration)).isTrue();
    assertThat(configurationService).containsExactly(configuration);

    ExtendedJdbcConfiguration jdbcConfiguration = configurationService.proxy(ExtendedJdbcConfiguration.class);

    assertThat(jdbcConfiguration).isNotNull();
    assertThat(jdbcConfiguration.getDriverClassName()).isEqualTo(jdbcProperties.getProperty("jdbc.driver-class-name"));
    assertThat(jdbcConfiguration.getUrl()).isEqualTo(URI.create("jdbc:mockdb://localhost:1234/testdb"));
    assertThat(jdbcConfiguration.getUsername()).isEqualTo(jdbcProperties.getProperty("jdbc.username"));
    assertThat(jdbcConfiguration.getPassword()).isEqualTo(jdbcProperties.getProperty("jdbc.password"));

    ExtendedJdbcConfiguration.ConnectionConfiguration jdbcConnectionConfiguration = jdbcConfiguration.getConnection();

    assertThat(jdbcConnectionConfiguration).isNotNull();
    assertThat(jdbcConnectionConfiguration.isAutoCommit()).isFalse();
    assertThat(jdbcConnectionConfiguration.getMax()).isEqualTo(100);
    assertThat(jdbcConnectionConfiguration.getMin()).isNull();
    assertThat(jdbcConnectionConfiguration.getMin(1)).isOne();
    assertThat(jdbcConnectionConfiguration.getMin(() -> 10)).isEqualTo(10);
  }

  @ConfigurationProperties(propertyPrefix = "jdbc")
  interface JdbcConfiguration {
    String getDriverClassName();
    URI getUrl();
    String getUsername();
    String getPassword();
  }

  interface ExtendedJdbcConfiguration extends JdbcConfiguration {

    ConnectionConfiguration getConnection();

    interface ConnectionConfiguration {

      Boolean isAutoCommit();

      @Required
      Integer getMax();

      Integer getMin();

      @Required
      Integer getMin(Integer defaultMin);

      @Required
      Integer getMin(Supplier<Integer> defaultMin);

    }
  }

  static class TestConfigurationService extends AbstractConfigurationService { }

}
