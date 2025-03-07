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
package org.cp.elements.context.configure.support;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;
import java.io.FileWriter;
import java.util.Properties;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import org.cp.elements.context.configure.AbstractConfiguration;
import org.cp.elements.context.configure.Configuration;
import org.cp.elements.io.FileSystemUtils;

/**
 * Integration Tests for {@link PropertiesConfiguration}.
 *
 * @author John Blum
 * @see java.io.File
 * @see java.util.Properties
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.context.configure.AbstractConfiguration
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.context.configure.support.PropertiesConfiguration
 * @since 1.0.0
 */
class PropertiesConfigurationIntegrationTests {

  @BeforeAll
  public static void beforeTestSuite() {
    FileSystemUtils.deleteRecursive(new File(FileSystemUtils.WORKING_DIRECTORY, "test-jdbc.properties"));
  }

  @Test
  @SuppressWarnings("unchecked")
  void propertiesFileBaseConfigurationIsCorrect() throws Exception {

    Properties testJdbcProperties = new Properties();

    testJdbcProperties.setProperty("jdbc.driverClassName", "com.mysql.jdbc.Driver");
    testJdbcProperties.setProperty("jdbc.url", "jdbc:mysql://localhost:3306/appDataStore");
    testJdbcProperties.setProperty("jdbc.username", "dba");
    testJdbcProperties.setProperty("jdbc.password", "p@55W0rd");

    File testJdbcPropertiesFile = new File(FileSystemUtils.WORKING_DIRECTORY, "test-jdbc.properties");

    assertThat(testJdbcPropertiesFile.createNewFile()).isTrue();
    assertThat(testJdbcPropertiesFile.isFile()).isTrue();

    testJdbcPropertiesFile.deleteOnExit();

    try (FileWriter fileWriter = new FileWriter(testJdbcPropertiesFile)) {
      testJdbcProperties.store(fileWriter, "Test JDBC Properties File");
    }

    PropertiesConfiguration configuration = new PropertiesConfiguration(testJdbcPropertiesFile);

    assertThat(configuration).isNotNull();
    assertThat(configuration.isPresent("jdbc.driverClassName")).isTrue();
    assertThat(configuration.isPresent("jdbc.url")).isTrue();
    assertThat(configuration.isPresent("jdbc.username")).isTrue();
    assertThat(configuration.isPresent("jdbc.password")).isTrue();
    assertThat(configuration.getPropertyValue("jdbc.driverClassName")).isEqualTo("com.mysql.jdbc.Driver");
    assertThat(configuration.getPropertyValue("jdbc.url")).isEqualTo("jdbc:mysql://localhost:3306/appDataStore");
    assertThat(configuration.getPropertyValue("jdbc.username")).isEqualTo("dba");
    assertThat(configuration.getPropertyValue("jdbc.password")).isEqualTo("p@55W0rd");
    assertThat(configuration.getPropertyValue("jdbc.auto-commit", false)).isNull();

    Configuration.Descriptor<File> configurationDescriptor =
      (Configuration.Descriptor<File>) configuration.getDescriptor();

    assertThat(configurationDescriptor).isInstanceOf(AbstractConfiguration.FileConfigurationDescriptor.class);
    assertThat(configurationDescriptor.getSource()).isEqualTo(testJdbcPropertiesFile);
    assertThat(configurationDescriptor.isFile()).isTrue();
    assertThat(configurationDescriptor.isProperties()).isFalse();
  }
}
