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

package org.cp.elements.context.configure.support;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

/**
 * The PropertiesConfigurationTest class is a test suite of test cases testing the contract and functionality of the
 * PropertiesConfiguration class.
 *
 * @author John J. Blum
 * @see org.cp.elements.context.configure.support.PropertiesConfiguration
 * @see org.junit.Test
 * @since 1.0.0
 */
public class PropertiesConfigurationTest {

  private Properties configurationSettings;

  private PropertiesConfiguration configuration;

  @Before
  public void setup() {
    configurationSettings = new Properties();
    configurationSettings.setProperty("bool.prop", "true");
    configurationSettings.setProperty("chr.prop", "X");
    configurationSettings.setProperty("dbl.prop", String.valueOf(Math.PI));
    configurationSettings.setProperty("int.prop", "2");
    configurationSettings.setProperty("str.prop", "test");
    configuration = new PropertiesConfiguration(configurationSettings);
    assertSame(configurationSettings, configuration.getProperties());
  }

  @Test(expected = IllegalArgumentException.class)
  public void testConstructConfigurationWithNullProperties() {
    try {
      new PropertiesConfiguration((Properties) null);
    }
    catch (IllegalArgumentException expected) {
      assertEquals("The Properties object used to back this Configuration cannot be null!", expected.getMessage());
      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void testConstructConfigurationWithNullPropertiesFile() throws IOException {
    try {
      new PropertiesConfiguration((File) null);
    }
    catch (IllegalArgumentException expected) {
      assertEquals("The file to load properties from cannot be null!", expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testIsPresent() {
    assertTrue(configuration.isPresent("bool.prop"));
    assertTrue(configuration.isPresent("chr.prop"));
    assertTrue(configuration.isPresent("dbl.prop"));
    assertTrue(configuration.isPresent("int.prop"));
    assertTrue(configuration.isPresent("str.prop"));
    assertFalse(configuration.isPresent("bigd.prop"));
    assertFalse(configuration.isPresent("bigi.prop"));
    assertFalse(configuration.isPresent("BOOL.prop"));
    assertFalse(configuration.isPresent("boolean.property"));
    assertFalse(configuration.isPresent("cal.prop"));
  }

  @Test
  public void testDoGetPropertyValue() {
    assertEquals("true", configuration.doGetPropertyValue("bool.prop"));
    assertEquals("X", configuration.doGetPropertyValue("chr.prop"));
    assertEquals(String.valueOf(Math.PI), configuration.doGetPropertyValue("dbl.prop"));
    assertEquals("2", configuration.doGetPropertyValue("int.prop"));
    assertEquals("test", configuration.doGetPropertyValue("str.prop"));
    assertNull(configuration.doGetPropertyValue("unset.property"));
    assertNull(configuration.doGetPropertyValue("BOOL.prop"));
    assertNull(configuration.doGetPropertyValue("boolean.property"));
  }

  @Test
  public void testGetParentPropertyValue() {
    Properties parentConfigurationSetting = new Properties();

    parentConfigurationSetting.setProperty("BOOL.PROP", "false");
    parentConfigurationSetting.setProperty("chr.prop", "A");
    parentConfigurationSetting.setProperty("char.prop", "Z");
    parentConfigurationSetting.setProperty("strng.prop", "mock");

    PropertiesConfiguration configuration = new PropertiesConfiguration(configurationSettings,
      new PropertiesConfiguration(parentConfigurationSetting));

    assertNotNull(configuration);
    assertSame(configurationSettings, configuration.getProperties());
    assertEquals("true", configuration.getPropertyValue("bool.prop"));
    assertEquals("false", configuration.getPropertyValue("BOOL.PROP"));
    assertEquals("X", configuration.getPropertyValue("chr.prop"));
    assertEquals("Z", configuration.getPropertyValue("char.prop"));
    assertEquals("test", configuration.getPropertyValue("str.prop"));
    assertEquals("mock", configuration.getPropertyValue("strng.prop"));
    assertNull(configuration.getPropertyValue("string.property", false));
  }

  @Test
  public void testIterator() {
    Set<String> expectedPropertyNames = new HashSet<String>(configurationSettings.stringPropertyNames());

    assertFalse(expectedPropertyNames.isEmpty());

    for (String actualPropertyName : configuration) {
      assertTrue(expectedPropertyNames.remove(actualPropertyName));
    }

    assertTrue(expectedPropertyNames.isEmpty());
  }

  @Test
  public void testPropertiesFileBaseConfiguration() throws Exception {
    Properties testJdbcProperties = new Properties();

    testJdbcProperties.setProperty("jdbc.driverClassName", "com.mysql.jdbc.Driver");
    testJdbcProperties.setProperty("jdbc.url", "jdbc:mysql://localhost:3306/appDataStore");
    testJdbcProperties.setProperty("jdbc.username", "dba");
    testJdbcProperties.setProperty("jdbc.password", "p@55W0rd");

    File testJdbcPropertiesFile = new File(System.getProperty("user.dir"), "test-jdbc.properties");

    assertTrue(testJdbcPropertiesFile.createNewFile());
    assertTrue(testJdbcPropertiesFile.isFile());

    testJdbcPropertiesFile.deleteOnExit();
    testJdbcProperties.store(new FileWriter(testJdbcPropertiesFile), "Test JDBC Properties File!");

    PropertiesConfiguration configuration = new PropertiesConfiguration(testJdbcPropertiesFile);

    assertNotNull(configuration);
    assertTrue(configuration.isPresent("jdbc.driverClassName"));
    assertTrue(configuration.isPresent("jdbc.url"));
    assertTrue(configuration.isPresent("jdbc.username"));
    assertTrue(configuration.isPresent("jdbc.password"));
    assertEquals("com.mysql.jdbc.Driver", configuration.getPropertyValue("jdbc.driverClassName"));
    assertEquals("jdbc:mysql://localhost:3306/appDataStore", configuration.getPropertyValue("jdbc.url"));
    assertEquals("dba", configuration.getPropertyValue("jdbc.username"));
    assertEquals("p@55W0rd", configuration.getPropertyValue("jdbc.password"));
  }
}
