/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.context.configure.support;

import static org.junit.Assert.*;

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
 * <p/>
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

  @Test(expected = NullPointerException.class)
  public void testConstructConfigurationWithNullProperties() {
    try {
      new PropertiesConfiguration((Properties) null);
    }
    catch (NullPointerException expected) {
      assertEquals("The Properties object used to back this Configuration cannot be null!", expected.getMessage());
      throw expected;
    }
  }

  @Test(expected = NullPointerException.class)
  public void testConstructConfigurationWithNullPropertiesFile() throws IOException {
    try {
      new PropertiesConfiguration((File) null);
    }
    catch (NullPointerException expected) {
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
