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

package org.cp.elements.context.configure;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;
import java.util.Properties;

import org.cp.elements.enums.Gender;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.cp.elements.util.convert.AbstractConversionService;
import org.cp.elements.util.convert.ConversionException;
import org.cp.elements.util.convert.ConversionService;
import org.cp.elements.util.convert.Converter;
import org.cp.elements.util.convert.ConverterAdapter;
import org.cp.elements.util.convert.provider.DefaultConversionService;
import org.jmock.Expectations;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * The AbstractConfigurationTest class is a test suite of test cases testing the contract and functionality of the 
 * AbstractConfiguration class.
 *
 * @author John J. Blum
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.context.configure.AbstractConfiguration
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.cp.elements.util.convert.ConversionService
 * @see org.junit.Test
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AbstractConfigurationTest extends AbstractMockingTestSuite {

  private Properties configurationSettings = new Properties();

  @Before
  public void setup() {
    configurationSettings.setProperty("jdbc.driverClassName", "com.mysql.jdbc.Driver");
    configurationSettings.setProperty("jdbc.url", "jdbc:mysql://localhost:3306/appDataStore");
    configurationSettings.setProperty("jdbc.username", "");
    configurationSettings.setProperty("jdbc.password", "");
  }

  @After
  public void tearDown() {
    configurationSettings.clear();
    configurationSettings = null;
  }

  @Test
  public void testConstruct() {
    AbstractConfiguration configuration = new TestConfiguration(new Properties());

    assertNotNull(configuration);
    assertNull(configuration.getParent());
  }

  @Test
  public void testConstructWithParent() {
    Configuration mockParentConfiguration = mockContext.mock(Configuration.class);
    AbstractConfiguration configuration = new TestConfiguration(mockParentConfiguration, new Properties());

    assertNotNull(configuration);
    assertSame(mockParentConfiguration, configuration.getParent());
  }

  @Test(expected = IllegalStateException.class)
  public void testGetUninitializedConversionService() {
    try {
      new TestConfiguration(new Properties()).getConversionService();
    }
    catch (IllegalStateException expected) {
      assertEquals("The ConversionService was not properly initialized!", expected.getMessage());
      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSetConversionServiceWithNull() {
    try {
      new TestConfiguration(new Properties()).setConversionService(null);
    }
    catch (IllegalArgumentException expected) {
      assertEquals("The ConversionService used to support this Configuration cannot be null!", expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testSetAndGetConversionService() {
    AbstractConfiguration configuration = new TestConfiguration(new Properties());
    ConversionService mockConversionService = mockContext.mock(ConversionService.class);

    configuration.setConversionService(mockConversionService);

    assertSame(mockConversionService, configuration.getConversionService());
  }

  @Test
  public void testConvert() {
    final ConversionService mockConversionService = mockContext.mock(ConversionService.class);

    mockContext.checking(new Expectations() {{
      atLeast(1).of(mockConversionService).canConvert(with(equal(String.class)), with(equal(Gender.class)));
      will(returnValue(true));
      oneOf(mockConversionService).convert(with(equal("FEMALE")), with(equal(Gender.class)));
      will(returnValue(Gender.FEMALE));
    }});

    AbstractConfiguration configuration = new TestConfiguration(new Properties());

    configuration.setConversionService(mockConversionService);

    assertSame(mockConversionService, configuration.getConversionService());
    assertEquals(Gender.FEMALE, configuration.convert("FEMALE", Gender.class));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testConvertWithNullType() {
    try {
      new TestConfiguration(new Properties()).convert("test", null);
    }
    catch (IllegalArgumentException expected) {
      assertEquals("The Class type to convert the String value to cannot be null!", expected.getMessage());
      throw expected;
    }
  }

  @Test(expected = ConversionException.class)
  public void testConvertWithUnsupportedConversion() {
    final ConversionService mockConversionService = mockContext.mock(ConversionService.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockConversionService).canConvert(with(equal(String.class)), with(equal(Process.class)));
      will(returnValue(false));
    }});

    AbstractConfiguration configuration = new TestConfiguration(new Properties());

    configuration.setConversionService(mockConversionService);

    assertSame(mockConversionService, configuration.getConversionService());

    try {
      configuration.convert("12345", Process.class);
    }
    catch (ConversionException expected) {
      assertEquals(String.format("Cannot convert String value (%1$s) into a value of type (%2$s)!", "12345",
        Process.class.getName()), expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testDefaultIfUnset() {
    AbstractConfiguration configuration = new TestConfiguration(new Properties());

    assertEquals("val", configuration.defaultIfUnset("val", "test"));
    assertEquals("nil", configuration.defaultIfUnset("nil", "test"));
    assertEquals("null", configuration.defaultIfUnset("null", "test"));
    assertEquals("test", configuration.defaultIfUnset(null, "test"));
    assertEquals("test", configuration.defaultIfUnset("", "test"));
    assertEquals("test", configuration.defaultIfUnset("  ", "test"));
  }

  @Test
  public void testIsPresent() {
    AbstractConfiguration configuration = new TestConfiguration(configurationSettings);

    assertTrue(configuration.isPresent("jdbc.driverClassName"));
    assertTrue(configuration.isPresent("jdbc.url"));
    assertTrue(configuration.isPresent("jdbc.username"));
    assertTrue(configuration.isPresent("jdbc.password"));
    assertFalse(configuration.isPresent("jdbc.driver.class.name"));
    assertFalse(configuration.isPresent("jdbc.URL"));
    assertFalse(configuration.isPresent("jdbc.user.name"));
    assertFalse(configuration.isPresent("jdbc.pwd"));
  }

  @Test
  public void testIsSet() {
    AbstractConfiguration configuration = new TestConfiguration(configurationSettings);

    assertTrue(configuration.isPresent("jdbc.driverClassName"));
    assertTrue(configuration.isSet("jdbc.driverClassName"));
    assertTrue(configuration.isPresent("jdbc.url"));
    assertTrue(configuration.isSet("jdbc.url"));
    assertTrue(configuration.isPresent("jdbc.username"));
    assertFalse(configuration.isSet("jdbc.username"));
    assertTrue(configuration.isPresent("jdbc.password"));
    assertFalse(configuration.isSet("jdbc.password"));
    assertFalse(configuration.isPresent("jdbc.driver.class.name"));
    assertFalse(configuration.isSet("jdbc.driver.class.name"));
    assertFalse(configuration.isPresent("jdbc.URL"));
    assertFalse(configuration.isSet("jdbc.URL"));
    assertFalse(configuration.isPresent("jdbc.user.name"));
    assertFalse(configuration.isSet("jdbc.user.name"));
    assertFalse(configuration.isPresent("jdbc.pwd"));
    assertFalse(configuration.isSet("jdbc.pwd"));
  }

  @Test
  public void testGetPropertyValue() {
    AbstractConfiguration configuration = new TestConfiguration(configurationSettings);

    assertEquals("com.mysql.jdbc.Driver", configuration.getPropertyValue("jdbc.driverClassName"));
    assertEquals("jdbc:mysql://localhost:3306/appDataStore", configuration.getPropertyValue("jdbc.url"));
    assertNull(configuration.getPropertyValue("jdbc.username", false));
    assertNull(configuration.getPropertyValue("jdbc.password", false));
    assertEquals("admin", configuration.getPropertyValue("jdbc.username", "admin"));
    assertEquals("s3CuR3!", configuration.getPropertyValue("jdbc.username", "s3CuR3!"));
  }

  @Test
  public void testGetParentPropertyValue() {
    Properties parentConfigurationSettings = new Properties();

    parentConfigurationSettings.setProperty("jdbc.username", "root");
    parentConfigurationSettings.setProperty("jdbc.password", "p@5s5sW0rDd!");

    AbstractConfiguration parentConfiguration = new TestConfiguration(parentConfigurationSettings);
    AbstractConfiguration configuration = new TestConfiguration(parentConfiguration, configurationSettings);

    assertNull(parentConfiguration.getParent());
    assertSame(parentConfiguration, configuration.getParent());
    assertEquals("root", configuration.getPropertyValue("jdbc.username"));
    assertEquals("p@5s5sW0rDd!", configuration.getPropertyValue("jdbc.password"));
  }

  @Test(expected = ConfigurationException.class)
  public void testGetRequiredUnsetPropertyValue() {
    try {
      new TestConfiguration(configurationSettings).getPropertyValue("jdbc.username");
    }
    catch (ConfigurationException expected) {
      assertEquals(String.format("The property (%1$s) is required!", "jdbc.username"), expected.getMessage());
      throw expected;
    }
  }

  @Test
  // NOTE integration test!
  public void testGetPropertyValueAs() {
    Properties customConfigurationSettings = new Properties();

    customConfigurationSettings.setProperty("boolProp", "true");
    customConfigurationSettings.setProperty("charProp", "X");
    customConfigurationSettings.setProperty("dblProp", String.valueOf(Math.PI));
    customConfigurationSettings.setProperty("intProp", "123");
    customConfigurationSettings.setProperty("strProp", "test");

    AbstractConfiguration configuration = new TestConfiguration(customConfigurationSettings);

    configuration.setConversionService(new DefaultConversionService());

    assertTrue(configuration.getConversionService() instanceof DefaultConversionService);
    assertTrue(configuration.getPropertyValueAs("boolProp", Boolean.class));
    assertEquals(new Character('X'), configuration.getPropertyValueAs("charProp", Character.class));
    assertEquals(new Double(Math.PI), configuration.getPropertyValueAs("dblProp", Double.class));
    assertEquals(new Integer(123), configuration.getPropertyValueAs("intProp", Integer.class));
    assertEquals("test", configuration.getPropertyValueAs("strProp", String.class));
  }

  @Test
  // NOTE integration test!
  public void testGetConvertedPropertyValueAs() {
    Properties customConfigurationProperties = new Properties();

    customConfigurationProperties.setProperty("os.admin.user", "root");

    AbstractConfiguration configuration = new TestConfiguration(customConfigurationProperties);

    configuration.setConversionService(new TestConversionService(new UserConverter()));

    assertTrue(configuration.getConversionService() instanceof TestConversionService);

    User root = configuration.getPropertyValueAs("os.admin.user", User.class);

    assertNotNull(root);
    assertEquals("root", root.getUsername());
  }

  @Test
  public void testGetDefaultPropertyValueAs() {
    User guestUser = () -> "GUEST";

    AbstractConfiguration configuration = new TestConfiguration(configurationSettings);

    configuration.setConversionService(new TestConversionService(new UserConverter()));

    assertTrue(configuration.getConversionService() instanceof TestConversionService);
    assertSame(guestUser, configuration.getPropertyValueAs("os.user", guestUser, User.class));
  }

  @Test
  // NOTE integration test!
  public void testGetNonRequiredPropertyValueAs() {
    AbstractConfiguration configuration = new TestConfiguration(configurationSettings);

    configuration.setConversionService(new TestConversionService(new UserConverter()));

    assertTrue(configuration.getConversionService() instanceof TestConversionService);
    assertNull(configuration.getPropertyValueAs("jdbc.username", false, User.class));
  }

  @Test(expected = ConfigurationException.class)
  // NOTE integration test!
  public void testGetRequiredPropertyValueAs() {
    try {
      AbstractConfiguration configuration = new TestConfiguration(configurationSettings);

      configurationSettings.setProperty("jdbc.username", "hacker123");
      configuration.setConversionService(new TestConversionService(new UserConverter()));

      assertTrue(configuration.getConversionService() instanceof TestConversionService);

      configuration.getPropertyValueAs("jdbc.username", true, User.class);
    }
    catch (ConfigurationException expected) {
      assertEquals(String.format("Failed to get the value of configuration setting property (%1$s) as type (%2$s)!",
        "jdbc.username", User.class.getName()), expected.getMessage());
      assertTrue(expected.getCause() instanceof ConversionException);
      assertEquals(String.format("Failed to convert username (%1$s) to a User!", "hacker123"),
        expected.getCause().getMessage());

      throw expected;
    }
  }

  protected static final class TestConfiguration extends AbstractConfiguration {

    private final Properties properties;

    public TestConfiguration(final Properties properties) {
      this(null, properties);
    }

    public TestConfiguration(final Configuration parent, final Properties properties) {
      super(parent);
      Assert.notNull(properties, "The configuration setting properties cannot be null!");
      this.properties = properties;
    }

    @Override
    protected String doGetPropertyValue(final String propertyName) {
      return properties.getProperty(propertyName);
    }

    @Override
    public Iterator<String> iterator() {
      return properties.stringPropertyNames().iterator();
    }
  }

  protected final class TestConversionService extends AbstractConversionService {

    public TestConversionService() {
    }

    public TestConversionService(final Converter<?, ?> defaultConverter) {
      register(defaultConverter);
    }
  }

  protected interface User {
    String getUsername();
  }

  protected final class UserConverter extends ConverterAdapter<String, User> {

    @Override
    public boolean canConvert(final Class<?> fromType, final Class<?> toType) {
      return User.class.equals(toType);
    }

    @Override
    public User convert(final String value) {
      Assert.isTrue(StringUtils.isLetters(value), new ConversionException(String.format(
        "Failed to convert username (%1$s) to a User!", value)));

      final User mockUser = mockContext.mock(User.class);

      mockContext.checking(new Expectations() {{
        allowing(mockUser).getUsername();
        will(returnValue(value));
      }});

      return mockUser;
    }
  }
}
