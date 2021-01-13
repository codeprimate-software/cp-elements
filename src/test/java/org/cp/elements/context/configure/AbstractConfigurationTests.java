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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Iterator;
import java.util.Properties;
import java.util.ServiceLoader;

import org.cp.elements.data.conversion.AbstractConversionService;
import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.ConversionService;
import org.cp.elements.data.conversion.Converter;
import org.cp.elements.enums.Gender;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.test.annotation.IntegrationTest;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for {@link AbstractConfiguration}.
 *
 * @author John J. Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.mockito.Mockito
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.context.configure.AbstractConfiguration
 * @see org.cp.elements.data.conversion.ConversionService
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AbstractConfigurationTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

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

  ConversionService loadConversionService() {

    ServiceLoader<ConversionService> serviceLoader = ServiceLoader.load(ConversionService.class);

    assertThat(serviceLoader, is(notNullValue()));

    Iterator<ConversionService> iterator = serviceLoader.iterator();

    assertThat(iterator.hasNext(), is(true));

    return iterator.next();
  }

  @Test
  public void construct() {

    AbstractConfiguration configuration = new TestConfiguration(new Properties());

    assertNotNull(configuration);
    assertNull(configuration.getParent());
  }

  @Test
  public void constructWithParent() {

    Configuration mockParentConfiguration = mock(Configuration.class);

    AbstractConfiguration configuration = new TestConfiguration(mockParentConfiguration, new Properties());

    assertThat(configuration, is(notNullValue(AbstractConfiguration.class)));
    assertThat(configuration.getParent(), is(sameInstance(mockParentConfiguration)));
  }

  @Test
  public void constructUninitializedConversionService() {

    exception.expect(IllegalStateException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("The ConversionService was not properly initialized!");

    new TestConfiguration(new Properties()).getConversionService();
  }

  @Test
  public void setConversionServiceToNull() {

    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("The ConversionService used to support this Configuration cannot be null!");

    new TestConfiguration(new Properties()).setConversionService(null);
  }

  @Test
  public void setAndGetConversionService() {

    AbstractConfiguration configuration = new TestConfiguration(new Properties());

    ConversionService mockConversionService = mock(ConversionService.class);

    configuration.setConversionService(mockConversionService);

    assertThat(configuration.getConversionService(), is(sameInstance(mockConversionService)));
  }

  @Test
  public void convert() {

    ConversionService mockConversionService = mock(ConversionService.class);

    when(mockConversionService.canConvert(eq(String.class), eq(Gender.class))).thenReturn(true);
    when(mockConversionService.convert(eq("FEMALE"), eq(Gender.class))).thenReturn(Gender.FEMALE);

    AbstractConfiguration configuration = new TestConfiguration(new Properties());

    configuration.setConversionService(mockConversionService);

    assertThat(configuration.getConversionService(), is(sameInstance(mockConversionService)));
    assertThat(configuration.convert("FEMALE", Gender.class), is(equalTo(Gender.FEMALE)));

    verify(mockConversionService, times(1)).canConvert(eq(String.class), eq(Gender.class));
    verify(mockConversionService, times(1)).convert(eq("FEMALE"), eq(Gender.class));
  }

  @Test
  public void convertWithNullType() {

    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("The Class type to convert the String value to cannot be null!");

    new TestConfiguration(new Properties()).convert("test", null);
  }

  @Test
  public void convertWithUnsupportedConversion() {

    ConversionService mockConversionService = mock(ConversionService.class);

    when(mockConversionService.canConvert(eq(String.class), eq(Process.class))).thenReturn(false);

    AbstractConfiguration configuration = new TestConfiguration(new Properties());

    configuration.setConversionService(mockConversionService);

    assertThat(configuration.getConversionService(), is(sameInstance(mockConversionService)));

    try {
      exception.expect(ConversionException.class);
      exception.expectCause(is(nullValue(Throwable.class)));
      exception.expectMessage(String.format("Cannot convert String value (12345) into a value of type (%1$s)!",
        Process.class.getName()));

      configuration.convert("12345", Process.class);
    }
    finally {
      verify(mockConversionService, times(1)).canConvert(eq(String.class), eq(Process.class));
      verify(mockConversionService, never()).convert(anyString(), eq(Process.class));
    }
  }

  @Test
  public void defaultIfUnset() {

    AbstractConfiguration configuration = new TestConfiguration(new Properties());

    assertEquals("val", configuration.defaultIfUnset("val", "test"));
    assertEquals("nil", configuration.defaultIfUnset("nil", "test"));
    assertEquals("null", configuration.defaultIfUnset("null", "test"));
    assertEquals("test", configuration.defaultIfUnset(null, "test"));
    assertEquals("test", configuration.defaultIfUnset("", "test"));
    assertEquals("test", configuration.defaultIfUnset("  ", "test"));
  }

  @Test
  public void isPresent() {

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
  public void isSet() {

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
  public void getPropertyValue() {

    AbstractConfiguration configuration = new TestConfiguration(configurationSettings);

    assertEquals("com.mysql.jdbc.Driver", configuration.getPropertyValue("jdbc.driverClassName"));
    assertEquals("jdbc:mysql://localhost:3306/appDataStore", configuration.getPropertyValue("jdbc.url"));
    assertNull(configuration.getPropertyValue("jdbc.username", false));
    assertNull(configuration.getPropertyValue("jdbc.password", false));
    assertEquals("admin", configuration.getPropertyValue("jdbc.username", "admin"));
    assertEquals("s3CuR3!", configuration.getPropertyValue("jdbc.username", "s3CuR3!"));
  }

  @Test
  public void getParentPropertyValue() {

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

  @Test
  public void getRequiredUnsetPropertyValue() {

    exception.expect(ConfigurationException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("The property (jdbc.username) is required!");

    new TestConfiguration(configurationSettings).getPropertyValue("jdbc.username");
  }

  @Test
  @IntegrationTest
  public void getPropertyValueAs() {

    Properties customConfigurationSettings = new Properties();

    customConfigurationSettings.setProperty("boolProp", "true");
    customConfigurationSettings.setProperty("charProp", "X");
    customConfigurationSettings.setProperty("dblProp", String.valueOf(Math.PI));
    customConfigurationSettings.setProperty("intProp", "123");
    customConfigurationSettings.setProperty("strProp", "test");

    AbstractConfiguration configuration = new TestConfiguration(customConfigurationSettings);

    configuration.setConversionService(loadConversionService());

    assertNotNull(configuration.getConversionService());
    assertTrue(configuration.getPropertyValueAs("boolProp", Boolean.class));
    assertEquals(new Character('X'), configuration.getPropertyValueAs("charProp", Character.class));
    assertEquals(new Double(Math.PI), configuration.getPropertyValueAs("dblProp", Double.class));
    assertEquals(new Integer(123), configuration.getPropertyValueAs("intProp", Integer.class));
    assertEquals("test", configuration.getPropertyValueAs("strProp", String.class));
  }

  @Test
  @IntegrationTest
  public void getConvertedPropertyValueAs() {

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
  public void getDefaultPropertyValueAs() {

    User guestUser = () -> "GUEST";

    AbstractConfiguration configuration = new TestConfiguration(configurationSettings);

    configuration.setConversionService(new TestConversionService(new UserConverter()));

    assertTrue(configuration.getConversionService() instanceof TestConversionService);
    assertSame(guestUser, configuration.getPropertyValueAs("os.user", guestUser, User.class));
  }

  @Test
  @IntegrationTest
  public void getNonRequiredPropertyValueAs() {

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

      assertThat(configuration.getConversionService() instanceof TestConversionService, is(true));

      configuration.getPropertyValueAs("jdbc.username", true, User.class);
    }
    catch (ConfigurationException expected) {

      assertEquals(String.format("Failed to get the value of configuration setting property (%1$s) as type (%2$s)!",
        "jdbc.username", User.class.getName()), expected.getMessage());
      assertTrue(expected.getCause() instanceof ConversionException);
      assertEquals(String.format("Failed to convert username (%1$s) into a User", "hacker123"),
        expected.getCause().getMessage());

      throw expected;
    }
  }

  protected static final class TestConfiguration extends AbstractConfiguration {

    private final Properties properties;

    public TestConfiguration(Properties properties) {
      this(null, properties);
    }

    public TestConfiguration(Configuration parent, Properties properties) {
      super(parent);
      Assert.notNull(properties, "Properties cannot be null");
      this.properties = properties;
    }

    @Override
    protected String doGetPropertyValue(String propertyName) {
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

  protected final class UserConverter extends org.cp.elements.data.conversion.AbstractConverter<String,User> {

    @Override
    public boolean canConvert(Class<?> fromType, Class<?> toType) {
      return User.class.equals(toType);
    }

    @Override
    public User convert(String value) {
      Assert.isTrue(StringUtils.isLetters(value), new ConversionException(String.format(
        "Failed to convert username (%1$s) into a User", value)));

      User mockUser = mock(User.class);

      when(mockUser.getUsername()).thenReturn(value);

      return mockUser;
    }
  }
}
