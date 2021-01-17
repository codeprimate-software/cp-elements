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
import org.junit.Test;

/**
 * Unit Tests for {@link AbstractConfiguration}.
 *
 * @author John J. Blum
 * @see java.util.Properties
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.context.configure.AbstractConfiguration
 * @see org.cp.elements.data.conversion.AbstractConversionService
 * @see org.cp.elements.data.conversion.ConversionService
 * @see org.cp.elements.data.conversion.Converter
 * @see org.cp.elements.test.annotation.IntegrationTest
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AbstractConfigurationTests {

  private Properties configurationSettings = new Properties();

  @Before
  public void setup() {

    this.configurationSettings.setProperty("jdbc.driverClassName", "com.mysql.jdbc.Driver");
    this.configurationSettings.setProperty("jdbc.url", "jdbc:mysql://localhost:3306/appDataStore");
    this.configurationSettings.setProperty("jdbc.username", "");
    this.configurationSettings.setProperty("jdbc.password", "");
  }

  @After
  public void tearDown() {

    this.configurationSettings.clear();
    this.configurationSettings = null;
  }

  ConversionService loadConversionService() {

    ServiceLoader<ConversionService> serviceLoader = ServiceLoader.load(ConversionService.class);

    assertThat(serviceLoader).isNotNull();

    Iterator<ConversionService> iterator = serviceLoader.iterator();

    assertThat(iterator.hasNext()).isTrue();

    return iterator.next();
  }

  @Test
  public void construct() {

    AbstractConfiguration configuration = new TestConfiguration(new Properties());

    assertThat(configuration).isNotNull();
    assertThat(configuration.getParent()).isNull();
  }

  @Test
  public void constructWithParent() {

    Configuration mockParentConfiguration = mock(Configuration.class);

    AbstractConfiguration configuration = new TestConfiguration(mockParentConfiguration, new Properties());

    assertThat(configuration).isNotNull();
    assertThat(configuration.getParent()).isSameAs(mockParentConfiguration);
  }

  @Test(expected = IllegalStateException.class)
  public void constructUninitializedConversionService() {

    try {
      new TestConfiguration(new Properties()).getConversionService();
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("The ConversionService was not properly initialized!");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void setConversionServiceToNull() {

    try {
      new TestConfiguration(new Properties()).setConversionService(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The ConversionService used to support this Configuration cannot be null!");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void setAndGetConversionService() {

    AbstractConfiguration configuration = new TestConfiguration(new Properties());

    ConversionService mockConversionService = mock(ConversionService.class);

    configuration.setConversionService(mockConversionService);

    assertThat(configuration.getConversionService()).isSameAs(mockConversionService);
  }

  @Test
  public void convert() {

    ConversionService mockConversionService = mock(ConversionService.class);

    when(mockConversionService.canConvert(eq(String.class), eq(Gender.class))).thenReturn(true);
    when(mockConversionService.convert(eq("FEMALE"), eq(Gender.class))).thenReturn(Gender.FEMALE);

    AbstractConfiguration configuration = new TestConfiguration(new Properties());

    configuration.setConversionService(mockConversionService);

    assertThat(configuration.getConversionService()).isSameAs(mockConversionService);
    assertThat(configuration.convert("FEMALE", Gender.class)).isEqualTo(Gender.FEMALE);

    verify(mockConversionService, times(1)).canConvert(eq(String.class), eq(Gender.class));
    verify(mockConversionService, times(1)).convert(eq("FEMALE"), eq(Gender.class));
  }

  @Test(expected = IllegalArgumentException.class)
  public void convertWithNullType() {

    try {
      new TestConfiguration(new Properties()).convert("test", null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The Class type to convert the String value to cannot be null!");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ConversionException.class)
  public void convertWithUnsupportedConversion() {

    ConversionService mockConversionService = mock(ConversionService.class);

    when(mockConversionService.canConvert(eq(String.class), eq(Process.class))).thenReturn(false);

    AbstractConfiguration configuration = new TestConfiguration(new Properties());

    configuration.setConversionService(mockConversionService);

    assertThat(configuration.getConversionService()).isSameAs(mockConversionService);

    try {
      configuration.convert("12345", Process.class);
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("Cannot convert String value (12345) into a value of type (%1$s)!",
        Process.class.getName());

      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockConversionService, times(1)).canConvert(eq(String.class), eq(Process.class));
      verify(mockConversionService, never()).convert(anyString(), eq(Process.class));
    }
  }

  @Test
  public void defaultIfUnset() {

    AbstractConfiguration configuration = new TestConfiguration(new Properties());

    assertThat(configuration.defaultIfUnset("val", "test")).isEqualTo("val");
    assertThat(configuration.defaultIfUnset("nil", "test")).isEqualTo("nil");
    assertThat(configuration.defaultIfUnset("null", "test")).isEqualTo("null");
    assertThat(configuration.defaultIfUnset(null, "test")).isEqualTo("test");
    assertThat(configuration.defaultIfUnset("", "test")).isEqualTo("test");
    assertThat(configuration.defaultIfUnset("  ", "test")).isEqualTo("test");
  }

  @Test
  public void isPresent() {

    AbstractConfiguration configuration = new TestConfiguration(configurationSettings);

    assertThat(configuration.isPresent("jdbc.driverClassName")).isTrue();
    assertThat(configuration.isPresent("jdbc.url")).isTrue();
    assertThat(configuration.isPresent("jdbc.username")).isTrue();
    assertThat(configuration.isPresent("jdbc.password")).isTrue();
    assertThat(configuration.isPresent("jdbc.driver.class.name")).isFalse();
    assertThat(configuration.isPresent("jdbc.URL")).isFalse();
    assertThat(configuration.isPresent("jdbc.user.name")).isFalse();
    assertThat(configuration.isPresent("jdbc.pwd")).isFalse();
  }

  @Test
  public void isSet() {

    AbstractConfiguration configuration = new TestConfiguration(configurationSettings);

    assertThat(configuration.isPresent("jdbc.driverClassName")).isTrue();
    assertThat(configuration.isSet("jdbc.driverClassName")).isTrue();
    assertThat(configuration.isPresent("jdbc.url")).isTrue();
    assertThat(configuration.isSet("jdbc.url")).isTrue();
    assertThat(configuration.isPresent("jdbc.username")).isTrue();
    assertThat(configuration.isSet("jdbc.username")).isFalse();
    assertThat(configuration.isPresent("jdbc.password")).isTrue();
    assertThat(configuration.isSet("jdbc.password")).isFalse();
    assertThat(configuration.isPresent("jdbc.driver.class.name")).isFalse();
    assertThat(configuration.isSet("jdbc.driver.class.name")).isFalse();
    assertThat(configuration.isPresent("jdbc.URL")).isFalse();
    assertThat(configuration.isSet("jdbc.URL")).isFalse();
    assertThat(configuration.isPresent("jdbc.user.name")).isFalse();
    assertThat(configuration.isSet("jdbc.user.name")).isFalse();
    assertThat(configuration.isPresent("jdbc.pwd")).isFalse();
    assertThat(configuration.isSet("jdbc.pwd")).isFalse();
  }

  @Test
  public void getPropertyValue() {

    AbstractConfiguration configuration = new TestConfiguration(configurationSettings);

    assertThat(configuration.getPropertyValue("jdbc.driverClassName")).isEqualTo("com.mysql.jdbc.Driver");
    assertThat(configuration.getPropertyValue("jdbc.url")).isEqualTo("jdbc:mysql://localhost:3306/appDataStore");
    assertThat(configuration.getPropertyValue("jdbc.username", false)).isNull();
    assertThat(configuration.getPropertyValue("jdbc.password", false)).isNull();
    assertThat(configuration.getPropertyValue("jdbc.username", "admin")).isEqualTo("admin");
    assertThat(configuration.getPropertyValue("jdbc.username", "s3CuR3!")).isEqualTo("s3CuR3!");
  }

  @Test
  public void getParentPropertyValue() {

    Properties parentConfigurationSettings = new Properties();

    parentConfigurationSettings.setProperty("jdbc.username", "root");
    parentConfigurationSettings.setProperty("jdbc.password", "p@5s5sW0rDd!");

    AbstractConfiguration parentConfiguration = new TestConfiguration(parentConfigurationSettings);
    AbstractConfiguration configuration = new TestConfiguration(parentConfiguration, configurationSettings);

    assertThat(parentConfiguration.getParent()).isNull();
    assertThat(configuration.getParent()).isSameAs(parentConfiguration);
    assertThat(configuration.getPropertyValue("jdbc.username")).isEqualTo("root");
    assertThat(configuration.getPropertyValue("jdbc.password")).isEqualTo("p@5s5sW0rDd!");
  }

  @Test(expected = ConfigurationException.class)
  public void getRequiredUnsetPropertyValue() {

    try {
      new TestConfiguration(configurationSettings).getPropertyValue("jdbc.username");
    }
    catch (ConfigurationException expected) {

      assertThat(expected).hasMessage("The property (jdbc.username) is required!");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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

    assertThat(configuration.getConversionService()).isNotNull();
    assertThat(configuration.getPropertyValueAs("boolProp", Boolean.class)).isTrue();
    assertThat(configuration.getPropertyValueAs("charProp", Character.class)).isEqualTo(new Character('X'));
    assertThat(configuration.getPropertyValueAs("dblProp", Double.class)).isEqualTo(new Double(Math.PI));
    assertThat(configuration.getPropertyValueAs("intProp", Integer.class)).isEqualTo(new Integer(123));
    assertThat(configuration.getPropertyValueAs("strProp", String.class)).isEqualTo("test");
  }

  @Test
  @IntegrationTest
  public void getConvertedPropertyValueAs() {

    Properties customConfigurationProperties = new Properties();

    customConfigurationProperties.setProperty("os.admin.user", "root");

    AbstractConfiguration configuration = new TestConfiguration(customConfigurationProperties);

    configuration.setConversionService(new TestConversionService(new UserConverter()));

    assertThat(configuration.getConversionService() instanceof TestConversionService).isTrue();

    User root = configuration.getPropertyValueAs("os.admin.user", User.class);

    assertThat(root).isNotNull();
    assertThat(root.getName()).isEqualTo("root");
  }

  @Test
  public void getDefaultPropertyValueAs() {

    User guestUser = () -> "GUEST";

    AbstractConfiguration configuration = new TestConfiguration(configurationSettings);

    configuration.setConversionService(new TestConversionService(new UserConverter()));

    assertThat(configuration.getConversionService() instanceof TestConversionService).isTrue();
    assertThat(configuration.getPropertyValueAs("os.user", guestUser, User.class)).isSameAs(guestUser);
  }

  @Test
  @IntegrationTest
  public void getNonRequiredPropertyValueAs() {

    AbstractConfiguration configuration = new TestConfiguration(configurationSettings);

    configuration.setConversionService(new TestConversionService(new UserConverter()));

    assertThat(configuration.getConversionService() instanceof TestConversionService).isTrue();
    assertThat(configuration.getPropertyValueAs("jdbc.username", false, User.class)).isNull();
  }

  @Test(expected = ConfigurationException.class)
  // NOTE integration test!
  public void testGetRequiredPropertyValueAs() {

    try {

      AbstractConfiguration configuration = new TestConfiguration(configurationSettings);

      configurationSettings.setProperty("jdbc.username", "hacker123");
      configuration.setConversionService(new TestConversionService(new UserConverter()));

      assertThat(configuration.getConversionService() instanceof TestConversionService).isTrue();

      configuration.getPropertyValueAs("jdbc.username", true, User.class);
    }
    catch (ConfigurationException expected) {

      assertThat(expected.getMessage()).isEqualTo(
        String.format("Failed to get the value of configuration setting property (%1$s) as type (%2$s)!",
          "jdbc.username", User.class.getName()));
      assertThat(expected.getCause() instanceof ConversionException).isTrue();
      assertThat(expected.getCause().getMessage()).isEqualTo(
        String.format("Failed to convert username [%s] into a User", "hacker123"));

      throw expected;
    }
  }

  private static final class TestConfiguration extends AbstractConfiguration {

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
      return this.properties.getProperty(propertyName);
    }

    @Override
    public Iterator<String> iterator() {
      return this.properties.stringPropertyNames().iterator();
    }
  }

  private static final class TestConversionService extends AbstractConversionService {

    public TestConversionService() {
    }

    public TestConversionService(final Converter<?, ?> defaultConverter) {
      register(defaultConverter);
    }
  }

  private interface User {
    String getName();
  }

  private static final class UserConverter extends org.cp.elements.data.conversion.AbstractConverter<String,User> {

    @Override
    public boolean canConvert(Class<?> fromType, Class<?> toType) {
      return User.class.equals(toType);
    }

    @Override
    public User convert(String value) {

      Assert.isTrue(StringUtils.isLetters(value), new ConversionException(String.format(
        "Failed to convert username [%s] into a User", value)));

      User mockUser = mock(User.class);

      when(mockUser.getName()).thenReturn(value);

      return mockUser;
    }
  }
}
