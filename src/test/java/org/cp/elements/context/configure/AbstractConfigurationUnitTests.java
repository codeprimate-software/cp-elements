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
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newConversionException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.File;
import java.net.URI;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.function.Supplier;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.cp.elements.context.annotation.Profile;
import org.cp.elements.context.configure.AbstractConfiguration.AbstractConfigurationDescriptor;
import org.cp.elements.context.configure.AbstractConfiguration.FileConfigurationDescriptor;
import org.cp.elements.context.configure.AbstractConfiguration.MapConfigurationDescriptor;
import org.cp.elements.context.configure.AbstractConfiguration.PropertiesConfigurationDescriptor;
import org.cp.elements.data.conversion.AbstractConversionService;
import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.ConversionService;
import org.cp.elements.data.conversion.Converter;
import org.cp.elements.data.conversion.converters.URIConverter;
import org.cp.elements.enums.Gender;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Nameable;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.ThrowableAssertions;
import org.cp.elements.test.annotation.IntegrationTest;
import org.cp.elements.util.ArrayUtils;

/**
 * Unit Tests for {@link AbstractConfiguration}.
 *
 * @author John J. Blum
 * @see java.util.Properties
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.context.configure.AbstractConfiguration
 * @see org.cp.elements.data.conversion.ConversionService
 * @see org.cp.elements.data.conversion.Converter
 * @see org.cp.elements.test.annotation.IntegrationTest
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AbstractConfigurationUnitTests {

  private Properties configurationProperties = new Properties();

  @BeforeEach
  public void setup() {

    this.configurationProperties.setProperty("jdbc.driverClassName", "com.mysql.jdbc.Driver");
    this.configurationProperties.setProperty("jdbc.url", "jdbc:mysql://localhost:3306/testDataStore");
    this.configurationProperties.setProperty("jdbc.username", "");
    this.configurationProperties.setProperty("jdbc.password", "");
  }

  @AfterEach
  public void tearDown() {

    this.configurationProperties.clear();
    this.configurationProperties = null;
  }

  private ConversionService loadConversionService() {
    return ConversionService.getLoader().getServiceInstance();
  }

  @Test
  public void constructAbstractConfiguration() {

    AbstractConfiguration configuration = new TestConfiguration(new Properties());

    assertThat(configuration).isNotNull();
    assertThat(configuration.getParent()).isNull();
  }

  @Test
  public void constructAbstractConfigurationWithParent() {

    Configuration mockParentConfiguration = mock(Configuration.class);

    AbstractConfiguration configuration = new TestConfiguration(mockParentConfiguration, new Properties());

    assertThat(configuration).isNotNull();
    assertThat(configuration.getParent()).isSameAs(mockParentConfiguration);
  }

  @Test
  public void setAndGetConversionService() {

    AbstractConfiguration configuration = new TestConfiguration(new Properties());

    ConversionService mockConversionService = mock(ConversionService.class);

    configuration.setConversionService(mockConversionService);

    assertThat(configuration.getConversionService()).isSameAs(mockConversionService);

    verifyNoInteractions(mockConversionService);
  }

  @Test
  public void setConversionServiceToNull() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new TestConfiguration(new Properties()).setConversionService(null))
      .withMessage("The ConversionService used by this Configuration is required")
      .withNoCause();
  }

  @Test
  public void getUninitializedConversionService() {

    assertThatIllegalStateException()
      .isThrownBy(() -> new TestConfiguration(new Properties()).getConversionService())
      .withMessage("The ConversionService was not properly initialized")
      .withNoCause();
  }

  @Test
  public void isParentConfigurationPresentWithNoParent() {

    AbstractConfiguration configuration = new TestConfiguration(this.configurationProperties);

    assertThat(configuration.getParent()).isNull();
    assertThat(configuration.isParentConfigurationPresent()).isFalse();
  }

  @Test
  public void isParentConfigurationPresentWithParent() {

    Configuration mockParentConfiguration = mock(Configuration.class);

    AbstractConfiguration configuration = new TestConfiguration(mockParentConfiguration, this.configurationProperties);

    assertThat(configuration.getParent()).isEqualTo(mockParentConfiguration);
    assertThat(configuration.isParentConfigurationPresent()).isTrue();

    verifyNoInteractions(mockParentConfiguration);
  }

  @Test
  public void isPresent() {

    AbstractConfiguration configuration = new TestConfiguration(this.configurationProperties);

    assertThat(configuration.isPresent("jdbc.driverClassName")).isTrue();
    assertThat(configuration.isPresent("jdbc.url")).isTrue();
    assertThat(configuration.isPresent("jdbc.username")).isTrue();
    assertThat(configuration.isPresent("jdbc.password")).isTrue();
  }

  @Test
  public void isNotPresent() {

    AbstractConfiguration configuration = new TestConfiguration(this.configurationProperties);

    assertThat(configuration.isPresent("jdbc.driver.class.name")).isFalse();
    assertThat(configuration.isPresent("jdbc.URL")).isFalse();
    assertThat(configuration.isPresent("jdbc.user.name")).isFalse();
    assertThat(configuration.isPresent("jdbc.pwd")).isFalse();
  }

  @Test
  public void isSet() {

    AbstractConfiguration configuration = new TestConfiguration(this.configurationProperties);

    assertThat(configuration.isPresent("jdbc.driverClassName")).isTrue();
    assertThat(configuration.isSet("jdbc.driverClassName")).isTrue();
    assertThat(configuration.isPresent("jdbc.url")).isTrue();
    assertThat(configuration.isSet("jdbc.url")).isTrue();
    assertThat(configuration.isPresent("jdbc.username")).isTrue();
    assertThat(configuration.isSet("jdbc.username")).isFalse();
    assertThat(configuration.isPresent("jdbc.password")).isTrue();
    assertThat(configuration.isSet("jdbc.password")).isFalse();
  }

  @Test
  public void isNotSet() {

    AbstractConfiguration configuration = new TestConfiguration(this.configurationProperties);

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
  public void convert() {

    ConversionService mockConversionService = mock(ConversionService.class);

    doReturn(true).when(mockConversionService).canConvert(eq(String.class), eq(Gender.class));
    doReturn(Gender.FEMALE).when(mockConversionService).convert(eq("FEMALE"), eq(Gender.class));

    AbstractConfiguration configuration = new TestConfiguration(new Properties());

    configuration.setConversionService(mockConversionService);

    assertThat(configuration.getConversionService()).isSameAs(mockConversionService);
    assertThat(configuration.convert("FEMALE", Gender.class)).isEqualTo(Gender.FEMALE);

    verify(mockConversionService, times(1)).canConvert(eq(String.class), eq(Gender.class));
    verify(mockConversionService, times(1)).convert(eq("FEMALE"), eq(Gender.class));
    verifyNoMoreInteractions(mockConversionService);
  }

  @Test
  public void convertWithNullType() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new TestConfiguration(new Properties()).convert("test", null))
      .withMessage("Class type to convert the String value to is required")
      .withNoCause();
  }

  @Test
  public void convertWithUnsupportedConversion() {

    ConversionService mockConversionService = mock(ConversionService.class);

    doReturn(false).when(mockConversionService).canConvert(eq(String.class), any());

    AbstractConfiguration configuration = new TestConfiguration(new Properties());

    configuration.setConversionService(mockConversionService);

    assertThat(configuration.getConversionService()).isSameAs(mockConversionService);

    assertThatExceptionOfType(ConversionException.class)
      .isThrownBy(() -> configuration.convert("12345", Process.class))
      .withMessage("Cannot convert String value [12345] into a value of type [%s]", Process.class.getName())
      .withNoCause();

    verify(mockConversionService, times(1)).canConvert(eq(String.class), eq(Process.class));
    verify(mockConversionService, never()).convert(anyString(), eq(Process.class));
    verifyNoMoreInteractions(mockConversionService);
  }

  @Test
  public void setAndGetDescriptor() {

    AbstractConfiguration configuration = new TestConfiguration(this.configurationProperties);

    Configuration.Descriptor<?> mockConfigurationDescriptor = mock(Configuration.Descriptor.class);

    assertThat(configuration.getDescriptor()).isNull();

    configuration.setDescriptor(mockConfigurationDescriptor);

    assertThat(configuration.getDescriptor()).isSameAs(mockConfigurationDescriptor);

    configuration.setDescriptor(null);

    assertThat(configuration.getDescriptor()).isNull();
  }

  @Test
  public void getPropertyValue() {

    AbstractConfiguration configuration = new TestConfiguration(this.configurationProperties);

    assertThat(configuration.getPropertyValue("jdbc.driverClassName")).isEqualTo("com.mysql.jdbc.Driver");
    assertThat(configuration.getPropertyValue("jdbc.url")).isEqualTo("jdbc:mysql://localhost:3306/testDataStore");
    assertThat(configuration.getPropertyValue("jdbc.username", false)).isNull();
    assertThat(configuration.getPropertyValue("jdbc.password", false)).isNull();
    assertThat(configuration.getPropertyValue("jdbc.username", "admin")).isEqualTo("admin");
    assertThat(configuration.getPropertyValue("jdbc.username", () -> "s3Cr3t!")).isEqualTo("s3Cr3t!");
  }

  @Test
  public void getPropertyValueForUndeclaredProperty() {

    AbstractConfiguration configuration = new TestConfiguration(this.configurationProperties);

    Arrays.stream(ArrayUtils.asArray("  ", "", null)).forEach(propertyName ->
      assertThatIllegalArgumentException()
        .isThrownBy(() -> configuration.getPropertyValue(propertyName))
        .withMessage("Property name [%s] is required", propertyName)
        .withNoCause());
  }

  @Test
  public void getParentPropertyValue() {

    Properties parentConfigurationProperties = new Properties();

    parentConfigurationProperties.setProperty("jdbc.username", "root");
    parentConfigurationProperties.setProperty("jdbc.password", "p@5s5sW0rDd!");

    AbstractConfiguration parentConfiguration = new TestConfiguration(parentConfigurationProperties);
    AbstractConfiguration configuration = new TestConfiguration(parentConfiguration, this.configurationProperties);

    assertThat(parentConfiguration.getParent()).isNull();
    assertThat(configuration.getParent()).isSameAs(parentConfiguration);
    assertThat(configuration.getPropertyValue("jdbc.username")).isEqualTo("root");
    assertThat(configuration.getPropertyValue("jdbc.password")).isEqualTo("p@5s5sW0rDd!");
  }

  @Test
  public void getNoParentDefaultPropertyValue() {

    AbstractConfiguration configuration = new TestConfiguration(this.configurationProperties);

    assertThat(configuration.getPropertyValue("non-existing.property", "default"))
      .isEqualTo("default");
  }

  @Test
  public void getNoParentDefaultSuppliedPropertyValue() {

    AbstractConfiguration configuration = new TestConfiguration(this.configurationProperties);

    assertThat(configuration.getPropertyValue("non-existing.property", () -> "mock"))
      .isEqualTo("mock");
  }

  @Test
  public void getNonRequiredPropertyValue() {

    AbstractConfiguration configuration = new TestConfiguration(this.configurationProperties);

    assertThat(configuration.getPropertyValue("non-existing.property", false)).isNull();
  }

  @Test
  public void getRequiredUndeclaredPropertyValue() {

    assertThatExceptionOfType(ConfigurationException.class)
      .isThrownBy(() -> new TestConfiguration(this.configurationProperties).getPropertyValue("jdbc.isolation-level"))
      .withMessage("Property [jdbc.isolation-level] is required, but was not declared")
      .withNoCause();
  }

  @Test
  public void getRequiredUnsetPropertyValue() {

    assertThatExceptionOfType(ConfigurationException.class)
      .isThrownBy(() -> new TestConfiguration(this.configurationProperties).getPropertyValue("jdbc.username"))
      .withMessage("Property [jdbc.username] is required, but was not defined")
      .withNoCause();
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
    assertThat(configuration.getPropertyValueAs("charProp", Character.class)).isEqualTo('X');
    assertThat(configuration.getPropertyValueAs("dblProp", Double.class)).isEqualTo(Math.PI);
    assertThat(configuration.getPropertyValueAs("intProp", Integer.class)).isEqualTo(123);
    assertThat(configuration.getPropertyValueAs("strProp", String.class)).isEqualTo("test");
  }

  @Test
  public void getPropertyValueAsForUndeclaredProperty() {

    AbstractConfiguration configuration = new TestConfiguration(this.configurationProperties);

    Arrays.stream(ArrayUtils.asArray("  ", "", null)).forEach(propertyName ->
      assertThatIllegalArgumentException()
        .isThrownBy(() -> configuration.getPropertyValueAs(propertyName, Object.class))
        .withMessage("Property name [%s] is required", propertyName)
        .withNoCause());
  }

  @Test
  @IntegrationTest
  public void getConvertedPropertyValueAs() {

    Properties customConfigurationProperties = new Properties();

    customConfigurationProperties.setProperty("system.os.user.admin", "root");

    AbstractConfiguration configuration = new TestConfiguration(customConfigurationProperties);

    configuration.setConversionService(new TestConversionService(new UserConverter()));

    assertThat(configuration.getConversionService()).isInstanceOf(TestConversionService.class);

    User root = configuration.getPropertyValueAs("system.os.user.admin", User.class);

    assertThat(root).isNotNull();
    assertThat(root.getName()).isEqualTo("root");
  }

  @Test
  @IntegrationTest
  public void getConvertedPropertyValueAsUri() {

    AbstractConfiguration configuration = new TestConfiguration(this.configurationProperties);

    configuration.setConversionService(new TestConversionService(new URIConverter()));

    assertThat(configuration.getConversionService()).isInstanceOf(TestConversionService.class);

    URI jdbcUri = configuration.getPropertyValueAs("jdbc.url", URI.class, false);

    assertThat(jdbcUri).isNotNull();
    assertThat(jdbcUri.toString()).isEqualTo("jdbc:mysql://localhost:3306/testDataStore");
  }

  @Test
  @IntegrationTest
  public void getDefaultPropertyValueAs() {

    User guestUser = () -> "GUEST";

    AbstractConfiguration configuration = new TestConfiguration(this.configurationProperties);

    configuration.setConversionService(new TestConversionService(new UserConverter()));

    assertThat(configuration.getConversionService()).isInstanceOf(TestConversionService.class);
    assertThat(configuration.getPropertyValueAs("os.user", User.class, guestUser)).isSameAs(guestUser);
  }

  @Test
  @IntegrationTest
  public void getDefaultSuppliedPropertyValueAs() {

    User mockUser = () -> "MOCK USER";

    Supplier<User> mockUserSupplier = () -> mockUser;

    AbstractConfiguration configuration = new TestConfiguration(this.configurationProperties);

    configuration.setConversionService(new TestConversionService(new UserConverter()));

    assertThat(configuration.getConversionService()).isInstanceOf(TestConversionService.class);
    assertThat(configuration.getPropertyValueAs("os.user", User.class, mockUserSupplier)).isSameAs(mockUser);
  }

  @Test
  @IntegrationTest
  public void getNonRequiredPropertyValueAs() {

    AbstractConfiguration configuration = new TestConfiguration(this.configurationProperties);

    configuration.setConversionService(new TestConversionService(new UserConverter()));

    assertThat(configuration.getConversionService()).isInstanceOf(TestConversionService.class);
    assertThat(configuration.getPropertyValueAs("jdbc.username", User.class, false)).isNull();
  }

  @Test
  public void getNonRequiredPropertyValueAsIllegalTypeThrowsException() {

    AbstractConfiguration configuration = new TestConfiguration(this.configurationProperties);

    this.configurationProperties.setProperty("jdbc.connection.max", "ten");

    configuration.setConversionService(loadConversionService());

    assertThat(configuration.getConversionService()).isNotNull();

    ThrowableAssertions.assertThatThrowableOfType(ConfigurationException.class)
      .isThrownBy(args -> configuration.getPropertyValueAs("jdbc.connection.max", Integer.class, false))
      .causedBy(ConversionException.class)
      .havingMessage("Cannot convert [ten] to [java.lang.Integer]")
      .withNoCause();
  }

  @Test
  @IntegrationTest
  public void getRequiredPropertyValueAsIsSuccessful() {

    AbstractConfiguration configuration = new TestConfiguration(this.configurationProperties);

    this.configurationProperties.setProperty("jdbc.username", "jonDoe");

    configuration.setConversionService(new TestConversionService(new UserConverter()));

    assertThat(configuration.getConversionService()).isInstanceOf(TestConversionService.class);

    User user = configuration.getPropertyValueAs("jdbc.username", User.class, true);

    assertThat(user).isNotNull();
    assertThat(user.getName()).isEqualTo("jonDoe");
  }

  @Test
  @IntegrationTest
  public void getRequiredPropertyValueAsThrowsException() {

    AbstractConfiguration configuration = new TestConfiguration(this.configurationProperties);

    this.configurationProperties.setProperty("jdbc.username", "hacker3000");

    configuration.setConversionService(new TestConversionService(new UserConverter()));

    assertThat(configuration.getConversionService()).isInstanceOf(TestConversionService.class);

    ThrowableAssertions.assertThatThrowableOfType(ConfigurationException.class)
      .isThrownBy(args -> configuration.getPropertyValueAs("jdbc.username", User.class, true))
      .havingMessage("Failed to get value [hacker3000] of configuration property [jdbc.username] as an instance of type [%s]",
        User.class.getName())
      .causedBy(ConversionException.class)
      .havingMessage("Cannot convert username [hacker3000] into a User")
      .withNoCause();
  }

  @Test
  public void constructNewAbstractConfigurationDescriptor() {

    Object source = spy(new Object());

    AbstractConfigurationDescriptor<Object> configurationDescriptor =
      new AbstractConfigurationDescriptor<Object>(source) {};

    assertThat(configurationDescriptor).isNotNull();

    verifyNoInteractions(source);
  }

  @Test
  public void constructNewAbstractConfigurationDescriptorWithNullSource() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new AbstractConfigurationDescriptor<Object>(null) { })
      .withMessage("Source is required")
      .withNoCause();
  }

  @Test
  public void fromNewFileConfigurationDescriptor() {

    File mockFile = mock(File.class);

    FileConfigurationDescriptor configurationDescriptor = FileConfigurationDescriptor.from(mockFile);

    assertThat(configurationDescriptor).isNotNull();
    assertThat(configurationDescriptor.getSource()).isEqualTo(mockFile);

    verifyNoInteractions(mockFile);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void fromNewMapConfigurationDescriptor() {

    Map<String, String> mockMap = mock(Map.class);

    MapConfigurationDescriptor configurationDescriptor = MapConfigurationDescriptor.from(mockMap);

    assertThat(configurationDescriptor).isNotNull();
    assertThat(configurationDescriptor.getSource()).isEqualTo(mockMap);

    verifyNoInteractions(mockMap);
  }

  @Test
  public void fromNewPropertiesConfigurationDescriptor() {

    Properties mockProperties = mock(Properties.class);

    PropertiesConfigurationDescriptor configurationDescriptor = PropertiesConfigurationDescriptor.from(mockProperties);

    assertThat(configurationDescriptor).isNotNull();
    assertThat(configurationDescriptor.getSource()).isEqualTo(mockProperties);

    verifyNoInteractions(mockProperties);
  }

  private static final class TestConfiguration extends AbstractConfiguration {

    private final Properties properties;

    public TestConfiguration(Properties properties) {
      this(null, properties);
    }

    public TestConfiguration(Configuration parent, Properties properties) {

      super(parent);

      this.properties =
        ObjectUtils.requireObject(properties, "Properties are required");
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

  @Profile(names = "test")
  private static class TestConversionService extends AbstractConversionService {

    public TestConversionService() { }

    public TestConversionService(Converter<?, ?> defaultConverter) {
      register(defaultConverter);
    }
  }

  private interface User extends Nameable<String> { }

  private static final class UserConverter extends org.cp.elements.data.conversion.AbstractConverter<String, User> {

    @Override
    public boolean canConvert(Class<?> fromType, Class<?> toType) {
      return User.class.equals(toType);
    }

    @Override
    public User convert(String value) {

      Assert.isTrue(StringUtils.isLetters(value),
        newConversionException("Cannot convert username [%s] into a User", value));

      User mockUser = mock(User.class);

      doReturn(value).when(mockUser).getName();

      return mockUser;
    }
  }
}
