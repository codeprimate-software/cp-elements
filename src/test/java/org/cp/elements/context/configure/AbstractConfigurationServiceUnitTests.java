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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.junit.Test;

import org.cp.elements.context.configure.AbstractConfigurationService.ConfigurationPropertiesInterfaceMethodInterceptor;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.Orderable;
import org.cp.elements.lang.Ordered;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.Order;
import org.cp.elements.lang.reflect.MethodInvocation;
import org.cp.elements.util.ArrayUtils;

/**
 * Unit Tests for {@link AbstractConfigurationService}
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.context.configure.AbstractConfigurationService
 * @see org.cp.elements.context.configure.AbstractConfigurationService.ConfigurationPropertiesInterfaceMethodInterceptor
 * @since 1.0.0
 */
public class AbstractConfigurationServiceUnitTests {

  @Test
  public void getActiveProfilesListCallsGetActiveProfiles() {

    String[] activeProfiles = { "dev", "qa", "prod" };

    AbstractConfigurationService configurationService = spy(new TestConfigurationService());

    doReturn(activeProfiles).when(configurationService).getActiveProfiles();

    List<String> activeProfilesList = configurationService.getActiveProfilesList();

    assertThat(activeProfilesList).isNotNull();
    assertThat(activeProfilesList).containsExactly(activeProfiles);
    assertThat(activeProfilesList).isSameAs(configurationService.getActiveProfilesList());
  }

  @Test
  public void getConfigurationPropertyNamesIsCorrect() {

    Configuration mockConfigurationOne = mock(Configuration.class);
    Configuration mockConfigurationTwo = mock(Configuration.class);

    doReturn(ArrayUtils.asIterable("configuration.one.property.one", "configuration.one.property.two").spliterator())
      .when(mockConfigurationOne).spliterator();

    doReturn(ArrayUtils.asIterable("configuration.two.property.one", "configuration.two.property.two").spliterator())
      .when(mockConfigurationTwo).spliterator();

    AbstractConfigurationService configurationService = new TestConfigurationService();

    assertThat(configurationService.register(mockConfigurationOne)).isTrue();
    assertThat(configurationService.register(mockConfigurationTwo)).isTrue();
    assertThat(configurationService).containsExactlyInAnyOrder(mockConfigurationOne, mockConfigurationTwo);

    assertThat(configurationService.getConfigurationPropertyNames()).containsExactlyInAnyOrder(
      "configuration.one.property.one",
      "configuration.one.property.two",
      "configuration.two.property.one",
      "configuration.two.property.two"
    );
  }

  @Test
  public void isProfileActiveWithConfigurationHavingUndeclaredProfilesReturnsTrue() {

    Configuration mockConfiguration = mock(Configuration.class);

    doReturn(new String[0]).when(mockConfiguration).getProfiles();

    AbstractConfigurationService configurationService = spy(new TestConfigurationService());

    assertThat(configurationService.isProfileActive(mockConfiguration)).isTrue();

    verify(configurationService, times(1)).isProfileActive(eq(mockConfiguration));
    verify(mockConfiguration, times(1)).getProfiles();
    verifyNoMoreInteractions(configurationService, mockConfiguration);
  }

  @Test
  public void isProfileActiveWithConfigurationHavingInvalidProfilesReturnsFalse() {

    Configuration mockConfiguration = mock(Configuration.class);

    doReturn(ArrayUtils.asArray("  ", "", null)).when(mockConfiguration).getProfiles();

    AbstractConfigurationService configurationService = spy(new TestConfigurationService());

    assertThat(configurationService.isProfileActive(mockConfiguration)).isFalse();

    verify(configurationService, times(1)).isProfileActive(eq(mockConfiguration));
    verify(configurationService, never()).getActiveProfilesList();
    verify(configurationService, never()).getActiveProfiles();
    verify(mockConfiguration, times(1)).getProfiles();
    verifyNoMoreInteractions(configurationService, mockConfiguration);
  }

  @Test
  public void isProfileActiveWithConfigurationHavingInactiveProfilesReturnsFalse() {

    Configuration mockConfiguration = mock(Configuration.class);

    doReturn(ArrayUtils.asArray("mock", "test", "undefined")).when(mockConfiguration).getProfiles();

    AbstractConfigurationService configurationService = spy(new TestConfigurationService());

    doReturn(ArrayUtils.asArray("dev", "qa")).when(configurationService).getActiveProfiles();

    assertThat(configurationService.isProfileActive(mockConfiguration)).isFalse();

    verify(configurationService, times(1)).isProfileActive(eq(mockConfiguration));
    verify(configurationService, times(3)).getActiveProfilesList();
    verify(configurationService, times(1)).getActiveProfiles();
    verify(mockConfiguration, times(1)).getProfiles();
    verifyNoMoreInteractions(configurationService, mockConfiguration);
  }

  @Test
  public void isProfileActiveWithConfigurationHavingActiveProfilesReturnsTrue() {

    Configuration mockConfiguration = mock(Configuration.class);

    doReturn(ArrayUtils.asArray("mock", "test", "undefined")).when(mockConfiguration).getProfiles();

    AbstractConfigurationService configurationService = spy(new TestConfigurationService());

    doReturn(ArrayUtils.asArray("dev", "qa", "test")).when(configurationService).getActiveProfiles();

    assertThat(configurationService.isProfileActive(mockConfiguration)).isTrue();

    verify(configurationService, times(1)).isProfileActive(eq(mockConfiguration));
    verify(configurationService, times(2)).getActiveProfilesList();
    verify(configurationService, times(1)).getActiveProfiles();
    verify(mockConfiguration, times(1)).getProfiles();
    verifyNoMoreInteractions(configurationService, mockConfiguration);
  }

  @Test
  public void isProfileActiveWithNullConfiguration() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new TestConfigurationService().isProfileActive(null))
      .withMessage("Configuration is required")
      .withNoCause();
  }

  @Test
  public void iteratorCallsGetConfigurationsIterator() {

    Configuration mockConfigurationOne = mock(Configuration.class);
    Configuration mockConfigurationTwo = mock(Configuration.class);

    AbstractConfigurationService configurationService = spy(TestConfigurationService.class);

    doReturn(Arrays.asList(mockConfigurationOne, mockConfigurationTwo)).when(configurationService).getConfigurations();

    Iterator<Configuration> configurations = configurationService.iterator();

    assertThat(configurations).isNotNull();
    assertThat(configurations.hasNext()).isTrue();
    assertThat(configurations.next()).isEqualTo(mockConfigurationOne);
    assertThat(configurations.hasNext()).isTrue();
    assertThat(configurations.next()).isEqualTo(mockConfigurationTwo);
    assertThat(configurations.hasNext()).isFalse();

    verify(configurationService, times(1)).iterator();
    verify(configurationService, times(1)).getConfigurations();
    verifyNoMoreInteractions(configurationService);
    verifyNoInteractions(mockConfigurationOne, mockConfigurationTwo);
  }

  @Test
  public void orderAfterRegistrationIsCorrect() {

    DefaultOrderTestConfiguration defaultTestConfiguration = new DefaultOrderTestConfiguration();
    FirstOrderTestConfiguration firstTestConfiguration = new FirstOrderTestConfiguration();
    LastOrderTestConfiguration lastTestConfiguration = new LastOrderTestConfiguration();
    MiddleOrderTestConfiguration middleTestConfiguration = new MiddleOrderTestConfiguration();

    AbstractConfigurationService configurationService = new TestConfigurationService();

    assertThat(configurationService).isEmpty();
    assertThat(configurationService.register(middleTestConfiguration)).isTrue();
    assertThat(configurationService.register(lastTestConfiguration)).isTrue();
    assertThat(configurationService.register(firstTestConfiguration)).isTrue();
    assertThat(configurationService.register(defaultTestConfiguration)).isTrue();
    assertThat(configurationService)
      .containsExactly(firstTestConfiguration, defaultTestConfiguration, middleTestConfiguration, lastTestConfiguration);
  }

  @Test
  public void registerAndUnregisterConfiguration() {

    Configuration mockConfigurationOne = mock(Configuration.class, "MockConfigurationOne");
    Configuration mockConfigurationTwo = mock(Configuration.class, "MockConfigurationTwo");

    AbstractConfigurationService configurationService = new TestConfigurationService();

    assertThat(configurationService).isEmpty();
    assertThat(configurationService.register(mockConfigurationOne)).isTrue();
    assertThat(configurationService).containsExactly(mockConfigurationOne);
    assertThat(configurationService.register(mockConfigurationOne)).isFalse();
    assertThat(configurationService).containsExactly(mockConfigurationOne);
    assertThat(configurationService.register(mockConfigurationTwo)).isTrue();
    assertThat(configurationService).containsExactlyInAnyOrder(mockConfigurationOne, mockConfigurationTwo);
    assertThat(configurationService.unregister(mockConfigurationOne)).isTrue();
    assertThat(configurationService).containsExactly(mockConfigurationTwo);
    assertThat(configurationService.unregister(mockConfigurationOne)).isFalse();
    assertThat(configurationService).containsExactly(mockConfigurationTwo);

    verify(mockConfigurationOne, times(2)).getProfiles();
    verify(mockConfigurationTwo, times(1)).getProfiles();
    verifyNoMoreInteractions(mockConfigurationOne, mockConfigurationTwo);
  }

  @Test
  public void registerNonActiveProfiledConfigurationIsCorrect() {

    Configuration mockConfiguration = mock(Configuration.class);

    doReturn(ArrayUtils.asArray("qa")).when(mockConfiguration).getProfiles();

    AbstractConfigurationService configurationService = spy(new TestConfigurationService());

    doReturn(ArrayUtils.asArray("dev", "test", "prod")).when(configurationService).getActiveProfiles();

    assertThat(configurationService).isEmpty();
    assertThat(configurationService.register(mockConfiguration)).isFalse();
    assertThat(configurationService).isEmpty();
    assertThat(configurationService.unregister(mockConfiguration)).isFalse();
    assertThat(configurationService).isEmpty();

    verify(mockConfiguration, times(1)).getProfiles();
    verifyNoMoreInteractions(mockConfiguration);
  }

  @Test
  public void registerNonProfiledConfigurationWhenProfilesActiveIsCorrect() {

    Configuration mockConfiguration = mock(Configuration.class);

    doReturn(StringUtils.EMPTY_STRING_ARRAY).when(mockConfiguration).getProfiles();

    ConfigurationService configurationService = spy(new TestConfigurationService());

    doReturn(ArrayUtils.asArray("dev", "qa", "test")).when(configurationService).getActiveProfiles();

    assertThat(configurationService).isEmpty();
    assertThat(configurationService.register(mockConfiguration)).isTrue();
    assertThat(configurationService).containsExactly(mockConfiguration);
    assertThat(configurationService.unregister(mockConfiguration)).isTrue();
    assertThat(configurationService).isEmpty();

    verify(mockConfiguration, times(1)).getProfiles();
    verifyNoMoreInteractions(mockConfiguration);
  }

  @Test
  public void registerProfiledConfigurationIsCorrect() {

    Configuration mockConfiguration = mock(Configuration.class);

    doReturn(ArrayUtils.asArray("mock", "test", "undefined")).when(mockConfiguration).getProfiles();

    ConfigurationService configurationService = spy(new TestConfigurationService());

    doReturn(ArrayUtils.asArray("dev", "qa", "test")).when(configurationService).getActiveProfiles();

    assertThat(configurationService).isEmpty();
    assertThat(configurationService.register(mockConfiguration)).isTrue();
    assertThat(configurationService).containsExactly(mockConfiguration);
    assertThat(configurationService.unregister(mockConfiguration)).isTrue();
    assertThat(configurationService).isEmpty();

    verify(mockConfiguration, times(1)).getProfiles();
    verifyNoMoreInteractions(mockConfiguration);
  }

  @Test
  public void registerProfiledConfigurationWhenNoProfilesActiveIsCorrect() {

    Configuration mockConfiguration = mock(Configuration.class);

    doReturn(ArrayUtils.asArray("mock", "test")).when(mockConfiguration).getProfiles();

    ConfigurationService configurationService = spy(new TestConfigurationService());

    doReturn(StringUtils.EMPTY_STRING_ARRAY).when(configurationService).getActiveProfiles();

    assertThat(configurationService).isEmpty();
    assertThat(configurationService.register(mockConfiguration)).isFalse();
    assertThat(configurationService).isEmpty();
    assertThat(configurationService.unregister(mockConfiguration)).isFalse();
    assertThat(configurationService).isEmpty();

    verify(mockConfiguration, times(1)).getProfiles();
    verifyNoMoreInteractions(mockConfiguration);
  }

  @Test
  public void registerIsNullSafe() {

    ConfigurationService configurationService = new TestConfigurationService();

    assertThat(configurationService).isEmpty();
    assertThat(configurationService.register(null)).isFalse();
    assertThat(configurationService).isEmpty();
  }

  @Test
  public void unregisterNonRegisteredConfigurationIsCorrect() {

    Configuration mockConfiguration = mock(Configuration.class);

    ConfigurationService configurationService = new TestConfigurationService();

    assertThat(configurationService).doesNotContain(mockConfiguration);
    assertThat(configurationService.unregister(mockConfiguration)).isFalse();

    verifyNoInteractions(mockConfiguration);
  }

  @Test
  public void unregisterIsNullSafe() {

    ConfigurationService configurationService = new TestConfigurationService();

    assertThat(configurationService).isEmpty();
    assertThat(configurationService.unregister(null)).isFalse();
    assertThat(configurationService).isEmpty();
  }

  @Test
  public void proxyClassType() {

    AbstractConfigurationService configurationService = new TestConfigurationService();

    assertThatIllegalArgumentException()
      .isThrownBy(() -> configurationService.proxy(TestClassType.class))
      .withMessage("The type to proxy [%s] must be an interface", TestClassType.class.getName())
      .withNoCause();
  }

  @Test
  public void proxyNull() {

    AbstractConfigurationService configurationService = new TestConfigurationService();

    assertThatIllegalArgumentException()
      .isThrownBy(() -> configurationService.proxy(null))
      .withMessage("The interface to proxy is required")
      .withNoCause();
  }

  @Test
  public void proxyWithInvalidPropertyPrefixes() {

    AbstractConfigurationService configurationService = new TestConfigurationService();

    Arrays.stream(ArrayUtils.asArray("  ", "", null)).forEach(propertyPrefix ->
      assertThatIllegalArgumentException()
        .isThrownBy(() -> configurationService.proxy(TestInterfaceType.class, propertyPrefix))
        .withMessage("Property prefix [%s] is required", propertyPrefix)
        .withNoCause());
  }

  @Test
  public void constructConfigurationPropertiesInterfaceMethodInterceptorIsCorrect() {

    AbstractConfigurationService configurationService = new TestConfigurationService();

    ConfigurationPropertiesInterfaceMethodInterceptor methodInterceptor =
      new ConfigurationPropertiesInterfaceMethodInterceptor(configurationService, TestInterfaceType.class,
        "test.property.prefix");

    assertThat(methodInterceptor).isNotNull();
    assertThat(methodInterceptor.getConfigurationPropertiesInterface()).isEqualTo(TestInterfaceType.class);
    assertThat(methodInterceptor.getConfigurationService()).isEqualTo(configurationService);
    assertThat(methodInterceptor.getPropertyPrefix()).isEqualTo("test.property.prefix");
    assertThat(methodInterceptor.getQualifiedPropertyName("mock")).isEqualTo("test.property.prefix.mock");
    assertThat(methodInterceptor.getTarget()).isNotNull();
  }

  @Test
  public void constructConfigurationPropertiesInterfaceMethodInterceptorWithNullConfigurationService() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ConfigurationPropertiesInterfaceMethodInterceptor(null,
        TestInterfaceType.class,"test.property.prefix"))
      .withMessage("ConfigurationService is required")
      .withNoCause();

  }

  @Test
  public void constructConfigurationPropertiesInterfaceMethodInterceptorWithNullConfigurationPropertiesInterface() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ConfigurationPropertiesInterfaceMethodInterceptor(mock(AbstractConfigurationService.class),
        null,"test.property.prefix"))
      .withMessage("Interface is required")
      .withNoCause();

  }

  @Test
  public void constructConfigurationPropertiesInterfaceMethodInterceptorWithInvalidPropertyPrefix() {

    Arrays.stream(ArrayUtils.asArray("  ", "", null)).forEach(propertyPrefix ->
      assertThatIllegalArgumentException()
        .isThrownBy(() ->
          new ConfigurationPropertiesInterfaceMethodInterceptor(mock(AbstractConfigurationService.class),
            TestInterfaceType.class, propertyPrefix))
        .withMessage("Property prefix [%s] is required", propertyPrefix)
        .withNoCause());
  }

  private ConfigurationPropertiesInterfaceMethodInterceptor newConfigurationPropertiesInterfaceMethodInterceptor() {
    return new ConfigurationPropertiesInterfaceMethodInterceptor(mock(AbstractConfigurationService.class),
      TestInterfaceType.class, "test.properties");
  }

  @Test
  public void interceptWithNullMethodInvocation() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> newConfigurationPropertiesInterfaceMethodInterceptor().intercept(null))
      .withMessage("MethodInvocation is required")
      .withNoCause();
  }

  @Test
  public void interceptNullPropertyAccessorMethod() {

    MethodInvocation mockMethodInvocation = mock(MethodInvocation.class);

    doReturn(null).when(mockMethodInvocation).getMethod();

    assertThatIllegalArgumentException()
      .isThrownBy(() -> newConfigurationPropertiesInterfaceMethodInterceptor().intercept(mockMethodInvocation))
      .withMessage("Property accessor method is required")
      .withNoCause();

    verify(mockMethodInvocation, times(1)).getMethod();
    verifyNoMoreInteractions(mockMethodInvocation);
  }

  @Test
  public void interceptInvalidPropertyAccessorMethod() throws NoSuchMethodException {

    MethodInvocation methodInvocation = MethodInvocation
      .newMethodInvocation(this, TestInterfaceType.class.getDeclaredMethod("invalidProperty"));

    assertThatIllegalArgumentException()
      .isThrownBy(() -> newConfigurationPropertiesInterfaceMethodInterceptor().intercept(methodInvocation))
      .withMessage("Property accessor method name [invalidProperty] must start with [[get, is]]")
      .withNoCause();
  }

  @Test
  public void interceptSetterPropertyAccessorMethod() throws NoSuchMethodException {

    MethodInvocation methodInvocation = MethodInvocation
      .newMethodInvocation(this, TestInterfaceType.class.getDeclaredMethod("setMockProperty", String.class),
        "testValue");

    assertThatIllegalArgumentException()
      .isThrownBy(() -> newConfigurationPropertiesInterfaceMethodInterceptor().intercept(methodInvocation))
      .withMessage("Using property setter methods [setMockProperty] to set properties is not supported")
      .withNoCause();
  }

  @Test
  public void interceptVoidPropertyAccessorMethod() throws NoSuchMethodException {

    MethodInvocation methodInvocation = MethodInvocation
      .newMethodInvocation(this, TestInterfaceType.class.getDeclaredMethod("getVoidProperty"));

    assertThatIllegalArgumentException()
      .isThrownBy(() -> newConfigurationPropertiesInterfaceMethodInterceptor().intercept(methodInvocation))
      .withMessage("Property accessor method [getVoidProperty] must not have a [%s] return type",
        Void.class.getSimpleName())
      .withNoCause();
  }

  static class TestConfigurationService extends AbstractConfigurationService { }

  static class AbstractBaseConfiguration implements Configuration {

    @Override
    public String getPropertyValue(String propertyName, boolean required) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }

    @SuppressWarnings("unused")
    public void setIndex(int index) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }
  }

  static class DefaultOrderTestConfiguration extends AbstractBaseConfiguration { }

  @Order(Ordered.LAST)
  static class FirstOrderTestConfiguration extends AbstractBaseConfiguration implements Orderable<Integer>, Ordered {

    @Override
    public Integer getOrder() {
      return Ordered.FIRST;
    }

    @Override
    public int getIndex() {
      return 1_000_00;
    }
  }

  static class MiddleOrderTestConfiguration extends AbstractBaseConfiguration implements Ordered {

    @Override
    public int getIndex() {
      return Ordered.DEFAULT + 1_000;
    }
  }

  @Order(Ordered.LAST)
  static class LastOrderTestConfiguration extends AbstractBaseConfiguration { }

  static class TestClassType { }

  interface TestInterfaceType {
    void getVoidProperty();
    String invalidProperty();
    void setMockProperty(String value);
  }
}
