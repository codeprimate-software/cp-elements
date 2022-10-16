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
import static org.cp.elements.lang.ThrowableAssertions.assertThatThrowableOfType;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Arrays;
import java.util.function.BiFunction;
import java.util.function.Supplier;

import org.junit.Test;

import org.cp.elements.context.annotation.ActiveProfiles;
import org.cp.elements.util.ArrayUtils;

import org.mockito.ArgumentMatchers;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

/**
 * Unit Tests for {@link ConfigurationService}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.context.configure.ConfigurationService
 * @since 1.0.0
 */
public class ConfigurationServiceUnitTests {

  @Test
  public void getLoaderReturnsSingleInstance() {

    ConfigurationService.Loader configurationServiceLoader = ConfigurationService.getLoader();

    assertThat(configurationServiceLoader).isNotNull();
    assertThat(configurationServiceLoader).isSameAs(ConfigurationService.getLoader());
  }

  @Test
  public void isPresentCallsIsSet() {

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod().when(configurationService).isPresent(anyString());
    doReturn(true).when(configurationService).isSet(eq("testProperty"));

    assertThat(configurationService.isPresent("mockProperty")).isFalse();
    assertThat(configurationService.isPresent("testProperty")).isTrue();
    assertThat(configurationService.isPresent("nonExistingProperty")).isFalse();

    verify(configurationService, times(1)).isPresent(eq("mockProperty"));
    verify(configurationService, times(1)).isSet(eq("mockProperty"));
    verify(configurationService, times(1)).isPresent(eq("testProperty"));
    verify(configurationService, times(1)).isSet(eq("testProperty"));
    verify(configurationService, times(1)).isPresent(eq("nonExistingProperty"));
    verify(configurationService, times(1)).isSet(eq("nonExistingProperty"));
    verifyNoMoreInteractions(configurationService);
  }

  @Test
  public void isSetReturnsFalseAfterAllConfigurationEvaluated() {

    Configuration mockConfigurationOne = mock(Configuration.class);
    Configuration mockConfigurationTwo = mock(Configuration.class);

    doReturn(false).when(mockConfigurationOne).isSet(anyString());
    doReturn(false).when(mockConfigurationTwo).isSet(anyString());

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod().when(configurationService).isSet(anyString());
    doReturn(ArrayUtils.asIterable(mockConfigurationOne, mockConfigurationTwo).spliterator())
      .when(configurationService).spliterator();

    assertThat(configurationService.isSet("testProperty")).isFalse();

    verify(configurationService, times(1)).isSet(eq("testProperty"));
    verify(configurationService, times(1)).spliterator();
    verify(mockConfigurationOne, times(1)).isSet(eq("testProperty"));
    verify(mockConfigurationTwo, times(1)).isSet(eq("testProperty"));
    verifyNoMoreInteractions(configurationService, mockConfigurationOne, mockConfigurationTwo);
  }

  @Test
  public void isSetReturnsTrueAfterFirstConfigurationIsEvaluated() {

    Configuration mockConfigurationOne = mock(Configuration.class);
    Configuration mockConfigurationTwo = mock(Configuration.class);

    doReturn(true).when(mockConfigurationOne).isSet(anyString());

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod().when(configurationService).isSet(anyString());
    doReturn(ArrayUtils.asIterable(mockConfigurationOne, mockConfigurationTwo).spliterator())
      .when(configurationService).spliterator();

    assertThat(configurationService.isSet("mockProperty")).isTrue();

    verify(configurationService, times(1)).isSet(eq("mockProperty"));
    verify(configurationService, times(1)).spliterator();
    verify(mockConfigurationOne, times(1)).isSet(eq("mockProperty"));
    verifyNoMoreInteractions(configurationService, mockConfigurationOne);
    verifyNoInteractions(mockConfigurationTwo);
  }

  @Test
  public void isSetReturnsTrueAfterLastConfigurationIsEvaluated() {

    Configuration mockConfigurationOne = mock(Configuration.class);
    Configuration mockConfigurationTwo = mock(Configuration.class);

    doReturn(false).when(mockConfigurationOne).isSet(anyString());
    doReturn(true).when(mockConfigurationTwo).isSet(anyString());

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod().when(configurationService).isSet(anyString());
    doReturn(ArrayUtils.asIterable(mockConfigurationOne, mockConfigurationTwo).spliterator())
      .when(configurationService).spliterator();

    assertThat(configurationService.isSet("mockProperty")).isTrue();

    verify(configurationService, times(1)).isSet(eq("mockProperty"));
    verify(configurationService, times(1)).spliterator();
    verify(mockConfigurationOne, times(1)).isSet(eq("mockProperty"));
    verify(mockConfigurationTwo, times(1)).isSet(eq("mockProperty"));
    verifyNoMoreInteractions(configurationService, mockConfigurationOne, mockConfigurationTwo);
  }

  @Test
  public void isSetReturnsFalseWhenNoConfigurationsArePresent() {

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod().when(configurationService).isSet(any());
    doReturn(ArrayUtils.asIterable().spliterator()).when(configurationService).spliterator();

    assertThat(configurationService.isSet("mockProperty")).isFalse();

    verify(configurationService, times(1)).isSet(eq("mockProperty"));
    verify(configurationService, times(1)).spliterator();
    verifyNoMoreInteractions(configurationService);
  }

  @Test
  public void isSetWithInvalidPropertyNamesReturnsFalse() {

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod().when(configurationService).isSet(any());

    Arrays.stream(ArrayUtils.asArray("  ", "", null)).forEach(propertyName -> {
      assertThat(configurationService.isSet(propertyName)).isFalse();
      verify(configurationService, times(1)).isSet(eq(propertyName));
    });

    verifyNoMoreInteractions(configurationService);
  }

  @Test
  public void getActiveProfilesForNonProfiledConfigurationService() {

    ConfigurationService configurationService = mock(NonProfiledConfigurationService.class);

    doReturn(false).when(configurationService).isPresent(any());
    doCallRealMethod().when(configurationService).getActiveProfiles();

    String[] activeProfiles = configurationService.getActiveProfiles();

    assertThat(activeProfiles).isNotNull();
    assertThat(activeProfiles).isEmpty();

    verify(configurationService, times(1)).getActiveProfiles();
    verify(configurationService, times(1))
      .isPresent(eq(ConfigurationService.ACTIVE_PROFILES_PROPERTY));
    verifyNoMoreInteractions(configurationService);
  }

  @Test
  public void getActiveProfilesForNonProfiledConfigurationServiceWithActiveProfilesPropertySet() {

    ConfigurationService configurationService = mock(NonProfiledConfigurationService.class);

    doCallRealMethod().when(configurationService).getActiveProfiles();
    doReturn(true).when(configurationService).isPresent(any());
    doReturn("DEV, QA").when(configurationService)
      .getPropertyValue(eq(ConfigurationService.ACTIVE_PROFILES_PROPERTY), eq(Configuration.NOT_REQUIRED));

    assertThat(configurationService.getActiveProfiles()).containsExactly("DEV", "QA");

    verify(configurationService, times(1)).getActiveProfiles();
    verify(configurationService, times(1))
      .isPresent(eq(ConfigurationService.ACTIVE_PROFILES_PROPERTY));
    verify(configurationService, times(1))
      .getPropertyValue(eq(ConfigurationService.ACTIVE_PROFILES_PROPERTY), eq(Configuration.NOT_REQUIRED));
    verifyNoMoreInteractions(configurationService);
  }

  @Test
  public void getActiveProfilesForProfiledConfigurationService() {

    ConfigurationService configurationService = mock(ProfiledConfigurationService.class);

    doReturn(false).when(configurationService).isPresent(any());
    doCallRealMethod().when(configurationService).getActiveProfiles();

    assertThat(configurationService.getActiveProfiles()).containsExactly("DEV", "QA", "PROD");

    verify(configurationService, times(1)).getActiveProfiles();
    verify(configurationService, times(1))
      .isPresent(eq(ConfigurationService.ACTIVE_PROFILES_PROPERTY));
    verifyNoMoreInteractions(configurationService);
  }

  @Test
  public void getActiveProfilesForProfiledConfigurationServiceWithActiveProfilesPropertySet() {

    ConfigurationService configurationService = mock(ProfiledConfigurationService.class);

    doCallRealMethod().when(configurationService).getActiveProfiles();
    doReturn(true).when(configurationService).isPresent(any());
    doReturn("STAGING").when(configurationService)
      .getPropertyValue(eq(ConfigurationService.ACTIVE_PROFILES_PROPERTY), eq(Configuration.NOT_REQUIRED));

    assertThat(configurationService.getActiveProfiles()).containsExactly("STAGING");

    verify(configurationService, times(1)).getActiveProfiles();
    verify(configurationService, times(1))
      .isPresent(eq(ConfigurationService.ACTIVE_PROFILES_PROPERTY));
    verify(configurationService, times(1))
      .getPropertyValue(eq(ConfigurationService.ACTIVE_PROFILES_PROPERTY), eq(Configuration.NOT_REQUIRED));
    verifyNoMoreInteractions(configurationService);
  }

  @Test
  public void nonPresentNonSetPropertyHandlerWithUnsetNonRequiredProperty() {

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod().when(configurationService).getNonPresentNonSetPropertyHandler();

    BiFunction<String, Boolean, String> function = configurationService.getNonPresentNonSetPropertyHandler();

    assertThat(function).isNotNull();
    assertThat(function.apply("testProperty", false)).isNull();

    verify(configurationService, times(1)).getNonPresentNonSetPropertyHandler();
    verifyNoMoreInteractions(configurationService);
  }

  @Test
  public void nonPresentNonSetPropertyHandlerWithUnsetRequiredProperty() {

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod().when(configurationService).getNonPresentNonSetPropertyHandler();

    BiFunction<String, Boolean, String> function = configurationService.getNonPresentNonSetPropertyHandler();

    assertThat(function).isNotNull();

    assertThatExceptionOfType(ConfigurationException.class)
      .isThrownBy(() -> function.apply("testProperty", true))
      .withMessage("Property [testProperty] not found")
      .withNoCause();

    verify(configurationService, times(1)).getNonPresentNonSetPropertyHandler();
    verifyNoMoreInteractions(configurationService);
  }

  @Test
  public void getPropertyValueIsCorrect() {

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod().when(configurationService).getPropertyValue(anyString());
    doReturn("test").when(configurationService).getPropertyValue(anyString(), anyBoolean());

    assertThat(configurationService.getPropertyValue("mockProperty")).isEqualTo("test");

    verify(configurationService, times(1)).getPropertyValue(eq("mockProperty"));
    verify(configurationService, times(1))
      .getPropertyValue(eq("mockProperty"), eq(Configuration.REQUIRED));
    verifyNoMoreInteractions(configurationService);
  }

  @Test
  public void getPropertyValueWithRequiredParameterReturnsFirstConfigurationPropertyValue() {

    Configuration mockConfigurationOne = mock(Configuration.class);
    Configuration mockConfigurationTwo = mock(Configuration.class);

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod().when(configurationService).getPropertyValue(anyString(), anyBoolean());

    doReturn(ArrayUtils.asIterable(null, null, mockConfigurationOne, null, mockConfigurationTwo).spliterator())
      .when(configurationService).spliterator();

    doReturn(true).when(mockConfigurationOne).isPresent(anyString());
    doReturn("test").when(mockConfigurationOne).getPropertyValue(anyString(), anyBoolean());

    assertThat(configurationService.getPropertyValue("mockProperty", Configuration.REQUIRED))
      .isEqualTo("test");

    verify(configurationService, times(1))
      .getPropertyValue(eq("mockProperty"), eq(Configuration.REQUIRED));
    verify(configurationService, times(1)).spliterator();
    verify(mockConfigurationOne, times(1)).isPresent(eq("mockProperty"));
    verify(mockConfigurationOne, times(1))
      .getPropertyValue(eq("mockProperty"), eq(Configuration.REQUIRED));
    verifyNoMoreInteractions(configurationService, mockConfigurationOne);
    verifyNoInteractions(mockConfigurationTwo);
  }

  @Test
  public void getPropertyValueWithRequiredParameterReturnsLastConfigurationPropertyValue() {

    Configuration mockConfigurationOne = mock(Configuration.class);
    Configuration mockConfigurationTwo = mock(Configuration.class);

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod().when(configurationService).getPropertyValue(anyString(), anyBoolean());

    doReturn(ArrayUtils.asIterable(null, null, mockConfigurationOne, null, mockConfigurationTwo).spliterator())
      .when(configurationService).spliterator();

    doReturn(false).when(mockConfigurationOne).isPresent(anyString());
    doReturn(true).when(mockConfigurationTwo).isPresent(anyString());
    doReturn("test").when(mockConfigurationTwo).getPropertyValue(anyString(), anyBoolean());

    assertThat(configurationService.getPropertyValue("mockProperty", Configuration.REQUIRED))
      .isEqualTo("test");

    verify(configurationService, times(1))
      .getPropertyValue(eq("mockProperty"), eq(Configuration.REQUIRED));
    verify(configurationService, times(1)).spliterator();
    verify(mockConfigurationOne, times(1)).isPresent(eq("mockProperty"));
    verify(mockConfigurationTwo, times(1)).isPresent(eq("mockProperty"));
    verify(mockConfigurationTwo, times(1))
      .getPropertyValue(eq("mockProperty"), eq(Configuration.REQUIRED));
    verifyNoMoreInteractions(configurationService, mockConfigurationOne, mockConfigurationTwo);
  }

  @Test
  public void getPropertyValueWithRequiredParameterReturnsNull() {

    Configuration mockConfigurationOne = mock(Configuration.class);
    Configuration mockConfigurationTwo = mock(Configuration.class);

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod().when(configurationService).getNonPresentNonSetPropertyHandler();
    doCallRealMethod().when(configurationService).getPropertyValue(anyString(), anyBoolean());

    doReturn(ArrayUtils.asIterable(null, null, mockConfigurationOne, null, mockConfigurationTwo).spliterator())
      .when(configurationService).spliterator();

    doReturn(false).when(mockConfigurationOne).isPresent(anyString());
    doReturn(false).when(mockConfigurationTwo).isPresent(anyString());

    assertThat(configurationService.getPropertyValue("mockProperty", Configuration.NOT_REQUIRED)).isNull();

    verify(configurationService, times(1))
      .getPropertyValue(eq("mockProperty"), eq(Configuration.NOT_REQUIRED));
    verify(configurationService, times(1)).spliterator();
    verify(configurationService, times(1)).getNonPresentNonSetPropertyHandler();
    verify(mockConfigurationOne, times(1)).isPresent(eq("mockProperty"));
    verify(mockConfigurationTwo, times(1)).isPresent(eq("mockProperty"));
    verifyNoMoreInteractions(configurationService, mockConfigurationOne, mockConfigurationTwo);
  }

  @Test
  public void getPropertyValueWithRequiredParameterThrowsConfigurationException() {

    Configuration mockConfigurationOne = mock(Configuration.class);
    Configuration mockConfigurationTwo = mock(Configuration.class);

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod().when(configurationService).getNonPresentNonSetPropertyHandler();
    doCallRealMethod().when(configurationService).getPropertyValue(anyString(), anyBoolean());

    doReturn(ArrayUtils.asIterable(null, null, mockConfigurationOne, null, mockConfigurationTwo).spliterator())
      .when(configurationService).spliterator();

    doReturn(false).when(mockConfigurationOne).isPresent(anyString());
    doReturn(false).when(mockConfigurationTwo).isPresent(anyString());

    assertThatThrowableOfType(ConfigurationException.class)
      .isThrownBy(args -> configurationService.getPropertyValue("mockProperty", Configuration.REQUIRED))
      .havingMessage("Property [mockProperty] not found")
      .withNoCause();

    verify(configurationService, times(1))
      .getPropertyValue(eq("mockProperty"), eq(Configuration.REQUIRED));
    verify(configurationService, times(1)).spliterator();
    verify(configurationService, times(1)).getNonPresentNonSetPropertyHandler();
    verify(mockConfigurationOne, times(1)).isPresent(eq("mockProperty"));
    verify(mockConfigurationTwo, times(1)).isPresent(eq("mockProperty"));
    verifyNoMoreInteractions(configurationService, mockConfigurationOne, mockConfigurationTwo);
  }

  @Test
  public void getPropertyValueWithDefaultValueReturnsPropertyValue() {

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod().when(configurationService).getPropertyValue(anyString(), anyString());
    doReturn("test").when(configurationService).getPropertyValue(anyString(), anyBoolean());

    assertThat(configurationService.getPropertyValue("mockProperty", "mock")).isEqualTo("test");

    verify(configurationService, times(1))
      .getPropertyValue(eq("mockProperty"), eq("mock"));
    verify(configurationService, times(1))
      .getPropertyValue(eq("mockProperty"), eq(Configuration.NOT_REQUIRED));
    verifyNoMoreInteractions(configurationService);
  }

  @Test
  public void getPropertyValueWithDefaultValueReturnsDefaultValue() {

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod().when(configurationService).getPropertyValue(anyString(), anyString());
    doReturn(null).when(configurationService).getPropertyValue(anyString(), anyBoolean());

    assertThat(configurationService.getPropertyValue("mockProperty", "mock")).isEqualTo("mock");

    verify(configurationService, times(1))
      .getPropertyValue(eq("mockProperty"), eq("mock"));
    verify(configurationService, times(1))
      .getPropertyValue(eq("mockProperty"), eq(Configuration.NOT_REQUIRED));
    verifyNoMoreInteractions(configurationService);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void getPropertyValueWithSuppliedDefaultValueReturnsPropertyValue() {

    ConfigurationService configurationService = mock(ConfigurationService.class);

    Supplier<String> mockSupplier = mock(Supplier.class);

    doCallRealMethod().when(configurationService).getPropertyValue(anyString(), ArgumentMatchers.<Supplier<String>>any());
    doReturn("test").when(configurationService).getPropertyValue(anyString(), anyBoolean());

    assertThat(configurationService.getPropertyValue("mockProperty", mockSupplier)).isEqualTo("test");

    verify(configurationService, times(1))
      .getPropertyValue(eq("mockProperty"), eq(mockSupplier));
    verify(configurationService, times(1))
      .getPropertyValue(eq("mockProperty"), eq(Configuration.NOT_REQUIRED));
    verifyNoMoreInteractions(configurationService);
    verifyNoInteractions(mockSupplier);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void getPropertyValueWithSuppliedDefaultValueReturnsSuppliedDefaultValue() {

    ConfigurationService configurationService = mock(ConfigurationService.class);

    Supplier<String> mockSupplier = mock(Supplier.class);

    doCallRealMethod().when(configurationService).getPropertyValue(anyString(), ArgumentMatchers.<Supplier<String>>any());
    doReturn(null).when(configurationService).getPropertyValue(anyString(), anyBoolean());
    doReturn("mock").when(mockSupplier).get();

    assertThat(configurationService.getPropertyValue("mockProperty", mockSupplier)).isEqualTo("mock");

    verify(configurationService, times(1))
      .getPropertyValue(eq("mockProperty"), eq(mockSupplier));
    verify(configurationService, times(1))
      .getPropertyValue(eq("mockProperty"), eq(Configuration.NOT_REQUIRED));
    verify(mockSupplier, times(1)).get();
    verifyNoMoreInteractions(configurationService, mockSupplier);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void getPropertyValueAsIsCorrect() {

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod().when(configurationService).getPropertyValueAs(anyString(), any(Class.class));
    doReturn(User.as("mockUser")).when(configurationService).getPropertyValueAs(anyString(), eq(User.class), anyBoolean());

    assertThat(configurationService.getPropertyValueAs("mockProperty", User.class))
      .isEqualTo(User.as("mockUser"));

    verify(configurationService, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(User.class));
    verify(configurationService, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(User.class), eq(Configuration.REQUIRED));
    verifyNoMoreInteractions(configurationService);
  }

  @Test
  public void getPropertyValueAsWithRequiredParameterReturnsFirstConfigurationPropertyValue() {

    Configuration mockConfigurationOne = mock(Configuration.class);
    Configuration mockConfigurationTwo = mock(Configuration.class);

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod().when(configurationService).getPropertyValueAs(anyString(), eq(User.class), anyBoolean());

    doReturn(ArrayUtils.asIterable(null, null, mockConfigurationOne, null, mockConfigurationTwo).spliterator())
      .when(configurationService).spliterator();

    doReturn(true).when(mockConfigurationOne).isPresent(anyString());

    doReturn(User.as("mockUser"))
      .when(mockConfigurationOne).getPropertyValueAs(anyString(), eq(User.class), anyBoolean());

    assertThat(configurationService.getPropertyValueAs("mockProperty", User.class, Configuration.REQUIRED))
      .isEqualTo(User.as("mockUser"));

    verify(configurationService, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(User.class), eq(Configuration.REQUIRED));
    verify(configurationService, times(1)).spliterator();
    verify(mockConfigurationOne, times(1)).isPresent(eq("mockProperty"));
    verify(mockConfigurationOne, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(User.class), eq(Configuration.REQUIRED));
    verifyNoMoreInteractions(configurationService, mockConfigurationOne);
    verifyNoInteractions(mockConfigurationTwo);
  }

  @Test
  public void getPropertyValueAsWithRequiredParameterReturnsLastConfigurationPropertyValue() {

    Configuration mockConfigurationOne = mock(Configuration.class);
    Configuration mockConfigurationTwo = mock(Configuration.class);

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod().when(configurationService).getPropertyValueAs(anyString(), eq(User.class), anyBoolean());

    doReturn(ArrayUtils.asIterable(null, null, mockConfigurationOne, null, mockConfigurationTwo).spliterator())
      .when(configurationService).spliterator();

    doReturn(false).when(mockConfigurationOne).isPresent(anyString());
    doReturn(true).when(mockConfigurationTwo).isPresent(anyString());

    doReturn(User.as("mockUser"))
      .when(mockConfigurationTwo).getPropertyValueAs(anyString(), eq(User.class), anyBoolean());

    assertThat(configurationService.getPropertyValueAs("mockProperty", User.class, Configuration.REQUIRED))
      .isEqualTo(User.as("mockUser"));

    verify(configurationService, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(User.class), eq(Configuration.REQUIRED));
    verify(configurationService, times(1)).spliterator();
    verify(mockConfigurationOne, times(1)).isPresent(eq("mockProperty"));
    verify(mockConfigurationTwo, times(1)).isPresent(eq("mockProperty"));
    verify(mockConfigurationTwo, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(User.class), eq(Configuration.REQUIRED));
    verifyNoMoreInteractions(configurationService, mockConfigurationOne, mockConfigurationTwo);
  }

  @Test
  public void getPropertyValueAsWithRequiredParameterReturnsNull() {

    Configuration mockConfigurationOne = mock(Configuration.class);
    Configuration mockConfigurationTwo = mock(Configuration.class);

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod().when(configurationService).getNonPresentNonSetPropertyHandler();
    doCallRealMethod().when(configurationService).getPropertyValueAs(anyString(), eq(User.class), anyBoolean());

    doReturn(ArrayUtils.asIterable(null, null, mockConfigurationOne, null, mockConfigurationTwo).spliterator())
      .when(configurationService).spliterator();

    doReturn(false).when(mockConfigurationOne).isPresent(anyString());
    doReturn(false).when(mockConfigurationTwo).isPresent(anyString());

    assertThat(configurationService
      .getPropertyValueAs("mockProperty", User.class, Configuration.NOT_REQUIRED)).isNull();

    verify(configurationService, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(User.class), eq(Configuration.NOT_REQUIRED));
    verify(configurationService, times(1)).spliterator();
    verify(configurationService, times(1)).getNonPresentNonSetPropertyHandler();
    verify(mockConfigurationOne, times(1)).isPresent(eq("mockProperty"));
    verify(mockConfigurationTwo, times(1)).isPresent(eq("mockProperty"));
    verifyNoMoreInteractions(configurationService, mockConfigurationOne, mockConfigurationTwo);
  }

  @Test
  public void getPropertyValueAsWithRequiredParameterThrowsConfigurationException() {

    Configuration mockConfigurationOne = mock(Configuration.class);
    Configuration mockConfigurationTwo = mock(Configuration.class);

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod().when(configurationService).getNonPresentNonSetPropertyHandler();
    doCallRealMethod().when(configurationService).getPropertyValueAs(anyString(), eq(User.class), anyBoolean());

    doReturn(ArrayUtils.asIterable(null, null, mockConfigurationOne, null, mockConfigurationTwo).spliterator())
      .when(configurationService).spliterator();

    doReturn(false).when(mockConfigurationOne).isPresent(anyString());
    doReturn(false).when(mockConfigurationTwo).isPresent(anyString());

    assertThatThrowableOfType(ConfigurationException.class)
      .isThrownBy(args ->configurationService.getPropertyValueAs("mockProperty", User.class, Configuration.REQUIRED))
      .havingMessage("Property [mockProperty] not found")
      .withNoCause();

    verify(configurationService, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(User.class), eq(Configuration.REQUIRED));
    verify(configurationService, times(1)).spliterator();
    verify(configurationService, times(1)).getNonPresentNonSetPropertyHandler();
    verify(mockConfigurationOne, times(1)).isPresent(eq("mockProperty"));
    verify(mockConfigurationTwo, times(1)).isPresent(eq("mockProperty"));
    verifyNoMoreInteractions(configurationService, mockConfigurationOne, mockConfigurationTwo);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void getPropertyValueAsWithDefaultValueReturnsPropertyValue() {

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod()
      .when(configurationService).getPropertyValueAs(anyString(), any(Class.class), isA(User.class));

    doReturn(User.as("jonDoe"))
      .when(configurationService).getPropertyValueAs(anyString(), any(Class.class), anyBoolean());

    assertThat(configurationService.getPropertyValueAs("mockProperty", User.class, User.as("mockUser")))
      .isEqualTo(User.as("jonDoe"));

    verify(configurationService, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(User.class), eq(User.as("mockUser")));
    verify(configurationService, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(User.class), eq(Configuration.NOT_REQUIRED));
    verifyNoMoreInteractions(configurationService);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void getPropertyValueAsWithDefaultValueReturnsDefaultValue() {

    ConfigurationService configurationService = mock(ConfigurationService.class);

    doCallRealMethod()
      .when(configurationService).getPropertyValueAs(anyString(), any(Class.class), isA(User.class));

    doReturn(null)
      .when(configurationService).getPropertyValueAs(anyString(), any(Class.class), anyBoolean());

    assertThat(configurationService.getPropertyValueAs("mockProperty", User.class, User.as("mockUser")))
      .isEqualTo(User.as("mockUser"));

    verify(configurationService, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(User.class), eq(User.as("mockUser")));
    verify(configurationService, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(User.class), eq(Configuration.NOT_REQUIRED));
    verifyNoMoreInteractions(configurationService);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void getPropertyValueAsWithSuppliedDefaultValueReturnsPropertyValue() {

    ConfigurationService configurationService = mock(ConfigurationService.class);

    Supplier<User> mockSupplier = mock(Supplier.class);

    doCallRealMethod()
      .when(configurationService).getPropertyValueAs(anyString(), eq(User.class), ArgumentMatchers.<Supplier<User>>any());

    doReturn(User.as("janeDoe"))
      .when(configurationService).getPropertyValueAs(anyString(), eq(User.class), anyBoolean());

    assertThat(configurationService.getPropertyValueAs("mockProperty", User.class, mockSupplier))
      .isEqualTo(User.as("janeDoe"));

    verify(configurationService, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(User.class), eq(mockSupplier));
    verify(configurationService, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(User.class), eq(Configuration.NOT_REQUIRED));
    verifyNoMoreInteractions(configurationService);
    verifyNoInteractions(mockSupplier);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void getPropertyValueAsWithSuppliedDefaultValueReturnsDefaultValue() {

    ConfigurationService configurationService = mock(ConfigurationService.class);

    Supplier<User> mockSupplier = mock(Supplier.class);

    doCallRealMethod()
      .when(configurationService).getPropertyValueAs(anyString(), eq(User.class), ArgumentMatchers.<Supplier<User>>any());

    doReturn(null)
      .when(configurationService).getPropertyValueAs(anyString(), eq(User.class), anyBoolean());

    doReturn(User.as("mockUser")).when(mockSupplier).get();

    assertThat(configurationService.getPropertyValueAs("mockProperty", User.class, mockSupplier))
      .isEqualTo(User.as("mockUser"));

    verify(configurationService, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(User.class), eq(mockSupplier));
    verify(configurationService, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(User.class), eq(Configuration.NOT_REQUIRED));
    verify(mockSupplier, times(1)).get();
    verifyNoMoreInteractions(configurationService, mockSupplier);
  }

  @ActiveProfiles(names = {})
  interface NonProfiledConfigurationService extends ConfigurationService { }

  @ActiveProfiles(names = { "  ", "DEV", "QA", "", "PROD" })
  interface ProfiledConfigurationService extends ConfigurationService { }

  @Getter
  @ToString(of = "name")
  @EqualsAndHashCode(of = "name")
  @RequiredArgsConstructor(staticName = "as")
  static class User {
    @lombok.NonNull
    private final String name;
  }
}
