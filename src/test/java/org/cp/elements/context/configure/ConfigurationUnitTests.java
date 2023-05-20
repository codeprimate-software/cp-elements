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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.File;
import java.util.Arrays;
import java.util.Properties;
import java.util.function.Supplier;

import org.junit.After;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;

import org.cp.elements.context.annotation.Profile;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.ThrowableAssertions;
import org.cp.elements.security.model.User;
import org.cp.elements.test.annotation.SubjectUnderTest;
import org.cp.elements.util.ArrayUtils;

import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit Tests for {@link Configuration}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.context.configure.Configuration
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ConfigurationUnitTests {

  @Mock
  @SubjectUnderTest
  private Configuration mockConfiguration;

  @After
  public void tearDown() {
    Mockito.reset(this.mockConfiguration);
  }

  @Test
  public void isPresentCallsIsSetByDefault() {

    doCallRealMethod().when(this.mockConfiguration).isPresent(any());
    doReturn(true).when(this.mockConfiguration).isSet(any());

    assertThat(this.mockConfiguration.isPresent("mockProperty")).isTrue();

    verify(this.mockConfiguration, times(1)).isPresent("mockProperty");
    verify(this.mockConfiguration, times(1)).isSet(eq("mockProperty"));
    verifyNoMoreInteractions(this.mockConfiguration);
  }

  @Test
  public void isSetCallsGetPropertyValue() {

    doCallRealMethod().when(this.mockConfiguration).isSet(anyString());

    doReturn("mockValue")
      .when(this.mockConfiguration).getPropertyValue(anyString(), eq(Configuration.NOT_REQUIRED));

    assertThat(this.mockConfiguration.isSet("mockProperty")).isTrue();

    verify(this.mockConfiguration, times(1)).isSet(eq("mockProperty"));
    verify(this.mockConfiguration, times(1))
      .getPropertyValue(eq("mockProperty"), eq(Configuration.NOT_REQUIRED));
    verifyNoMoreInteractions(this.mockConfiguration);
  }

  @Test
  public void isSetWhenPropertyNameIsUndeclared() {

    doCallRealMethod().when(this.mockConfiguration).isSet(any());

    Arrays.stream(ArrayUtils.asArray("  ", "", null)).forEach(propertyName -> {
      assertThat(this.mockConfiguration.isSet(propertyName)).isFalse();
      verify(this.mockConfiguration, times(1)).isSet(eq(propertyName));
    });

    verify(this.mockConfiguration, never()).getPropertyValue(any(), eq(Configuration.NOT_REQUIRED));
    verifyNoMoreInteractions(this.mockConfiguration);
  }

  @Test
  public void isSetWhenPropertyValueIsUndefined() {

    doCallRealMethod().when(this.mockConfiguration).isSet(anyString());

    Arrays.stream(ArrayUtils.asArray("  ", "", null)).forEach(propertyValue -> {
      doReturn(propertyValue).when(this.mockConfiguration).getPropertyValue(anyString(), eq(Configuration.NOT_REQUIRED));
      assertThat(this.mockConfiguration.isSet("mockProperty")).isFalse();
    });

    verify(this.mockConfiguration, times(3)).isSet(eq("mockProperty"));
    verify(this.mockConfiguration, times(3))
      .getPropertyValue(eq("mockProperty"), eq(Configuration.NOT_REQUIRED));
    verifyNoMoreInteractions(this.mockConfiguration);
  }

  @Test
  public void getDescriptorIsNull() {

    doCallRealMethod().when(this.mockConfiguration).getDescriptor();

    assertThat(this.mockConfiguration.getDescriptor()).isNull();

    verify(this.mockConfiguration, times(1)).getDescriptor();
    verifyNoMoreInteractions(this.mockConfiguration);
  }

  @Test
  @SuppressWarnings("all")
  public void getNameReturnsImplementClassName() {

    Configuration configuration = new TestConfiguration();

    assertThat(configuration.getName()).isEqualTo(TestConfiguration.class.getName());
  }

  @Test
  public void getProfilesForConfigurationWithNoProfiles() {

    Configuration mockConfiguration = mock(NonProfiledConfiguration.class);

    doCallRealMethod().when(mockConfiguration).getProfiles();

    String[] profiles = mockConfiguration.getProfiles();

    assertThat(profiles).isNotNull();
    assertThat(profiles).isEmpty();

    verify(mockConfiguration, times(1)).getProfiles();
    verifyNoMoreInteractions(mockConfiguration);
  }

  @Test
  public void getProfilesForConfigurationWithProfiles() {

    Configuration mockConfiguration = mock(ProfiledConfiguration.class);

    doCallRealMethod().when(mockConfiguration).getProfiles();

    String[] profiles = mockConfiguration.getProfiles();

    assertThat(profiles).isNotNull();
    assertThat(profiles).containsExactly("profileOne", "profileTwo");

    verify(mockConfiguration, times(1)).getProfiles();
    verifyNoMoreInteractions(mockConfiguration);
  }

  @Test
  public void getProfilesForConfigurationWithInheritedProfiles() {

    Configuration testConfiguration = new TestConfiguration();

    assertThat(testConfiguration.getProfiles()).isNotNull();
    assertThat(testConfiguration.getProfiles()).containsExactly("mockProfile");
  }

  @Test
  public void getProfilesForConfigurationWithUndeclaredProfiles() {

    Configuration mockConfiguration = mock(UndeclaredProfileConfiguration.class);

    doCallRealMethod().when(mockConfiguration).getProfiles();

    String[] profiles = mockConfiguration.getProfiles();

    assertThat(profiles).isNotNull();
    assertThat(profiles).isEmpty();

    verify(mockConfiguration, times(1)).getProfiles();
    verifyNoMoreInteractions(mockConfiguration);
  }

  @Test
  public void getPropertyValueWithPropertyName() {

    doCallRealMethod().when(this.mockConfiguration).getPropertyValue(anyString());
    doReturn("mockValue").when(this.mockConfiguration).getPropertyValue(anyString(), anyBoolean());

    assertThat(this.mockConfiguration.getPropertyValue("mockProperty")).isEqualTo("mockValue");

    verify(this.mockConfiguration, times(1)).getPropertyValue(eq("mockProperty"));
    verify(this.mockConfiguration, times(1))
      .getPropertyValue(eq("mockProperty"), eq(Configuration.REQUIRED));
    verifyNoMoreInteractions(this.mockConfiguration);
  }

  @Test
  public void getPropertyValueWithDefaultValueReturnsPropertyValue() {

    doCallRealMethod().when(this.mockConfiguration).getPropertyValue(anyString(), ArgumentMatchers.<String>any());
    doReturn("mockValue").when(this.mockConfiguration).getPropertyValue(anyString(), anyBoolean());

    assertThat(this.mockConfiguration.getPropertyValue("mockProperty", (String) null))
      .isEqualTo("mockValue");

    verify(this.mockConfiguration, times(1))
      .getPropertyValue(eq("mockProperty"), ArgumentMatchers.<String>eq(null));
    verify(this.mockConfiguration, times(1))
      .getPropertyValue(eq("mockProperty"), eq(Configuration.NOT_REQUIRED));
    verifyNoMoreInteractions(this.mockConfiguration);
  }

  @Test
  public void getPropertyValueWithDefaultValueReturnsDefaultValue() {

    doCallRealMethod().when(this.mockConfiguration).getPropertyValue(anyString(), ArgumentMatchers.<String>any());

    Arrays.stream(ArrayUtils.asArray("  ", "", null)).forEach(propertyValue -> {
      doReturn(propertyValue).when(this.mockConfiguration).getPropertyValue(anyString(), anyBoolean());
      assertThat(this.mockConfiguration.getPropertyValue("mockProperty", "testValue"))
        .isEqualTo("testValue");
    });

    verify(this.mockConfiguration, times(3))
      .getPropertyValue(eq("mockProperty"), eq("testValue"));
    verify(this.mockConfiguration, times(3))
      .getPropertyValue(eq("mockProperty"), eq(Configuration.NOT_REQUIRED));
    verifyNoMoreInteractions(this.mockConfiguration);
  }

  @Test
  public void getPropertyValueWithSuppliedDefaultValueReturnsPropertyValue() {

    doCallRealMethod().when(this.mockConfiguration).getPropertyValue(anyString(), ArgumentMatchers.<Supplier<String>>any());
    doReturn("mockValue").when(this.mockConfiguration).getPropertyValue(anyString(), anyBoolean());

    assertThat(this.mockConfiguration.getPropertyValue("mockProperty", (Supplier<String>) null))
      .isEqualTo("mockValue");

    verify(this.mockConfiguration, times(1))
      .getPropertyValue(eq("mockProperty"), ArgumentMatchers.<Supplier<String>>eq(null));
    verify(this.mockConfiguration, times(1))
      .getPropertyValue(eq("mockProperty"), eq(Configuration.NOT_REQUIRED));
    verifyNoMoreInteractions(this.mockConfiguration);
  }

  @Test
  public void getPropertyValueWithSuppliedDefaultValueReturnsSuppliedDefaultValue() {

    doCallRealMethod().when(this.mockConfiguration).getPropertyValue(anyString(), ArgumentMatchers.<Supplier<String>>any());

    Arrays.stream(ArrayUtils.asArray("  ", "", null)).forEach(propertyValue -> {
      doReturn(propertyValue).when(this.mockConfiguration).getPropertyValue(anyString(), anyBoolean());
      assertThat(this.mockConfiguration.getPropertyValue("mockProperty", () -> "testValue"))
        .isEqualTo("testValue");
    });

    verify(this.mockConfiguration, times(3))
      .getPropertyValue(eq("mockProperty"), ArgumentMatchers.<Supplier<String>>isNotNull());
    verify(this.mockConfiguration, times(3))
      .getPropertyValue(eq("mockProperty"), eq(Configuration.NOT_REQUIRED));
    verifyNoMoreInteractions(this.mockConfiguration);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void getPropertyValueAsWithPropertyNameAndType() {

    User<?> mockUser = mock(User.class);

    doCallRealMethod().when(this.mockConfiguration).getPropertyValueAs(anyString(), eq(User.class));
    doReturn(mockUser).when(this.mockConfiguration).getPropertyValueAs(anyString(), eq(User.class), anyBoolean());

    assertThat(this.mockConfiguration.getPropertyValueAs("mockProperty", User.class)).isEqualTo(mockUser);

    verify(this.mockConfiguration, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(User.class));
    verify(this.mockConfiguration, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(User.class), eq(Configuration.REQUIRED));
    verifyNoMoreInteractions(this.mockConfiguration);
  }

  @Test
  public void getPropertyValueAsWithPropertyNameAndTypeAndRequired() {

    doCallRealMethod().when(this.mockConfiguration).getPropertyValueAs(anyString(), eq(Integer.class), anyBoolean());
    doReturn("1").when(this.mockConfiguration).getPropertyValue(anyString(), anyBoolean());

    assertThat(this.mockConfiguration.getPropertyValueAs("mockProperty", Integer.class, true))
      .isOne();

    verify(this.mockConfiguration, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(Integer.class), eq(true));
    verify(this.mockConfiguration, times(1))
      .getPropertyValue(eq("mockProperty"), eq(true));
    verifyNoMoreInteractions(this.mockConfiguration);
  }

  @Test
  public void getPropertyValueAsWithPropertyNameAndDefaultValueReturnsPropertyValue() {

    doCallRealMethod()
      .when(this.mockConfiguration).getPropertyValueAs(anyString(), eq(Integer.class), ArgumentMatchers.<Integer>any());

    doReturn(2)
      .when(this.mockConfiguration).getPropertyValueAs(anyString(), eq(Integer.class), anyBoolean());

    assertThat(this.mockConfiguration.getPropertyValueAs("mockProperty", Integer.class, (Integer) null))
      .isEqualTo(2);

    verify(this.mockConfiguration, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(Integer.class), ArgumentMatchers.<Integer>eq(null));
    verify(this.mockConfiguration, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(Integer.class), eq(Configuration.NOT_REQUIRED));
    verifyNoMoreInteractions(this.mockConfiguration);
  }

  @Test
  public void getPropertyValueAsWithPropertyNameAndDefaultValueReturnsDefaultValue() {

    doCallRealMethod().when(this.mockConfiguration).getPropertyValueAs(anyString(), eq(Integer.class), anyInt());

    doReturn(null)
      .when(this.mockConfiguration).getPropertyValueAs(anyString(), eq(Integer.class), anyBoolean());

    assertThat(this.mockConfiguration.getPropertyValueAs("mockProperty", Integer.class, 4))
      .isEqualTo(4);

    verify(this.mockConfiguration, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(Integer.class), eq(4));
    verify(this.mockConfiguration, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(Integer.class), eq(Configuration.NOT_REQUIRED));
    verifyNoMoreInteractions(this.mockConfiguration);
  }

  @Test
  public void getPropertyValueAsWithPropertyNameAndSuppliedDefaultValueReturnsPropertyValue() {

    doCallRealMethod().when(this.mockConfiguration).getPropertyValueAs(anyString(), eq(Integer.class),
      ArgumentMatchers.<Supplier<Integer>>any());

    doReturn(8)
      .when(this.mockConfiguration).getPropertyValueAs(anyString(), eq(Integer.class), anyBoolean());

    assertThat(this.mockConfiguration.getPropertyValueAs("mockProperty", Integer.class,
      (Supplier<Integer>) null)).isEqualTo(8);

    verify(this.mockConfiguration, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(Integer.class),
        ArgumentMatchers.<Supplier<Integer>>eq(null));
    verify(this.mockConfiguration, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(Integer.class), eq(Configuration.NOT_REQUIRED));
    verifyNoMoreInteractions(this.mockConfiguration);
  }

  @Test
  public void getPropertyValueAsWithPropertyNameAndSuppliedDefaultValueReturnsSuppliedDefaultValue() {

    doCallRealMethod().when(this.mockConfiguration).getPropertyValueAs(anyString(), eq(Integer.class),
      ArgumentMatchers.<Supplier<Integer>>any());

    doReturn(null)
      .when(this.mockConfiguration).getPropertyValueAs(anyString(), eq(Integer.class), anyBoolean());

    assertThat(this.mockConfiguration.<Integer>getPropertyValueAs("mockProperty", Integer.class, () -> 16))
      .isEqualTo(16);

    verify(this.mockConfiguration, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(Integer.class), ArgumentMatchers.<Supplier<Integer>>isNotNull());
    verify(this.mockConfiguration, times(1))
      .getPropertyValueAs(eq("mockProperty"), eq(Integer.class), eq(Configuration.NOT_REQUIRED));
    verifyNoMoreInteractions(this.mockConfiguration);
  }

  @Test
  public void iteratorIsUnsupported() {

    doCallRealMethod().when(this.mockConfiguration).iterator();

    ThrowableAssertions.assertThatUnsupportedOperationException()
      .isThrownBy(args -> this.mockConfiguration.iterator())
      .havingMessage(Constants.NOT_IMPLEMENTED)
      .withNoCause();
  }

  @Test
  public void composeConfigurationWithConfiguration() {

    Configuration mockConfigurationTwo = mock(Configuration.class);

    doCallRealMethod().when(this.mockConfiguration).andThen(any());

    Configuration composite = this.mockConfiguration.andThen(mockConfigurationTwo);

    assertThat(composite).isNotNull();
    assertThat(composite).isNotEqualTo(this.mockConfiguration);
    assertThat(composite).isNotEqualTo(mockConfigurationTwo);

    verify(this.mockConfiguration, times(1)).andThen(eq(mockConfigurationTwo));
    verifyNoMoreInteractions(this.mockConfiguration);
    verifyNoInteractions(mockConfigurationTwo);
  }

  @Test
  public void composeConfigurationWithNull() {

    doCallRealMethod().when(this.mockConfiguration).andThen(any());

    assertThat(this.mockConfiguration.andThen(null)).isSameAs(this.mockConfiguration);

    verify(this.mockConfiguration, times(1)).andThen(eq(null));
    verifyNoMoreInteractions(this.mockConfiguration);
  }

  @Test
  public void andThenCallsThisConfiguration() {

    Configuration mockConfigurationTwo = mock(Configuration.class);

    doCallRealMethod().when(this.mockConfiguration).andThen(any());

    doReturn("mockValue")
      .when(this.mockConfiguration).getPropertyValue(anyString(), ArgumentMatchers.<Supplier<String>>isNotNull());

    Configuration composite = this.mockConfiguration.andThen(mockConfigurationTwo);

    assertThat(composite).isNotNull();
    assertThat(composite).isNotEqualTo(this.mockConfiguration);
    assertThat(composite).isNotEqualTo(mockConfigurationTwo);
    assertThat(composite.getPropertyValue("mockProperty")).isEqualTo("mockValue");

    verify(this.mockConfiguration, times(1)).andThen(eq(mockConfigurationTwo));
    verify(this.mockConfiguration, times(1))
      .getPropertyValue(eq("mockProperty"), ArgumentMatchers.<Supplier<String>>isNotNull());
    verifyNoMoreInteractions(this.mockConfiguration);
    verifyNoInteractions(mockConfigurationTwo);
  }

  @Test
  public void andThenCallsPassedConfiguration() {

    Configuration mockConfigurationTwo = mock(Configuration.class);

    doReturn("testValue").when(mockConfigurationTwo).getPropertyValue(anyString(), anyBoolean());

    doCallRealMethod().when(this.mockConfiguration).andThen(any());

    doCallRealMethod()
      .when(this.mockConfiguration).getPropertyValue(anyString(), ArgumentMatchers.<Supplier<String>>isNotNull());

    doReturn(null).when(this.mockConfiguration).getPropertyValue(anyString(), anyBoolean());

    Configuration composite = this.mockConfiguration.andThen(mockConfigurationTwo);

    assertThat(composite).isNotNull();
    assertThat(composite).isNotEqualTo(this.mockConfiguration);
    assertThat(composite).isNotEqualTo(mockConfigurationTwo);
    assertThat(composite.getPropertyValue("mockProperty", false)).isEqualTo("testValue");

    verify(this.mockConfiguration, times(1)).andThen(eq(mockConfigurationTwo));
    verify(this.mockConfiguration, times(1))
      .getPropertyValue(eq("mockProperty"), ArgumentMatchers.<Supplier<String>>isNotNull());
    verify(this.mockConfiguration, times(1))
      .getPropertyValue(eq("mockProperty"), eq(Configuration.NOT_REQUIRED));
    verify(mockConfigurationTwo, times(1))
      .getPropertyValue(eq("mockProperty"), eq(false));
    verifyNoMoreInteractions(this.mockConfiguration, mockConfigurationTwo);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void configurationDescriptorWithFileSource() {

    File mockFile = mock(File.class);

    Configuration.Descriptor<File> configurationDescriptor = mock(Configuration.Descriptor.class);

    doReturn(mockFile).when(configurationDescriptor).getSource();
    doCallRealMethod().when(configurationDescriptor).isFile();
    doCallRealMethod().when(configurationDescriptor).isProperties();

    assertThat(configurationDescriptor.isFile()).isTrue();
    assertThat(configurationDescriptor.isProperties()).isFalse();

    verify(configurationDescriptor, times(2)).getSource();
    verify(configurationDescriptor, times(1)).isFile();
    verify(configurationDescriptor, times(1)).isProperties();
    verifyNoMoreInteractions(configurationDescriptor);
    verifyNoInteractions(mockFile);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void configurationDescriptorWithPropertiesSource() {

    Properties mockProperties = mock(Properties.class);

    Configuration.Descriptor<File> configurationDescriptor = mock(Configuration.Descriptor.class);

    doReturn(mockProperties).when(configurationDescriptor).getSource();
    doCallRealMethod().when(configurationDescriptor).isFile();
    doCallRealMethod().when(configurationDescriptor).isProperties();

    assertThat(configurationDescriptor.isFile()).isFalse();
    assertThat(configurationDescriptor.isProperties()).isTrue();

    verify(configurationDescriptor, times(2)).getSource();
    verify(configurationDescriptor, times(1)).isFile();
    verify(configurationDescriptor, times(1)).isProperties();
    verifyNoMoreInteractions(configurationDescriptor);
    verifyNoInteractions(mockProperties);
  }

  interface NonProfiledConfiguration extends Configuration { }

  @Profile(names = { "profileOne", "profileTwo" })
  interface ProfiledConfiguration extends Configuration { }

  @Profile(names = { "  ", "" })
  interface UndeclaredProfileConfiguration extends Configuration { }

  @SuppressWarnings("all")
  @Profile(names = "mockProfile")
  static class MockConfiguration implements ProfiledConfiguration {

    @Override
    public String getPropertyValue(String propertyName, boolean required) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }
  }

  static class TestConfiguration extends MockConfiguration { }

}
