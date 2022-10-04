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
package org.cp.elements.context.env;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.time.Instant;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.Properties;
import java.util.function.Supplier;

import org.junit.Test;

import org.cp.elements.io.FileUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.Version;
import org.cp.elements.security.model.User;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.MapBuilder;
import org.cp.elements.util.PropertiesAdapter;
import org.cp.elements.util.PropertiesBuilder;
import org.cp.elements.util.PropertiesUtils;

import org.mockito.ArgumentMatchers;

/**
 * Unit Tests for {@link Environment}.
 *
 * @author John Blum
 * @see java.util.Map
 * @see java.util.Properties
 * @see org.junit.Test
 * @see org.cp.elements.context.env.Environment
 * @since 1.0.0
 */
public class EnvironmentUnitTests {

  @Test
  public void fromAssociativeArray() {

    String[] associativeArray = { "one=1", "two=2" };

    Environment environment = Environment.from(associativeArray);

    assertThat(environment).isNotNull();
    assertThat(environment).hasSameSizeAs(associativeArray);
    assertThat(environment.getVariable("one")).isEqualTo("1");
    assertThat(environment.getVariable("two")).isEqualTo("2");
  }

  @Test
  public void fromEmptyAssociativeArray() {

    Environment environment = Environment.from(new String[0]);

    assertThat(environment).isNotNull();
    assertThat(environment).isEmpty();
  }

  @Test
  public void fromNullAssociativeArrayIsNullSafe() {

    Environment environment = Environment.from((String[]) null);

    assertThat(environment).isNotNull();
    assertThat(environment).isEmpty();
  }

  @Test
  public void fromSingletonAssociativeArray() {

    Environment environment = Environment.from(new String[] { "one=1" });

    assertThat(environment).isNotNull();
    assertThat(environment).hasSize(1);
    assertThat(environment.getVariable("one")).isEqualTo("1");
  }

  @Test
  public void fromMap() {

    Map<String, String> map = MapBuilder.<String, String>newHashMap()
      .put("one", "1")
      .put("two", "2")
      .build();

    Environment environment = Environment.from(map);

    assertThat(environment).isNotNull();
    assertThat(environment).hasSize(map.size());

    for (String key : map.keySet()) {
      assertThat(environment.getVariable(key)).isEqualTo(map.get(key));
    }
  }

  @Test
  public void fromEmptyMap() {

    Environment environment = Environment.from(Collections.emptyMap());

    assertThat(environment).isNotNull();
    assertThat(environment).isEmpty();
  }

  @Test
  public void fromNullMapThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Environment.from((Map<String, String>) null))
      .withMessage("Map is required")
      .withNoCause();
  }

  @Test
  public void fromSingletonMap() {

    Environment environment = Environment.from(Collections.singletonMap("one", "1"));

    assertThat(environment).isNotNull();
    assertThat(environment).hasSize(1);
    assertThat(environment.getVariable("one")).isEqualTo("1");
  }

  @Test
  public void fromProperties() {

    Properties properties = PropertiesBuilder.newInstance()
      .set("one", "1")
      .set("two", "2")
      .build();

    Environment environment = Environment.from(properties);

    assertThat(environment).isNotNull();
    assertThat(environment).hasSize(properties.size());

    for (String key : properties.stringPropertyNames()) {
      assertThat(environment.getVariable(key)).isEqualTo(properties.get(key));
    }
  }

  @Test
  public void fromEmptyProperties() {

    Environment environment = Environment.from(new Properties());

    assertThat(environment).isNotNull();
    assertThat(environment).isEmpty();
  }

  @Test
  public void fromNullPropertiesThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Environment.from((Properties) null))
      .withMessage("The Properties to adapt is required")
      .withNoCause();
  }

  @Test
  public void fromSingleProperty() {

    Environment environment = Environment.from(PropertiesUtils.singletonProperties("one", "1"));

    assertThat(environment).isNotNull();
    assertThat(environment).hasSize(1);
    assertThat(environment.getVariable("one")).isEqualTo("1");
  }

  @Test
  public void fromEnvironmentVariables() {

    Environment environment = Environment.fromEnvironmentVariables();

    assertThat(environment).isNotNull();
    assertThat(environment.isEmpty()).isFalse();
    assertThat(environment.getVariable("HOME")).isEqualTo(System.getProperty("user.home"));
    assertThat(environment.getVariable("PATH")).isEqualTo(System.getenv().get("PATH"));
    assertThat(environment.getVariable("USER")).isEqualTo(System.getProperty("user.name"));
  }

  @Test
  public void constructsEnvironment() {

    Environment environment = new Environment(PropertiesAdapter.empty());

    assertThat(environment).isNotNull();
    assertThat(environment).isEmpty();
    assertThat(environment.environmentVariables()).isInstanceOf(PropertiesAdapter.class);
    assertThat(environment.systemProperties()).isInstanceOf(PropertiesAdapter.class);
  }

  @Test
  public void constructsEnvironmentWithNull() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new Environment(null))
      .withMessage("An initial environment is required")
      .withNoCause();
  }

  @Test
  public void isEmptyIsTrue() {
    assertThat(Environment.from(Collections.emptyMap()).isEmpty()).isTrue();
  }

  @Test
  public void isEmptyIsFalse() {
    assertThat(Environment.from(PropertiesUtils.singletonProperties("one", "1")).isEmpty())
      .isFalse();
  }

  @Test
  public void isPresentIsSetGetAndGetAsTypeAreAllCorrect() {

    Map<String, String> map = MapBuilder.<String, String>newHashMap()
      .put("booleanProperty", "true")
      .put("characterProperty", "X")
      .put("doubleProperty", "3.14159")
      .put("integerProperty", "2")
      .put("stringProperty", "string")
      .put("unsetProperty", null)
      .build();

    Environment environment = Environment.from(map);

    assertThat(environment).isNotNull();
    assertThat(environment).hasSize(map.size());
    assertThat(environment.isPresent("booleanProperty")).isTrue();
    assertThat(environment.isSet("booleanProperty")).isTrue();
    assertThat(environment.getVariable("booleanProperty")).isEqualTo("true");
    assertThat(environment.getVariableAsType("booleanProperty", Boolean.class)).isTrue();
    assertThat(environment.isPresent("characterProperty")).isTrue();
    assertThat(environment.isSet("characterProperty")).isTrue();
    assertThat(environment.getVariable("characterProperty")).isEqualTo("X");
    assertThat(environment.getVariableAsType("characterProperty", Character.class)).isEqualTo('X');
    assertThat(environment.isPresent("doubleProperty")).isTrue();
    assertThat(environment.isSet("doubleProperty")).isTrue();
    assertThat(environment.getVariable("doubleProperty")).isEqualTo("3.14159");
    assertThat(environment.getVariableAsType("doubleProperty", Double.class)).isEqualTo(3.14159d);
    assertThat(environment.isPresent("integerProperty")).isTrue();
    assertThat(environment.isSet("integerProperty")).isTrue();
    assertThat(environment.getVariable("integerProperty")).isEqualTo("2");
    assertThat(environment.getVariableAsType("integerProperty", Integer.class)).isEqualTo(2);
    assertThat(environment.isPresent("stringProperty")).isTrue();
    assertThat(environment.isSet("stringProperty")).isTrue();
    assertThat(environment.getVariable("stringProperty")).isEqualTo("string");
    assertThat(environment.getVariableAsType("stringProperty", String.class)).isEqualTo("string");
    assertThat(environment.isPresent("unsetProperty")).isTrue();
    assertThat(environment.isSet("unsetProperty")).isFalse();
    assertThat(environment.getVariable("unsetProperty")).isNull();
    assertThat(environment.getVariableAsType("unsetProperty", String.class)).isNull();
  }

  @Test
  public void copyToMap() {

    Map<String, String> source = MapBuilder.<String, String>newHashMap()
      .put("one", "1")
      .put("two", "2")
      .put("three", "3")
      .build();

    Map<String, String> target = MapBuilder.<String, String>newHashMap()
      .put("one", "1")
      .put("two", "-2")
      .put("four", "4")
      .build();

    Environment environment = Environment.from(source);

    assertThat(environment).isNotNull();
    assertThat(environment).hasSize(source.size());

    Map<String, String> targetCopy = environment.copyTo(target);

    assertThat(targetCopy).isSameAs(target);
    assertThat(targetCopy).isNotSameAs(source);
    assertThat(targetCopy).hasSize(4);
    assertThat(targetCopy.get("one")).isEqualTo("1");
    assertThat(targetCopy.get("two")).isEqualTo("2");
    assertThat(targetCopy.get("three")).isEqualTo("3");
    assertThat(targetCopy.get("four")).isEqualTo("4");
  }

  @Test
  public void copyToNullMap() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Environment.fromEnvironmentVariables().copyTo((Map<String, String>) null))
      .withMessage("The Map to copy to is required")
      .withNoCause();
  }

  @Test
  public void copyToProperties() {

    Properties source = PropertiesBuilder.newInstance()
      .set("one", "1")
      .set("two", "2")
      .set("three", "3")
      .build();

    Properties target = PropertiesBuilder.newInstance()
      .set("one", "1")
      .set("two", "-2")
      .set("four", "4")
      .build();

    Environment environment = Environment.from(source);

    assertThat(environment).isNotNull();
    assertThat(environment).hasSize(source.size());

    Properties targetCopy = environment.copyTo(target);

    assertThat(targetCopy).isSameAs(target);
    assertThat(targetCopy).isNotSameAs(source);
    assertThat(targetCopy).hasSize(4);
    assertThat(targetCopy.getProperty("one")).isEqualTo("1");
    assertThat(targetCopy.getProperty("two")).isEqualTo("2");
    assertThat(targetCopy.getProperty("three")).isEqualTo("3");
    assertThat(targetCopy.getProperty("four")).isEqualTo("4");
  }

  @Test
  public void copyToNullProperties() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Environment.fromEnvironmentVariables().copyTo((Properties) null))
      .withMessage("The Properties object to copy to is required")
      .withNoCause();
  }

  @Test
  public void environmentGetPropertyDetectsChangesToSystemProperties() {

    Environment environment = Environment.fromEnvironmentVariables();

    assertThat(environment).isNotNull();
    assertThat(environment.getProperty("mock.system.property")).isNull();

    try {
      System.setProperty("mock.system.property", "test");

      assertThat(environment.getProperty("mock.system.property")).isEqualTo("test");
    }
    finally {
      System.clearProperty("mock.system.property");
    }
  }

  // NOTE: The Map returned by System.getenv() is unmodifiable.
  // See: https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#getenv--
  @Test(expected = UnsupportedOperationException.class)
  public void environmentGetVariableDetectsChangesToSystemEnvironmentVariables() {

    Environment environment = Environment.fromEnvironmentVariables();

    assertThat(environment).isNotNull();
    assertThat(environment.getVariable("mock.system.environment.variable")).isNull();

    try {
      System.getenv().put("mock.system.environment.variable", "test");

      assertThat(environment.getVariable("mock.system.environment.variable")).isEqualTo("test");
    }
    finally {
      System.getenv().remove("mock.system.environment.variable");
    }
  }

  @Test
  public void getPropertyCallsSystemPropertiesGetWithPropertyName() {

    PropertiesAdapter mockSystemProperties = mock(PropertiesAdapter.class);

    Environment environment = spy(Environment.fromEnvironmentVariables());

    doReturn(mockSystemProperties).when(environment).systemProperties();
    doReturn("test").when(mockSystemProperties).get(anyString());

    assertThat(environment.getProperty("mockProperty")).isEqualTo("test");

    verify(environment, times(1)).getProperty(eq("mockProperty"));
    verify(environment, times(1)).systemProperties();
    verify(mockSystemProperties, times(1)).get(eq("mockProperty"));
    verifyNoMoreInteractions(mockSystemProperties, environment);
  }

  @Test
  public void getPropertyWithDefaultValueCallsSystemPropertiesGetWithPropertyNameAndDefaultValue() {

    PropertiesAdapter mockSystemProperties = mock(PropertiesAdapter.class);

    Environment environment = spy(Environment.fromEnvironmentVariables());

    doReturn(mockSystemProperties).when(environment).systemProperties();
    doReturn("test").when(mockSystemProperties).get(anyString(), ArgumentMatchers.<String>any());

    assertThat(environment.getProperty("mockProperty", "DEFAULT")).isEqualTo("test");

    verify(environment, times(1)).getProperty(eq("mockProperty"), eq("DEFAULT"));
    verify(environment, times(1)).systemProperties();
    verify(mockSystemProperties, times(1)).get(eq("mockProperty"), eq("DEFAULT"));
    verifyNoMoreInteractions(mockSystemProperties, environment);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void getPropertyWithSuppliedDefaultValueCallsSystemPropertiesGetWithPropertyNameAndSuppliedDefaultValue() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    PropertiesAdapter mockSystemProperties = mock(PropertiesAdapter.class);

    Environment environment = spy(Environment.fromEnvironmentVariables());

    doReturn(mockSystemProperties).when(environment).systemProperties();
    doReturn("test").when(mockSystemProperties).get(anyString(), any(Supplier.class));

    assertThat(environment.getProperty("mockProperty", mockSupplier)).isEqualTo("test");

    verify(environment, times(1)).getProperty(eq("mockProperty"), eq(mockSupplier));
    verify(environment, times(1)).systemProperties();
    verify(mockSystemProperties, times(1)).get(eq("mockProperty"), eq(mockSupplier));
    verifyNoMoreInteractions(mockSystemProperties, environment);
  }

  @Test
  public void getPropertyAsTypeCallsSystemPropertiesGetPropertyNameAndClassType() {

    PropertiesAdapter mockSystemProperties = mock(PropertiesAdapter.class);

    Environment environment = spy(Environment.fromEnvironmentVariables());

    doReturn(mockSystemProperties).when(environment).systemProperties();
    doReturn(true).when(mockSystemProperties).getAsType(anyString(), eq(Boolean.class));

    assertThat(environment.getPropertyAsType("mockProperty", Boolean.class)).isTrue();

    verify(environment, times(1)).getPropertyAsType(eq("mockProperty"), eq(Boolean.class));
    verify(environment, times(1)).systemProperties();
    verify(mockSystemProperties, times(1)).getAsType(eq("mockProperty"), eq(Boolean.class));
    verifyNoMoreInteractions(environment, mockSystemProperties);
  }

  @Test
  public void getPropertyAsTypeWihDefaultValueCallsSystemPropertiesGetPropertyNameClassTypeAndDefaultValue() {

    PropertiesAdapter mockSystemProperties = mock(PropertiesAdapter.class);

    Environment environment = spy(Environment.fromEnvironmentVariables());

    doReturn(mockSystemProperties).when(environment).systemProperties();
    doReturn(2).when(mockSystemProperties).getAsType(anyString(), eq(Integer.class), any(Integer.class));

    assertThat(environment.getPropertyAsType("mockProperty", Integer.class, 4)).isEqualTo(2);

    verify(environment, times(1))
      .getPropertyAsType(eq("mockProperty"), eq(Integer.class), eq(4));

    verify(environment, times(1)).systemProperties();

    verify(mockSystemProperties, times(1))
      .getAsType(eq("mockProperty"), eq(Integer.class), eq(4));

    verifyNoMoreInteractions(environment, mockSystemProperties);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void getPropertyAsTypeWihSuppliedDefaultValueCallsSystemPropertiesGetPropertyNameClassTypeAndSuppliedDefaultValue() {

    Instant now = Instant.now();

    Supplier<Instant> mockSupplier = mock(Supplier.class);

    PropertiesAdapter mockSystemProperties = mock(PropertiesAdapter.class);

    Environment environment = spy(Environment.fromEnvironmentVariables());

    doReturn(mockSystemProperties).when(environment).systemProperties();
    doReturn(now).when(mockSystemProperties).getAsType(anyString(), eq(Instant.class), any(Supplier.class));

    assertThat(environment.getPropertyAsType("mockProperty", Instant.class, mockSupplier)).isEqualTo(now);

    verify(environment, times(1))
      .getPropertyAsType(eq("mockProperty"), eq(Instant.class), eq(mockSupplier));

    verify(environment, times(1)).systemProperties();

    verify(mockSystemProperties, times(1))
      .getAsType(eq("mockProperty"), eq(Instant.class), eq(mockSupplier));

    verifyNoMoreInteractions(environment, mockSystemProperties);
  }

  @Test
  public void getVariableCallsEnvironmentVariablesGetWithPropertyName() {

    PropertiesAdapter mockEnvironmentVariables = mock(PropertiesAdapter.class);

    Environment environment = spy(Environment.fromEnvironmentVariables());

    doReturn(mockEnvironmentVariables).when(environment).environmentVariables();
    doReturn("test").when(mockEnvironmentVariables).get(anyString());

    assertThat(environment.getVariable("mockVariable")).isEqualTo("test");

    verify(environment, times(1)).getVariable(eq("mockVariable"));
    verify(environment, times(1)).environmentVariables();
    verify(mockEnvironmentVariables, times(1)).get(eq("mockVariable"));
    verifyNoMoreInteractions(environment, mockEnvironmentVariables);
  }

  @Test
  public void getVariableWithDefaultValueCallsEnvironmentVariablesGetWithPropertyNameAndDefaultValue() {

    PropertiesAdapter mockEnvironmentVariables = mock(PropertiesAdapter.class);

    Environment environment = spy(Environment.fromEnvironmentVariables());

    doReturn(mockEnvironmentVariables).when(environment).environmentVariables();
    doReturn("test").when(mockEnvironmentVariables).get(anyString(), ArgumentMatchers.<String>any());

    assertThat(environment.getVariable("mockVariable", "DEFAULT")).isEqualTo("test");

    verify(environment, times(1)).getVariable(eq("mockVariable"), eq("DEFAULT"));
    verify(environment, times(1)).environmentVariables();
    verify(mockEnvironmentVariables, times(1)).get(eq("mockVariable"), eq("DEFAULT"));
    verifyNoMoreInteractions(environment, mockEnvironmentVariables);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void getVariableWithSuppliedDefaultValueCallsEnvironmentVariablesGetWithPropertyNameAndSuppliedDefaultValue() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    PropertiesAdapter mockEnvironmentVariables = mock(PropertiesAdapter.class);

    Environment environment = spy(Environment.fromEnvironmentVariables());

    doReturn(mockEnvironmentVariables).when(environment).environmentVariables();
    doReturn("test").when(mockEnvironmentVariables).get(anyString(), any(Supplier.class));

    assertThat(environment.getVariable("mockVariable", mockSupplier)).isEqualTo("test");

    verify(environment, times(1)).getVariable(eq("mockVariable"), eq(mockSupplier));
    verify(environment, times(1)).environmentVariables();
    verify(mockEnvironmentVariables, times(1)).get(eq("mockVariable"), eq(mockSupplier));
    verifyNoMoreInteractions(environment, mockEnvironmentVariables);
  }

  @Test
  public void getEnvironmentVariableAsTypeCallsEnvironmentVariablesGetPropertyNameAndClassType() {

    PropertiesAdapter mockEnvironmentVariables = mock(PropertiesAdapter.class);

    Environment environment = spy(Environment.fromEnvironmentVariables());

    doReturn(mockEnvironmentVariables).when(environment).environmentVariables();
    doReturn(false).when(mockEnvironmentVariables).getAsType(anyString(), eq(Boolean.class));

    assertThat(environment.getVariableAsType("mockVariable", Boolean.class)).isFalse();

    verify(environment, times(1)).getVariableAsType(eq("mockVariable"), eq(Boolean.class));
    verify(environment, times(1)).environmentVariables();
    verify(mockEnvironmentVariables, times(1)).getAsType(eq("mockVariable"), eq(Boolean.class));
    verifyNoMoreInteractions(environment, mockEnvironmentVariables);
  }

  @Test
  public void getEnvironmentVariableAsTypeWithDefaultValueCallsEnvironmentVariablesGetPropertyNameClassTypeAndDefaultValue() {

    PropertiesAdapter mockEnvironmentVariables = mock(PropertiesAdapter.class);

    Environment environment = spy(Environment.fromEnvironmentVariables());

    doReturn(mockEnvironmentVariables).when(environment).environmentVariables();
    doReturn(Math.PI).when(mockEnvironmentVariables).getAsType(anyString(), eq(Double.class), any(Double.class));

    assertThat(environment.getVariableAsType("mockVariable", Double.class, 3.143159d))
      .isEqualTo(Math.PI);

    verify(environment, times(1))
      .getVariableAsType(eq("mockVariable"), eq(Double.class), eq(3.143159d));

    verify(environment, times(1)).environmentVariables();

    verify(mockEnvironmentVariables, times(1))
      .getAsType(eq("mockVariable"), eq(Double.class), eq(3.143159d));

    verifyNoMoreInteractions(environment, mockEnvironmentVariables);
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void getEnvironmentVariableAsTypeWithSuppliedDefaultValueCallsEnvironmentVariablesGetPropertyNameClassTypeAndSuppliedDefaultValue() {

    User mockUser = mock(User.class);

    Supplier<User> mockSupplier = mock(Supplier.class);

    PropertiesAdapter mockEnvironmentVariables = mock(PropertiesAdapter.class);

    Environment environment = spy(Environment.fromEnvironmentVariables());

    doReturn(mockEnvironmentVariables).when(environment).environmentVariables();
    doReturn(mockUser).when(mockEnvironmentVariables).getAsType(anyString(), eq(User.class), any(Supplier.class));

    assertThat(environment.getVariableAsType("mockVariable", User.class, mockSupplier))
      .isEqualTo(mockUser);

    verify(environment, times(1))
      .getVariableAsType(eq("mockVariable"), eq(User.class), eq(mockSupplier));

    verify(environment, times(1)).environmentVariables();

    verify(mockEnvironmentVariables, times(1))
      .getAsType(eq("mockVariable"), eq(User.class), eq(mockSupplier));

    verifyNoMoreInteractions(environment, mockEnvironmentVariables);
  }

  @Test
  public void getJavaClassPathMatchesSystemProperty() {
    assertThat(Environment.fromEnvironmentVariables().getJavaClassPath())
      .isEqualTo(System.getProperty("java.class.path"));
  }

  @Test
  public void getJavaHomeMatchesSystemProperty() {
    assertThat(Environment.fromEnvironmentVariables().getJavaHome())
      .isEqualTo(FileUtils.newFile(System.getProperty("java.home")));
  }

  @Test
  public void getJavaLibraryPathMatchesSystemProperty() {
    assertThat(Environment.fromEnvironmentVariables().getJavaLibraryPath())
      .isEqualTo(System.getProperty("java.library.path"));
  }

  @Test
  public void getJavaVendorMatchesSystemProperty() {
    assertThat(Environment.fromEnvironmentVariables().getJavaVendor()).isEqualTo(System.getProperty("java.vendor"));
  }

  @Test
  public void getJavaVersionMatchesSystemProperty() {
    assertThat(Environment.fromEnvironmentVariables().getJavaVersion())
      .isEqualTo(Version.parse(System.getProperty("java.version")));
  }

  @Test
  public void getJavaJvmNameMatchesSystemProperty() {
    assertThat(Environment.fromEnvironmentVariables().getJvmName()).isEqualTo(System.getProperty("java.vm.name"));
  }

  @Test
  public void getJavaJvmVendorMatchesSystemProperty() {
    assertThat(Environment.fromEnvironmentVariables().getJvmVendor()).isEqualTo(System.getProperty("java.vm.vendor"));
  }

  @Test
  public void getJavaJvmVersionMatchesSystemProperty() {
    assertThat(Environment.fromEnvironmentVariables().getJvmVersion())
      .isEqualTo(Version.parse(System.getProperty("java.vm.version")));
  }

  @Test
  public void getPathSeparatorMatchesSystemProperty() {
    assertThat(Environment.fromEnvironmentVariables().getPathSeparator())
      .isEqualTo(System.getProperty("path.separator"));
  }

  @Test
  public void getOperatingSystemArchitectureMatchesSystemProperty() {
    assertThat(Environment.fromEnvironmentVariables().getOperatingSystemArchitecture())
      .isEqualTo(System.getProperty("os.arch"));
  }

  @Test
  public void getOperatingSystemNameMatchesSystemProperty() {
    assertThat(Environment.fromEnvironmentVariables().getOperatingSystemName())
      .isEqualTo(System.getProperty("os.name"));
  }

  @Test
  public void getOperatingSystemVersionMatchesSystemProperty() {
    assertThat(Environment.fromEnvironmentVariables().getOperatingSystemVersion())
      .isEqualTo(Version.parse(System.getProperty("os.version")));
  }

  @Test
  public void getSystemPathMatchesEnvironmentPath() {
    assertThat(Environment.fromEnvironmentVariables().getSystemPath()).isEqualTo(System.getenv().get("PATH"));
  }

  @Test
  public void getUserDirectoryMatchesSystemProperty() {
    assertThat(Environment.fromEnvironmentVariables().getUserDirectory())
      .isEqualTo(FileUtils.newFile(System.getProperty("user.dir")));
  }

  @Test
  public void getUserHomeMatchesSystemProperty() {
    assertThat(Environment.fromEnvironmentVariables().getUserHome())
      .isEqualTo(FileUtils.newFile(System.getProperty("user.home")));
  }

  @Test
  public void getUserNameMatchesSystemProperty() {
    assertThat(Environment.fromEnvironmentVariables().getUserName()).isEqualTo(System.getProperty("user.name"));
  }

  @Test
  public void iterationIsSuccessful() {

    Properties properties = PropertiesBuilder.newInstance()
      .set("one", "1")
      .set("two", "2")
      .set("three", "3")
      .build();

    Environment environment = Environment.from(properties);

    assertThat(environment).isNotNull();
    assertThat(environment).hasSize(properties.size());
    assertThat(CollectionUtils.asSet(environment)).containsAll(ArrayUtils.asIterable("one", "two", "three"));
  }

  @Test
  public void sizeIsZero() {
    assertThat(Environment.from(Collections.emptyMap())).hasSize(0);
  }

  @Test
  public void sizeIsOne() {
    assertThat(Environment.from(PropertiesUtils.singletonProperties("one", "1"))).hasSize(1);
  }

  @Test
  public void sizeIsMany() {
    assertThat(Environment.fromEnvironmentVariables()).hasSizeGreaterThan(1);
  }

  @Test
  public void equalEnvironmentsIsTrue() {

    Environment environmentOne = Environment.from(Collections.singletonMap("one", "1"));

    Environment environmentTwo =
      Environment.from(PropertiesUtils.singletonProperties("one", "1"));

    assertThat(environmentOne).isNotNull();
    assertThat(environmentTwo).isNotNull();
    assertThat(environmentOne).isNotSameAs(environmentTwo);
    assertThat(environmentOne.equals(environmentTwo)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void equalsItselfIsTrue() {

    Environment environment = Environment.fromEnvironmentVariables();

    assertThat(environment).isNotNull();
    assertThat(environment.equals(environment)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void equalsNullIsFalse() {
    assertThat(Environment.fromEnvironmentVariables().equals(null)).isFalse();
  }

  @Test
  public void equalsObjectIsFalse() {
    assertThat(Environment.fromEnvironmentVariables().equals(new Object())).isFalse();
  }

  @Test
  public void hashCodeIsCorrect() {

    Environment environmentArray = Environment.from(new String[] { "key=test" });
    Environment environmentMap = Environment.from(Collections.singletonMap("key", "test"));
    Environment environmentProperties =
      Environment.from(PropertiesUtils.singletonProperties("key", "test"));
    Environment environmentVariables = Environment.fromEnvironmentVariables();

    assertThat(environmentArray.hashCode()).isNotZero();
    assertThat(environmentMap.hashCode()).isNotZero();
    assertThat(environmentProperties.hashCode()).isNotZero();
    assertThat(environmentVariables.hashCode()).isNotZero();
    assertThat(environmentArray.hashCode()).isEqualTo(environmentMap.hashCode());
    assertThat(environmentMap.hashCode()).isEqualTo(environmentProperties.hashCode());
    assertThat(environmentProperties.hashCode()).isEqualTo(environmentArray.hashCode());
    assertThat(environmentProperties.hashCode()).isNotEqualTo(environmentVariables.hashCode());
    assertThat(environmentVariables.hashCode()).isNotEqualTo(environmentArray.hashCode());
  }

  @Test
  public void toStringIsCorrect() {

    Environment environment = Environment.from(PropertiesBuilder.newInstance()
      .set("one", "1")
      .set("two", "2")
      .build());

    assertThat(StringUtils.trimAll(environment.toString()))
      .describedAs("ACTUAL = [%s]", environment.toString())
      .isEqualTo("[one=1,two=2]");
  }

  @Test
  public void toAssociativeArrayFromEnvironment() {

    Map<String, String> map = MapBuilder.<String, String>newSortedMap()
      .put("one", "1")
      .put("two", "2")
      .build();

    Environment environment = Environment.from(map);

    assertThat(environment).isNotNull();
    assertThat(environment).hasSize(map.size());

    String[] associativeArray = environment.toAssociativeArray();

    assertThat(associativeArray).isNotNull();
    assertThat(associativeArray).hasSize(map.size());

    Arrays.sort(associativeArray);

    int index = 0;

    for (String key : map.keySet()) {
      assertThat(associativeArray[index++]).isEqualTo(String.format("%1$s=%2$s", key, map.get(key)));
    }

    assertThat(index).isEqualTo(map.size());
  }

  @Test
  public void toAssociativeArrayFromEmptyEnvironment() {

    String[] associativeArray = Environment.from(Collections.emptyMap()).toAssociativeArray();

    assertThat(associativeArray).isNotNull();
    assertThat(associativeArray).isEmpty();
  }

  @Test
  public void toMapFromEnvironment() {

    String[] associativeArray = { "one=1", "two=2" };

    Environment environment = Environment.from(associativeArray);

    assertThat(environment).isNotNull();
    assertThat(environment).hasSameSizeAs(associativeArray);

    Map<String, String> map = environment.toMap();

    assertThat(map).isNotNull();
    assertThat(map).hasSameSizeAs(associativeArray);
    assertThat(map.containsKey("one")).isTrue();
    assertThat(map.get("one")).isEqualTo("1");
    assertThat(map.containsKey("two")).isTrue();
    assertThat(map.get("two")).isEqualTo("2");
  }

  @Test
  public void toMapFromEmptyEnvironment() {

    Map<String, String> map = Environment.from(new String[0]).toMap();

    assertThat(map).isEmpty();
    assertThat(map).isNotNull();
  }

  @Test
  public void toPropertiesFromEnvironment() {

    Properties env = PropertiesBuilder.newInstance()
      .set("one", "1")
      .set("two", "2")
      .build();

    Environment environment = Environment.from(env);

    assertThat(environment).isNotNull();
    assertThat(environment).hasSize(env.size());

    Properties properties = environment.toProperties();

    assertThat(properties).isNotNull();
    assertThat(properties).isNotSameAs(env);
    assertThat(properties).hasSize(env.size());
    assertThat(properties).isEqualTo(env);
  }

  @Test
  public void toPropertiesFromEmptyEnvironment() {

    Properties properties = Environment.from(new Properties()).toProperties();

    assertThat(properties).isNotNull();
    assertThat(properties).isEmpty();
  }
}
