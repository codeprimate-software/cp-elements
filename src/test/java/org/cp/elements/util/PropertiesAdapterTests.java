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
package org.cp.elements.util;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.cp.elements.util.PropertiesUtils.singletonProperties;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.cp.elements.data.conversion.ConversionService;

import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit Tests for {@link PropertiesAdapter}.

 * @author John J. Blum
 * @see java.util.Map
 * @see java.util.Properties
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.util.PropertiesAdapter
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class PropertiesAdapterTests {

  private static PropertiesAdapter propertiesAdapter;

  @Mock
  private Properties mockProperties;

  @BeforeClass
  public static void setupPropertiesAdapter() {

    Properties properties = new Properties();

    properties.setProperty("booleanProperty", "true");
    properties.setProperty("characterProperty", "X");
    properties.setProperty("doubleProperty", "3.14159");
    properties.setProperty("integerProperty", "2");
    properties.setProperty("stringProperty", "test");

    propertiesAdapter = PropertiesAdapter.from(properties);
  }

  @Before
  public void setup() {

    assertThat(propertiesAdapter).isNotNull();
    assertThat(propertiesAdapter.getProperties()).isNotNull();
    assertThat(propertiesAdapter.getConversionService()).isNotNull();
    assertThat(propertiesAdapter.isEmpty()).isFalse();
    assertThat(propertiesAdapter.size()).isEqualTo(5);
  }

  @Test
  public void constructPropertiesAdapterWithProperties() {

    PropertiesAdapter propertiesAdapter = new PropertiesAdapter(this.mockProperties);

    assertThat(propertiesAdapter.getProperties()).isSameAs(this.mockProperties);
    assertThat(propertiesAdapter.getConversionService()).isNotNull();
  }

  @Test
  public void constructPropertiesAdapterWithNull() {

    assertThatIllegalArgumentException()
      .isThrownBy(() ->new PropertiesAdapter(null))
      .withMessage("Properties to adapt is required")
      .withNoCause();
  }

  @Test
  public void emptyIsCorrect() {

    PropertiesAdapter propertiesAdapter = PropertiesAdapter.empty();

    assertThat(propertiesAdapter).isNotNull();
    assertThat(propertiesAdapter).isEmpty();
  }

  @Test
  public void fromProperties() {

    PropertiesAdapter propertiesAdapter = PropertiesAdapter.from(this.mockProperties);

    assertThat(propertiesAdapter).isNotNull();
    assertThat(propertiesAdapter.getProperties()).isSameAs(this.mockProperties);
    assertThat(propertiesAdapter.getConversionService()).isNotNull();
  }

  @Test
  public void containsExistingPropertiesIsTrue() {

    assertThat(propertiesAdapter.contains("booleanProperty")).isTrue();
    assertThat(propertiesAdapter.contains("characterProperty")).isTrue();
    assertThat(propertiesAdapter.contains("doubleProperty")).isTrue();
    assertThat(propertiesAdapter.contains("integerProperty")).isTrue();
    assertThat(propertiesAdapter.contains("stringProperty")).isTrue();
  }

  @Test
  public void containsNonExistingPropertiesIsFalse() {

    assertThat(propertiesAdapter.contains("boolProperty")).isFalse();
    assertThat(propertiesAdapter.contains("charProperty")).isFalse();
    assertThat(propertiesAdapter.contains("floatProperty")).isFalse();
    assertThat(propertiesAdapter.contains("intProperty")).isFalse();
    assertThat(propertiesAdapter.contains("strProperty")).isFalse();
    assertThat(propertiesAdapter.contains("STRINGPROPERTY")).isFalse();
  }

  @Test
  public void isEmpty() {

    Properties properties = new Properties();

    PropertiesAdapter propertiesAdapter = PropertiesAdapter.from(properties);

    assertThat(propertiesAdapter).isNotNull();
    assertThat(propertiesAdapter.isEmpty()).isTrue();

    properties.setProperty("one", "1");

    assertThat(propertiesAdapter.isEmpty()).isFalse();

    properties.remove("1");

    assertThat(propertiesAdapter.isEmpty()).isFalse();

    properties.setProperty("one", "null");

    assertThat(propertiesAdapter.isEmpty()).isFalse();

    properties.clear();

    assertThat(propertiesAdapter.isEmpty()).isTrue();
  }

  @Test
  public void isSetWithSetProperties() {

    Properties properties = new Properties();

    properties.setProperty("one", "test");
    properties.setProperty("two", "null");
    properties.setProperty("three", "nil");
    properties.setProperty("four", "__");
    properties.setProperty("five", "0");
    properties.setProperty("six", "!");
    properties.setProperty("seven", "void");

    PropertiesAdapter propertiesAdapter = PropertiesAdapter.from(properties);

    assertThat(propertiesAdapter).isNotNull();
    assertThat(propertiesAdapter.size()).isEqualTo(7);
    assertThat(propertiesAdapter.isSet("one")).isTrue();
    assertThat(propertiesAdapter.isSet("two")).isFalse();
    assertThat(propertiesAdapter.isSet("three")).isTrue();
    assertThat(propertiesAdapter.isSet("four")).isTrue();
    assertThat(propertiesAdapter.isSet("five")).isTrue();
    assertThat(propertiesAdapter.isSet("six")).isTrue();
    assertThat(propertiesAdapter.isSet("seven")).isTrue();
  }

  @Test
  public void isUnsetWithUnsetProperties() {

    Properties properties = new Properties();

    properties.setProperty("one", "  ");
    properties.setProperty("two", "");
    properties.setProperty("three", "null");

    PropertiesAdapter propertiesAdapter = PropertiesAdapter.from(properties);

    assertThat(propertiesAdapter).isNotNull();
    assertThat(propertiesAdapter.size()).isEqualTo(3);
    assertThat(propertiesAdapter.isUnset("one")).isTrue();
    assertThat(propertiesAdapter.isUnset("two")).isTrue();
    assertThat(propertiesAdapter.isUnset("three")).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void existingUnsetPropertiesAreNotSetAndAreUnset() {

    Properties properties = new Properties();

    properties.put("NULLProperty", "NULL");
    properties.put("NullProperty", "Null");
    properties.put("nullProperty", "null");

    PropertiesAdapter propertiesAdapter = PropertiesAdapter.from(properties);

    assertThat(propertiesAdapter).isNotNull();
    assertThat(propertiesAdapter.size()).isEqualTo(properties.size());

    int count = 0;

    for (String property : propertiesAdapter) {
      assertThat(propertiesAdapter.contains(property)).isTrue();
      assertThat(propertiesAdapter.isSet(property)).isFalse();
      assertThat(propertiesAdapter.isUnset(property)).isTrue();
      count++;
    }

    assertThat(count).isEqualTo(properties.size());
  }

  @Test
  public void nonExistingPropertiesAreNotSetNorUnset() {

    assertThat(propertiesAdapter.isSet("nonExistingProperty")).isFalse();
    assertThat(propertiesAdapter.isUnset("nonExistingProperty")).isFalse();
  }

  @Test
  public void convertUsesConversionService() {

    ConversionService mockConversionService = mock(ConversionService.class);

    when(mockProperties.getProperty(anyString(), any())).thenReturn("test");
    when(mockConversionService.convert(anyString(), eq(String.class))).thenAnswer(
      (invocationOnMock) -> invocationOnMock.getArgument(0));

    PropertiesAdapter propertiesAdapter = new PropertiesAdapter(mockProperties) {
      @Override protected ConversionService getConversionService() {
        return mockConversionService;
      }
    };

    assertThat(propertiesAdapter.getConversionService()).isSameAs(mockConversionService);
    assertThat(propertiesAdapter.getProperties()).isSameAs(mockProperties);
    assertThat(propertiesAdapter.convert("propertyName", String.class)).isEqualTo("test");

    verify(mockProperties, times(1)).getProperty(eq("propertyName"), eq(null));
    verify(mockConversionService, times(1)).convert(eq("test"), eq(String.class));
  }

  @Test
  public void defaultIfNotSetReturnsValueOfExistingProperty() {

    assertThat(propertiesAdapter.defaultIfNotSet("booleanProperty", "false", String.class)).isEqualTo("true");
    assertThat(propertiesAdapter.defaultIfNotSet("characterProperty", "Y", String.class)).isEqualTo("X");
    assertThat(propertiesAdapter.defaultIfNotSet("doubleProperty", "1.21", String.class)).isEqualTo("3.14159");
    assertThat(propertiesAdapter.defaultIfNotSet("integerProperty", "4", String.class)).isEqualTo("2");
    assertThat(propertiesAdapter.defaultIfNotSet("stringProperty", "mock", String.class)).isEqualTo("test");
  }

  @Test
  public void defaultIfNotSetReturnsDefaultValue() {

    assertThat(propertiesAdapter.defaultIfNotSet("boolProperty", false, Boolean.TYPE)).isFalse();
    assertThat(propertiesAdapter.defaultIfNotSet("charProperty", 'Y', Character.TYPE)).isEqualTo('Y');
    assertThat(propertiesAdapter.defaultIfNotSet("floatProperty", 3.14f, Float.TYPE)).isEqualTo(3.14f);
    assertThat(propertiesAdapter.defaultIfNotSet("intProperty", 4, Integer.TYPE)).isEqualTo(4);
    assertThat(propertiesAdapter.defaultIfNotSet("strProperty", "TEST", String.class)).isEqualTo("TEST");
  }

  @Test
  public void valueOfStringIsString() {
    assertThat(propertiesAdapter.valueOf("test")).isEqualTo("test");
  }

  @Test
  public void valueOfNullStringIsNull() {
    assertThat(propertiesAdapter.valueOf(" null  ")).isNull();
  }

  @Test
  public void valueOfNullIsNull() {
    assertThat(propertiesAdapter.valueOf(null)).isNull();
  }

  @Test
  public void filterForTextBasedProperties() {

    PropertiesAdapter filteredPropertiesAdapter = propertiesAdapter.filter(
      (property) -> property.startsWith("char") || property.startsWith("str"));

    assertThat(filteredPropertiesAdapter).isNotNull();
    assertThat(filteredPropertiesAdapter.contains("characterProperty")).isTrue();
    assertThat(filteredPropertiesAdapter.contains("stringProperty")).isTrue();
    assertThat(filteredPropertiesAdapter.contains("booleanProperty")).isFalse();
    assertThat(filteredPropertiesAdapter.contains("doubleProperty")).isFalse();
    assertThat(filteredPropertiesAdapter.contains("integerProperty")).isFalse();
  }

  @Test
  public void filterForNonExistingProperties() {

    PropertiesAdapter filteredPropertiesAdapter = propertiesAdapter.filter((property) -> false);

    assertThat(filteredPropertiesAdapter).isNotNull();
    assertThat(filteredPropertiesAdapter.isEmpty()).isTrue();
  }

  @Test
  public void getExistingPropertyReturnsValue() {

    assertThat(propertiesAdapter.get("booleanProperty")).isEqualTo("true");
    assertThat(propertiesAdapter.get("characterProperty")).isEqualTo("X");
    assertThat(propertiesAdapter.get("doubleProperty")).isEqualTo("3.14159");
    assertThat(propertiesAdapter.get("integerProperty")).isEqualTo("2");
    assertThat(propertiesAdapter.get("stringProperty")).isEqualTo("test");
  }

  @Test
  public void getExistingPropertyWithDefaultValueReturnsPropertyValue() {
    assertThat(propertiesAdapter.get("stringProperty", "MOCK")).isEqualTo("test");
  }

  @Test
  public void getExistingPropertyAsTypeReturnsTypedValue() {

    assertThat(propertiesAdapter.getAsType("booleanProperty", Boolean.class)).isTrue();
    assertThat(propertiesAdapter.getAsType("characterProperty", Character.class)).isEqualTo('X');
    assertThat(propertiesAdapter.getAsType("doubleProperty", Double.class)).isEqualTo(3.14159d);
    assertThat(propertiesAdapter.getAsType("integerProperty", Integer.class)).isEqualTo(2);
    assertThat(propertiesAdapter.getAsType("stringProperty", String.class)).isEqualTo("test");
  }

  @Test
  public void getExistingPropertyAsTypeWithDefaultValueReturnsTypedPropertyValue() {
    assertThat(propertiesAdapter.getAsType("characterProperty", Character.class, 'Y')).isEqualTo('X');
  }

  @Test
  public void getNonExistingPropertyReturnsNull() {
    assertThat(propertiesAdapter.get("nonExistingProperty")).isNull();
  }

  @Test
  public void getNonExistingPropertyWithDefaultValueReturnsDefaultValue() {
    assertThat(propertiesAdapter.get("nonExistingProperty", "TEST")).isEqualTo("TEST");
  }

  @Test
  public void getNonExistingPropertyAsTypeReturnsNull() {
    assertThat(propertiesAdapter.getAsType("nonExistingProperty", Double.TYPE)).isNull();
  }

  @Test
  public void getNonExistingPropertyAsTypeWithDefaultValueReturnsDefaultValue() {
    assertThat(propertiesAdapter.getAsType("nonExistingProperty", Double.TYPE, 123.45d)).isEqualTo(123.45d);
  }

  @Test
  public void iterableOnPropertyNames() {

    Set<String> expectedPropertyNames = new HashSet<>(Arrays.asList("booleanProperty", "characterProperty",
      "doubleProperty", "integerProperty", "stringProperty"));

    for (String propertyName : propertiesAdapter) {
      expectedPropertyNames.remove(propertyName);
    }

    assertThat(expectedPropertyNames.isEmpty()).isTrue();
  }

  @Test
  public void sizeIsEqualToNumberOfProperties() {

    Properties properties = new Properties();

    PropertiesAdapter propertiesAdapter = PropertiesAdapter.from(properties);

    assertThat(propertiesAdapter).isNotNull();
    assertThat(propertiesAdapter.size()).isEqualTo(0);

    properties.setProperty("one", "1");

    assertThat(propertiesAdapter.size()).isEqualTo(1);

    properties.setProperty("two", "2");
    properties.setProperty("three", "3");

    assertThat(propertiesAdapter.size()).isEqualTo(3);

    properties.remove("three");
    properties.setProperty("two", "null");

    assertThat(propertiesAdapter.size()).isEqualTo(2);

    properties.clear();

    assertThat(propertiesAdapter.size()).isEqualTo(0);
  }

  @Test
  public void equalPropertiesAdaptersIsTrue() {

    Properties properties = singletonProperties("one", "1");

    PropertiesAdapter propertiesAdapterOne = PropertiesAdapter.from(properties);
    PropertiesAdapter propertiesAdapterTwo = PropertiesAdapter.from(properties);

    assertThat(propertiesAdapterOne).isNotNull();
    assertThat(propertiesAdapterTwo).isNotNull();
    assertThat(propertiesAdapterOne).isNotSameAs(propertiesAdapterTwo);
    assertThat(propertiesAdapterOne.equals(propertiesAdapterTwo)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void equalsItselfIsTrue() {

    PropertiesAdapter propertiesAdapter =
      PropertiesAdapter.from(singletonProperties("one", "1"));

    assertThat(propertiesAdapter).isNotNull();
    assertThat(propertiesAdapter.equals(propertiesAdapter)).isTrue();
  }

  @Test
  public void equalsObjectIsFalse() {
    assertThat(PropertiesAdapter.from(mockProperties).equals(new Object())).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void equalsNullIsFalse() {
    assertThat(PropertiesAdapter.from(mockProperties).equals(null)).isFalse();
  }

  @Test
  public void hashCodeIsCorrect() {

    Properties properties = singletonProperties("one", "1");

    PropertiesAdapter propertiesAdapterOne = PropertiesAdapter.from(properties);
    PropertiesAdapter propertiesAdapterTwo = PropertiesAdapter.from(properties);
    PropertiesAdapter propertiesAdapterThree =
      PropertiesAdapter.from(singletonProperties("two", "2"));

    assertThat(propertiesAdapterOne.hashCode()).isNotEqualTo(0);
    assertThat(propertiesAdapterOne.hashCode()).isEqualTo(propertiesAdapterTwo.hashCode());
    assertThat(propertiesAdapterOne.hashCode()).isNotEqualTo(propertiesAdapterThree.hashCode());
  }

  @Test
  public void toStringIsSuccessful() {

    String actualPropertiesString = propertiesAdapter.toString();

    String expectedPropertiesString = "[\n\tbooleanProperty = true,\n\tcharacterProperty = X"
      + ",\n\tdoubleProperty = 3.14159,\n\tintegerProperty = 2,\n\tstringProperty = test\n]";

    assertThat(actualPropertiesString).isEqualTo(expectedPropertiesString);
  }

  @Test
  public void toStringWithSinglePropertyIsSuccessful() {

    String actualPropertiesString = PropertiesAdapter.from(singletonProperties("one", "1")).toString();
    String expectedPropertiesString = "[\n\tone = 1\n]";

    assertThat(actualPropertiesString).isEqualTo(expectedPropertiesString);
  }

  @Test
  public void toStringWithNoPropertiesIsSuccessful() {

    String actualPropertiesString = PropertiesAdapter.from(new Properties()).toString();
    String expectedPropertiesString = "[]";

    assertThat(actualPropertiesString).isEqualTo(expectedPropertiesString);
  }

  @Test
  public void toPropertiesIsSuccessful() {

    Properties properties = new Properties();

    properties.setProperty("one", "1");
    properties.setProperty("two", "2");

    PropertiesAdapter propertiesAdapter = PropertiesAdapter.from(properties);

    assertThat(propertiesAdapter).isNotNull();
    assertThat(propertiesAdapter).hasSize(2);

    Properties actual = propertiesAdapter.toProperties();

    assertThat(actual).isNotNull();
    assertThat(actual).isNotSameAs(properties);
    assertThat(actual).isEqualTo(properties);
  }

  @Test
  public void toPropertiesWithEmptyPropertiesAdapter() {

    PropertiesAdapter propertiesAdapter = PropertiesAdapter.empty();

    assertThat(propertiesAdapter).isNotNull();
    assertThat(propertiesAdapter).isEmpty();

    Properties actual = propertiesAdapter.toProperties();

    assertThat(actual).isNotNull();
    assertThat(actual).isEmpty();
  }

  @Test
  public void toMapIsSuccessful() {

    Properties properties = new Properties();

    properties.setProperty("one", "1");
    properties.setProperty("two", "2");

    PropertiesAdapter propertiesAdapter = PropertiesAdapter.from(properties);

    assertThat(propertiesAdapter).isNotNull();
    assertThat(propertiesAdapter).hasSize(properties.size());

    Map<String, String> map = propertiesAdapter.toMap();

    assertThat(map).isNotNull();
    assertThat(map).hasSize(properties.size());

    for (String propertyName : properties.stringPropertyNames()) {
      assertThat(map.containsKey(propertyName)).isTrue();
      assertThat(map.get(propertyName)).isEqualTo(properties.getProperty(propertyName));
    }
  }

  @Test
  public void toMapWithEmptyPropertiesAdapter() {

    PropertiesAdapter propertiesAdapter = PropertiesAdapter.empty();

    assertThat(propertiesAdapter).isNotNull();
    assertThat(propertiesAdapter).isEmpty();

    Map<String, String> map = propertiesAdapter.toMap();

    assertThat(map).isNotNull();
    assertThat(map).isEmpty();
  }
}
