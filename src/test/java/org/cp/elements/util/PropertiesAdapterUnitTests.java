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
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.cp.elements.util.PropertiesUtils.singletonProperties;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.ConversionService;
import org.cp.elements.test.annotation.SubjectUnderTest;

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
public class PropertiesAdapterUnitTests {

  @SubjectUnderTest
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
    assertThat(propertiesAdapter).isNotEmpty();
    assertThat(propertiesAdapter).hasSize(5);
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
      .isThrownBy(() -> new PropertiesAdapter(null))
      .withMessage("The Properties to adapt is required")
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
  public void isEmpty() {

    Properties properties = new Properties();

    PropertiesAdapter propertiesAdapter = PropertiesAdapter.from(properties);

    assertThat(propertiesAdapter).isNotNull();
    assertThat(propertiesAdapter).isEmpty();

    properties.setProperty("1", "one");

    assertThat(propertiesAdapter.isEmpty()).isFalse();

    properties.remove("one");

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
    properties.setProperty("two", "nu11");
    properties.setProperty("three", "nil");
    properties.setProperty("four", "__");
    properties.setProperty("five", "0");
    properties.setProperty("six", "!");
    properties.setProperty("seven", "void");

    PropertiesAdapter propertiesAdapter = PropertiesAdapter.from(properties);

    assertThat(propertiesAdapter).isNotNull();
    assertThat(propertiesAdapter).hasSize(properties.size());

    properties.stringPropertyNames().forEach(propertyName ->
      assertThat(propertiesAdapter.isSet(propertyName)).isTrue());
  }

  @Test
  public void isUnsetWithUnsetProperties() {

    Properties properties = new Properties();

    properties.setProperty("one", "  ");
    properties.setProperty("two", "");
    properties.setProperty("three", "null");
    properties.setProperty("four", " null  ");

    PropertiesAdapter propertiesAdapter = PropertiesAdapter.from(properties);

    assertThat(propertiesAdapter).isNotNull();
    assertThat(propertiesAdapter).hasSize(properties.size());

    properties.stringPropertyNames().forEach(propertyName ->
      assertThat(propertiesAdapter.isUnset(propertyName)).isTrue());
  }

  @Test
  public void existingPropertiesAreSetAndNotUnset() {

    Properties properties = new Properties();

    properties.put("mockKey", "mockValue");
    properties.put("testKey", "testValue");

    PropertiesAdapter propertiesAdapter = PropertiesAdapter.from(properties);

    assertThat(propertiesAdapter).isNotNull();
    assertThat(propertiesAdapter).hasSize(properties.size());

    int count = 0;

    for (String property : propertiesAdapter) {
      assertThat(propertiesAdapter).contains(property);
      assertThat(propertiesAdapter.isSet(property)).isTrue();
      assertThat(propertiesAdapter.isUnset(property)).isFalse();
      count++;
    }

    assertThat(count).isEqualTo(properties.size());
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
    assertThat(propertiesAdapter).hasSize(properties.size());

    int count = 0;

    for (String property : propertiesAdapter) {
      assertThat(propertiesAdapter).contains(property);
      assertThat(propertiesAdapter.isSet(property)).isFalse();
      assertThat(propertiesAdapter.isUnset(property)).isTrue();
      count++;
    }

    assertThat(count).isEqualTo(properties.size());
  }

  @Test
  public void nonExistingPropertiesAreNotSetAndNotUnset() {

    assertThat(propertiesAdapter.isSet("nonExistingProperty")).isFalse();
    assertThat(propertiesAdapter.isUnset("nonExistingProperty")).isFalse();
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
    assertThat(propertiesAdapter.contains("characterProp")).isFalse();
    assertThat(propertiesAdapter.contains("floatProperty")).isFalse();
    assertThat(propertiesAdapter.contains("intProperty")).isFalse();
    assertThat(propertiesAdapter.contains("strProperty")).isFalse();
    assertThat(propertiesAdapter.contains("stringProp")).isFalse();
    assertThat(propertiesAdapter.contains("stingProperty")).isFalse();
    assertThat(propertiesAdapter.contains("StringProperty")).isFalse();
    assertThat(propertiesAdapter.contains("STRINGPROPERTY")).isFalse();
  }

  @Test
  public void convertUsesConversionService() {

    ConversionService mockConversionService = mock(ConversionService.class);

    doReturn("test").when(this.mockProperties).getProperty(anyString(), any());

    doAnswer((invocationOnMock) -> invocationOnMock.getArgument(0)).
      when(mockConversionService).convert(anyString(), eq(String.class));

    PropertiesAdapter propertiesAdapter = spy(new PropertiesAdapter(this.mockProperties));

    doReturn(mockConversionService).when(propertiesAdapter).getConversionService();

    assertThat(propertiesAdapter.getConversionService()).isSameAs(mockConversionService);
    assertThat(propertiesAdapter.getProperties()).isSameAs(this.mockProperties);
    assertThat(propertiesAdapter.convert("propertyName", String.class)).isEqualTo("test");

    verify(this.mockProperties, times(1)).getProperty(eq("propertyName"), eq(null));
    verify(mockConversionService, times(1)).convert(eq("test"), eq(String.class));
    verifyNoMoreInteractions(mockConversionService);
  }

  @Test
  public void convertWithNullClassType() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> propertiesAdapter.convert("integerProperty", null))
      .withMessage("Class type to convert the property value to is required")
      .withNoCause();
  }

  @Test
  public void convertWithUndeclaredProperty() {

    assertThatExceptionOfType(ConversionException.class)
      .isThrownBy(() -> propertiesAdapter.convert("nonExistingProperty", Integer.class))
      .withMessage("Cannot convert [null] to [java.lang.Integer]")
      .withNoCause();
  }

  @Test
  public void convertWithUndefinedProperty() {

    Properties properties = new Properties();

    properties.setProperty("blankProperty", "  ");
    properties.setProperty("emptyProperty", "");

    PropertiesAdapter propertiesAdapter = PropertiesAdapter.from(properties);

    assertThat(propertiesAdapter).isNotNull();
    assertThat(properties).hasSize(2);

    properties.stringPropertyNames().forEach(propertyName ->
      assertThatExceptionOfType(ConversionException.class)
        .isThrownBy(() -> propertiesAdapter.convert(propertyName, Integer.class))
        .withMessage("[%s] is not a valid number of the qualifying type [java.lang.Integer]",
          properties.getProperty(propertyName))
        .withNoCause());
  }

  @Test
  public void returnDefaultValueIfNotSetReturnsPropertyValue() {

    assertThat(propertiesAdapter.returnDefaultValueIfNotSet("booleanProperty", String.class, "false")).isEqualTo("true");
    assertThat(propertiesAdapter.returnDefaultValueIfNotSet("characterProperty", String.class, "Y")).isEqualTo("X");
    assertThat(propertiesAdapter.returnDefaultValueIfNotSet("doubleProperty", String.class, "1.21")).isEqualTo("3.14159");
    assertThat(propertiesAdapter.returnDefaultValueIfNotSet("integerProperty", String.class, "4")).isEqualTo("2");
    assertThat(propertiesAdapter.returnDefaultValueIfNotSet("stringProperty", String.class, "mock")).isEqualTo("test");
  }

  @Test
  public void returnDefaultValueIfNotSetReturnsDefaultValue() {

    assertThat(propertiesAdapter.returnDefaultValueIfNotSet("boolProperty", Boolean.TYPE, false)).isFalse();
    assertThat(propertiesAdapter.returnDefaultValueIfNotSet("charProperty", Character.TYPE, 'Y')).isEqualTo('Y');
    assertThat(propertiesAdapter.returnDefaultValueIfNotSet("floatProperty", Float.TYPE, 3.14f)).isEqualTo(3.14f);
    assertThat(propertiesAdapter.returnDefaultValueIfNotSet("intProperty", Integer.TYPE, 4)).isEqualTo(4);
    assertThat(propertiesAdapter.returnDefaultValueIfNotSet("strProperty", String.class, "TEST")).isEqualTo("TEST");
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
  public void filterNonExistingProperties() {

    PropertiesAdapter filteredPropertiesAdapter = propertiesAdapter.filter(property -> false);

    assertThat(filteredPropertiesAdapter).isNotNull();
    assertThat(filteredPropertiesAdapter).isNotSameAs(propertiesAdapter);
    assertThat(filteredPropertiesAdapter).isEmpty();
  }

  @Test
  public void filterTextBasedProperties() {

    PropertiesAdapter filteredPropertiesAdapter = propertiesAdapter
      .filter(property -> property.startsWith("char") || property.startsWith("str"));

    assertThat(filteredPropertiesAdapter).isNotNull();
    assertThat(filteredPropertiesAdapter.contains("characterProperty")).isTrue();
    assertThat(filteredPropertiesAdapter.contains("stringProperty")).isTrue();
    assertThat(filteredPropertiesAdapter.contains("booleanProperty")).isFalse();
    assertThat(filteredPropertiesAdapter.contains("doubleProperty")).isFalse();
    assertThat(filteredPropertiesAdapter.contains("integerProperty")).isFalse();
  }

  @Test
  public void filterWithNullFilter() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> PropertiesAdapter.empty().filter(null))
      .withMessage("Predicate used to filter properties is required")
      .withNoCause();
  }

  @Test
  public void getExistingPropertyReturnsPropertyValue() {

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
  public void getExistingPropertyAsTypeReturnsTypedPropertyValue() {

    assertThat(propertiesAdapter.getAsType("booleanProperty", Boolean.class)).isTrue();
    assertThat(propertiesAdapter.getAsType("characterProperty", Character.class)).isEqualTo('X');
    assertThat(propertiesAdapter.getAsType("doubleProperty", Double.class)).isEqualTo(3.14159d);
    assertThat(propertiesAdapter.getAsType("integerProperty", Integer.class)).isEqualTo(2);
    assertThat(propertiesAdapter.getAsType("stringProperty", String.class)).isEqualTo("test");
  }

  @Test
  public void getExistingPropertyAsTypeWithDefaultValueReturnsTypedPropertyValue() {
    assertThat(propertiesAdapter.getAsType("characterProperty", Character.class, 'Y'))
      .isEqualTo('X');
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
    assertThat(propertiesAdapter.getAsType("nonExistingProperty", Double.TYPE, 123.45d))
      .isEqualTo(123.45d);
  }

  @Test
  public void iterableOnPropertyNames() {

    Set<String> expectedPropertyNames = new HashSet<>(Arrays.asList("booleanProperty", "characterProperty",
      "doubleProperty", "integerProperty", "stringProperty"));

    for (String propertyName : propertiesAdapter) {
      expectedPropertyNames.remove(propertyName);
    }

    assertThat(expectedPropertyNames).isEmpty();
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
  public void sizeOfEmptyPropertiesIsZero() {
    assertThat(PropertiesAdapter.empty().size()).isZero();
  }

  @Test
  public void equalPropertiesAdaptersIsTrue() {

    Properties properties = PropertiesUtils.singletonProperties("one", "1");

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
      PropertiesAdapter.from(PropertiesUtils.singletonProperties("one", "1"));

    assertThat(propertiesAdapter).isNotNull();
    assertThat(propertiesAdapter.equals(propertiesAdapter)).isTrue();
  }

  @Test
  public void equalsObjectIsFalse() {
    assertThat(PropertiesAdapter.from(this.mockProperties).equals(new Object())).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void equalsNullIsFalse() {
    assertThat(PropertiesAdapter.from(this.mockProperties).equals(null)).isFalse();
  }

  @Test
  public void hashCodeIsCorrect() {

    Properties properties = PropertiesUtils.singletonProperties("1", "one");

    PropertiesAdapter propertiesAdapterOne = PropertiesAdapter.from(properties);
    PropertiesAdapter propertiesAdapterTwo = PropertiesAdapter.from(properties);
    PropertiesAdapter propertiesAdapterThree =
      PropertiesAdapter.from(singletonProperties("2", "two"));

    assertThat(propertiesAdapterOne.hashCode()).isNotEqualTo(0);
    assertThat(propertiesAdapterOne.hashCode()).isEqualTo(propertiesAdapterTwo.hashCode());
    assertThat(propertiesAdapterOne.hashCode()).isNotEqualTo(propertiesAdapterThree.hashCode());
  }

  @Test
  public void toStringIsCorrect() {

    String actualPropertiesString = propertiesAdapter.toString();

    String expectedPropertiesString = "["
      + "\n\tbooleanProperty = true,"
      + "\n\tcharacterProperty = X,"
      + "\n\tdoubleProperty = 3.14159,"
      + "\n\tintegerProperty = 2,"
      + "\n\tstringProperty = test"
      + "\n]";

    assertThat(actualPropertiesString).isEqualTo(expectedPropertiesString);
  }

  @Test
  public void toStringWithSinglePropertyIsSuccessful() {

    String actualPropertiesString = PropertiesAdapter.from(singletonProperties("1", "one")).toString();
    String expectedPropertiesString = "[\n\t1 = one\n]";

    assertThat(actualPropertiesString).isEqualTo(expectedPropertiesString);
  }

  @Test
  public void toStringWithNoPropertiesIsSuccessful() {

    String actualPropertiesString = PropertiesAdapter.empty().toString();
    String expectedPropertiesString = "[]";

    assertThat(actualPropertiesString).isEqualTo(expectedPropertiesString);
  }

  @Test
  public void toPropertiesIsSuccessful() {

    Properties properties = new Properties();

    properties.setProperty("1", "one");
    properties.setProperty("2", "two");

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

    properties.setProperty("1", "one");
    properties.setProperty("2", "two");

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