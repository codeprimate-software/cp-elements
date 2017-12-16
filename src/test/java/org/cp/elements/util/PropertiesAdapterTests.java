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

package org.cp.elements.util;

import static org.cp.elements.util.PropertiesAdapter.from;
import static org.cp.elements.util.PropertiesUtils.singletonProperties;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.cp.elements.data.conversion.ConversionService;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link PropertiesAdapter}.

 * @author John J. Blum
 * @see org.junit.Rule
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

  @Rule
  public ExpectedException exception = ExpectedException.none();

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

    propertiesAdapter = from(properties);
  }

  @Before
  public void setup() {
    assertThat(propertiesAdapter, is(notNullValue(PropertiesAdapter.class)));
    assertThat(propertiesAdapter.getProperties(), is(notNullValue()));
    assertThat(propertiesAdapter.getConversionService(), is(notNullValue()));
    assertThat(propertiesAdapter.isEmpty(), is(false));
    assertThat(propertiesAdapter.size(), is(equalTo(5)));
  }

  @Test
  public void constructPropertiesAdapterWithProperties() {
    PropertiesAdapter propertiesAdapter = new PropertiesAdapter(mockProperties);

    assertThat(propertiesAdapter.getProperties(), is(sameInstance(mockProperties)));
    assertThat(propertiesAdapter.getConversionService(), is(notNullValue(ConversionService.class)));
  }

  @Test
  public void constructPropertiesAdapterWithNull() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("The Properties to wrap cannot be null");

    new PropertiesAdapter(null);
  }

  @Test
  public void fromProperties() {
    PropertiesAdapter propertiesAdapter = from(mockProperties);

    assertThat(propertiesAdapter, is(notNullValue(PropertiesAdapter.class)));
    assertThat(propertiesAdapter.getProperties(), is(sameInstance(mockProperties)));
    assertThat(propertiesAdapter.getConversionService(), is(notNullValue(ConversionService.class)));
  }

  @Test
  public void containsExistingPropertiesIsTrue() {
    assertThat(propertiesAdapter.contains("booleanProperty"), is(true));
    assertThat(propertiesAdapter.contains("characterProperty"), is(true));
    assertThat(propertiesAdapter.contains("doubleProperty"), is(true));
    assertThat(propertiesAdapter.contains("integerProperty"), is(true));
    assertThat(propertiesAdapter.contains("stringProperty"), is(true));
  }

  @Test
  public void containsNonExistingPropertiesIsFalse() {
    assertThat(propertiesAdapter.contains("boolProperty"), is(false));
    assertThat(propertiesAdapter.contains("charProperty"), is(false));
    assertThat(propertiesAdapter.contains("floatProperty"), is(false));
    assertThat(propertiesAdapter.contains("intProperty"), is(false));
    assertThat(propertiesAdapter.contains("strProperty"), is(false));
    assertThat(propertiesAdapter.contains("STRINGPROPERTY"), is(false));
  }

  @Test
  public void isEmpty() {
    Properties properties = new Properties();
    PropertiesAdapter propertiesAdapter = from(properties);

    assertThat(propertiesAdapter, is(notNullValue(PropertiesAdapter.class)));
    assertThat(propertiesAdapter.isEmpty(), is(true));

    properties.setProperty("one", "1");

    assertThat(propertiesAdapter.isEmpty(), is(false));

    properties.remove("1");

    assertThat(propertiesAdapter.isEmpty(), is(false));

    properties.setProperty("one", "null");

    assertThat(propertiesAdapter.isEmpty(), is(false));

    properties.clear();

    assertThat(propertiesAdapter.isEmpty(), is(true));
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

    PropertiesAdapter propertiesAdapter = from(properties);

    assertThat(propertiesAdapter, is(notNullValue(PropertiesAdapter.class)));
    assertThat(propertiesAdapter.size(), is(equalTo(7)));
    assertThat(propertiesAdapter.isSet("one"), is(true));
    assertThat(propertiesAdapter.isSet("two"), is(false));
    assertThat(propertiesAdapter.isSet("three"), is(true));
    assertThat(propertiesAdapter.isSet("four"), is(true));
    assertThat(propertiesAdapter.isSet("five"), is(true));
    assertThat(propertiesAdapter.isSet("six"), is(true));
    assertThat(propertiesAdapter.isSet("seven"), is(true));
  }

  @Test
  public void isUnsetWithUnsetProperties() {
    Properties properties = new Properties();

    properties.setProperty("one", "  ");
    properties.setProperty("two", "");
    properties.setProperty("three", "null");

    PropertiesAdapter propertiesAdapter = from(properties);

    assertThat(propertiesAdapter, is(notNullValue(PropertiesAdapter.class)));
    assertThat(propertiesAdapter.size(), is(equalTo(3)));
    assertThat(propertiesAdapter.isUnset("one"), is(true));
    assertThat(propertiesAdapter.isUnset("two"), is(true));
    assertThat(propertiesAdapter.isUnset("three"), is(true));
  }

  @Test
  @SuppressWarnings("all")
  public void existingUnsetPropertiesAreNotSetAndAreUnset() {
    Properties properties = new Properties();

    properties.put("NULLProperty", "NULL");
    properties.put("NullProperty", "Null");
    properties.put("nullProperty", "null");

    PropertiesAdapter propertiesAdapter = from(properties);

    assertThat(propertiesAdapter, is(notNullValue(PropertiesAdapter.class)));
    assertThat(propertiesAdapter.size(), is(equalTo(properties.size())));

    int count = 0;

    for (String property : propertiesAdapter) {
      assertThat(propertiesAdapter.contains(property), is(true));
      assertThat(propertiesAdapter.isSet(property), is(false));
      assertThat(propertiesAdapter.isUnset(property), is(true));
      count++;
    }

    assertThat(count, is(equalTo(properties.size())));
  }

  @Test
  public void nonExistingPropertiesAreNotSetNorUnset() {
    assertThat(propertiesAdapter.isSet("nonExistingProperty"), is(false));
    assertThat(propertiesAdapter.isUnset("nonExistingProperty"), is(false));
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

    assertThat(propertiesAdapter.getConversionService(), is(sameInstance(mockConversionService)));
    assertThat(propertiesAdapter.getProperties(), is(sameInstance(mockProperties)));
    assertThat(propertiesAdapter.convert("propertyName", String.class), is(equalTo("test")));

    verify(mockProperties, times(1)).getProperty(eq("propertyName"), eq(null));
    verify(mockConversionService, times(1)).convert(eq("test"), eq(String.class));
  }

  @Test
  public void defaultIfNotSetReturnsValueOfExistingProperty() {
    assertThat(propertiesAdapter.defaultIfNotSet("booleanProperty", "false", String.class), is(equalTo("true")));
    assertThat(propertiesAdapter.defaultIfNotSet("characterProperty", "Y", String.class), is(equalTo("X")));
    assertThat(propertiesAdapter.defaultIfNotSet("doubleProperty", "1.21", String.class), is(equalTo("3.14159")));
    assertThat(propertiesAdapter.defaultIfNotSet("integerProperty", "4", String.class), is(equalTo("2")));
    assertThat(propertiesAdapter.defaultIfNotSet("stringProperty", "mock", String.class), is(equalTo("test")));
  }

  @Test
  public void defaultIfNotSetReturnsDefaultValue() {
    assertThat(propertiesAdapter.defaultIfNotSet("boolProperty", false, Boolean.TYPE), is(false));
    assertThat(propertiesAdapter.defaultIfNotSet("charProperty", 'Y', Character.TYPE), is('Y'));
    assertThat(propertiesAdapter.defaultIfNotSet("floatProperty", 3.14f, Float.TYPE), is(3.14f));
    assertThat(propertiesAdapter.defaultIfNotSet("intProperty", 4, Integer.TYPE), is(4));
    assertThat(propertiesAdapter.defaultIfNotSet("strProperty", "TEST", String.class), is("TEST"));
  }

  @Test
  public void valueOfStringIsString() {
    assertThat(propertiesAdapter.valueOf("test"), is(equalTo("test")));
  }

  @Test
  public void valueOfNullStringIsNull() {
    assertThat(propertiesAdapter.valueOf(" null  "), is(nullValue(String.class)));
  }

  @Test
  public void valueOfNullIsNull() {
    assertThat(propertiesAdapter.valueOf(null), is(nullValue(String.class)));
  }

  @Test
  public void filterForTextBasedProperties() {
    PropertiesAdapter filteredPropertiesAdapter = propertiesAdapter.filter(
      (property) -> property.startsWith("char") || property.startsWith("str"));

    assertThat(filteredPropertiesAdapter, is(notNullValue()));
    assertThat(filteredPropertiesAdapter.contains("characterProperty"), is(true));
    assertThat(filteredPropertiesAdapter.contains("stringProperty"), is(true));
    assertThat(filteredPropertiesAdapter.contains("booleanProperty"), is(false));
    assertThat(filteredPropertiesAdapter.contains("doubleProperty"), is(false));
    assertThat(filteredPropertiesAdapter.contains("integerProperty"), is(false));
  }

  @Test
  public void filterForNonExistingProperties() {
    PropertiesAdapter filteredPropertiesAdapter = propertiesAdapter.filter((property) -> false);

    assertThat(filteredPropertiesAdapter, is(notNullValue()));
    assertThat(filteredPropertiesAdapter.isEmpty(), is(true));
  }

  @Test
  public void getExistingPropertyReturnsValue() {
    assertThat(propertiesAdapter.get("booleanProperty"), is(equalTo("true")));
    assertThat(propertiesAdapter.get("characterProperty"), is(equalTo("X")));
    assertThat(propertiesAdapter.get("doubleProperty"), is(equalTo("3.14159")));
    assertThat(propertiesAdapter.get("integerProperty"), is(equalTo("2")));
    assertThat(propertiesAdapter.get("stringProperty"), is(equalTo("test")));
  }

  @Test
  public void getExistingPropertyWithDefaultValueReturnsPropertyValue() {
    assertThat(propertiesAdapter.get("stringProperty", "MOCK"), is(equalTo("test")));
  }

  @Test
  public void getExistingPropertyAsTypeReturnsTypedValue() {
    assertThat(propertiesAdapter.getAsType("booleanProperty", Boolean.class), is(equalTo(true)));
    assertThat(propertiesAdapter.getAsType("characterProperty", Character.class), is(equalTo('X')));
    assertThat(propertiesAdapter.getAsType("doubleProperty", Double.class), is(equalTo(3.14159d)));
    assertThat(propertiesAdapter.getAsType("integerProperty", Integer.class), is(equalTo(2)));
    assertThat(propertiesAdapter.getAsType("stringProperty", String.class), is(equalTo("test")));
  }

  @Test
  public void getExistingPropertyAsTypeWithDefaultValueReturnsTypedPropertyValue() {
    assertThat(propertiesAdapter.getAsType("characterProperty", Character.class, 'Y'), is(equalTo('X')));
  }

  @Test
  public void getNonExistingPropertyReturnsNull() {
    assertThat(propertiesAdapter.get("nonExistingProperty"), is(nullValue()));
  }

  @Test
  public void getNonExistingPropertyWithDefaultValueReturnsDefaultValue() {
    assertThat(propertiesAdapter.get("nonExistingProperty", "TEST"), is(equalTo("TEST")));
  }

  @Test
  public void getNonExistingPropertyAsTypeReturnsNull() {
    assertThat(propertiesAdapter.getAsType("nonExistingProperty", Double.TYPE), is(nullValue()));
  }

  @Test
  public void getNonExistingPropertyAsTypeWithDefaultValueReturnsDefaultValue() {
    assertThat(propertiesAdapter.getAsType("nonExistingProperty", Double.TYPE, 123.45d), is(equalTo(123.45d)));
  }

  @Test
  public void iterableOnPropertyNames() {
    Set<String> expectedPropertyNames = new HashSet<>(Arrays.asList("booleanProperty", "characterProperty",
      "doubleProperty", "integerProperty", "stringProperty"));

    for (String propertyName : propertiesAdapter) {
      expectedPropertyNames.remove(propertyName);
    }

    assertThat(expectedPropertyNames.isEmpty(), is(true));
  }

  @Test
  public void sizeIsEqualToNumberOfProperties() {
    Properties properties = new Properties();
    PropertiesAdapter propertiesAdapter = from(properties);

    assertThat(propertiesAdapter, is(notNullValue()));
    assertThat(propertiesAdapter.size(), is(equalTo(0)));

    properties.setProperty("one", "1");

    assertThat(propertiesAdapter.size(), is(equalTo(1)));

    properties.setProperty("two", "2");
    properties.setProperty("three", "3");

    assertThat(propertiesAdapter.size(), is(equalTo(3)));

    properties.remove("three");
    properties.setProperty("two", "null");

    assertThat(propertiesAdapter.size(), is(equalTo(2)));

    properties.clear();

    assertThat(propertiesAdapter.size(), is(equalTo(0)));
  }

  @Test
  public void equalPropertiesAdaptersIsTrue() {
    Properties properties = singletonProperties("one", "1");

    PropertiesAdapter propertiesAdapterOne = from(properties);
    PropertiesAdapter propertiesAdapterTwo = from(properties);

    assertThat(propertiesAdapterOne, is(notNullValue(PropertiesAdapter.class)));
    assertThat(propertiesAdapterTwo, is(notNullValue(PropertiesAdapter.class)));
    assertThat(propertiesAdapterOne, is(not(sameInstance(propertiesAdapterTwo))));
    assertThat(propertiesAdapterOne.equals(propertiesAdapterTwo), is(true));
  }

  @Test
  @SuppressWarnings("all")
  public void equalsItselfIsTrue() {
    PropertiesAdapter propertiesAdapter = from(singletonProperties("one", "1"));

    assertThat(propertiesAdapter, is(notNullValue(PropertiesAdapter.class)));
    assertThat(propertiesAdapter.equals(propertiesAdapter), is(true));
  }

  @Test
  public void equalsObjectIsFalse() {
    assertThat(from(mockProperties).equals(new Object()), is(false));
  }

  @Test
  @SuppressWarnings("all")
  public void equalsNullIsFalse() {
    assertThat(from(mockProperties).equals(null), is(false));
  }

  @Test
  public void hashCodeIsCorrect() {
    Properties properties = singletonProperties("one", "1");

    PropertiesAdapter propertiesAdapterOne = from(properties);
    PropertiesAdapter propertiesAdapterTwo = from(properties);
    PropertiesAdapter propertiesAdapterThree = from(singletonProperties("two", "2"));

    assertThat(propertiesAdapterOne.hashCode(), is(not(equalTo(0))));
    assertThat(propertiesAdapterOne.hashCode(), is(equalTo(propertiesAdapterTwo.hashCode())));
    assertThat(propertiesAdapterOne.hashCode(), is(not(equalTo(propertiesAdapterThree.hashCode()))));
  }

  @Test
  public void toStringIsSuccessful() {
    String actualPropertiesString = propertiesAdapter.toString();

    String expectedPropertiesString = "[\n\tbooleanProperty = true,\n\tcharacterProperty = X"
      + ",\n\tdoubleProperty = 3.14159,\n\tintegerProperty = 2,\n\tstringProperty = test\n]";

    assertThat(actualPropertiesString, is(equalTo(expectedPropertiesString)));
  }

  @Test
  public void toStringWithSinglePropertyIsSuccessful() {
    String actualPropertiesString = from(singletonProperties("one", "1")).toString();
    String expectedPropertiesString = "[\n\tone = 1\n]";

    assertThat(actualPropertiesString, is(equalTo(expectedPropertiesString)));
  }

  @Test
  public void toStringWithNoPropertiesIsSuccessful() {
    String actualPropertiesString = from(new Properties()).toString();
    String expectedPropertiesString = "[]";

    assertThat(actualPropertiesString, is(equalTo(expectedPropertiesString)));
  }

  @Test
  public void toMapIsSuccessful() {
    Properties properties = new Properties();

    properties.setProperty("one", "1");
    properties.setProperty("two", "2");

    PropertiesAdapter propertiesAdapter = from(properties);

    assertThat(propertiesAdapter, is(notNullValue(PropertiesAdapter.class)));
    assertThat(propertiesAdapter.size(), is(equalTo(properties.size())));

    Map<String, String> map = propertiesAdapter.toMap();

    assertThat(map, is(notNullValue(Map.class)));
    assertThat(map.size(), is(equalTo(properties.size())));

    for (String propertyName : properties.stringPropertyNames()) {
      assertThat(map.containsKey(propertyName), is(true));
      assertThat(map.get(propertyName), is(equalTo(properties.getProperty(propertyName))));
    }
  }

  @Test
  public void toMapWithEmptyPropertiesIsSuccessful() {
    PropertiesAdapter propertiesAdapter = from(new Properties());

    assertThat(propertiesAdapter, is(notNullValue(PropertiesAdapter.class)));
    assertThat(propertiesAdapter.isEmpty(), is(true));

    Map<String, String> map = propertiesAdapter.toMap();

    assertThat(map, is(notNullValue(Map.class)));
    assertThat(map.isEmpty(), is(true));
  }
}
