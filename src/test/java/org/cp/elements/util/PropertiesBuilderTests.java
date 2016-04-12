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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.cp.elements.io.NoSuchFileException;
import org.cp.elements.lang.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * The PropertiesBuilderTest class is a test suite of test cases testing the contract and functionality
 * of the {@link PropertiesBuilder} class.
 *
 * @author John J. Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.PropertiesBuilder
 * @since 1.0.0
 */
public class PropertiesBuilderTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Test
  public void fromFileLoadsProperties() throws IOException {
    File testProperties = new File("test.properties");

    testProperties.deleteOnExit();

    Properties expected = new Properties();

    expected.setProperty("one", "1");
    expected.setProperty("two", "2");
    expected.store(new FileWriter(testProperties), "Test Properties");

    PropertiesBuilder propertiesBuilder = PropertiesBuilder.from(testProperties);

    assertThat(propertiesBuilder, is(notNullValue()));
    assertThat(propertiesBuilder.build(), is(equalTo(expected)));
  }

  @Test
  public void fromNonExistingFile() {
    File nonExistingFile = new File("/absolute/path/to/non/existing/file.properties");

    exception.expect(NoSuchFileException.class);
    exception.expectCause(is(instanceOf(FileNotFoundException.class)));
    exception.expectMessage(String.format("[%1$s] not found", nonExistingFile));

    PropertiesBuilder.from(nonExistingFile);
  }

  @Test
  public void fromInputStreamLoadsProperties() throws IOException {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    Properties expected = new Properties();

    expected.setProperty("one", "1");
    expected.setProperty("two", "2");
    expected.store(out, "Test Properties");

    byte[] buffer = out.toByteArray();

    PropertiesBuilder propertiesBuilder = PropertiesBuilder.from(new ByteArrayInputStream(buffer));

    assertThat(propertiesBuilder, is(notNullValue()));
    assertThat(propertiesBuilder.build(), is(equalTo(expected)));
  }

  @Test
  public void fromInputStreamThrowingIOExceptionIsHandledProperly() throws IOException {
    InputStream mockInputStream = mock(InputStream.class);

    IOException ioException = new IOException("test");

    when(mockInputStream.read()).thenThrow(ioException);
    when(mockInputStream.read(any(byte[].class))).thenThrow(ioException);
    when(mockInputStream.read(any(byte[].class), anyInt(), anyInt())).thenThrow(ioException);

    exception.expect(SystemException.class);
    exception.expectCause(is(instanceOf(IOException.class)));
    exception.expectMessage(String.format("failed to load properties from input stream [%1$s]", mockInputStream));

    PropertiesBuilder.from(mockInputStream);
  }

  @Test
  public void fromMapLoadsProperties() {
    Map<String, Object> expected = new HashMap<>();

    expected.put("booleanProperty", true);
    expected.put("characterProperty", 'X');
    expected.put("doubleProperty", 3.14159d);
    expected.put("integerProperty", 2);
    expected.put("nullProperty", null);
    expected.put("stringProperty", "test");

    PropertiesBuilder propertiesBuilder = PropertiesBuilder.from(expected);

    assertThat(propertiesBuilder, is(notNullValue()));

    Properties actual = propertiesBuilder.build();

    assertThat(actual, is(notNullValue()));
    assertThat(actual.size(), is(equalTo(expected.size())));

    for (String propertyName : expected.keySet()) {
      assertThat(actual.getProperty(propertyName), is(equalTo(String.valueOf(expected.get(propertyName)))));
    }
  }

  @Test
  public void fromPropertiesLoadsProperties() {
    Properties expected = new Properties();

    expected.setProperty("one", "1");
    expected.setProperty("two", "2");

    PropertiesBuilder propertiesBuilder = PropertiesBuilder.from(expected);

    assertThat(propertiesBuilder, is(notNullValue()));
    assertThat(propertiesBuilder.build(), is(equalTo(expected)));
  }

  @Test
  public void fromSystemPropertiesLoadsProperties() {
    PropertiesBuilder propertiesBuilder = PropertiesBuilder.fromSystemProperties();

    assertThat(propertiesBuilder, is(notNullValue()));
    assertThat(propertiesBuilder.build(), is(equalTo(System.getProperties())));
  }

  @Test
  public void constructPropertiesBuilderWithNoProperties() {
    PropertiesBuilder propertiesBuilder = new PropertiesBuilder();
    Properties properties = propertiesBuilder.getProperties();

    assertThat(properties, is(notNullValue()));
    assertThat(properties.isEmpty(), is(true));
  }

  @Test
  public void constructPropertiesBuilderWithProperties() {
    Properties expected = new Properties();

    expected.setProperty("one", "1");
    expected.setProperty("two", "2");

    PropertiesBuilder propertiesBuilder = new PropertiesBuilder(expected);

    Properties actual = propertiesBuilder.getProperties();

    assertThat(actual, is(notNullValue()));
    assertThat(actual, is(not(sameInstance(expected))));
    assertThat(actual, is(equalTo(expected)));
  }

  @Test
  public void setAndBuildProperties() {
    Properties actual = PropertiesBuilder.newInstance()
      .set("booleanProperty", false)
      .set("characterProperty", 'A')
      .set("doubleProperty", 3.14159d)
      .set("integerProperty", 0)
      .set("stringProperty", "test")
      .set("enumProperty", Numbers.ZERO)
      .set("personProperty", Person.create("Jon", "Doe"))
      .build();

    Properties expected = new Properties();

    expected.setProperty("booleanProperty", "false");
    expected.setProperty("characterProperty", "A");
    expected.setProperty("doubleProperty", "3.14159");
    expected.setProperty("integerProperty", "0");
    expected.setProperty("stringProperty", "test");
    expected.setProperty("enumProperty", "ZERO");
    expected.setProperty("personProperty", "Jon Doe");

    assertThat(actual, is(notNullValue()));
    assertThat(actual, is(equalTo(expected)));
  }

  @Test
  public void buildPropertiesAdapter() {
    PropertiesAdapter propertiesAdapter = PropertiesBuilder.newInstance()
      .set("booleanProperty", false)
      .set("characterProperty", 'A')
      .set("doubleProperty", 3.14159d)
      .set("integerProperty", 0)
      .set("stringProperty", "test")
      .set("enumProperty", Numbers.ZERO)
      .set("personProperty", Person.create("Jon", "Doe"))
      .buildPropertiesAdapter();

    assertThat(propertiesAdapter, is(notNullValue()));
    assertThat(propertiesAdapter.getAsType("booleanProperty", Boolean.class), is(equalTo(false)));
    assertThat(propertiesAdapter.getAsType("characterProperty", Character.class), is(equalTo('A')));
    assertThat(propertiesAdapter.getAsType("doubleProperty", Double.class), is(equalTo(3.14159d)));
    assertThat(propertiesAdapter.getAsType("integerProperty", Integer.class), is(equalTo(0)));
    assertThat(propertiesAdapter.getAsType("stringProperty", String.class), is(equalTo("test")));
    assertThat(propertiesAdapter.getAsType("enumProperty", Numbers.class), is(equalTo(Numbers.ZERO)));
    //assertThat(propertiesAdapter.getAsType("personProperty", Person.class), is(equalTo(Person.create("Jon", "Doe"))));
  }

  @Test
  public void propertiesBuilderToStringIsEqualToPropertiesToString() {
    PropertiesBuilder propertiesBuilder = PropertiesBuilder.newInstance();

    Properties properties = propertiesBuilder.set("propertyName", "propertyValue").getProperties();

    assertThat(propertiesBuilder.toString(), is(equalTo(properties.toString())));
  }

  enum Numbers {
    ZERO, ONE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE
  }

  static class Person {

    private final String firstName;
    private final String lastName;

    static Person create(String firstName, String lastName) {
      return new Person(firstName, lastName);
    }

    Person(String firstName, String lastName) {
      Assert.notNull(firstName, "firstName must not be null");
      Assert.notNull(lastName, "lastName must not be null");
      this.firstName = firstName;
      this.lastName = lastName;
    }

    String getFirstName() {
      return firstName;
    }

    String getLastName() {
      return lastName;
    }

    String getName() {
      return String.format("%1$s %2$s", getFirstName(), getLastName());
    }

    @Override
    public boolean equals(final Object obj) {
      if (obj == this) {
        return true;
      }

      if (!(obj instanceof Person)) {
        return false;
      }

      Person that = (Person) obj;

      return this.getFirstName().equals(that.getFirstName())
        && this.getLastName().equals(that.getLastName());
    }

    @Override
    public int hashCode() {
      int hashValue = 17;
      hashValue = 37 * hashValue + getFirstName().hashCode();
      hashValue = 37 * hashValue + getLastName().hashCode();
      return hashValue;
    }

    @Override
    public String toString() {
      return getName();
    }
  }

}
