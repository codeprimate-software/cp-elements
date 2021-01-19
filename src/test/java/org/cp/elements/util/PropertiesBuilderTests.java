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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.nio.CharBuffer;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.cp.elements.io.NoSuchFileException;
import org.junit.Test;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

/**
 * Unit Tests for {@link PropertiesBuilder}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.PropertiesBuilder
 * @since 1.0.0
 */
public class PropertiesBuilderTests {

  @Test
  public void fromFileLoadsProperties() throws IOException {

    File testProperties = new File("test.properties");

    testProperties.deleteOnExit();

    Properties expected = new Properties();

    expected.setProperty("one", "1");
    expected.setProperty("two", "2");
    expected.store(new FileWriter(testProperties), "Test Properties");

    PropertiesBuilder propertiesBuilder = PropertiesBuilder.from(testProperties);

    assertThat(propertiesBuilder).isNotNull();
    assertThat(propertiesBuilder.build()).isEqualTo(expected);
  }

  @Test(expected = NoSuchFileException.class)
  public void fromNonExistingFile() {

    File nonExistingFile = new File("/absolute/path/to/non/existing/file.properties");

    try {
      PropertiesBuilder.from(nonExistingFile);
    }
    catch (NoSuchFileException expected) {

      assertThat(expected).hasMessage(String.format("[%s] not found", nonExistingFile));
      assertThat(expected).hasCauseInstanceOf(FileNotFoundException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
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

    assertThat(propertiesBuilder).isNotNull();
    assertThat(propertiesBuilder.build()).isEqualTo(expected);
  }

  @Test(expected = SystemException.class)
  public void fromInputStreamThrowingIOExceptionIsHandledProperly() throws IOException {

    InputStream mockInputStream = mock(InputStream.class);

    IOException ioException = new IOException("test");

    when(mockInputStream.read()).thenThrow(ioException);
    when(mockInputStream.read(any(byte[].class))).thenThrow(ioException);
    when(mockInputStream.read(any(byte[].class), anyInt(), anyInt())).thenThrow(ioException);

    try {
      PropertiesBuilder.from(mockInputStream);
    }
    catch (SystemException expected) {

      assertThat(expected).hasMessage("Failed to load properties from input stream [%s]", mockInputStream);
      assertThat(expected).hasCauseInstanceOf(IOException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
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

    assertThat(propertiesBuilder).isNotNull();

    Properties actual = propertiesBuilder.build();

    assertThat(actual).isNotNull();
    assertThat(actual.size()).isEqualTo(expected.size());

    for (String propertyName : expected.keySet()) {
      assertThat(actual.getProperty(propertyName)).isEqualTo(String.valueOf(expected.get(propertyName)));
    }
  }

  @Test
  public void fromPropertiesLoadsProperties() {

    Properties expected = new Properties();

    expected.setProperty("one", "1");
    expected.setProperty("two", "2");

    PropertiesBuilder propertiesBuilder = PropertiesBuilder.from(expected);

    assertThat(propertiesBuilder).isNotNull();
    assertThat(propertiesBuilder.build()).isEqualTo(expected);
  }

  @Test
  public void fromReaderLoadsProperties() throws IOException {

    ByteArrayOutputStream out = new ByteArrayOutputStream();

    Properties expected = new Properties();

    expected.setProperty("one", "1");
    expected.setProperty("two", "2");
    expected.store(new OutputStreamWriter(out), "Test Properties");

    byte[] buffer = out.toByteArray();

    Properties actual = PropertiesBuilder.from(new InputStreamReader(new ByteArrayInputStream(buffer))).build();

    assertThat(actual).isNotNull();
    assertThat(actual).isEqualTo(expected);
  }

  @Test(expected = SystemException.class)
  public void fromReaderThrowingIOExceptionIsHandledProperly() throws IOException {

    Reader mockReader = mock(Reader.class);

    IOException ioException = new IOException("test");

    when(mockReader.read()).thenThrow(ioException);
    when(mockReader.read(any(char[].class))).thenThrow(ioException);
    when(mockReader.read(any(char[].class), anyInt(), anyInt())).thenThrow(ioException);
    when(mockReader.read(any(CharBuffer.class))).thenThrow(ioException);

    try {
      PropertiesBuilder.from(mockReader);
    }
    catch (SystemException expected) {

      assertThat(expected).hasMessage("Failed to load properties from reader [%s]", mockReader);
      assertThat(expected).hasCauseInstanceOf(IOException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void fromEnvironmentVariablesLoadsProperties() {

    PropertiesBuilder propertiesBuilder = PropertiesBuilder.fromEnvironmentVariables();

    assertThat(propertiesBuilder).isNotNull();
    assertThat(propertiesBuilder.build()).isEqualTo(System.getenv());
  }

  @Test
  public void fromSystemPropertiesLoadsProperties() {

    PropertiesBuilder propertiesBuilder = PropertiesBuilder.fromSystemProperties();

    assertThat(propertiesBuilder).isNotNull();
    assertThat(propertiesBuilder.build()).isEqualTo(System.getProperties());
  }

  @Test
  public void fromXmlLoadsProperties() throws IOException {

    Properties source = new Properties();

    source.setProperty("keyOne", "testOne");
    source.setProperty("keyTwo", "testTwo");

    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

    source.storeToXML(outputStream, "Test XML Properties");

    byte[] xmlProperties = outputStream.toByteArray();

    ByteArrayInputStream inputStream = new ByteArrayInputStream(xmlProperties);

    Properties target = PropertiesBuilder.fromXml(inputStream).build();

    assertThat(target).isNotNull();
    assertThat(target.size()).isEqualTo(source.size());
    assertThat(target).isEqualTo(source);
  }

  @Test(expected = SystemException.class)
  public void fromXmlThrowingIOExceptionIsHandledProperly() throws IOException {

    InputStream mockInputStream = mock(InputStream.class);

    IOException ioException = new IOException("test");

    when(mockInputStream.read()).thenThrow(ioException);
    when(mockInputStream.read(any(byte[].class))).thenThrow(ioException);
    when(mockInputStream.read(any(byte[].class), anyInt(), anyInt())).thenThrow(ioException);

    try {
      PropertiesBuilder.fromXml(mockInputStream);
    }
    catch (SystemException expected) {

      assertThat(expected).hasMessage("Failed to load properties from input stream [%s]", mockInputStream);
      assertThat(expected).hasCauseInstanceOf(IOException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void constructPropertiesBuilderWithNoProperties() {

    PropertiesBuilder propertiesBuilder = new PropertiesBuilder();
    Properties properties = propertiesBuilder.getProperties();

    assertThat(properties).isNotNull();
    assertThat(properties.isEmpty()).isTrue();
  }

  @Test
  public void constructPropertiesBuilderWithProperties() {

    Properties expected = new Properties();

    expected.setProperty("one", "1");
    expected.setProperty("two", "2");

    PropertiesBuilder propertiesBuilder = new PropertiesBuilder(expected);

    Properties actual = propertiesBuilder.getProperties();

    assertThat(actual).isNotNull();
    assertThat(actual).isNotSameAs(expected);
    assertThat(actual).isEqualTo(expected);
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
      .set("personProperty", Person.newPerson("Jon", "Doe"))
      .build();

    Properties expected = new Properties();

    expected.setProperty("booleanProperty", "false");
    expected.setProperty("characterProperty", "A");
    expected.setProperty("doubleProperty", "3.14159");
    expected.setProperty("integerProperty", "0");
    expected.setProperty("stringProperty", "test");
    expected.setProperty("enumProperty", "ZERO");
    expected.setProperty("personProperty", "Jon Doe");

    assertThat(actual).isNotNull();
    assertThat(actual).isEqualTo(expected);
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
      .set("personProperty", Person.newPerson("Jon", "Doe"))
      .buildPropertiesAdapter();

    assertThat(propertiesAdapter).isNotNull();
    assertThat(propertiesAdapter.getAsType("booleanProperty", Boolean.class)).isFalse();
    assertThat(propertiesAdapter.getAsType("characterProperty", Character.class)).isEqualTo('A');
    assertThat(propertiesAdapter.getAsType("doubleProperty", Double.class)).isEqualTo(3.14159d);
    assertThat(propertiesAdapter.getAsType("integerProperty", Integer.class)).isEqualTo(0);
    assertThat(propertiesAdapter.getAsType("stringProperty", String.class)).isEqualTo("test");
    assertThat(propertiesAdapter.getAsType("enumProperty", Numbers.class)).isEqualTo(Numbers.ZERO);
    //assertThat(propertiesAdapter.getAsType("personProperty", Person.class), is(equalTo(Person.newPerson("Jon", "Doe"))));
  }

  @Test
  public void propertiesBuilderToStringIsEqualToPropertiesToString() {

    PropertiesBuilder propertiesBuilder = PropertiesBuilder.newInstance();

    Properties properties = propertiesBuilder.set("propertyName", "propertyValue").getProperties();

    assertThat(propertiesBuilder.toString()).isEqualTo(properties.toString());
  }

  enum Numbers {
    ZERO, ONE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE
  }

  @Getter
  @EqualsAndHashCode
  @RequiredArgsConstructor(staticName = "newPerson")
  static class Person {

    @NonNull
    private final String firstName;

    @NonNull
    private final String lastName;

    @Override
    public String toString() {
      return String.format("%1$s %2$s", getFirstName(), getLastName());
    }
  }
}
