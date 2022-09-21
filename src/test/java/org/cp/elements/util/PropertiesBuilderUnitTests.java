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
import static org.cp.elements.lang.ThrowableAssertions.assertThatIllegalArgumentException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatThrowableOfType;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;

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
import java.io.Writer;
import java.nio.CharBuffer;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.junit.Test;

import org.cp.elements.io.NoSuchFileException;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.test.annotation.IntegrationTest;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

/**
 * Unit Tests for {@link PropertiesBuilder}.
 *
 * @author John J. Blum
 * @see java.util.Properties
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.PropertiesBuilder
 * @since 1.0.0
 */
public class PropertiesBuilderUnitTests {

  @Test
  @IntegrationTest
  public void fromFileLoadsProperties() throws IOException {

    File testProperties = new File("test.properties");

    testProperties.deleteOnExit();

    Properties expected = new Properties();

    expected.setProperty("one", "1");
    expected.setProperty("two", "2");

    try (Writer writer = new FileWriter(testProperties)) {
      expected.store(writer, "Test File Properties");
    }

    PropertiesBuilder propertiesBuilder = PropertiesBuilder.from(testProperties);

    assertThat(propertiesBuilder).isNotNull();
    assertThat(propertiesBuilder.build()).isEqualTo(expected);
  }

  @Test
  public void fromNonExistingFile() {

    File nonExistingFile = new File("/absolute/path/to/non/existing/file.properties");

    assertThatThrowableOfType(NoSuchFileException.class)
      .isThrownBy(args -> PropertiesBuilder.from(nonExistingFile))
      .havingMessage("[%s] not found", nonExistingFile)
      .causedBy(FileNotFoundException.class)
      .withNoCause();
  }

  @Test
  public void fromNullFile() {

    assertThatIllegalArgumentException()
      .isThrownBy(args -> PropertiesBuilder.from((File) null))
      .havingMessage("Properties file is required")
      .withNoCause();
  }

  @Test
  public void fromInputStreamLoadsProperties() throws IOException {

    Properties expected = new Properties();

    expected.setProperty("keyOne", "valueOne");
    expected.setProperty("keyTwo", "valueTwo");

    byte[] buffer;

    try (ByteArrayOutputStream out = new ByteArrayOutputStream()) {
      expected.store(out, "Test InputStream Properties");
      buffer = out.toByteArray();
    }

    PropertiesBuilder propertiesBuilder;

    try (ByteArrayInputStream in = new ByteArrayInputStream(buffer)) {
      propertiesBuilder = PropertiesBuilder.from(in);
    }

    assertThat(propertiesBuilder).isNotNull();
    assertThat(propertiesBuilder.build()).isEqualTo(expected);
  }

  @Test
  public void fromInputStreamThrowingIOException() throws IOException {

    InputStream mockInputStream = mock(InputStream.class);

    IOException ioException = new IOException("test");

    doThrow(ioException).when(mockInputStream).read();
    doThrow(ioException).when(mockInputStream).read(any(byte[].class));
    doThrow(ioException).when(mockInputStream).read(any(byte[].class), anyInt(), anyInt());

    assertThatThrowableOfType(SystemException.class)
      .isThrownBy(args -> PropertiesBuilder.from(mockInputStream))
      .havingMessage("Failed to load properties from input stream [%s]", mockInputStream)
      .causedBy(IOException.class)
      .havingMessage("test")
      .withNoCause();
  }

  @Test
  public void fromNullInputStream() {

    assertThatIllegalArgumentException()
      .isThrownBy(args -> PropertiesBuilder.from((InputStream) null))
      .havingMessage("InputStream is required")
      .withNoCause();
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
    assertThat(actual).hasSize(expected.size());
    expected.forEach((key, value) -> assertThat(actual.getProperty(key)).isEqualTo(String.valueOf(value)));
  }

  @Test
  public void fromMapWithNullKeys() {

    Map<String, Object> map = MapBuilder.<String, Object>newHashMap()
      .put("mockKey", "mockValue")
      .put(null, "testValue")
      .build();

    assertThatIllegalArgumentException()
      .isThrownBy(args -> PropertiesBuilder.from(map))
      .havingMessage("Map must not contain null keys")
      .withNoCause();
  }

  @Test
  public void fromNullMap() {

    assertThatIllegalArgumentException()
      .isThrownBy(args -> PropertiesBuilder.from((Map<String, ?>) null))
      .havingMessage("Map is required")
      .withNoCause();
  }

  @Test
  public void fromPropertiesLoadsProperties() {

    Properties expected = new Properties();

    expected.setProperty("keyOne", "valueOne");
    expected.setProperty("keyTwo", "valueTwo");

    PropertiesBuilder propertiesBuilder = PropertiesBuilder.from(expected);

    assertThat(propertiesBuilder).isNotNull();
    assertThat(propertiesBuilder.build()).isEqualTo(expected);
  }

  @Test
  public void fromNullPropertiesIsNullSafe() {

    PropertiesBuilder propertiesBuilder = PropertiesBuilder.from((Properties) null);

    assertThat(propertiesBuilder).isNotNull();
    assertThat(propertiesBuilder.build()).isEmpty();
  }

  @Test
  public void fromReaderLoadsProperties() throws IOException {

    ByteArrayOutputStream out = new ByteArrayOutputStream();

    Properties expected = new Properties();

    expected.setProperty("keyOne", "valueOne");
    expected.setProperty("keyTwo", "valueTwo");

    try (Writer writer = new OutputStreamWriter(out)) {
      expected.store(writer, "Test Reader Properties");
    }

    byte[] buffer = out.toByteArray();

    Properties actual;

    try (Reader reader = new InputStreamReader(new ByteArrayInputStream(buffer))) {
      actual = PropertiesBuilder.from(reader).build();
    }

    assertThat(actual).isNotNull();
    assertThat(actual).hasSize(expected.size());
    assertThat(actual).isEqualTo(expected);
  }

  @Test
  public void fromNullReader() {

    assertThatIllegalArgumentException()
      .isThrownBy(args -> PropertiesBuilder.from((Reader) null))
      .havingMessage("Reader is required")
      .withNoCause();
  }

  @Test
  public void fromReaderThrowingIOException() throws IOException {

    Reader mockReader = mock(Reader.class);

    IOException ioException = new IOException("test");

    doThrow(ioException).when(mockReader).read();
    doThrow(ioException).when(mockReader).read(any(char[].class));
    doThrow(ioException).when(mockReader).read(any(char[].class), anyInt(), anyInt());
    doThrow(ioException).when(mockReader).read(any(CharBuffer.class));

    assertThatThrowableOfType(SystemException.class)
      .isThrownBy(args -> PropertiesBuilder.from(mockReader))
      .havingMessage("Failed to load properties from reader [%s]", mockReader)
      .causedBy(IOException.class)
      .havingMessage("test")
      .withNoCause();
  }

  @Test
  public void fromAssociativeArrayLoadsProperties() {

    String[] associativeArray = {
      "booleanProperty=true",
      "characterProperty=x",
      "doubleProperty=3.14159",
      "integerProperty=2",
      "stringProperty=test"
    };

    PropertiesBuilder propertiesBuilder = PropertiesBuilder.fromAssociativeArray(associativeArray);

    assertThat(propertiesBuilder).isNotNull();
    assertThat(propertiesBuilder.getProperties()).isNotNull();
    assertThat(propertiesBuilder.getProperties()).hasSize(5);
    assertThat(propertiesBuilder.getProperties().getProperty("booleanProperty")).isEqualTo("true");
    assertThat(propertiesBuilder.getProperties().getProperty("characterProperty")).isEqualTo("x");
    assertThat(propertiesBuilder.getProperties().getProperty("doubleProperty")).isEqualTo("3.14159");
    assertThat(propertiesBuilder.getProperties().getProperty("integerProperty")).isEqualTo("2");
    assertThat(propertiesBuilder.getProperties().getProperty("stringProperty")).isEqualTo("test");
  }

  @Test
  public void fromEmptyAssociativeArray() {

    PropertiesBuilder propertiesBuilder = PropertiesBuilder.fromAssociativeArray(StringUtils.EMPTY_STRING_ARRAY);

    assertThat(propertiesBuilder).isNotNull();
    assertThat(propertiesBuilder.getProperties()).isNotNull();
    assertThat(propertiesBuilder.getProperties()).isEmpty();
  }

  @Test
  public void fromNullAssociativeArray() {

    PropertiesBuilder propertiesBuilder = PropertiesBuilder.fromAssociativeArray(null);

    assertThat(propertiesBuilder).isNotNull();
    assertThat(propertiesBuilder.getProperties()).isNotNull();
    assertThat(propertiesBuilder.getProperties()).isEmpty();
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

    source.setProperty("keyOne", "valueOne");
    source.setProperty("keyTwo", "valueTwo");

    byte[] xmlProperties;

    try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
      source.storeToXML(outputStream, "Test XML Properties");
      xmlProperties = outputStream.toByteArray();
    }

    Properties target;

    try (ByteArrayInputStream inputStream = new ByteArrayInputStream(xmlProperties)) {
      target = PropertiesBuilder.fromXml(inputStream).build();
    }

    assertThat(target).isNotNull();
    assertThat(target).hasSize(source.size());
    assertThat(target).isEqualTo(source);
  }

  @Test
  public void fromXmlThrowingIOException() throws IOException {

    InputStream mockInputStream = mock(InputStream.class);

    IOException ioException = new IOException("test");

    doThrow(ioException).when(mockInputStream).read();
    doThrow(ioException).when(mockInputStream).read(any(byte[].class));
    doThrow(ioException).when(mockInputStream).read(any(byte[].class), anyInt(), anyInt());

    assertThatThrowableOfType(SystemException.class)
      .isThrownBy(args -> PropertiesBuilder.fromXml(mockInputStream))
      .havingMessage("Failed to load properties from input stream [%s]", mockInputStream)
      .causedBy(IOException.class)
      .havingMessage("test")
      .withNoCause();
  }

  @Test
  public void fromXmlWithNullInputStream() {

    assertThatIllegalArgumentException()
      .isThrownBy(args -> PropertiesBuilder.fromXml(null))
      .havingMessage("InputStream is required")
      .withNoCause();
  }

  @Test
  public void newInstanceIsCorrect() {

    PropertiesBuilder propertiesBuilder = PropertiesBuilder.newInstance();

    assertThat(propertiesBuilder).isNotNull();
    assertThat(propertiesBuilder.getProperties()).isEmpty();
  }

  @Test
  public void constructPropertiesBuilderWithNoProperties() {

    PropertiesBuilder propertiesBuilder = new PropertiesBuilder();
    Properties properties = propertiesBuilder.getProperties();

    assertThat(properties).isNotNull();
    assertThat(properties).isEmpty();
  }

  @Test
  public void constructPropertiesBuilderWithProperties() {

    Properties expected = new Properties();

    expected.setProperty("keyOne", "valueOne");
    expected.setProperty("keyTwo", "valueTwo");

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
