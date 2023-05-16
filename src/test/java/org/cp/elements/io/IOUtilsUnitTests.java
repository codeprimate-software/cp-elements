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
package org.cp.elements.io;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.cp.elements.io.IOUtils.ClassLoaderObjectInputStream;
import static org.cp.elements.lang.CheckedExceptionsFactory.newIOException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectStreamClass;
import java.io.OutputStream;
import java.io.Serializable;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Test;

import org.cp.elements.test.annotation.IntegrationTest;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

/**
 * Unit and Integration Tests for {@link IOUtils}.
 *
 * @author John J. Blum
 * @see java.io.Closeable
 * @see java.io.InputStream
 * @see java.io.OutputStream
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.IOUtils
 * @see org.cp.elements.test.annotation.IntegrationTest
 * @since 1.0.0
 */
public class IOUtilsUnitTests {

  @Test
  public void closeWithCloseableReturnsTrue() throws Exception {

    Closeable mockCloseable = mock(Closeable.class);

    assertThat(IOUtils.close(mockCloseable)).isTrue();

    verify(mockCloseable, times(1)).close();
    verifyNoMoreInteractions(mockCloseable);
  }

  @Test
  public void closeWithCloseableThrowingIOExceptionReturnsFalse() throws Exception {

    Closeable mockCloseable = mock(Closeable.class);

    doThrow(newIOException("test")).when(mockCloseable).close();

    assertThat(IOUtils.close(mockCloseable)).isFalse();

    verify(mockCloseable, times(1)).close();
    verifyNoMoreInteractions(mockCloseable);
  }

  @Test
  public void closeWithNullIsNullSafe() {
    assertThat(IOUtils.close(null)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void copyIsCorrect() throws IOException {

    InputStream mockInputStream = mock(InputStream.class);

    OutputStream mockOutputStream = mock(OutputStream.class);

    doReturn(IOUtils.DEFAULT_BUFFER_SIZE).doReturn(8192).doReturn(0)
      .when(mockInputStream).read(any(byte[].class));

    IOUtils.copy(mockInputStream, mockOutputStream);

    verify(mockInputStream, times(3)).read(any(byte[].class));
    verify(mockOutputStream, times(1)).write(any(byte[].class), eq(0), eq(IOUtils.DEFAULT_BUFFER_SIZE));
    verify(mockOutputStream, times(1)).write(any(byte[].class), eq(0), eq(8192));
    verify(mockOutputStream, never()).write(any(byte[].class), eq(0), eq(0));
    verify(mockOutputStream, times(2)).flush();
    verifyNoMoreInteractions(mockInputStream, mockOutputStream);
  }

  @Test
  @IntegrationTest
  public void copyActualIsCorrect() throws IOException {

    InputStream source = spy(new ByteArrayInputStream("This is an integration test!".getBytes()));

    ByteArrayOutputStream target = spy(new ByteArrayOutputStream(source.available()));

    IOUtils.copy(source, target);

    assertThat(new String(target.toByteArray(), StandardCharsets.UTF_8)).isEqualTo("This is an integration test!");

    verify(source, never()).close();
    verify(target, never()).close();
  }

  @Test
  public void copyFromNullInputStream() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> IOUtils.copy(null, mock(OutputStream.class)))
      .withMessage("InputStream is required")
      .withNoCause();
  }

  @Test
  public void copyToNullOutputStream() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> IOUtils.copy(mock(InputStream.class), null))
      .withMessage("OutputStream is required")
      .withNoCause();
  }

  @Test
  public void doSafeIoReturnsTrue() {
    assertThat(IOUtils.doSafeIo(() -> { })).isTrue();
  }

  @Test
  public void doSafeIoThrowingIoExceptionReturnsFalse() {
    assertThat(IOUtils.doSafeIo(() -> { throw newIOException("test"); })).isFalse();
  }

  @Test
  public void doSafeIoWithNull() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> IOUtils.doSafeIo(null))
      .withMessage("I/O operation is required")
      .withNoCause();
  }

  @Test
  @IntegrationTest
  public void serializeThenDeserializeIsCorrect() throws ClassNotFoundException, IOException {

    assertThat(IOUtils.<Boolean>deserialize(IOUtils.serialize(true))).isTrue();
    assertThat(IOUtils.<Character>deserialize(IOUtils.serialize('x'))).isEqualTo('x');
    assertThat(IOUtils.<Double>deserialize(IOUtils.serialize(3.14159d))).isEqualTo(3.14159d);
    assertThat(IOUtils.<Integer>deserialize(IOUtils.serialize(2))).isEqualTo(2);
    assertThat(IOUtils.<String>deserialize(IOUtils.serialize("test"))).isEqualTo("test");
  }

  @Test
  @IntegrationTest
  public void serializeThenDeserializeComplexObjectIsCorrect() throws ClassNotFoundException, IOException {

    User jonDoe = User.as("Jon Doe");

    User serializedDeserializedJonDoe = IOUtils.deserialize(IOUtils.serialize(jonDoe));

    assertThat(serializedDeserializedJonDoe).isNotNull();
    assertThat(serializedDeserializedJonDoe).isNotSameAs(jonDoe);
    assertThat(serializedDeserializedJonDoe).isEqualTo(jonDoe);
  }

  @Test
  @IntegrationTest
  public void serializeThenDeserializeWithClassLoaderIsCorrect() throws ClassNotFoundException, IOException {
    assertThat(IOUtils.<User>deserialize(IOUtils.serialize(User.as("Jane Doe")),
      Thread.currentThread().getContextClassLoader())).isEqualTo(User.as("Jane Doe"));
  }

  @Test
  public void deserializeEmptyByteArrayThrowsEOFException() {

    assertThatExceptionOfType(EOFException.class)
      .isThrownBy(() -> IOUtils.deserialize(new byte[0]))
      .withNoCause();
  }

  @Test
  @IntegrationTest
  public void deserializeNonNullByteArrayUsingNullClassLoaderIsNullSafe() throws ClassNotFoundException, IOException {
    assertThat(IOUtils.<String>deserialize(IOUtils.serialize("test"), null)).isEqualTo("test");
  }

  @Test
  public void deserializeNullByteArrayThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> IOUtils.deserialize(null))
      .withMessage("An array of bytes to deserialize as an Object is required")
      .withNoCause();
  }

  @Test
  public void deserializeNullByteArrayWithClassLoaderThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> IOUtils.deserialize(null, mock(ClassLoader.class)))
      .withMessage("An array of bytes to deserialize as an Object is required")
      .withNoCause();
  }

  @Test
  public void toByteArrayIsSuccessful() throws IOException {

    InputStream mockInputStream = mock(InputStream.class);

    doReturn(4).when(mockInputStream).available();

    doAnswer(invocation -> {
      byte[] buffer = invocation.getArgument(0);

      buffer[0] = (byte) 0xCA;
      buffer[1] = (byte) 0xFE;
      buffer[2] = (byte) 0xBA;
      buffer[3] = (byte) 0xBE;

      return 4;
    })
    .doAnswer(invocation -> -1)
      .when(mockInputStream).read(any(byte[].class));

    byte[] bytes = IOUtils.toByteArray(mockInputStream);

    assertThat(bytes).isNotNull();
    assertThat(bytes).hasSize(4);
    assertThat(bytes[0]).isEqualTo((byte) 0xCA);
    assertThat(bytes[1]).isEqualTo((byte) 0xFE);
    assertThat(bytes[2]).isEqualTo((byte) 0xBA);
    assertThat(bytes[3]).isEqualTo((byte) 0xBE);

    verify(mockInputStream, times(1)).available();
    verify(mockInputStream, times(2)).read(any(byte[].class));
    verify(mockInputStream, never()).close();
  }

  @Test
  public void toByteArrayThrowsIOException() throws IOException {

    InputStream mockInputStream = mock(InputStream.class);

    doReturn(0).when(mockInputStream).available();
    doThrow(newIOException("test")).when(mockInputStream).read(any(byte[].class));

    assertThatExceptionOfType(IOException.class)
      .isThrownBy(() -> IOUtils.toByteArray(mockInputStream))
      .withMessage("test")
      .withNoCause();

    verify(mockInputStream, times(1)).available();
    verify(mockInputStream, never()).close();
  }

  @Test
  public void toByteArrayWithEmptyInputStreamReturnsEmptyByteArray() throws IOException {

    InputStream mockInputStream = mock(InputStream.class);

    doReturn(0).when(mockInputStream).available();
    doReturn(-1).when(mockInputStream).read(any(byte[].class));

    byte[] byteArray = IOUtils.toByteArray(mockInputStream);

    assertThat(byteArray).isNotNull();
    assertThat(byteArray).isEmpty();
  }

  @Test
  public void toByteArrayWithNullInputStream() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> IOUtils.toByteArray(null))
      .withMessage("InputStream is required")
      .withNoCause();
  }

  @Test
  public void constructsClassLoaderObjectInputStreamWithInputStreamAndSystemClassLoader() throws IOException {

    InputStream in = new ByteArrayInputStream(IOUtils.serialize("test"));

    ClassLoaderObjectInputStream inputStream = new ClassLoaderObjectInputStream(in, ClassLoader.getSystemClassLoader());

    assertThat(inputStream).isNotNull();
    assertThat(inputStream.getClassLoader()).isEqualTo(ClassLoader.getSystemClassLoader());
  }

  @Test
  public void constructsClassLoaderObjectInputStreamWithInputStreamAndNullClassLoaderIsNullSafe() throws IOException {

    InputStream in = new ByteArrayInputStream(IOUtils.serialize("test"));

    ClassLoaderObjectInputStream inputStream = new ClassLoaderObjectInputStream(in, null);

    assertThat(inputStream).isNotNull();
    assertThat(inputStream.getClassLoader()).isEqualTo(ClassLoaderObjectInputStream.DEFAULT_CLASS_LOADER);
  }

  @Test
  public void classLoaderObjectInputStreamResolvesClass() throws ClassNotFoundException, IOException {

    InputStream in = new ByteArrayInputStream(IOUtils.serialize(User.as("Pie Doe")));

    ObjectStreamClass descriptor = ObjectStreamClass.lookup(User.class);

    assertThat(new ClassLoaderObjectInputStream(in, Thread.currentThread().getContextClassLoader())
      .resolveClass(descriptor)).isEqualTo(User.class);
  }

  @Getter
  @ToString(of = "name")
  @EqualsAndHashCode(of = "name")
  @RequiredArgsConstructor(staticName = "as")
  static class User implements Serializable {

    @lombok.NonNull
    private final String name;

  }
}
