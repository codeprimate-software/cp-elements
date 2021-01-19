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
import static org.cp.elements.io.IOUtils.ClassLoaderObjectInputStream;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectStreamClass;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;

import org.cp.elements.test.annotation.IntegrationTest;
import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;

/**
 * Unit and Integration Tests for {@link IOUtils}.
 *
 * @author John J. Blum
 * @see java.io.Closeable
 * @see java.io.InputStream
 * @see java.io.OutputStream
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.IOUtils
 * @see org.cp.elements.test.annotation.IntegrationTest
 * @since 1.0.0
 */
public class IOUtilsTests {

  @Test
  public void closeWithCloseableReturnsTrue() throws Exception {

    Closeable mockCloseable = mock(Closeable.class);

    assertThat(IOUtils.close(mockCloseable)).isTrue();

    verify(mockCloseable, times(1)).close();
  }

  @Test
  public void closeWithCloseableThrowingIOExceptionReturnsFalse() throws Exception {

    Closeable mockCloseable = mock(Closeable.class);

    doThrow(new IOException("test")).when(mockCloseable).close();

    assertThat(IOUtils.close(mockCloseable)).isFalse();

    verify(mockCloseable, times(1)).close();
  }

  @Test
  public void closeWithNullReturnsFalse() {
    assertThat(IOUtils.close(null)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void copyIsSuccessful() throws IOException {

    InputStream mockInputStream = mock(InputStream.class);

    OutputStream mockOutputStream = mock(OutputStream.class);

    when(mockInputStream.read(any(byte[].class))).thenReturn(IOUtils.DEFAULT_BUFFER_SIZE).thenReturn(8192).thenReturn(0);

    IOUtils.copy(mockInputStream, mockOutputStream);

    verify(mockInputStream, times(3)).read(any(byte[].class));
    verify(mockOutputStream, times(1)).write(any(byte[].class), eq(0), eq(IOUtils.DEFAULT_BUFFER_SIZE));
    verify(mockOutputStream, times(1)).write(any(byte[].class), eq(0), eq(8192));
    verify(mockOutputStream, never()).write(any(byte[].class), eq(0), eq(0));
    verify(mockOutputStream, times(2)).flush();
  }

  @Test
  @IntegrationTest
  @SuppressWarnings("all")
  public void copyActualIsSuccessful() throws IOException {

    InputStream source = new ByteArrayInputStream("This is an actual integration test!".getBytes());

    OutputStream target = new ByteArrayOutputStream(source.available());

    IOUtils.copy(source, target);

    assertThat(new String(((ByteArrayOutputStream) target).toByteArray(), StandardCharsets.UTF_8)).isEqualTo(
      "This is an actual integration test!");
  }

  @Test
  public void doIoSuccessfullyReturnsTrue() {
    assertThat(IOUtils.doSafeIo(() -> { })).isTrue();
  }

  @Test
  public void doIoThrowingIoExceptionReturnsFalse() {
    assertThat(IOUtils.doSafeIo(() -> { throw new IOException("test"); })).isFalse();
  }

  @Test
  public void serializationDeserializationIsSuccessful() throws ClassNotFoundException, IOException {
    assertThat(IOUtils.<String>deserialize(IOUtils.serialize("test"))).isEqualTo("test");
  }

  @Test
  public void serializationDeserializationWithClassLoaderIsSuccessful() throws ClassNotFoundException, IOException {
    assertThat(IOUtils.<String>deserialize(IOUtils.serialize("test"), Thread.currentThread().getContextClassLoader()))
      .isEqualTo("test");
  }

  @Test
  @SuppressWarnings("all")
  public void toByteArrayIsSuccessful() throws IOException {

    InputStream mockInputStream = mock(InputStream.class);

    when(mockInputStream.available()).thenReturn(4);

    when(mockInputStream.read(any(byte[].class))).thenAnswer((InvocationOnMock invocation) -> {
      byte[] buffer = invocation.getArgument(0);

      buffer[0] = (byte) 0xCA;
      buffer[1] = (byte) 0xFE;
      buffer[2] = (byte) 0xBA;
      buffer[3] = (byte) 0xBE;

      return 4;
    }).thenAnswer((InvocationOnMock invocation) -> -1);

    byte[] bytes = IOUtils.toByteArray(mockInputStream);

    assertThat(bytes).isNotNull();
    assertThat(bytes.length).isEqualTo(4);
    assertThat(bytes[0]).isEqualTo((byte) 0xCA);
    assertThat(bytes[1]).isEqualTo((byte) 0xFE);
    assertThat(bytes[2]).isEqualTo((byte) 0xBA);
    assertThat(bytes[3]).isEqualTo((byte) 0xBE);

    verify(mockInputStream, times(1)).available();
    verify(mockInputStream, times(2)).read(any(byte[].class));
    verify(mockInputStream, never()).close();
  }

  @Test(expected = IOException.class)
  public void toByteArrayThrowsIOException() throws IOException {

    InputStream mockInputStream = mock(InputStream.class);

    when(mockInputStream.available()).thenReturn(0);
    when(mockInputStream.read(any(byte[].class))).thenThrow(new IOException("test"));

    try {
      IOUtils.toByteArray(mockInputStream);
    }
    catch (IOException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockInputStream, times(1)).available();
      verify(mockInputStream, never()).close();
    }
  }

  @Test
  public void toByteArrayWithEmptyInputStreamReturnEmptyByteArray() throws IOException {

    InputStream mockInputStream = mock(InputStream.class);

    when(mockInputStream.available()).thenReturn(0);
    when(mockInputStream.read(any(byte[].class))).thenReturn(-1);

    byte[] byteArray = IOUtils.toByteArray(mockInputStream);

    assertThat(byteArray).isNotNull();
    assertThat(byteArray.length).isEqualTo(0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void toByteArrayWithNull() throws IOException {

    try {
      IOUtils.toByteArray(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The InputStream to read bytes from cannot be null");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void constructsClassLoaderObjectInputStreamWithInputStreamAndSystemClassLoader() throws IOException {

    InputStream in = new ByteArrayInputStream(IOUtils.serialize("test"));

    ClassLoaderObjectInputStream inputStream = new ClassLoaderObjectInputStream(in, ClassLoader.getSystemClassLoader());

    assertThat(inputStream).isNotNull();
    assertThat(inputStream.getClassLoader()).isEqualTo(ClassLoader.getSystemClassLoader());
  }

  @Test
  public void constructsClassLoaderObjectInputStreamWithInputStreamAndNullClassLoader() throws IOException {

    InputStream in = new ByteArrayInputStream(IOUtils.serialize("test"));

    ClassLoaderObjectInputStream inputStream = new ClassLoaderObjectInputStream(in, null);

    assertThat(inputStream).isNotNull();
    assertThat(inputStream.getClassLoader()).isEqualTo(Thread.currentThread().getContextClassLoader());
  }

  @Test
  public void classLoaderObjectInputStreamResolvesClass() throws ClassNotFoundException, IOException {

    InputStream in = new ByteArrayInputStream(IOUtils.serialize(1));

    ObjectStreamClass descriptor = ObjectStreamClass.lookup(Integer.class);

    assertThat(new ClassLoaderObjectInputStream(in, Thread.currentThread().getContextClassLoader())
      .resolveClass(descriptor)).isEqualTo(Integer.class);
  }
}
