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

package org.cp.elements.io;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
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
import java.io.OutputStream;
import java.nio.charset.Charset;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.invocation.InvocationOnMock;

/**
 * The IOUtilsTest class is a test suite of test cases testing the contract and functionality of the IOUtils class.
 *
 * @author John J. Blum
 * @see java.io.Closeable
 * @see java.io.InputStream
 * @see java.io.OutputStream
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.IOUtils
 * @since 1.0.0
 */
public class IOUtilsTest {

  @Rule
  public ExpectedException expectedException = ExpectedException.none();

  @Test
  public void closeIsSuccessful() throws Exception {
    Closeable mockCloseable = mock(Closeable.class);
    assertTrue(IOUtils.close(mockCloseable));
    verify(mockCloseable, times(1)).close();
  }

  @Test
  public void closeThrowsIOException() throws Exception {
    Closeable mockCloseable = mock(Closeable.class);

    doThrow(new IOException("test")).when(mockCloseable).close();

    assertFalse(IOUtils.close(mockCloseable));

    verify(mockCloseable, times(1)).close();
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
  }

  @Test
  public void copyActualIsSuccessful() throws IOException {
    InputStream source = new ByteArrayInputStream("This is a test!".getBytes());
    OutputStream target = new ByteArrayOutputStream(source.available());

    IOUtils.copy(source, target);

    assertThat(new String(((ByteArrayOutputStream) target).toByteArray(), Charset.forName("UTF-8")),
      is(equalTo("This is a test!")));
  }

  @Test
  public void serializationDeserializationIsSuccessful() throws ClassNotFoundException, IOException {
    assertThat(IOUtils.deserializeObject(IOUtils.serializeObject("TEST")), is(equalTo("TEST")));
  }

  @Test
  public void serializationDeserializationWithClassLoaderIsSuccessful() throws ClassNotFoundException, IOException {
    assertThat(IOUtils.deserializeObject(IOUtils.serializeObject("TEST"), Thread.currentThread().getContextClassLoader()),
      is(equalTo("TEST")));
  }

  @Test
  @SuppressWarnings("all")
  public void toByteArrayIsSuccessful() throws IOException {
    InputStream mockInputStream = mock(InputStream.class);

    when(mockInputStream.read(any(byte[].class))).thenAnswer((InvocationOnMock invocation) -> {
      byte[] buffer = invocation.getArgumentAt(0, byte[].class);

      buffer[0] = (byte) 0xCA;
      buffer[1] = (byte) 0xFE;
      buffer[2] = (byte) 0xBA;
      buffer[3] = (byte) 0xBE;

      return 4;
    }).thenAnswer((InvocationOnMock invocation) -> -1);

    byte[] bytes = IOUtils.toByteArray(mockInputStream);

    assertThat(bytes, is(notNullValue()));
    assertThat(bytes.length, is(equalTo(4)));
    assertThat(bytes[0], is(equalTo((byte) 0xCA)));
    assertThat(bytes[1], is(equalTo((byte) 0xFE)));
    assertThat(bytes[2], is(equalTo((byte) 0xBA)));
    assertThat(bytes[3], is(equalTo((byte) 0xBE)));

    verify(mockInputStream, times(2)).read(any(byte[].class));
    verify(mockInputStream, times(1)).close();
  }

  @Test
  public void toByteArrayThrowsIOException() throws IOException {
    InputStream mockInputStream = mock(InputStream.class);

    when(mockInputStream.read(any(byte[].class))).thenThrow(new IOException("test"));

    expectedException.expect(IOException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    IOUtils.toByteArray(mockInputStream);

    verify(mockInputStream, times(1)).close();
  }

}
