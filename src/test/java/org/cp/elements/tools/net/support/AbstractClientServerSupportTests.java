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
package org.cp.elements.tools.net.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.lang.CheckedExceptionsFactory.newIOException;
import static org.cp.elements.lang.NumberUtils.intValue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketAddress;

import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;

import org.cp.elements.lang.ThrowableAssertions;
import org.cp.elements.test.annotation.SubjectUnderTest;

import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit Tests for {@link AbstractClientServerSupport}.
 *
 * @author John Blum
 * @see java.io.InputStream
 * @see java.io.OutputStream
 * @see java.net.ServerSocket
 * @see java.net.Socket
 * @see org.junit.jupiter.api.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.tools.net.support.AbstractClientServerSupport
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class AbstractClientServerSupportTests {

  @SubjectUnderTest
  private final AbstractClientServerSupport clientServerSupport = spy(new TestClientServerSupport());

  @Mock
  private InputStream mockInputStream;

  @Mock
  private OutputStream mockOutputStream;

  @Mock
  private PrintWriter mockPrintWriter;

  @Mock
  private ServerSocket mockServerSocket;

  @Mock
  private Socket mockSocket;

  @Test
  public void isRunningWithNonClosedBoundServerSocketReturnsTrue() {

    when(mockServerSocket.isClosed()).thenReturn(false);
    when(mockServerSocket.isBound()).thenReturn(true);

    assertThat(clientServerSupport.isRunning(mockServerSocket)).isTrue();

    verify(mockServerSocket, times(1)).isClosed();
    verify(mockServerSocket, times(1)).isBound();
  }

  @Test
  public void isRunningWithClosedServerSocketReturnsFalse() {

    when(mockServerSocket.isClosed()).thenReturn(true);

    assertThat(clientServerSupport.isRunning(mockServerSocket)).isFalse();

    verify(mockServerSocket, times(1)).isClosed();
    verify(mockServerSocket, never()).isBound();
  }

  @Test
  public void isRunningWithUnboundServerSocketReturnsFalse() {

    when(mockServerSocket.isClosed()).thenReturn(false);
    when(mockServerSocket.isBound()).thenReturn(false);

    assertThat(clientServerSupport.isRunning(mockServerSocket)).isFalse();

    verify(mockServerSocket, times(1)).isClosed();
    verify(mockServerSocket, times(1)).isBound();
  }

  @Test
  public void isRunningWithNullServerSocketReturnsFalse() {
    assertThat(clientServerSupport.isRunning(null)).isFalse();
  }

  @Test
  public void newBufferedReaderIsSuccessful() throws IOException {

    when(mockSocket.getInputStream()).thenReturn(mockInputStream);

    assertThat(clientServerSupport.newBufferedReader(mockSocket)).isNotNull();

    verify(mockSocket, times(1)).getInputStream();
  }

  @Test
  public void newPrintWriterIsSuccessful() throws IOException {

    when(mockSocket.getOutputStream()).thenReturn(mockOutputStream);

    assertThat(clientServerSupport.newPrintWriter(mockSocket)).isNotNull();

    verify(mockSocket, times(1)).getOutputStream();
  }

  @Test
  public void newServerSocketIsSuccessful() throws IOException {

    doReturn(mockServerSocket).when(clientServerSupport).newServerSocket();

    assertThat(clientServerSupport.newServerSocket(1234)).isNotNull();

    verify(clientServerSupport, times(1)).newServerSocket();
    verify(mockServerSocket, times(1))
      .setReuseAddress(eq(AbstractClientServerSupport.DEFAULT_REUSE_ADDRESS));
    verify(mockServerSocket, times(1)).bind(isA(SocketAddress.class));
  }

  @Test
  public void newServerSocketHandlesIOException() throws IOException {

    ThrowableAssertions.assertThatRuntimeException()
      .isThrownBy(args -> {

        doReturn(mockServerSocket).when(clientServerSupport).newServerSocket();
        doThrow(newIOException("test")).when(mockServerSocket).bind(any(SocketAddress.class));

        clientServerSupport.newServerSocket(1234);

        return null;
      })
      .havingMessage("Failed to create a ServerSocket on port [1234]")
      .causedBy(IOException.class)
      .havingMessage("test")
      .withNoCause();

    verify(clientServerSupport, times(1)).newServerSocket();
    verify(mockServerSocket, times(1))
      .setReuseAddress(eq(AbstractClientServerSupport.DEFAULT_REUSE_ADDRESS));
    verify(mockServerSocket, times(1)).bind(isA(SocketAddress.class));
  }

  @Test
  public void newSocketIsSuccessful() throws IOException {

    doReturn(mockSocket).when(clientServerSupport).newSocket();

    assertThat(clientServerSupport.newSocket("skullbox", 1234)).isNotNull();

    verify(clientServerSupport, times(1)).newSocket();
    verify(mockSocket, times(1))
      .setReuseAddress(eq(AbstractClientServerSupport.DEFAULT_REUSE_ADDRESS));
    verify(mockSocket, times(1))
      .setSoTimeout(eq(intValue(AbstractClientServerSupport.DEFAULT_SO_TIMEOUT)));
    verify(mockSocket, times(1)).connect(isA(SocketAddress.class));
  }

  @Test
  public void newSocketHandlesIOException() throws IOException {

    ThrowableAssertions.assertThatRuntimeException()
      .isThrownBy(args -> {

        doReturn(mockSocket).when(clientServerSupport).newSocket();
        doThrow(newIOException("test")).when(mockSocket).connect(any(SocketAddress.class));

        clientServerSupport.newSocket("skullbox", 1234);

        return null;
      })
      .havingMessage("Failed to create a client Socket on host [skullbox] and port [1234]")
      .causedBy(IOException.class)
      .havingMessage("test")
      .withNoCause();

    verify(clientServerSupport, times(1)).newSocket();
    verify(mockSocket, times(1))
      .setReuseAddress(eq(AbstractClientServerSupport.DEFAULT_REUSE_ADDRESS));
    verify(mockSocket, times(1))
      .setSoTimeout(eq(intValue(AbstractClientServerSupport.DEFAULT_SO_TIMEOUT)));
    verify(mockSocket, times(1)).connect(isA(SocketAddress.class));
  }

  @Test
  public void receiveMessageIsSuccessful() throws IOException {

    when(mockSocket.getInputStream()).thenReturn(new ByteArrayInputStream("This is the end of the line.\n".getBytes()));

    assertThat(clientServerSupport.receiveMessage(mockSocket)).isEqualTo("This is the end of the line.");

    verify(mockSocket, times(1)).getInputStream();
  }

  @Test
  public void setMessageIsSuccessful() throws IOException {

    doReturn(mockPrintWriter).when(clientServerSupport).newPrintWriter(any(Socket.class));

    assertThat(clientServerSupport.sendMessage(mockSocket, "test")).isSameAs(mockSocket);

    verify(clientServerSupport, times(1)).newPrintWriter(eq(mockSocket));
    verify(mockPrintWriter, times(1)).println(eq("test"));
    verify(mockPrintWriter, times(1)).flush();
    verifyNoInteractions(mockSocket);
  }

  static class TestClientServerSupport extends AbstractClientServerSupport { }

}
