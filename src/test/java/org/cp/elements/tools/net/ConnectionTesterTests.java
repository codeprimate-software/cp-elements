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
package org.cp.elements.tools.net;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.cp.elements.lang.CheckedExceptionsFactory.newIOException;
import static org.cp.elements.lang.NumberUtils.intValue;
import static org.cp.elements.tools.net.ConnectionTester.newConnectionTester;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.net.Socket;
import java.net.SocketAddress;

import org.junit.jupiter.api.Test;

/**
 * Unit Rests for {@link ConnectionTester}.
 *
 * @author John Blum
 * @see java.net.Socket
 * @see java.net.SocketAddress
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.cp.elements.tools.net.ConnectionTester
 * @since 1.0.0
 */
public class ConnectionTesterTests {

  @Test
  public void newConnectionTestWithPort() {

    ConnectionTester connectionTester = newConnectionTester(1234);

    assertThat(connectionTester).isNotNull();
    assertThat(connectionTester.getHost()).isNull();
    assertThat(connectionTester.getPort()).isEqualTo(1234);
  }

  @Test
  public void newConnectionTesterWithHostAndPort() {

    ConnectionTester connectionTester = newConnectionTester("skullbox", 1234);

    assertThat(connectionTester).isNotNull();
    assertThat(connectionTester.getHost()).isEqualTo("skullbox");
    assertThat(connectionTester.getPort()).isEqualTo(1234);
  }

  @Test
  public void constructConnectionTester() {

    ConnectionTester connectionTester = new ConnectionTester("localhost", 9876);

    assertThat(connectionTester.getHost()).isEqualTo("localhost");
    assertThat(connectionTester.getPort()).isEqualTo(9876);
  }

  @Test
  public void constructConnectionTesterWithNegativePortThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ConnectionTester("localhost", -1))
      .withMessage("[-1] is not a valid port number")
      .withNoCause();
  }

  @Test
  public void constructConnectionTesterWithPortExceedingMaximumThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ConnectionTester("localhost", 123456789))
      .withMessage("[123456789] is not a valid port number")
      .withNoCause();
  }

  @Test
  public void constructConnectionTesterWithBlankHostReturnsNullHost() {
    assertThat(new ConnectionTester("  ", 1234).getHost()).isNull();
  }

  @Test
  public void constructConnectionTesterWithEmptyHostReturnsNullHost() {
    assertThat(new ConnectionTester("", 1234).getHost()).isNull();
  }

  @Test
  public void setGetAndIsConnected() {

    ConnectionTester connectionTester = newConnectionTester(1234);

    assertThat(connectionTester).isNotNull();
    assertThat(connectionTester.isConnected()).isFalse();
    assertThat(connectionTester.isNotConnected()).isTrue();
    assertThat(connectionTester.setConnected(true)).isSameAs(connectionTester);
    assertThat(connectionTester.isConnected()).isTrue();
    assertThat(connectionTester.isNotConnected()).isFalse();
    assertThat(connectionTester.setConnected(false)).isSameAs(connectionTester);
    assertThat(connectionTester.isConnected()).isFalse();
    assertThat(connectionTester.isNotConnected()).isTrue();
  }

  @Test
  public void evaluateCallsTest() {

    ConnectionTester connectionTester = spy(newConnectionTester(1234));

    assertThat(connectionTester).isNotNull();

    doReturn(true).when(connectionTester).test();

    assertThat(connectionTester.evaluate()).isTrue();

    verify(connectionTester, times(1)).test();
  }

  @Test
  public void retestCallsSetConnectedWithFalseAndTest() {

    ConnectionTester connectionTester = spy(newConnectionTester(1234));

    assertThat(connectionTester).isNotNull();
    assertThat(connectionTester.setConnected(true)).isSameAs(connectionTester);
    assertThat(connectionTester.isConnected()).isTrue();

    doReturn(true).when(connectionTester).test();

    assertThat(connectionTester.retest()).isTrue();

    assertThat(connectionTester.isConnected()).isFalse();

    verify(connectionTester, times(1)).setConnected(eq(false));
    verify(connectionTester, times(1)).test();
  }

  @Test
  public void testConnectsSuccessfully() throws IOException {

    ConnectionTester connectionTester = spy(newConnectionTester(1234));

    Socket mockSocket = mock(Socket.class);

    assertThat(connectionTester).isNotNull();

    doReturn(mockSocket).when(connectionTester).newSocket();
    when(mockSocket.isConnected()).thenReturn(true);

    assertThat(connectionTester.isConnected()).isFalse();
    assertThat(connectionTester.test()).isTrue();
    assertThat(connectionTester.isConnected()).isTrue();
    assertThat(connectionTester.test()).isTrue();
    assertThat(connectionTester.isConnected()).isTrue();

    verify(connectionTester, times(1)).newSocket();
    verify(connectionTester, times(1)).setConnected(eq(true));
    verify(mockSocket, times(1)).setReuseAddress(eq(ConnectionTester.SO_REUSEADDR));
    verify(mockSocket, times(1))
      .connect(isA(SocketAddress.class), eq(intValue(ConnectionTester.CONNECT_TIMEOUT)));
    verify(mockSocket, times(1)).isConnected();
    verify(mockSocket, times(1)).close();
    verifyNoMoreInteractions(mockSocket);
  }

  @Test
  public void testFailsToConnect() throws IOException {

    ConnectionTester connectionTester = spy(newConnectionTester(1234));

    Socket mockSocket = mock(Socket.class);

    assertThat(connectionTester).isNotNull();

    doReturn(mockSocket).when(connectionTester).newSocket();
    when(mockSocket.isConnected()).thenReturn(false);

    assertThat(connectionTester.isConnected()).isFalse();
    assertThat(connectionTester.test()).isFalse();
    assertThat(connectionTester.isConnected()).isFalse();
    assertThat(connectionTester.test()).isFalse();
    assertThat(connectionTester.isConnected()).isFalse();

    verify(connectionTester, times(2)).newSocket();
    verify(connectionTester, times(2)).setConnected(eq(false));
    verify(mockSocket, times(2)).setReuseAddress(eq(ConnectionTester.SO_REUSEADDR));
    verify(mockSocket, times(2))
      .connect(isA(SocketAddress.class), eq(intValue(ConnectionTester.CONNECT_TIMEOUT)));
    verify(mockSocket, times(2)).isConnected();
    verify(mockSocket, times(2)).close();
    verifyNoMoreInteractions(mockSocket);
  }

  @Test
  public void testHandlesIOException() throws IOException {

    ConnectionTester connectionTester = spy(newConnectionTester(1234));

    Socket mockSocket = mock(Socket.class);

    assertThat(connectionTester).isNotNull();

    doReturn(mockSocket).when(connectionTester).newSocket();
    doThrow(newIOException("test")).when(mockSocket).connect(any(SocketAddress.class), anyInt());

    assertThat(connectionTester.isConnected()).isFalse();
    assertThat(connectionTester.test()).isFalse();
    assertThat(connectionTester.isConnected()).isFalse();

    verify(connectionTester, times(1)).newSocket();
    verify(connectionTester, never()).setConnected(eq(false));
    verify(mockSocket, times(1)).setReuseAddress(eq(ConnectionTester.SO_REUSEADDR));
    verify(mockSocket, times(1))
      .connect(isA(SocketAddress.class), eq(intValue(ConnectionTester.CONNECT_TIMEOUT)));
    verify(mockSocket, never()).isConnected();
    verify(mockSocket, times(1)).close();
    verifyNoMoreInteractions(mockSocket);
  }
}
