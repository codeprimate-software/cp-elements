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
package org.cp.elements.net;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketAddress;

import org.junit.Test;

/**
 * Unit Tests for {@link NetworkUtils}.
 *
 * @author John J. Blum
 * @see java.net.InetSocketAddress
 * @see java.net.ServerSocket
 * @see java.net.Socket
 * @see java.net.SocketAddress
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.net.NetworkUtils
 * @since 1.0.0
 */
public class NetworkUtilsTests {

  private static final int COUNT = 100;

  @Test
  public void availablePortReturnsNonZeroPortGreaterThan1024AndLessThan65536() {

    for (int count = 0, port = NetworkUtils.availablePort(); count < COUNT;
         count++, port = NetworkUtils.availablePort()) {

      assertThat(port).isGreaterThan(ServicePort.MIN_PORT);
      assertThat(port).isLessThanOrEqualTo(ServicePort.MAX_PORT);
    }
  }

  @Test
  public void closeServerSocketReturnsTrue() throws Exception {

    ServerSocket mockServerSocket = mock(ServerSocket.class);

    assertThat(NetworkUtils.close(mockServerSocket)).isTrue();

    verify(mockServerSocket, times(1)).close();
  }

  @Test
  public void closeServerSocketReturnsFalse() throws Exception {

    ServerSocket mockServerSocket = mock(ServerSocket.class);

    doThrow(new IOException("test")).when(mockServerSocket).close();

    assertThat(NetworkUtils.close(mockServerSocket)).isFalse();

    verify(mockServerSocket, times(1)).close();
  }

  @Test
  public void closeNullServerSocketReturnsFalse() {
    assertThat(NetworkUtils.close((ServerSocket) null)).isFalse();
  }

  @Test
  public void closeSocketReturnsTrue() throws Exception {

    Socket mockSocket = mock(Socket.class);

    assertThat(NetworkUtils.close(mockSocket)).isTrue();

    verify(mockSocket, times(1)).close();
  }

  @Test
  public void closeSocketReturnsFalse() throws Exception {

    Socket mockSocket = mock(Socket.class);

    doThrow(new IOException("test")).when(mockSocket).close();

    assertThat(NetworkUtils.close(mockSocket)).isFalse();

    verify(mockSocket, times(1)).close();
  }

  @Test
  public void closeNullSocketReturnsFalse() {
    assertThat(NetworkUtils.close((Socket) null)).isFalse();
  }

  @Test
  public void lenientParsePortIsSuccessful() {
    assertThat(NetworkUtils.lenientParsePort("1234")).isEqualTo(1234);
  }

  @Test
  public void lenientParsePortWithHostnamePortNumberIsSuccessful() {
    assertThat(NetworkUtils.lenientParsePort("skullbox:8080")).isEqualTo(8080);
  }

  @Test
  public void lenientParsePortWithMixedPortNumberIsSuccessful() {
    assertThat(NetworkUtils.lenientParsePort("1 2#4%O L^B8(-?n 0 I")).isEqualTo(12480);
  }

  @Test
  public void lenientParsePortWithInvalidPortNumberHavingDefaultPortReturnsDefaultPort() {
    assertThat(NetworkUtils.lenientParsePort("$O.OOL", 1234)).isEqualTo(1234);
  }

  @Test(expected = IllegalArgumentException.class)
  public void lenientParsePortWithInvalidPortNumberThrowsIllegalArgumentExceptoin() {

    try {
      NetworkUtils.lenientParsePort("invalid");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Port [] is not valid");
      assertThat(expected).hasCauseInstanceOf(NumberFormatException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void parsePortIsSuccessful() {

    assertThat(NetworkUtils.parsePort("1234")).isEqualTo(1234);
    assertThat(NetworkUtils.parsePort("  5678 ")).isEqualTo(5678);
  }

  @Test
  public void parsePortWithInvalidPortNumberAndDefaultPortReturnsDefaultPort() {
    assertThat(NetworkUtils.parsePort(" 1  23 4", 5678)).isEqualTo(5678);
  }

  @Test(expected = IllegalArgumentException.class)
  public void parsePortWithInvalidPortNumberThrowsIllegalArgumentException() {

    try {
      NetworkUtils.parsePort("invalid");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Port [invalid] is not valid");
      assertThat(expected).hasCauseInstanceOf(NumberFormatException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void newSocketAddressWithHostAndPort() {

    SocketAddress socketAddress = NetworkUtils.newSocketAddress("skullbox", 9876);

    assertThat(socketAddress).isInstanceOf(InetSocketAddress.class);
    assertThat(((InetSocketAddress) socketAddress).getHostName()).isEqualTo("skullbox");
    assertThat(((InetSocketAddress) socketAddress).getPort()).isEqualTo(9876);
  }

  @Test
  public void newSocketAddressWithNullHostAndPort() {

    SocketAddress socketAddress = NetworkUtils.newSocketAddress(null, 9876);

    assertThat(socketAddress).isInstanceOf(InetSocketAddress.class);
    assertThat(((InetSocketAddress) socketAddress).getHostName()).isEqualTo("0.0.0.0");
    assertThat(((InetSocketAddress) socketAddress).getPort()).isEqualTo(9876);
  }

  @Test
  public void newSocketAddressWithPort() {

    SocketAddress socketAddress = NetworkUtils.newSocketAddress(1234);

    assertThat(socketAddress).isInstanceOf(InetSocketAddress.class);
    assertThat(((InetSocketAddress) socketAddress).getHostName()).isEqualTo("0.0.0.0");
    assertThat(((InetSocketAddress) socketAddress).getPort()).isEqualTo(1234);
  }
}
