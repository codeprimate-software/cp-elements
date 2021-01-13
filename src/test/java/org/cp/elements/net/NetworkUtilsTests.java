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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.isA;
import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketAddress;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for {@link NetworkUtils}.
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

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Test
  public void availablePortReturnsNonZeroPortGreaterThan1024AndLessThan65536() {
    for (int count = 0, port = NetworkUtils.availablePort(); count < COUNT;
         count++, port = NetworkUtils.availablePort()) {
      assertThat(port, is(greaterThan(ServicePort.MIN_PORT)));
      assertThat(port, is(lessThanOrEqualTo(ServicePort.MAX_PORT)));
    }
  }

  @Test
  public void closeServerSocketReturnsTrue() throws Exception {
    ServerSocket mockServerSocket = mock(ServerSocket.class);

    assertThat(NetworkUtils.close(mockServerSocket), is(true));

    verify(mockServerSocket, times(1)).close();
  }

  @Test
  public void closeServerSocketReturnsFalse() throws Exception {
    ServerSocket mockServerSocket = mock(ServerSocket.class);

    doThrow(new IOException("test")).when(mockServerSocket).close();

    assertThat(NetworkUtils.close(mockServerSocket), is(false));

    verify(mockServerSocket, times(1)).close();
  }

  @Test
  public void closeNullServerSocketReturnsFalse() {
    assertThat(NetworkUtils.close((ServerSocket) null), is(false));
  }

  @Test
  public void closeSocketReturnsTrue() throws Exception {
    Socket mockSocket = mock(Socket.class);

    assertThat(NetworkUtils.close(mockSocket), is(true));

    verify(mockSocket, times(1)).close();
  }

  @Test
  public void closeSocketReturnsFalse() throws Exception {
    Socket mockSocket = mock(Socket.class);

    doThrow(new IOException("test")).when(mockSocket).close();

    assertThat(NetworkUtils.close(mockSocket), is(false));

    verify(mockSocket, times(1)).close();
  }

  @Test
  public void closeNullSocketReturnsFalse() {
    assertThat(NetworkUtils.close((Socket) null), is(false));
  }

  @Test
  public void lenientParsePortIsSuccessful() {
    assertThat(NetworkUtils.lenientParsePort("1234"), is(equalTo(1234)));
  }

  @Test
  public void lenientParsePortWithHostnamePortNumberIsSuccessful() {
    assertThat(NetworkUtils.lenientParsePort("skullbox:8080"), is(equalTo(8080)));
  }

  @Test
  public void lenientParsePortWithMixedPortNumberIsSuccessful() {
    assertThat(NetworkUtils.lenientParsePort("1 2#4%O L^B8(-?n 0 I"), is(equalTo(12480)));
  }

  @Test
  public void lenientParsePortWithInvalidPortNumberHavingDefaultPortReturnsDefaultPort() {
    assertThat(NetworkUtils.lenientParsePort("$O.OOL", 1234), is(equalTo(1234)));
  }

  @Test
  public void lenientParsePortWithInvalidPortNumberThrowsIllegalArgumentExceptoin() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(isA(NumberFormatException.class));
    exception.expectMessage("Port [] is not valid");

    NetworkUtils.lenientParsePort("invalid");
  }

  @Test
  public void parsePortIsSuccessful() {
    assertThat(NetworkUtils.parsePort("1234"), is(equalTo(1234)));
    assertThat(NetworkUtils.parsePort("  5678 "), is(equalTo(5678)));
  }

  @Test
  public void parsePortWithInvalidPortNumberAndDefaultPortReturnsDefaultPort() {
    assertThat(NetworkUtils.parsePort(" 1  23 4", 5678), is(equalTo(5678)));
  }

  @Test
  public void parsePortWithInvalidPortNumberThrowsIllegalArgumentException() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(isA(NumberFormatException.class));
    exception.expectMessage("Port [invalid] is not valid");

    NetworkUtils.parsePort("invalid");
  }

  @Test
  public void newSocketAddressWithHostAndPort() {
    SocketAddress socketAddress = NetworkUtils.newSocketAddress("skullbox", 9876);

    assertThat(socketAddress, is(instanceOf(InetSocketAddress.class)));
    assertThat(((InetSocketAddress) socketAddress).getHostName(), is(equalTo("skullbox")));
    assertThat(((InetSocketAddress) socketAddress).getPort(), is(equalTo(9876)));
  }

  @Test
  public void newSocketAddressWithNullHostAndPort() {
    SocketAddress socketAddress = NetworkUtils.newSocketAddress(null, 9876);

    assertThat(socketAddress, is(instanceOf(InetSocketAddress.class)));
    assertThat(((InetSocketAddress) socketAddress).getHostName(), is(equalTo("0.0.0.0")));
    assertThat(((InetSocketAddress) socketAddress).getPort(), is(equalTo(9876)));
  }

  @Test
  public void newSocketAddressWithPort() {
    SocketAddress socketAddress = NetworkUtils.newSocketAddress(1234);

    assertThat(socketAddress, is(instanceOf(InetSocketAddress.class)));
    assertThat(((InetSocketAddress) socketAddress).getHostName(), is(equalTo("0.0.0.0")));
    assertThat(((InetSocketAddress) socketAddress).getPort(), is(equalTo(1234)));
  }
}
