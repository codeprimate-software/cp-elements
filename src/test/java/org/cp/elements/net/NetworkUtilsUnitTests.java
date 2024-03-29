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
import static org.cp.elements.lang.CheckedExceptionsFactory.newIOException;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketAddress;
import java.util.Arrays;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.ThrowableAssertions;

import org.assertj.core.api.InstanceOfAssertFactories;

/**
 * Unit Tests for {@link NetworkUtils}.
 *
 * @author John J. Blum
 * @see java.net.InetSocketAddress
 * @see java.net.ServerSocket
 * @see java.net.Socket
 * @see java.net.SocketAddress
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.net.NetworkUtils
 * @since 1.0.0
 */
public class NetworkUtilsUnitTests {

  private static final int COUNT = 100;

  @Test
  public void availablePortReturnsNonZeroPortGreaterThan0AndLessThan65536() {

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
    verifyNoMoreInteractions(mockServerSocket);
  }

  @Test
  public void closeServerSocketThrowingIOExceptionReturnsFalse() throws Exception {

    ServerSocket mockServerSocket = mock(ServerSocket.class);

    doThrow(newIOException("test")).when(mockServerSocket).close();

    assertThat(NetworkUtils.close(mockServerSocket)).isFalse();

    verify(mockServerSocket, times(1)).close();
    verifyNoMoreInteractions(mockServerSocket);
  }

  @Test
  public void closeNullServerSocketIsNullSafeReturnsFalse() {
    assertThat(NetworkUtils.close((ServerSocket) null)).isFalse();
  }

  @Test
  public void closeSocketReturnsTrue() throws Exception {

    Socket mockSocket = mock(Socket.class);

    assertThat(NetworkUtils.close(mockSocket)).isTrue();

    verify(mockSocket, times(1)).close();
    verifyNoMoreInteractions(mockSocket);
  }

  @Test
  public void closeSocketThrowingIOExceptionReturnsFalse() throws Exception {

    Socket mockSocket = mock(Socket.class);

    doThrow(newIOException("test")).when(mockSocket).close();

    assertThat(NetworkUtils.close(mockSocket)).isFalse();

    verify(mockSocket, times(1)).close();
    verifyNoMoreInteractions(mockSocket);
  }

  @Test
  public void closeNullSocketIsNullSafeReturnsFalse() {
    assertThat(NetworkUtils.close((Socket) null)).isFalse();
  }

  @Test
  public void lenientParsePortIsSuccessful() {
    assertThat(NetworkUtils.lenientParsePort("1234")).isEqualTo(1234);
  }

  @Test
  public void lenientParsePortWithHostnameAndPortNumberIsSuccessful() {
    assertThat(NetworkUtils.lenientParsePort("skullbox:8080")).isEqualTo(8080);
  }

  @Test
  public void lenientParsePortWithScrambledPortNumberIsSuccessful() {
    assertThat(NetworkUtils.lenientParsePort("1 2#4%O L^B8(-?n 0 I")).isEqualTo(12480);
  }

  @Test
  public void lenientParsePortWithInvalidPortNumberHavingDefaultPortReturnsDefaultPort() {
    assertThat(NetworkUtils.lenientParsePort("$O.OOL", 1234)).isEqualTo(1234);
  }

  @Test
  public void lenientParsePortWithInvalidPortNumberThrowsIllegalArgumentException() {

    Arrays.asList("invalid", "BOBO", "$##!", "  ", "", null).forEach(invalidPort ->
      ThrowableAssertions.assertThatThrowableOfType(IllegalArgumentException.class)
        .isThrownBy(args -> NetworkUtils.lenientParsePort(invalidPort))
        .havingMessage("Port [] is not valid")
        .causedBy(NumberFormatException.class)
        .withNoCause());
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

  @Test
  public void parsePortWithInvalidPortNumberThrowsIllegalArgumentException() {

    Arrays.asList("invalid", "BOBO", "$##!", "  ", "", null).forEach(invalidPort ->
      ThrowableAssertions.assertThatThrowableOfType(IllegalArgumentException.class)
        .isThrownBy(args -> NetworkUtils.parsePort(invalidPort))
        .havingMessage("Port [%s] is not valid", invalidPort)
        .causedBy(NumberFormatException.class)
        .withNoCause());
  }

  @Test
  public void newSocketAddressWithHostAndPort() {

    SocketAddress socketAddress = NetworkUtils.newSocketAddress("skullbox", 9876);

    assertThat(socketAddress).isInstanceOf(InetSocketAddress.class);

    assertThat(socketAddress)
      .asInstanceOf(InstanceOfAssertFactories.type(InetSocketAddress.class))
      .extracting(InetSocketAddress::getHostName)
      .isEqualTo("skullbox");

    assertThat(socketAddress)
      .asInstanceOf(InstanceOfAssertFactories.type(InetSocketAddress.class))
      .extracting(InetSocketAddress::getPort)
      .isEqualTo(9876);
  }

  @Test
  public void newSocketAddressWithUndeclaredHostAndPort() {

    Arrays.asList("  ", "", null).forEach(host -> {

      SocketAddress socketAddress = NetworkUtils.newSocketAddress(host, 9876);

      assertThat(socketAddress).isInstanceOf(InetSocketAddress.class);

      assertThat(socketAddress)
        .asInstanceOf(InstanceOfAssertFactories.type(InetSocketAddress.class))
        .extracting(InetSocketAddress::getHostName)
        .isEqualTo("0.0.0.0");

      assertThat(socketAddress)
        .asInstanceOf(InstanceOfAssertFactories.type(InetSocketAddress.class))
        .extracting(InetSocketAddress::getPort)
        .isEqualTo(9876);
    });
  }

  @Test
  public void newSocketAddressWithPort() {

    SocketAddress socketAddress = NetworkUtils.newSocketAddress(1234);

    assertThat(socketAddress).isInstanceOf(InetSocketAddress.class);

    assertThat(socketAddress)
      .asInstanceOf(InstanceOfAssertFactories.type(InetSocketAddress.class))
      .extracting(InetSocketAddress::getHostName)
      .isEqualTo("0.0.0.0");

    assertThat(socketAddress)
      .asInstanceOf(InstanceOfAssertFactories.type(InetSocketAddress.class))
      .extracting(InetSocketAddress::getPort)
      .isEqualTo(1234);
  }
}
