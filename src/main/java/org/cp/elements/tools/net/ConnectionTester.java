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

import static org.cp.elements.lang.LangExtensions.assertThat;
import static org.cp.elements.lang.NumberUtils.intValue;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;
import static org.cp.elements.net.NetworkUtils.lenientParsePort;
import static org.cp.elements.net.NetworkUtils.newSocketAddress;

import java.io.IOException;
import java.net.Socket;
import java.util.Arrays;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.cp.elements.lang.Condition;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.net.ServicePort;
import org.cp.elements.test.Tester;

/**
 * The {@link ConnectionTester} class is used to test a network connection to a given network endpoint.
 *
 * @author John Blum
 * @see java.net.Socket
 * @see java.util.Optional
 * @see org.cp.elements.lang.Condition
 * @see org.cp.elements.net.NetworkUtils
 * @see org.cp.elements.net.ServicePort
 * @see org.cp.elements.test.Tester
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ConnectionTester implements Condition, Tester {

  protected static final boolean SO_REUSEADDR = true;

  protected static final long CONNECT_TIMEOUT = TimeUnit.SECONDS.toMillis(1L);

  protected static final String HOST_PORT_SEPARATOR = ":";

  public static void main(String[] args) {

    Arrays.stream(args).forEach(hostPort -> {

      String[] hostPortSplit = hostPort.split(HOST_PORT_SEPARATOR);
      String host = (hostPortSplit.length > 1 ? hostPortSplit[0] : null);

      int port = lenientParsePort(hostPortSplit.length > 1 ? hostPortSplit[1] : hostPortSplit[0]);

      System.out.printf("Connection to [%1$s:%2$d] was %3$ssuccessful%n",
        Optional.ofNullable(host).orElse("localhost"), port,
          newConnectionTester(host, port).test() ? StringUtils.EMPTY_STRING : "not ");
    });
  }

  /**
   * Factory method used to construct a new {@link ConnectionTester} in order to test a connection
   * to {@literal localhost} on the specified {@link Integer port}.
   *
   * @param port {@link Integer} value specifying the port number to which the connection is bound.
   * @return a new instance of {@link ConnectionTester} bound to the specified {@link Integer port}.
   * @throws IllegalArgumentException if the {@code port} number is not valid.
   * @see #ConnectionTester(String, int)
   */
  public static ConnectionTester newConnectionTester(int port) {
    return new ConnectionTester(null, port);
  }

  /**
   * Factory method used to construct a new {@link ConnectionTester} in order to test a connection
   * to the specified {@link String host} on the specified {@link Integer port}.
   *
   * @param host {@link String name} of the host to which the connection is bound.
   * @param port {@link Integer} value specifying the port number to which the connection is bound.
   * @return a new instance of {@link ConnectionTester} bound to the specified {@link String host}
   * and {@link Integer port}.
   * @throws IllegalArgumentException if the {@code port} number is not valid.
   * @see #ConnectionTester(String, int)
   */
  public static ConnectionTester newConnectionTester(String host, int port) {
    return new ConnectionTester(host, port);
  }

  private final AtomicBoolean connected = new AtomicBoolean(false);

  private final String host;

  private final int port;

  /**
   * Constructs a new the {@link ConnectionTester} bound to the given {@link String host}
   * and {@link Integer port}.
   *
   * @param host {@link String name} of the host to which the connection is bound.
   * @param port {@link Integer} value specifying the port number to which the connection is bound.
   * @throws IllegalArgumentException if the {@code port} number is not valid.
   */
  public ConnectionTester(String host, int port) {
    assertThat(port).throwing(newIllegalArgumentException("[%d] is not a valid port number", port))
      .isGreaterThanAndLessThanEqualTo(0, ServicePort.MAX_PORT);

    this.host = (host == null || host.trim().isEmpty() ? null : host);
    this.port = port;
  }

  /**
   * Determines whether a network connection to the specified {@link #getHost()} and {@link #getPort()} was established.
   *
   * @return a boolean value indicating whether a network connection to the specified {@link #getHost()}
   * and {@link #getPort()} was successfully established.
   * @see #isNotConnected()
   */
  public boolean isConnected() {
    return this.connected.get();
  }

  /**
   * Determines whether a network connection to the specified {@link #getHost()} and {@link #getPort()} was established.
   *
   * @return a boolean value indicating whether a network connection to the specified {@link #getHost()}
   * and {@link #getPort()} was successfully established.
   * @see #isConnected()
   */
  public boolean isNotConnected() {
    return !isConnected();
  }

  /**
   * Sets whether a network connection to the specified {@link #getHost()} and {@link #getPort()} could be established.
   *
   * @param connected boolean value indicating whether a network connection to the specified {@link #getHost()}
   * and {@link #getPort()} was successfully established.
   * @return this {@link ConnectionTester}.
   * @see #isNotConnected()
   * @see #isConnected()
   */
  protected ConnectionTester setConnected(boolean connected) {
    this.connected.set(connected);
    return this;
  }

  /**
   * Return the {@link String name} of the host tested in the connection.
   *
   * @return the {@link String name} of the host tested in the connection.
   * @see #getPort()
   */
  public String getHost() {
    return this.host;
  }

  /**
   * Return the {@link Integer port number} tested in the connection.
   *
   * @return the {@link Integer port number} tested in the connection.
   * @see #getHost()
   */
  public int getPort() {
    return this.port;
  }

  /**
   * Executes the test.
   *
   * @return a boolean value indicating if the test was successful.
   * @see #test()
   */
  @Override
  public boolean evaluate() {
    return test();
  }

  /**
   * Resets the state of the {@link #isConnected() connected property} and retests the connection.
   *
   * @return a boolean value indicating whether a connection to the specified network {@link #getHost()}
   * and {@link #getPort()} was successful.
   * @see #setConnected(boolean)
   * @see #test()
   */
  public boolean retest() {
    return setConnected(false).test();
  }

  /**
   * Tests whether a connection to the specified network {@link #getHost()} and {@link #getPort()} can be established.
   *
   * @return a boolean value indicating whether a connection to the specified network {@link #getHost()}
   * and {@link #getPort()} was successful.
   * @see java.net.Socket#isConnected()
   * @see org.cp.elements.net.NetworkUtils#newSocketAddress(String, int)
   * @see #newSocket()
   * @see #getHost()
   * @see #getPort()
   */
  @Override
  public boolean test() { // NOPMD

    if (isNotConnected()) {
      try (Socket socket = newSocket()) {
        socket.setReuseAddress(SO_REUSEADDR);
        socket.connect(newSocketAddress(getHost(), getPort()), intValue(CONNECT_TIMEOUT));
        setConnected(socket.isConnected());
      }
      catch (IOException ignore) { }
    }

    return isConnected();
  }

  /**
   * Constructs a new, uninitialized {@link Socket}.
   *
   * @return a new, uninitialized {@link Socket}.
   * @see java.net.Socket#Socket()
   */
  protected Socket newSocket() {
    return new Socket();
  }
}
