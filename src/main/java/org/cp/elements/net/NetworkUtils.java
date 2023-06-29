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

import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketAddress;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract utility class encapsulating functionality related to networking.
 *
 * @author John J. Blum
 * @see java.net.InetSocketAddress
 * @see java.net.ServerSocket
 * @see java.net.Socket
 * @see java.net.SocketAddress
 * @since 1.0.0
 */
public abstract class NetworkUtils {

  /**
   * Gets an {@link Integer available network port} used by a network service to listen for network client
   * {@link Socket} connections.
   *
   * @return an {@link Integer available network port}.
   */
  public static int availablePort() {

    try (ServerSocket serverSocket = new ServerSocket(0)) {
      serverSocket.setReuseAddress(true);
      return serverSocket.getLocalPort();
    }
    catch (IOException cause) {
      throw new NoAvailablePortException("No port available", cause);
    }
  }

  /**
   * Attempts to close the given {@link ServerSocket} returning a boolean value to indicate whether the operation
   * was successful.
   * <p>
   * Ignores any {@link IOException} thrown while attempting to {@link ServerSocket#close()} the {@link ServerSocket}.
   *
   * @param serverSocket {@link ServerSocket} to close.
   * @return a boolean value indicating whether the {@link ServerSocket} was successfully closed.
   * @see java.net.ServerSocket#close()
   * @see java.net.ServerSocket
   */
  @NullSafe
  public static boolean close(@Nullable ServerSocket serverSocket) {

    if (serverSocket != null) {
      try {
        serverSocket.close();
        return true;
      }
      catch (IOException ignore) { }
    }

    return false;
  }

  /**
   * Attempts to close the given {@link Socket} returning a boolean value to indicate whether the operation
   * was successful.
   * <p>
   * Ignores any {@link IOException} thrown while attempting to {@link Socket#close()} the {@link Socket}.
   *
   * @param socket {@link Socket} to close.
   * @return a boolean value indicating whether the {@link Socket} was successfully closed.
   * @see java.net.Socket#close()
   * @see java.net.Socket
   */
  @NullSafe
  public static boolean close(@Nullable Socket socket) {

    if (socket != null) {
      try {
        socket.close();
        return true;
      }
      catch (IOException ignore) { }
    }

    return false;
  }

  /**
   * Leniently parses the given, required {@link String} as a {@link Integer numeric value} representing a network port.
   * <p>
   * This parse method works by stripping the digits from the given {@link String}. Therefore, this method is capable
   * of parsing {@link String values} of the form {@literal hostname:port}, for example: {@literal skullbox:8080}.
   *
   * @param port {@link String} to parse as a {@link Integer numeric value} representing a network port;
   * must not be {@literal null} or {@literal empty}.
   * @return a {@link Integer numeric value} representing the network port parsed from the given,
   * required {@link String}.
   * @throws IllegalArgumentException if {@link String port} is not valid and the {@link Integer defaultPort}
   * is {@literal null}.
   * @see #lenientParsePort(String, Integer)
   */
  public static int lenientParsePort(@NotNull String port) {
    return lenientParsePort(port, null);
  }

  /**
   * Leniently parses the given {@link String} as a {@link Integer numeric value} representing a network port.
   * <p>
   * This parse method works by stripping the digits from the given {@link String}. Therefore, this method is capable
   * of parsing {@link String values} of the form {@literal hostname:port}, for example: {@literal skullbox:8080}.
   *
   * @param port {@link String} to parse as a {@link Integer numeric value} representing a network port.
   * @param defaultPort {@link Integer value} used as the default port if {@link String port} is not valid.
   * @return a {@link Integer numeric value} representing the network port parsed from the given {@link String port}.
   * Returns the {@link Integer default port} if {@link String port} is not valid.
   * @throws IllegalArgumentException if {@link String port} is not valid and the {@link Integer defaultPort}
   * is {@literal null}.
   * @see #parsePort(String, Integer)
   */
  public static int lenientParsePort(@Nullable String port, @Nullable Integer defaultPort) {
    return parsePort(StringUtils.getDigits(String.valueOf(port)), defaultPort);
  }

  /**
   * Parses the given, required {@link String} as a {@link Integer numeric value} representing a network port.
   *
   * @param port {@link String} to parse as a {@link Integer numeric value} representing a network port;
   * must not be {@literal null} or {@literal empty}.
   * @return a {@link Integer numeric value} representing the network port parsed from the given,
   * required {@link String}.
   * @throws IllegalArgumentException if {@link String port} is not valid and the {@link Integer defaultPort}
   * is {@literal null}.
   * @see #parsePort(String, Integer)
   */
  public static int parsePort(@NotNull String port) {
    return parsePort(port, null);
  }

  /**
   * Parses the given {@link String} as a {@link Integer numeric value} representing a network port.
   *
   * @param port {@link String} to parse as a {@link Integer numeric value} representing a network port.
   * @param defaultPort {@link Integer value} used as the default port if {@link String port} is not valid.
   * @return a {@link Integer numeric value} representing the network port parsed from the given {@link String port}.
   * Returns the {@link Integer default port} if {@link String port} is not valid.
   * @throws IllegalArgumentException if {@link String port} is not valid and the {@link Integer defaultPort}
   * is {@literal null}.
   * @see java.lang.Integer#parseInt(String)
   */
  public static int parsePort(@Nullable String port, @Nullable Integer defaultPort) {

    try {
      return Integer.parseInt(StringUtils.trim(port));
    }
    catch (NumberFormatException cause) {
      Assert.notNull(defaultPort, newIllegalArgumentException(cause, "Port [%s] is not valid", port));
      return defaultPort;
    }
  }

  /**
   * Constructs a new instance of {@link SocketAddress} bound to the given {@link Integer port}.
   *
   * @param port {@link Integer} declaring the network port on which the {@link SocketAddress} will be bound.
   * @return a new {@link SocketAddress} bound to the given {@link Integer port}.
   * @throws IllegalArgumentException if the {@link Integer port} is outside the range of valid network ports.
   * @see #newSocketAddress(String, int)
   */
  public static SocketAddress newSocketAddress(int port) {
    return newSocketAddress(null, port);
  }

  /**
   * Constructs a new instance of {@link SocketAddress} bound to the given {@link String host} and {@link Integer port}.
   *
   * @param host {@link String} containing the name of the host on which the {@link SocketAddress} will be bound.
   * @param port {@link Integer} declaring the network port on which the {@link SocketAddress} will be bound.
   * @return a new {@link SocketAddress} bound to the given {@link String host} and {@link Integer port}.
   * If the {@link String host} is {@literal null} or {@literal empty}, then the {@link SocketAddress} will only
   * be bound to the given {@link Integer port}.
   * @throws IllegalArgumentException if the {@link Integer port} is outside the range of valid network ports.
   * @see java.net.InetSocketAddress
   * @see java.net.SocketAddress
   */
  public static SocketAddress newSocketAddress(@Nullable String host, int port) {

    return StringUtils.hasText(host)
      ? new InetSocketAddress(host, port)
      : new InetSocketAddress(port);
  }
}
