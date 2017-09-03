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

package org.cp.elements.net;

import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;
import static org.cp.elements.lang.StringUtils.getDigits;
import static org.cp.elements.lang.StringUtils.trim;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketAddress;
import java.util.Optional;

import org.cp.elements.lang.annotation.NullSafe;

/**
 * The {@link NetworkUtils} class encapsulates utility methods related to networking.
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
   * Gets an available network port used by a network service on which to listen for client {@link Socket} connections.
   *
   * @return in integer value indicating an available network port.
   */
  public static int availablePort() {
    ServerSocket serverSocket = null;

    try {
      serverSocket = new ServerSocket(0);
      return serverSocket.getLocalPort();
    }
    catch (IOException cause) {
      throw new NoAvailablePortException("No port available", cause);
    }
    finally {
      close(serverSocket);
    }
  }

  /**
   * Attempts to close the given {@link ServerSocket} returning a boolean value to indicate whether the operation
   * was successful or not.
   *
   * @param serverSocket {@link ServerSocket} to close.
   * @return a boolean value indicating whether the {@link ServerSocket} was successfully closed or not.
   * @see java.net.ServerSocket#close()
   */
  @NullSafe
  public static boolean close(ServerSocket serverSocket) {
    if (serverSocket != null) {
      try {
        serverSocket.close();
        return true;
      }
      catch (IOException ignore) {
      }
    }

    return false;
  }

  /**
   * Attempts to close the given {@link Socket} returning a boolean value to indicate whether the operation
   * was successful or not.
   *
   * @param socket {@link Socket} to close.
   * @return a boolean value indicating whether the {@link Socket} was successfully closed or not.
   * @see java.net.Socket#close()
   */
  @NullSafe
  public static boolean close(Socket socket) {
    if (socket != null) {
      try {
        socket.close();
        return true;
      }
      catch (IOException ignore) {
      }
    }

    return false;
  }

  /**
   * Leniently parses the given {@link String} as a numeric port number.
   *
   * This method works by stripping the digits from the given {@link String}.  Therefore, this method
   * is capable of handling {@link String} values such as "hostname:port".
   *
   * For example: skullbox:8080
   *
   * @param port {@link String} to parse as a numeric port number.
   * @return a numeric port number from the given {@link String}.
   * @throws IllegalArgumentException if the {@link String} port number is not valid
   * and {@code defaultPort} is {@literal null}.
   * @see #lenientParsePort(String, Integer)
   */
  public static int lenientParsePort(String port) {
    return lenientParsePort(port, null);
  }

  /**
   * Leniently parses the given {@link String} as a numeric port number.
   *
   * This method works by stripping the digits from the given {@link String}.  Therefore, this method
   * is capable of handling {@link String} values such as "hostname:port".
   *
   * For example: skullbox:8080
   *
   * @param port {@link String} to parse as a numeric port number.
   * @param defaultPort {@link Integer} value used as the default port number
   * if the {@link String} port number is not valid.
   * @return a numeric port number from the given {@link String}.
   * @throws IllegalArgumentException if the {@link String} port number is not valid
   * and {@code defaultPort} is {@literal null}.
   * @see #parsePort(String, Integer)
   */
  public static int lenientParsePort(String port, Integer defaultPort) {
    return parsePort(getDigits(port), defaultPort);
  }

  /**
   * Parses the given {@link String} as a numeric port number.
   *
   * @param port {@link String} to parse as a numeric port number.
   * @return a numeric port number from the given {@link String}.
   * @throws IllegalArgumentException if the {@link String} port number is not valid
   * and {@code defaultPort} is {@literal null}.
   * @see #parsePort(String, Integer)
   */
  public static int parsePort(String port) {
    return parsePort(port, null);
  }

  /**
   * Parses the given {@link String} as a numeric port number.
   *
   * @param port {@link String} to parse as a numeric port number.
   * @param defaultPort {@link Integer} value used as the default port number
   * if the {@link String} port number is not valid.
   * @return a numeric port number from the given {@link String}.
   * @throws IllegalArgumentException if the {@link String} port number is not valid
   * and {@code defaultPort} is {@literal null}.
   * @see java.lang.Integer#parseInt(String)
   */
  public static int parsePort(String port, Integer defaultPort) {
    try {
      return Integer.parseInt(trim(port));
    }
    catch (NumberFormatException cause) {
      return Optional.ofNullable(defaultPort)
        .orElseThrow(() -> newIllegalArgumentException(cause, "Port [%s] is not valid", port));
    }
  }

  /**
   * Constructs a new instance of {@link SocketAddress} bound {@link Integer port}.
   *
   * @param port {@link Integer} specifying the port number to which the {@link SocketAddress} will bind.
   * @return a new instance of {@link SocketAddress} bound to the given {@link Integer port}.
   * @throws IllegalArgumentException if the port parameter is outside the range of valid port values.
   * @see #newSocketAddress(String, int)
   */
  public static SocketAddress newSocketAddress(int port) {
    return newSocketAddress(null, port);
  }

  /**
   * Constructs a new instance of {@link SocketAddress} bound to the given {@link String host} and {@link Integer port}.
   *
   * @param host {@link String} containing the name of the host to whichthe {@link SocketAddress} will be bound.
   * @param port {@link Integer} specifying the port number to which the {@link SocketAddress} will be bound.
   * @return a new instance of {@link SocketAddress} bound to the given {@link Integer port}.
   * @throws IllegalArgumentException if the port parameter is outside the range of valid port values.
   * @see #newSocketAddress(String, int)
   * @see java.net.InetSocketAddress
   * @see java.net.SocketAddress
   */
  public static SocketAddress newSocketAddress(String host, int port) {
    return Optional.ofNullable(host).map(hostname -> new InetSocketAddress(host, port))
      .orElseGet(() -> new InetSocketAddress(port));
  }
}
