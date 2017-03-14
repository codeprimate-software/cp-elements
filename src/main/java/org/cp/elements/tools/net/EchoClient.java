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

package org.cp.elements.tools.net;

import static org.cp.elements.net.NetworkUtils.close;
import static org.cp.elements.net.NetworkUtils.lenientParsePort;

import java.io.IOException;
import java.net.Socket;

import org.cp.elements.lang.ThrowableUtils;
import org.cp.elements.tools.net.support.AbstractClientServerSupport;
import org.cp.elements.util.ArrayUtils;

/**
 * The {@link EchoClient} class is used to send echo messages to a echo server.
 *
 * @author John Blum
 * @see java.net.Socket
 * @see org.cp.elements.tools.net.support.AbstractClientServerSupport
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class EchoClient extends AbstractClientServerSupport {

  protected static final String DEFAULT_HOST = null;

  /**
   * Main method used to run the {@link EchoClient} program.
   *
   * @param args array of {@link String arguments} passed into this program from the command-line.
   * @see #validateArguments(String[])
   * @see #newEchoClient(int)
   * @see #sendMessage(String)
   */
  public static void main(String[] args) {
    validateArguments(args);

    EchoClient echoClient = newEchoClient(lenientParsePort(args[0]));

    for (int index = 1; index < args.length; index++) {
      System.out.println(echoClient.sendMessage(args[index]));
    }
  }

  /**
   * Validates the array of {@link String arguments} passed into the {@link EchoClient} program from the command-line.
   *
   * @param args array of {@link String arguments} passed into this program from the command-line.
   */
  private static void validateArguments(String[] args) {
    if (ArrayUtils.isEmpty(args)) {
      System.err.printf("$ java ... %s <port> [<message1> <message2> ... <messageN>]%n", EchoClient.class.getName());
      System.exit(1);
    }
  }

  /**
   * Factory method used to construct a new instance of the {@link EchoClient} initialized with the given {@code port}.
   *
   * @param port {@link Integer} value indicating the port number on which the {@link EchoServer} is listening
   * for {@link EchoClient} connections.
   * @return a new instance of the {@link EchoClient} initialized on the given {@code port}.
   * @see #EchoClient(String, int)
   */
  public static EchoClient newEchoClient(int port) {
    return new EchoClient(DEFAULT_HOST, port);
  }

  /**
   * Factory method used to construct a new instance of the {@link EchoClient} initialized with
   * the given {@code host} and {@code port}.
   *
   * @param host {@link String} specifying host on which the {@link EchoServer} is running.
   * @param port {@link Integer} value indicating the port number on which the {@link EchoServer} is listening
   * for {@link EchoClient} connections.
   * @return a new instance of the {@link EchoClient} initialized on the given {@code host} and {@code port}.
   * @see #EchoClient(String, int)
   */
  public static EchoClient newEchoClient(String host, int port) {
    return new EchoClient(host, port);
  }

  private final int port;

  private final String host;

  /**
   * Constructs a new instance of {@link EchoClient} connected to the default host (localhost)
   * and given {@link Integer port}.
   *
   * @param port {@link Integer} value indicating the port number on which the {@link EchoServer} is listening
   * for {@link EchoClient} connections.
   * @see #EchoClient(String, int)
   */
  public EchoClient(int port) {
    this(DEFAULT_HOST, port);
  }

  /**
   * Constructs a new instance of {@link EchoClient} connected to the given {@link String host}
   * on the given {@link Integer port}.
   *
   * @param host {@link String} specifying host on which the {@link EchoServer} is running.
   * @param port {@link Integer} value indicating the port number on which the {@link EchoServer} is listening
   * for {@link EchoClient} connections.
   */
  public EchoClient(String host, int port) {
    this.host = host;
    this.port = port;
  }

  /**
   * Gets the host to which this {@link EchoClient} is connected.
   *
   * @return a {@link String} containing the hostname to which this {@link EchoClient} is connected.
   */
  protected String getHost() {
    return this.host;
  }

  /**
   * Gets the port to which this {@link EchoClient} is connected.
   *
   * @return an {@link Integer} value indicating the port number to which this {@link EchoClient} is connected.
   */
  protected int getPort() {
    return this.port;
  }

  /**
   * Sends the given {@link String message} to the {@link EchoClient}.
   *
   * @param message {@link String} containing the message to send.
   * @return the {@link String response} returned by the {@link EchoServer}.
   * @see #newSocket(String, int)
   * @see #sendMessage(Socket, String)
   * @see #receiveResponse(Socket)
   */
  public String sendMessage(String message) {
    Socket socket = null;

    try {
      socket = newSocket(getHost(), getPort());
      return receiveResponse(sendMessage(socket, message));
    }
    finally {
      close(socket);
    }
  }

  /* (non-Javadoc) */
  protected Socket sendMessage(Socket socket, String message) {
    try {
      return super.sendMessage(socket, message);
    }
    catch (IOException e) {
      logger.severe(String.format("Failed to send message [%1$s] to EchoServer [%2$s]",
        message, socket.getRemoteSocketAddress()));
      logger.severe(ThrowableUtils.getStackTrace(e));

      return socket;
    }
  }

  /* (non-Javadoc) */
  protected String receiveResponse(Socket socket) {
    try {
      return super.receiveMessage(socket);
    }
    catch (IOException e) {
      logger.severe(String.format("Failed to receive response from EchoServer [%s]", socket.getRemoteSocketAddress()));
      logger.severe(ThrowableUtils.getStackTrace(e));
      return "No Reply";
    }
  }
}
