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

package org.cp.elements.tools.net.support;

import static org.cp.elements.lang.NumberUtils.intValue;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newRuntimeException;
import static org.cp.elements.net.NetworkUtils.newSocketAddress;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import org.cp.elements.lang.NullSafe;

/**
 * {@link AbstractClientServerSupport} is an abstract class supporting the development of simple
 * Java Network clients and servers.
 *
 * @author John Blum
 * @see java.io.InputStream
 * @see java.io.OutputStream
 * @see java.net.ServerSocket
 * @see java.net.Socket
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractClientServerSupport {

  public static final boolean DEFAULT_REUSE_ADDRESS = true;

  public static final long DEFAULT_SO_TIMEOUT = TimeUnit.SECONDS.toMillis(15);

  protected Logger logger = Logger.getLogger(getClass().getName());

  /**
   * Determines whether the {@link ServerSocket} is alive and accepting client connections.
   *
   * @param serverSocket {@link ServerSocket} to evaluate.
   * @return a boolean value indicating whether the {@link ServerSocket} is valid.
   * @see java.net.ServerSocket#isBound()
   * @see java.net.ServerSocket#isClosed()
   */
  @NullSafe
  protected boolean isRunning(ServerSocket serverSocket) {
    return Optional.ofNullable(serverSocket)
      .map(localServerSocket -> !localServerSocket.isClosed() && localServerSocket.isBound())
      .orElse(false);
  }

  /**
   * Returns a reference to the {@link Logger}.
   *
   * @return a reference to the {@link Logger}.
   * @see java.util.logging.Logger
   */
  protected Logger getLogger() {
    return this.logger;
  }

  /**
   * Returns a new {@link BufferedReader} to read from the {@link Socket#getInputStream()}.
   *
   * @param socket {@link Socket} from which the {@link InputStream} is read.
   * @return a new {@link BufferedReader} to read from the {@link Socket#getInputStream()}.
   * @throws IOException if an IO error occurs while reading from the {@link Socket}.
   * @see java.io.BufferedReader
   * @see java.net.Socket
   */
  protected BufferedReader newBufferedReader(Socket socket) throws IOException {
    return new BufferedReader(new InputStreamReader(socket.getInputStream()));
  }

  /**
   * Returns a new {@link PrintWriter} to write to the {@link Socket#getOutputStream()}.
   *
   * @param socket {@link Socket} to which the {@link OutputStream} is written.
   * @return a new {@link PrintWriter} to write to the {@link Socket#getOutputStream()}.
   * @throws IOException if an IO error occurs while writing to the {@link Socket}.
   * @see java.io.PrintWriter
   * @see java.net.Socket
   */
  protected PrintWriter newPrintWriter(Socket socket) throws IOException {
    return new PrintWriter(new BufferedWriter(new OutputStreamWriter(socket.getOutputStream())), true);
  }

  /**
   * Constructs and configures a new {@link ServerSocket}.
   *
   * @param port {@link Integer} value indicating the port number to which the {@link ServerSocket} is bound
   * listening for client connections.
   * @return a new {@link ServerSocket}.
   * @throws RuntimeException if the {@link ServerSocket} could not be created.
   * @see java.net.ServerSocket
   */
  public ServerSocket newServerSocket(int port) {
    try {
      ServerSocket serverSocket = newServerSocket();
      serverSocket.setReuseAddress(DEFAULT_REUSE_ADDRESS);
      serverSocket.bind(newSocketAddress(port));
      return serverSocket;
    }
    catch (IOException cause) {
      throw newRuntimeException(cause, "Failed to create a ServerSocket on port [%d]", port);
    }
  }

  /**
   * Constructs a new uninitialized instance of {@link ServerSocket}.
   *
   * @return a new uninitialized instance of {@link ServerSocket}.
   * @throws IOException if the {@link ServerSocket} could not be created.
   * @see java.net.ServerSocket
   */
  protected ServerSocket newServerSocket() throws IOException {
    return new ServerSocket();
  }

  /**
   * Constructs and configures a new client {@link Socket}.
   *
   * Sets {@link Socket#setReuseAddress(boolean)} to {@literal true} and sets the {@link Socket#setSoTimeout(int)}
   * to {@literal 15 seconds}.
   *
   * @param host {@link String} specifying the host on which the server is running.
   * @param port {@link Integer} specifying the port on which the server is listening.
   * @return a new client {@link Socket} connected to host and port.
   * @throws RuntimeException if the {@link Socket} could not be created.
   * @see java.net.Socket
   */
  public Socket newSocket(String host, int port) {
    try {
      Socket socket = newSocket();
      socket.setReuseAddress(DEFAULT_REUSE_ADDRESS);
      socket.setSoTimeout(intValue(DEFAULT_SO_TIMEOUT));
      socket.connect(newSocketAddress(host, port));
      return socket;
    }
    catch (IOException cause) {
      throw newRuntimeException(cause, "Failed to create a client Socket on host [%s] and port [%d]", host, port);
    }
  }

  /**
   * Constructs an uninitialized instance of {@link Socket}.
   *
   * @return an uninitialized instance of {@link Socket}.
   * @see java.net.Socket
   */
  protected Socket newSocket() {
    return new Socket();
  }

  /**
   * Receives a simple {@link String message} over the given {@link Socket}.
   *
   * @param socket {@link Socket} on which the {@link String message} is received.
   * @return a {@link String} containing the message received on the {@link Socket}.
   * @throws IOException if an I/O error occurs while reading the message from the {@link Socket}.
   * @see #newBufferedReader(Socket)
   * @see java.io.BufferedReader#readLine()
   * @see java.net.Socket
   */
  protected String receiveMessage(Socket socket) throws IOException {
    return newBufferedReader(socket).readLine();
  }

  /**
   * Sends a simple {@link String message} over the given {@link Socket}.
   *
   * @param socket {@link Socket} on which the {@link String message} is sent.
   * @param message {@link String} containing the message to send over the {@link Socket}.
   * @return the given {@link Socket} in order to chain multiple send operations.
   * @throws IOException if an I/O error occurs while sending the given {@code message}
   * using the provided {@link Socket}.
   * @see #newBufferedReader(Socket)
   * @see java.io.BufferedReader#readLine()
   * @see java.net.Socket
   */
  protected Socket sendMessage(Socket socket, String message) throws IOException {
    PrintWriter printWriter = newPrintWriter(socket);

    printWriter.println(message);
    printWriter.flush();

    return socket;
  }
}
