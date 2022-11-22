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
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;
import static org.cp.elements.net.NetworkUtils.close;
import static org.cp.elements.net.NetworkUtils.lenientParsePort;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Optional;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import org.cp.elements.lang.Integers;
import org.cp.elements.lang.ThrowableUtils;
import org.cp.elements.lang.concurrent.ThreadUtils;
import org.cp.elements.net.ServicePort;
import org.cp.elements.tools.net.support.AbstractClientServerSupport;
import org.cp.elements.util.ArrayUtils;

/**
 * The {@link EchoServer} class is used to echo messages back to a echo client.
 *
 * @author John Blum
 * @see java.lang.Runnable
 * @see java.net.ServerSocket
 * @see java.net.Socket
 * @see java.util.concurrent.ExecutorService
 * @see java.util.concurrent.Executors
 * @see java.util.concurrent.TimeUnit
 * @see org.cp.elements.net.ServicePort
 * @see org.cp.elements.tools.net.support.AbstractClientServerSupport
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class EchoServer extends AbstractClientServerSupport implements Runnable {

  protected static final int EXECUTOR_THREAD_POOL_SIZE = 10;

  protected static final long DEFAULT_DURATION_MILLISECONDS = TimeUnit.SECONDS.toMillis(15);

  private static final int THIRTY = 30;

  /**
   * Main method used to run the {@link EchoServer} program.
   *
   * @param args array of {@link String arguments} passed into this program from the command-line.
   * @see #validateArguments(String[])
   * @see #newEchoServer(int)
   */
  public static void main(String[] args) {

    validateArguments(args);
    newEchoServer(lenientParsePort(args[0])).run();
  }

  /**
   * Validates the array of {@link String arguments} passed into the {@link EchoServer} program from the command-line.
   *
   * @param args array of {@link String arguments} passed into this program from the command-line.
   */
  private static void validateArguments(String[] args) {

    if (ArrayUtils.isEmpty(args)) {
      System.err.printf("$ java -server ... %s <port>%n", EchoServer.class.getName());
      System.exit(1);
    }
  }

  /**
   * Factory method used to construct a new instance of the {@link EchoServer} initialized with the given {@code port}.
   *
   * @param port {@link Integer} value indicating the port number on which the {@link EchoServer} will listen.
   * @return a new instance of the {@link EchoServer} initialized on the given {@code port}.
   * @throws IllegalArgumentException if the given {@code port} number is not valid.
   * @see #EchoServer(int)
   */
  public static EchoServer newEchoServer(int port) {
    return new EchoServer(port);
  }

  private final int port;

  private ExecutorService echoService;

  private final ServerSocket serverSocket;

  /**
   * Constructs a new instance of the {@link EchoServer} listening on the given {@code port}.
   *
   * @param port {@link Integer} value indicating the port number on which the {@link EchoServer} will listen.
   * @throws IllegalArgumentException if the given {@code port} number is not valid.
   * @see #newServerSocket(int)
   */
  public EchoServer(int port) {

    assertThat(port)
      .throwing(newIllegalArgumentException("Port [%d] must be greater than 1024 and less than equal to 65535", port))
      .isGreaterThanAndLessThanEqualTo(ServicePort.MAX_RESERVED_PORT, ServicePort.MAX_PORT);

    this.port = port;
    this.serverSocket = newServerSocket(port);

    Runtime.getRuntime().addShutdownHook(new Thread(this::shutdown));
  }

  /**
   * Determine whether this {@link EchoServer} is running.
   *
   * @return a boolean value indicating whether this {@link EchoServer} is running.
   * @see #isRunning()
   */
  public boolean isNotRunning() {
    return !isRunning();
  }

  /**
   * Determine whether this {@link EchoServer} is running.
   *
   * @return a boolean value indicating whether this {@link EchoServer} is running.
   * @see #isRunning(ServerSocket)
   * @see #getServerSocket()
   */
  public boolean isRunning() {
    return isRunning(getServerSocket());
  }

  /**
   * Returns a reference to the Echo Service.
   *
   * @return a reference to the Echo Service.
   * @see java.util.concurrent.ExecutorService
   */
  protected ExecutorService getEchoService() {
    return this.echoService;
  }

  /**
   * Returns the port on which this {@link EchoServer} is listening for {@link EchoClient} connections.
   *
   * @return a {@link Integer} value indicating the port number the {@link EchoServer} is using to listen
   * for {@link EchoClient} connections.
   */
  public int getPort() {
    return this.port;
  }

  /**
   * Gets the {@link ServerSocket} used by this {@link EchoServer} to accept {@link EchoClient} connections.
   *
   * @return the {@link ServerSocket} used by this {@link EchoServer} to accept {@link EchoClient} connections.
   * @see java.net.ServerSocket
   */
  protected ServerSocket getServerSocket() {
    return this.serverSocket;
  }

  /**
   * Runs the {@link EchoServer}.
   *
   * @see #runEchoService(ServerSocket)
   * @see #getServerSocket()
   */
  @Override
  public void run() {
    getLogger().info(() -> String.format("Starting EchoServer on port [%d]...", getPort()));
    runEchoService(getServerSocket());
  }

  /**
   * Starts this {@link EchoServer} and waits up to {@literal 15 seconds} for the {@link EchoServer} to start.
   *
   * @return this {@link EchoServer}.
   * @see #runAndWaitFor(long)
   */
  public EchoServer runAndWaitFor() {
    return runAndWaitFor(DEFAULT_DURATION_MILLISECONDS);
  }

  /**
   * Starts this {@link EchoServer} and waits up to the given {@code duration} for the {@link EchoServer} to start.
   *
   * @param duration length of time in milliseconds to wait for this {@link EchoServer} to start.
   * @return this {@link EchoServer}.
   * @see #run()
   * @see #waitFor(long)
   */
  public EchoServer runAndWaitFor(long duration) {

    run();
    waitFor(duration);

    return this;
  }

  /**
   * Starts the 'echo service'.
   *
   * @param serverSocket {@link ServerSocket} used by this {@link EchoServer} to accept {@link EchoClient} connections.
   * @see java.net.ServerSocket
   */
  protected void runEchoService(ServerSocket serverSocket) {

    if (isRunning(serverSocket)) {

      this.echoService = newExecutorService();

      this.echoService.submit(() -> {
        try {
          while (isRunning(serverSocket)) {
            Socket echoClient = serverSocket.accept();

            getLogger().info(() -> String.format("EchoClient connected from [%s]",
              echoClient.getRemoteSocketAddress()));

            this.echoService.submit(() -> {
              sendResponse(echoClient, receiveMessage(echoClient));
              close(echoClient);
            });
          }
        }
        catch (IOException cause) {
          if (isRunning(serverSocket)) {
            getLogger().warning(() -> String.format("An IO error occurred while listening for EchoClients:%n%s",
              ThrowableUtils.getStackTrace(cause)));
          }
        }
      });

      getLogger().info(() -> String.format("EchoServer running on port [%d]", getPort()));
    }
  }

  /**
   * Constructs a new instance of an {@link ExecutorService} to run the Echo Service.
   *
   * @return a new instance of {@link ExecutorService} used to run the Echo Service.
   * @see java.util.concurrent.Executors#newFixedThreadPool(int)
   * @see java.util.concurrent.ExecutorService
   * @see #runEchoService(ServerSocket)
   */
  protected ExecutorService newExecutorService() {
    return Executors.newFixedThreadPool(EXECUTOR_THREAD_POOL_SIZE);
  }

  /**
   * Receives a {@link String message} from a {@link EchoClient} over the given {@link Socket}.
   *
   * @param socket {@link Socket} used to receive the {@link EchoClient EchoClient's} {@link String message}.
   * @return a {@link String} containing the message sent by the {@link EchoClient}.
   */
  @Override
  protected String receiveMessage(Socket socket) {

    try {

      String message = super.receiveMessage(socket);

      getLogger().fine(() -> String.format("Received message [%1$s] from EchoClient [%2$s]",
        message, socket.getRemoteSocketAddress()));

      return message;
    }
    catch (IOException cause) {
      getLogger().warning(() -> String.format("Failed to receive message from EchoClient [%s]",
        socket.getRemoteSocketAddress()));

      getLogger().fine(() -> ThrowableUtils.getStackTrace(cause));

      return "What?";
    }
  }

  /**
   * Sends the {@link String message} received from the Echo Client back to the Echo Client on the given {@link Socket}.
   *
   * @param socket {@link Socket} used to send the Echo Client's {@link String message} back to the Echo Client.
   * @param message {@link String} containing the message to send the Echo Client.  This is the same message
   * sent by the Echo Client and received by the Echo Server.
   * @see AbstractClientServerSupport#sendMessage(Socket, String)
   */
  protected void sendResponse(Socket socket, String message) {

    try {
      getLogger().info(() -> String.format("Sending response [%1$s] to EchoClient [%2$s]",
        message, socket.getRemoteSocketAddress()));

      sendMessage(socket, message);
    }
    catch (IOException cause) {
      getLogger().warning(() -> String.format("Failed to send response [%1$s] to EchoClient [%2$s]",
        message, socket.getRemoteSocketAddress()));

      getLogger().fine(() -> ThrowableUtils.getStackTrace(cause));
    }
  }

  /**
   * Stops this {@link EchoServer} taking it offline and out-of-service.
   */
  public void shutdown() {

    getLogger().info("Stopping EchoServer...");

    closeServerSocket();
    stopEchoService();

    getLogger().info("EchoServer stopped");
  }

  /**
   * Closes the {@link ServerSocket} used by the {@link EchoServer} to receive {@link EchoClient} connections.
   *
   * @see #getServerSocket()
   */
  protected void closeServerSocket() {

    ServerSocket serverSocket = getServerSocket();

    if (!close(serverSocket)) {
      getLogger().warning(() -> String.format(
        "Failed to close ServerSocket bound to address [%s], listening on port [%d]",
          serverSocket.getInetAddress(), serverSocket.getLocalPort()));
    }
  }

  /**
   * Stops the Echo Service, taking it offline and out-of-service.
   *
   * @return a boolean value indicating whether the Echo Service has been shutdown successfully.
   * @see #getEchoService()
   */
  protected boolean stopEchoService() {

    return Optional.ofNullable(getEchoService())
      .map(localEchoService -> {

        localEchoService.shutdown();

        try {
          if (!localEchoService.awaitTermination(THIRTY, TimeUnit.SECONDS)) {
            localEchoService.shutdownNow();

            if (!localEchoService.awaitTermination(THIRTY, TimeUnit.SECONDS)) {
              getLogger().warning("Failed to shutdown EchoService");
            }
          }
        }
        catch (InterruptedException ignore) {
          Thread.currentThread().interrupt();
        }

        return localEchoService.isShutdown();

      }).orElse(false);
  }

  /**
   * Waits up to {@literal 15 seconds} for this {@link EchoServer} to start.
   *
   * @return a boolean value indicating whether this {@link EchoServer} started successfully.
   * @see #waitFor(long)
   */
  public boolean waitFor() {
    return waitFor(DEFAULT_DURATION_MILLISECONDS);
  }

  /**
   * Waits for the given {@code duration} for this {@link EchoServer} to start.
   *
   * @param duration length of time to wait for this {@link EchoServer} to start.
   * @return a boolean value indicating whether this {@link EchoServer} started successfully.
   * @see ThreadUtils#waitFor(long)
   */
  public boolean waitFor(long duration) {

    return ThreadUtils.waitFor(duration)
      .checkEvery(Integers.FIVE_HUNDRED)
      .on(this::isRunning);
  }
}
