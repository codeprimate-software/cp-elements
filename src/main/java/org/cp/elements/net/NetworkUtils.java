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

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

import org.cp.elements.lang.NullSafe;

/**
 * The NetworkUtils class encapsulates utility methods related to networking.
 *
 * @author John J. Blum
 * @see java.net.ServerSocket
 * @see java.net.Socket
 * @since 1.0.0
 */
public abstract class NetworkUtils {

  /**
   * Gets an available network port used by a network service on which to listen for client {@link Socket} connections.
   *
   * @return in integer value indicating an available network port.
   */
  public static int availablePort() {
    try {
      ServerSocket serverSocket = new ServerSocket(0);
      int availablePort = serverSocket.getLocalPort();
      close(serverSocket);
      return availablePort;
    }
    catch (IOException ignore) {
      throw new IllegalStateException("No port available", ignore);
    }
  }

  /**
   * Attempts to close the given {@link ServerSocket} returning a boolean value to indicate whether the operation
   * was successful or not.
   *
   * @param socket {@link ServerSocket} to close.
   * @return a boolean value indicating whether the {@link ServerSocket} was successfully closed or not.
   * @see java.net.ServerSocket#close()
   */
  @NullSafe
  public static boolean close(ServerSocket socket) {
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
}
