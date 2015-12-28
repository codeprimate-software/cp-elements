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

/**
 * The NetworkUtils class provides utility methods related to networking.
 * 
 * @author John J. Blum
 * @see java.net.ServerSocket
 * @see java.net.Socket
 * @since 1.0.0
 */
public abstract class NetworkUtils {

  /**
   * Attempts to close the specified ServerSocket returning a boolean value indicating whether the operation
   * was successful or not.
   * 
   * @param socket the ServerSocket to be closed.
   * @return a boolean value indicating whether the ServerSocket was successfully closed or not.
   * @see java.net.ServerSocket#close()
   */
  public static boolean close(final ServerSocket socket) {
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
   * Attempts to close the specified Socket return a boolean value indicating whether the operaton was successful
   * or not.
   * 
   * @param socket the Socket to be closed.
   * @return a boolean value indicating whether the Socket was successfully closed or not.
   * @see java.net.Socket#close()
   */
  public static boolean close(final Socket socket) {
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
