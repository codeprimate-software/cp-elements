/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 *
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 *
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 *
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 *
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
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
