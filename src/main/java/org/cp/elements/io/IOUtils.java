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

package org.cp.elements.io;

import java.io.Closeable;
import java.io.IOException;

import org.cp.elements.lang.NullSafe;

/**
 * The IOUtils class provides basic input and output utility operations.
 * <p/>
 * @author John J. Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class IOUtils {

  /**
   * Attempts to close the Closeable object ignoring any IOException that may occur as a result of the close operation.
   * </p>
   * @param obj the Closeable object who's close method will be called.
   * @return a boolean value indicating if the close operation was successful or not.
   * @see java.io.Closeable
   */
  @NullSafe
  public static boolean close(final Closeable obj) {
    if (obj != null) {
      try {
        obj.close();
        return true;
      }
      catch (IOException ignore) {
      }
    }

    return false;
  }

}
