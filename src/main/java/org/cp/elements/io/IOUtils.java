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

package org.cp.elements.io;

import java.io.Closeable;
import java.io.IOException;

import org.cp.elements.lang.NullSafe;

/**
 * The IOUtils class provides basic input and output utility operations.
 *
 * @author John J. Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class IOUtils {

  /**
   * Attempts to close the Closeable object ignoring any IOException that may occur as a result of the close operation.
   *
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
