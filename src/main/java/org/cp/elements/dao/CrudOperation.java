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

package org.cp.elements.dao;

import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.StringUtils;

/**
 * The CrudOperation enum type is an enumeration of CRUD data access operations.
 *
 * @author John J. Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum CrudOperation {
  CREATE,
  READ,
  UPDATE,
  DELETE;

  /**
   * Returns the CrudOperation enumerated value matching the given String name or null if no match was found.  A match
   * is found by ignoring case and trimming leading/trailing whitespace in the String name.
   *
   * @param name the String name used to match the CrudOperation.
   * @return a CrudOperation enumerated value matching the String name or null if no match was found.
   * @see java.lang.String#equalsIgnoreCase(String)
   * @see org.cp.elements.dao.CrudOperation#name()
   * @see org.cp.elements.lang.StringUtils#trim(String)
   */
  @NullSafe
  public static CrudOperation valueOfIgnoreCase(final String name) {
    for (CrudOperation crudOperation : values()) {
      if (crudOperation.name().equalsIgnoreCase(StringUtils.trim(name))) {
        return crudOperation;
      }
    }

    return null;
  }

}
