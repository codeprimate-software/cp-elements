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
