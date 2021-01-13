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
package org.cp.elements.dao;

import java.util.Optional;

import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * The {@link CrudOperation} enumerated type is an enumeration of CRUD (CREATE, READ, UPDATE, DELETE)
 * data access operations.
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
   * Returns a {@link CrudOperation} enumerated value matching the given {@link String name} or {@link Optional#empty()}
   * if no match was found.
   *
   * A match is performed by ignoring case and trimming all leading and trailing whitespace
   * in the given {@link String name}.
   *
   * @param name {@link String} containing the {@literal name} used to match the {@link CrudOperation} enumerated value.
   * @return a {@link CrudOperation} enumerated value matching the {@link String name} or {@link Optional#empty()}
   * if no match was found.
   * @see org.cp.elements.dao.CrudOperation#name()
   * @see org.cp.elements.lang.StringUtils#trim(String)
   * @see java.lang.String#equalsIgnoreCase(String)
   */
  @NullSafe
  public static Optional<CrudOperation> valueOfIgnoreCase(String name) {

    for (CrudOperation crudOperation : values()) {
      if (crudOperation.name().equalsIgnoreCase(StringUtils.trim(name))) {
        return Optional.of(crudOperation);
      }
    }

    return Optional.empty();
  }
}
