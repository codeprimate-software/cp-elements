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

package org.cp.elements.net.protocols.http;

import org.cp.elements.dao.CrudOperation;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * The HttpMethod enum is an enumeration of all HTTP protocol methods (POST, GET, PUT, DELETE, HEADERS, etc).
 *
 * @author John J. Blum
 * @see org.cp.elements.dao.CrudOperation
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum HttpMethod {
  POST(CrudOperation.CREATE),
  GET(CrudOperation.READ),
  PUT(CrudOperation.UPDATE),
  DELETE(CrudOperation.DELETE),
  CONNECT(null),
  HEAD(null),
  OPTIONS(null),
  TRACE(null);

  private final CrudOperation crudOperation;

  /**
   * Constructs an instance of the {@link HttpMethod} enum initialized with the corresponding CRUD operation.
   *
   * @param crudOperation {@link CrudOperation} corresponding to this HTTP method.
   * @see org.cp.elements.dao.CrudOperation
   */
  HttpMethod(CrudOperation crudOperation) {
    this.crudOperation = crudOperation;
  }

  /**
   * Returns an {@link HttpMethod} enumerated value corresponding to the given CRUD operation
   * or null if no match was found.
   *
   * @param crudOperation CRUD operation used to match the HTTP method.
   * @return an {@link HttpMethod} enumerated value corresponding to the given CRUD operation
   * or null if no match was found.
   * @see org.cp.elements.dao.CrudOperation
   * @see #crudOperation()
   */
  @NullSafe
  public static HttpMethod valueOf(CrudOperation crudOperation) {
    for (HttpMethod httpMethod : values()) {
      if (ObjectUtils.equals(httpMethod.crudOperation(), crudOperation)) {
        return httpMethod;
      }
    }

    return null;
  }

  /**
   * Returns an {@link HttpMethod} enumerated value matching the case insensitive name of the HTTP method
   * or null if no match was found.
   *
   * @param name name of the HTTP method.
   * @return an {@link HttpMethod} enumerated value matching the case insensitive name of the HTTP method
   * or null if no match is found.
   * @see #name()
   */
  @NullSafe
  public static HttpMethod valueOfIgnoreCase(String name) {
    for (HttpMethod httpMethod : values()) {
      if (httpMethod.name().equalsIgnoreCase(StringUtils.trim(name))) {
        return httpMethod;
      }
    }

    return null;
  }

  /**
   * Gets the corresponding CRUD operation for this HTTP method.
   *
   * @return a {@link CrudOperation} corresponding to this {@link HttpMethod}.
   * @see org.cp.elements.dao.CrudOperation
   */
  public CrudOperation crudOperation() {
    return crudOperation;
  }
}
