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
import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;

/**
 * The HttpMethod enum is an enumeration of all HTTP protocol methods (POST, GET, PUT, DELETE, HEADERS, etc).
 *
 * @author John J. Blum
 * @see org.cp.elements.dao.CrudOperation
 * @since 1.1.0
 */
@SuppressWarnings("unused")
public enum HttpMethod {
  CONNECT(null),
  DELETE(CrudOperation.DELETE),
  GET(CrudOperation.READ),
  HEAD(null),
  OPTIONS(null),
  POST(CrudOperation.CREATE),
  PUT(CrudOperation.UPDATE),
  TRACE(null);

  private final CrudOperation crudOperation;

  /**
   * Constructs an instance of the HttpMethod enum initialized with the corresponding CRUD operation.
   *
   * @param crudOperation the CrudOperation corresponding to this HTTP method.
   * @see org.cp.elements.dao.CrudOperation
   */
  HttpMethod(final CrudOperation crudOperation) {
    this.crudOperation = crudOperation;
  }

  /**
   * Returns a HttpMethod enumerated value corresponding to the given CRUD operation or null if no match was found.
   *
   * @param crudOperation the CRUD operation used to match the HttpMethod.
   * @return a HttpMethod enumerated value corresponding to the given CRUD operation or null if no match was found.
   * @see org.cp.elements.dao.CrudOperation
   */
  @NullSafe
  public static HttpMethod valueOf(final CrudOperation crudOperation) {
    for (HttpMethod httpMethod : values()) {
      if (ObjectUtils.equals(httpMethod.getCrudOperation(), crudOperation)) {
        return httpMethod;
      }
    }

    return null;
  }

  /**
   * Returns a HttpMethod enumerated value matching the case insensitive String name of the HTTP method
   * or null if no match was found.
   *
   * @param name the String name used to match the HttpMethod.
   * @return the HttpMethod enumerated value matching the String name of the HTTP method or null if no match is found.
   * @see org.cp.elements.lang.StringUtils#trim(String)
   * @see #name()
   */
  @NullSafe
  public static HttpMethod valueOfIgnoreCase(final String name) {
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
   * @return a CrudOperation for this HTTP method.
   * @see org.cp.elements.dao.CrudOperation
   */
  public CrudOperation getCrudOperation() {
    return crudOperation;
  }

}


