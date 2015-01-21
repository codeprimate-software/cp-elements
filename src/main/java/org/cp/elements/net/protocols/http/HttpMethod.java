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


