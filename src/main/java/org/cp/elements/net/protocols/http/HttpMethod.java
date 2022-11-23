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
package org.cp.elements.net.protocols.http;

import java.util.function.Predicate;

import org.cp.elements.dao.CrudOperation;
import org.cp.elements.function.FunctionUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * An {@link Enum Enumeration} of all HTTP methods:
 * [ {@literal POST}, {@literal GET}, {@literal PUT}, {@literal DELETE}, ... ].
 *
 * Additionally, The {@link HttpMethod} is mapped to a corresponding and equivalent {@link CrudOperation}.
 *
 * @author John J. Blum
 * @see java.lang.Enum
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
   * Constructs a new instance of {@link HttpMethod} initialized with an equivalent
   * {@link CrudOperation CRUD operation}.
   *
   * @param crudOperation {@link CrudOperation} equivalent to this HTTP method.
   * @see org.cp.elements.dao.CrudOperation
   */
  HttpMethod(@Nullable CrudOperation crudOperation) {
    this.crudOperation = crudOperation;
  }

  /**
   * Factory method used to search for and return an instance of {@link HttpMethod} matching the given,
   * required {@link Predicate} or returns {@literal null} if not match was found.
   *
   * @param httpMethodPredicate {@link Predicate} used to search for and match the requested {@link HttpMethod};
   * must not be {@literal null}.
   * @return an {@link HttpMethod} matching the given, required {@link Predicate} or returns {@literal null}
   * if not match was found.
   * @see java.util.function.Predicate
   */
  private static @Nullable HttpMethod valueOf(@NotNull Predicate<HttpMethod> httpMethodPredicate) {

    for (HttpMethod httpMethod : values()) {
      if (FunctionUtils.nullSafePredicateMatchNone(httpMethodPredicate).test(httpMethod)) {
        return httpMethod;
      }
    }

    return null;
  }

  /**
   * Returns an {@link HttpMethod} enumerated value corresponding to the given {@link CrudOperation CRUD operation}
   * or {@literal null} if no match was found.
   *
   * @param crudOperation {@link CrudOperation CRUD operation} used to match the {@link HttpMethod HTTP method}.
   * @return an {@link HttpMethod} enumerated value corresponding to the given {@link CrudOperation CRUD operation}
   * or {@literal null} if no match was found.
   * @see org.cp.elements.dao.CrudOperation
   * @see #valueOf(Predicate)
   * @see #crudOperation()
   */
  public static @Nullable HttpMethod valueOf(@Nullable CrudOperation crudOperation) {
    return valueOf(httpMethod -> ObjectUtils.equals(httpMethod.crudOperation(), crudOperation));
  }

  /**
   * Returns an {@link HttpMethod} enumerated value matching the case-insensitive {@link String name}
   * of the {@link HttpMethod HTTP method} or {@literal null} if no match was found.
   *
   * @param name {@link String} containing the {@literal name} of the {@link HttpMethod HTTP method}.
   * @return an {@link HttpMethod} enumerated value matching the case-insensitive {@link String name}
   * of the {@link HttpMethod HTTP method} or {@literal null} if no match was found.
   * @see #valueOf(Predicate)
   * @see #name()
   */
  public static @Nullable HttpMethod valueOfNameIgnoreCase(@Nullable String name) {
    return valueOf(httpMethod -> httpMethod.name().equalsIgnoreCase(StringUtils.trim(name)));
  }

  /**
   * Gets the equivalent {@link CrudOperation CRUD operation} for this {@link HttpMethod HTTP method}.
   *
   * @return the equivalent {@link CrudOperation CRUD operation} for this {@link HttpMethod HTTP method}.
   * @see org.cp.elements.dao.CrudOperation
   */
  public @Nullable CrudOperation crudOperation() {
    return this.crudOperation;
  }
}
