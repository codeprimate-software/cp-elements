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

import org.cp.elements.function.FunctionUtils;
import org.cp.elements.lang.RelationalOperator;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * An {@link Enum Enumeration} of all HTTP status codes.
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @see <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html">Hypertext Transfer Protocol - HTTP/1.1 - RFC-2616</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum HttpStatus {

  // 1xx Informational
  CONTINUE(100, "Continue"),
  SWITCHING_PROTOCOLS(101, "Switching Protocols"),
  // 2xx Successful
  OK(200, "OK"),
  CREATED(201, "Created"),
  ACCEPTED(202, "Accepted"),
  NON_AUTHORITATIVE_INFORMATION(203, "Non-Authoritative Information"),
  NO_CONTENT(204, "No Content"),
  RESET_CONTENT(205, "Reset Content"),
  PARTIAL_CONTENT(206, "Partial Content"),
  // 3xx Redirection
  MULTIPLE_CHOICES(300, "Multiple Choices"),
  MOVED_PERMANENTLY(301, "Moved Permanently"),
  FOUND(302, "Found"),
  SEE_OTHER(303, "See Other"),
  NOT_MODIFIED(304, "Not Modified"),
  USE_PROXY(305, "Use Proxy"),
  UNUSED(306, "(Unused)"),
  TEMPORARY_REDIRECT(307, "Temporary Redirect"),
  // 4xx Client Error
  BAD_REQUEST(400, "Bad Request"),
  UNAUTHORIZED(401, "Unauthorized"),
  PAYMENT_REQUIRED(402, "Payment Required"),
  FORBIDDEN(403, "Forbidden"),
  NOT_FOUND(404, "Not Found"),
  METHOD_NOT_ALLOWED(405, "Method Not Allowed"),
  NOT_ACCEPTABLE(406, "Not Acceptable"),
  PROXY_AUTHENTICATION_REQUIRED(407, "Proxy Authentication Required"),
  REQUEST_TIMEOUT(408, "Request Timeout"),
  CONFLICT(409, "Conflict"),
  GONE(410, "Gone"),
  LENGTH_REQUIRED(411, "Length Required"),
  PRECONDITION_FAILED(412, "Precondition Failed"),
  REQUEST_ENTITY_TOO_LARGE(413, "Request Entity Too Large"),
  REQUEST_URI_TOO_LONG(414, "Request-URI Too Long"),
  UNSUPPORTED_MEDIA_TYPE(415, "Unsupported Media Type"),
  REQUESTED_RANGE_NOT_SATISFIABLE(416, "Request Range Not Satisfiable"),
  EXPECTATION_FAILED(417, "Expectation Failed"),
  // 5xx Server Error
  INTERNAL_SERVER_ERROR(500, "Internal Server Error"),
  NOT_IMPLEMENTED(501, "Not Implemented"),
  BAD_GATEWAY(502, "Bad Gateway"),
  SERVICE_UNAVAILABLE(503, "Service Unavailable"),
  GATEWAY_TIMEOUT(504, "Gateway Timeout"),
  HTTP_VERSION_NOT_SUPPORTED(505, "HTTP Version Not Supported");

  public static final int INFORMATIONAL_HTTP_STATUS_CODE_BASE = 100;
  public static final int SUCCESSFUL_HTTP_STATUS_CODE_BASE = 200;
  public static final int REDIRECTION_HTTP_STATUS_CODE_BASE = 300;
  public static final int CLIENT_ERROR_HTTP_STATUS_CODE_BASE = 400;
  public static final int SERVER_ERROR_HTTP_STATUS_CODE_BASE = 500;

  private final int httpStatusCode;

  private final String description;

  /**
   * Constructs a new {@link HttpStatus} initialized with the corresponding {@link Integer numeric value}
   * and {@link String description} for the HTTP status code.
   *
   * @param httpStatusCode {@link Integer} declaring the numeric value for the HTTP status code.
   * @param description {@link String} containing a description of the HTTP status code.
   * @throws IllegalArgumentException if the {@link String description} is {@literal null}
   * or {@literal empty}.
   */
  HttpStatus(int httpStatusCode, @NotNull String description) {
    this.httpStatusCode = httpStatusCode;
    this.description = StringUtils.requireText(description, "Description [%s] is required");
  }

  /**
   * Factory method used to search for and return an instance of {@link HttpStatus} matching the given,
   * required {@link Predicate} or returns {@literal null} if no match was found.
   *
   * @param httpStatusPredicate {@link Predicate} used to match the requested {@link HttpStatus};
   * must not be {@literal null}.
   * @return an {@link HttpStatus} matching the given, required {@link Predicate} or returns {@literal null}
   * if no match was found.
   * @see java.util.function.Predicate
   * @see #values()
   */
  private static @Nullable HttpStatus valueOf(@NotNull Predicate<HttpStatus> httpStatusPredicate) {

    for (HttpStatus httpStatus : values()) {
      if (FunctionUtils.nullSafePredicateMatchingNone(httpStatusPredicate).test(httpStatus)) {
        return httpStatus;
      }
    }

    return null;
  }

  /**
   * Returns an {@link HttpStatus} enumerated value matching the {@link Integer numeric value} of the HTTP status code
   * or {@literal null} if no match was found.
   *
   * @param httpStatusCode {@link Integer} with the numeric value of the HTTP status code
   * used to match the {@link HttpStatus}.
   * @return an {@link HttpStatus} enumerated value matching the {@link Integer numeric value} of the HTTP status code
   * or {@literal null} if no match was found.
   * @see #valueOf(Predicate)
   * @see #code()
   */
  public static @Nullable HttpStatus valueOf(int httpStatusCode) {
    return valueOf(httpStatus -> httpStatus.code() == httpStatusCode);
  }

  /**
   * Returns an {@link HttpStatus} enumerated value matching the given {@link String description} of the HTTP status
   * or {@literal null} if no match was found.
   *
   * @param description {@link String} containing a {@literal description} of the HTTP status.
   * @return an {@link HttpStatus} enumerated value matching the given {@link String description} of the HTTP status
   * or {@literal null} if no match was found.
   * @see #valueOf(Predicate)
   * @see #description()
   */
  public static @Nullable HttpStatus valueOfDescription(@Nullable String description) {
    return valueOf(httpStatus -> httpStatus.description().equalsIgnoreCase(StringUtils.trim(description)));
  }

  /**
   * Determines whether this {@link HttpStatus} enumerated value is in a given HTTP status code classification
   * or category based on it's HTTP status code.
   *
   * @param httpStatusCodeBase {@link Integer base numeric value} used for classification or categorization
   * of the HTTP status code, for example {@literal 400}.
   * @return a boolean valued indicating whether this {@link HttpStatus} enumerated value is in a given HTTP status code
   * classification or category based on it's HTTP status code.
   * @see #code()
   */
  boolean is(int httpStatusCodeBase) {
    return RelationalOperator.greaterThanAndLessThan(-1, 100)
      .evaluate(code() - httpStatusCodeBase);
  }

  /**
   * Determines whether this {@link HttpStatus} enumerated value represents an {@literal Informational}
   * HTTP status code.
   *
   * @return a boolean value indicating whether this {@link HttpStatus} enumerated value represents
   * an {@literal Informational} HTTP status code.
   * @see #is(int)
   */
  public boolean isInformational() {
    return is(INFORMATIONAL_HTTP_STATUS_CODE_BASE);
  }

  /**
   * Determines whether this {@link HttpStatus} enumerated value represents a {@literal Successful} HTTP status code.
   *
   * @return a boolean value indicating whether this {@link HttpStatus} enumerated value represents
   * a {@literal Successful} HTTP status code.
   * @see #is(int)
   */
  public boolean isSuccessful() {
    return is(SUCCESSFUL_HTTP_STATUS_CODE_BASE);
  }

  /**
   * Determines whether this {@link HttpStatus} enumerated value represents a {@literal Redirection} HTTP status code.
   *
   * @return a boolean value indicating whether this {@link HttpStatus} enumerated value represents
   * a {@literal Redirection} HTTP status code.
   * @see #is(int)
   */
  public boolean isRedirection() {
    return is(REDIRECTION_HTTP_STATUS_CODE_BASE);
  }

  /**
   * Determines whether this {@link HttpStatus} enumerated value represents a {@literal Client Error} HTTP status code.
   *
   * @return a boolean value indicating whether this {@link HttpStatus} enumerated value represents
   * a {@literal Client Error} HTTP status code.
   * @see #is(int)
   */
  public boolean isClientError() {
    return is(CLIENT_ERROR_HTTP_STATUS_CODE_BASE);
  }

  /**
   * Determines whether this {@link HttpStatus} enumerated value represents a {@literal Server Error} HTTP status code.
   *
   * @return a boolean value indicating whether this {@link HttpStatus} enumerated value represents
   * a {@literal Server Error} HTTP status code.
   * @see #is(int)
   */
  public boolean isServerError() {
    return is(SERVER_ERROR_HTTP_STATUS_CODE_BASE);
  }

  /**
   * Returns the {@link Integer numeric HTTP status code} for this {@link HttpStatus} enumerated value.
   *
   * @return the {@link Integer numeric HTTP status code} for this {@link HttpStatus} enumerated value.
   */
  public int code() {
    return this.httpStatusCode;
  }

  /**
   * Returns a {@link String description} to describe this {@link HttpStatus}.
   *
   * @return a {@link String description} to describe this {@link HttpStatus}.
   */
  public @NotNull String description() {
    return this.description;
  }

  /**
   * Returns a {@link String description} of the {@link HttpStatus} enumerated value as {@literal ### - description},
   * where {@literal ###} is the {@link Integer numeric HTTP status code} and the {@literal description}
   * describes the HTTP status code.
   *
   * @return a {@link String} describing this {@link HttpStatus} enumerated value.
   * @see java.lang.Object#toString()
   * @see #description()
   * @see #code()
   */
  @Override
  public @NotNull String toString() {
    return String.format("%1$d - %2$s", code(), description());
  }
}
