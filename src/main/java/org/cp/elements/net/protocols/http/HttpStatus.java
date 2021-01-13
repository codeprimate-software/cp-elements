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

import org.cp.elements.lang.RelationalOperator;
import org.cp.elements.lang.StringUtils;

/**
 * The HttpStatus enum is an enumeration of all HTTP status codes.
 *
 * @author John J. Blum
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
   * Constructs an instance of the {@link HttpStatus} enum initialized with the corresponding HTTP status code
   * and description of the HTTP status code.
   *
   * @param httpStatusCode numeric HTTP status code.
   * @param description HTTP status code description.
   */
  HttpStatus(int httpStatusCode, String description) {
    this.httpStatusCode = httpStatusCode;
    this.description = description;
  }

  /**
   * Returns an {@link HttpStatus} enumerated value matching the numeric HTTP status code
   * or null if no match was found.
   *
   * @param httpStatusCode numeric HTTP status code used to match the {@link HttpStatus}.
   * @return an {@link HttpStatus} enumerated value for the given numeric HTTP status code
   * or null if no match was found.
   * @see #code()
   */
  public static HttpStatus valueOf(int httpStatusCode) {
    for (HttpStatus httpStatus : values()) {
      if (httpStatus.code() == httpStatusCode) {
        return httpStatus;
      }
    }

    return null;
  }

  /**
   * Returns an {@link HttpStatus} enumerated value for the given HTTP status code description
   * or null if no match was found.
   *
   * @param description HTTP status code description.
   * @return an {@link HttpStatus} enumerated value for the given HTTP status code description
   * or null if no match was found.
   * @see #description()
   */
  public static HttpStatus valueOfDescription(String description) {
    for (HttpStatus httpStatus : values()) {
      if (httpStatus.description().equalsIgnoreCase(StringUtils.trim(description))) {
        return httpStatus;
      }
    }

    return null;
  }

  /**
   * Determines whether this {@link HttpStatus} enumerated value is in a given HTTP statsus code category
   * based on it's HTTP status code.
   *
   * @param httpStatusCodeBase base numeric HTTP status code for the category (e.g. 400).
   * @return a boolean valued indicating whether this {@link HttpStatus} enumerated value is in
   * a particular category of HTTP status codes.
   * @see org.cp.elements.lang.RelationalOperator#greaterThanAndLessThan(Comparable, Comparable)
   */
  boolean is(int httpStatusCodeBase) {
    return RelationalOperator.greaterThanAndLessThan(-1, 100).evaluate(code() - httpStatusCodeBase);
  }

  /**
   * Determines whether this {@link HttpStatus} enumerated value represents an "Informational" HTTP status code.
   *
   * @return a boolean value indicating whether this {@link HttpStatus} enumerated value represents an "Informational"
   * HTTP status code.
   * @see #is(int)
   */
  public boolean isInformational() {
    return is(INFORMATIONAL_HTTP_STATUS_CODE_BASE);
  }

  /**
   * Determines whether this {@link HttpStatus} enumerated value represents an "Successful" HTTP status code.
   *
   * @return a boolean value indicating whether this {@link HttpStatus} enumerated value represents an "Successful"
   * HTTP status code.
   * @see #is(int)
   */
  public boolean isSuccessful() {
    return is(SUCCESSFUL_HTTP_STATUS_CODE_BASE);
  }

  /**
   * Determines whether this {@link HttpStatus} enumerated value represents an "Redirection" HTTP status code.
   *
   * @return a boolean value indicating whether this {@link HttpStatus} enumerated value represents an "Redirection"
   * HTTP status code.
   * @see #is(int)
   */
  public boolean isRedirection() {
    return is(REDIRECTION_HTTP_STATUS_CODE_BASE);
  }

  /**
   * Determines whether this {@link HttpStatus} enumerated value represents an "Client Error" HTTP status code.
   *
   * @return a boolean value indicating whether this {@link HttpStatus} enumerated value represents an "Client Error"
   * HTTP status code.
   * @see #is(int)
   */
  public boolean isClientError() {
    return is(CLIENT_ERROR_HTTP_STATUS_CODE_BASE);
  }

  /**
   * Determines whether this {@link HttpStatus} enumerated value represents an "Server Error" HTTP status code.
   *
   * @return a boolean value indicating whether this {@link HttpStatus} enumerated value represents an "Server Error"
   * HTTP status code.
   * @see #is(int)
   */
  public boolean isServerError() {
    return is(SERVER_ERROR_HTTP_STATUS_CODE_BASE);
  }

  /**
   * Returns the numeric HTTP status code represented by this {@link HttpStatus} enumerated value.
   *
   * @return the numeric HTTP status code for this enum.
   */
  public int code() {
    return httpStatusCode;
  }

  /**
   * Returns a description of the HTTP status code represented by this {@link HttpStatus} enumerated value.
   *
   * @return a description of the HTTP status code for this enum.
   */
  public String description() {
    return description;
  }

  /**
   * Returns a String description of the {@link HttpStatus} enumerated value as ### - description, where ###
   * is the numeric HTTP status code.
   *
   * @return a String describing this {@link HttpStatus} enumerated value.
   * @see #description()
   * @see #code()
   */
  @Override
  public String toString() {
    return String.format("%1$d - %2$s", code(), description());
  }
}
