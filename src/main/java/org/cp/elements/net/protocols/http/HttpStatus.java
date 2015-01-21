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

import org.cp.elements.lang.RelationalOperator;
import org.cp.elements.lang.StringUtils;

/**
 * The HttpStatus enum is an enumeration of all HTTP status codes.
 *
 * @author John J. Blum
 * @link http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
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
  BAD_REEQUEST(400, "Bad Request"),
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

  private final int statusCode;

  private final String description;

  /**
   * Constructs an instance of the HttpStatus enum initialized with the corresponding HTTP status code
   * and description of the HTTP status code.
   *
   * @param statusCode an integer value specifying the numeric HTTP status code.
   * @param description a String describing the HTTP status.
   */
  HttpStatus(final int statusCode, final String description) {
    this.statusCode = statusCode;
    this.description = description;
  }

  /**
   * Returns an HttpStatus enumerated value matching the numeric HTTP status code or null if no match was found.
   *
   * @param statusCode an integer value specifying the numeric HTTP status code used to match the HttpStatus.
   * @return a HttpStatus enumerated value for the given numeric HTTP status code or null if no match was found.
   * @see #getCode()
   */
  public static HttpStatus valueOf(final int statusCode) {
    for (HttpStatus httpStatus : values()) {
      if (httpStatus.getCode() == statusCode) {
        return httpStatus;
      }
    }

    return null;
  }

  /**
   * Returns an HttpStatus enumerated value for the given HTTP status description or null if no match was found.
   *
   * @param description a String describing the HTTP status.
   * @return a HttpStatus enumerated value for the given HTTP status description or null if no match was found.
   * @see java.lang.String#equalsIgnoreCase(String)
   * @see org.cp.elements.lang.StringUtils#trim(String)
   * @see #getDescription()
   */
  public static HttpStatus valueOfDescription(final String description) {
    for (HttpStatus httpStatus : values()) {
      if (httpStatus.getDescription().equalsIgnoreCase(StringUtils.trim(description))) {
        return httpStatus;
      }
    }

    return null;
  }

  /**
   * Determines whether this HttpStatus enumerated type is in a given category based on it's HTTP status code.
   *
   * @param statusCodeBase an integer value indicating the base HTTP status code for the category.
   * @return a boolean valued indicating whether this HttpStatus enumerated type is in the particular category
   * of HTTP status codes.
   * @see org.cp.elements.lang.RelationalOperator#greaterThanAndLessThan(Comparable, Comparable)
   */
  private boolean is(final int statusCodeBase) {
    return RelationalOperator.greaterThanAndLessThan(-1, 100).evaluate(getCode() - statusCodeBase);
  }

  /**
   * Determines whether this HttpStatus enumerated type represents an "Informational" HTTP status code.
   *
   * @return a boolean value indicating whether this HttpStatus enumerated type represents
   * an "Informational" HTTP status code.
   */
  public boolean isInformational() {
    return is(100);
  }

  /**
   * Determines whether this HttpStatus enumerated type represents a "Successful" HTTP status code.
   *
   * @return a boolean value indicating whether this HttpStatus enumerated type represents
   * a "Successful" HTTP status code.
   */
  public boolean isSuccessful() {
    return is(200);
  }

  /**
   * Determines whether this HttpStatus enumerated type represents a "Redirection" HTTP status code.
   *
   * @return a boolean value indicating whether this HttpStatus enumerated type represents
   * a "Redirection" HTTP status code.
   */
  public boolean isRedirection() {
    return is(300);
  }

  /**
   * Determines whether this HttpStatus enumerated type represents a "Client Error" HTTP status code.
   *
   * @return a boolean value indicating whether this HttpStatus enumerated type represents
   * a "Client Error" HTTP status code.
   */
  public boolean isClientError() {
    return is(400);
  }

  /**
   * Determines whether this HttpStatus enumerated type represents a "Server Error" HTTP status code.
   *
   * @return a boolean value indicating whether this HttpStatus enumerated type represents
   * a "Server Error" HTTP status code.
   */
  public boolean isServerError() {
    return is(500);
  }

  /**
   * Returns the numeric HTTP status code represented by this HttpStatus enumerated value.
   *
   * @return an integer value indicating the numeric HTTP status code.
   */
  public int getCode() {
    return statusCode;
  }

  /**
   * Returns a description of the HTTP status represented by this HttpStatus enumerated value.
   *
   * @return a String describing the corresponding HTTP status.
   */
  public String getDescription() {
    return description;
  }

  /**
   * Returns a String description of the HttpStatus enumerated type as ### - description, where ###
   * is the numeric HTTP status code.
   *
   * @return a String describing this HttpStatus enumerated type.
   * @see #getDescription()
   * @see #getCode()
   */
  @Override
  public String toString() {
    return String.format("%1$d - %2$s", getCode(), getDescription());
  }

}
