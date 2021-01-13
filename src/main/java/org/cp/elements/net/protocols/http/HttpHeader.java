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

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.StringUtils;

/**
 * The HttpHeader enum is an enumeration of all HTTP protocol request/response headers.
 *
 * @author John J. Blum
 * @see <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html">Hypertext Transfer Protocol - HTTP/1.1 - RFC2616</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum HttpHeader {
  ACCEPT("Accept"),
  ACCEPT_CHARSET("Accept-Charset"),
  ACCEPT_ENCODING("Accept-Encoding"),
  ACCEPT_LANGUAGE("Accept-Language"),
  ACCEPT_RANGES("Accept-Ranges"),
  AGE("Age"),
  ALLOW("Allow"),
  AUTHORIZATION("Authorization"),
  CACHE_CONTROL("Cache-Control"),
  CONNECTION("Connection"),
  CONTENT_ENCODING("Content-Encoding"),
  CONTENT_LANGUAGE("Content-Language"),
  CONTENT_LENGTH("Content-Length"),
  CONTENT_LOCATION("Content-Location"),
  CONTENT_MD5("Content-MD5"),
  CONTENT_RANGE("Content-Range"),
  CONTENT_TYPE("Content-Type"),
  DATE("Date"),
  ETAG("ETag"),
  EXPECT("Expect"),
  EXPIRES("Expires"),
  FROM("From"),
  HOST("Host"),
  IF_MATCH("If-Match"),
  IF_MODIFIED_SINCE("If-Modified-Since"),
  IF_NONE_MATCH("If-None-Match"),
  IF_RANGE("If-Range"),
  IF_UNMODIFIED_SINCE("If-Unmodified-Since"),
  LAST_MODIFIED("Last-Modified"),
  LOCATION("Location"),
  MAX_FORWARDS("Max-Forwards"),
  PRAGMA("Pragma"),
  PROXY_AUTHENTICATE("Proxy-Authenticate"),
  PROXY_AUTHORIZATION("Proxy-Authorization"),
  RANGE("Range"),
  REFERER("Referer"),
  RETRY_AFTER("Retry-After"),
  SERVER("Server"),
  TE("TE"),
  TRAILER("Trailer"),
  TRANSFER_ENCODING("Transfer-Encoding"),
  UPGRADE("Upgrade"),
  USER_AGENT("User-Agent"),
  VARY("Vary"),
  VIA("Via"),
  WARNING("Warning"),
  WWW_AUTHENTICATE("WWW-Authenticate");

  // name of the Http request/response header
  private final String name;

  /**
   * Constructs an instance of the {@link HttpHeader} enum initialized with the given HTTP request/response header name.
   *
   * @param name name of the HTTP request/response header.
   * @throws java.lang.IllegalArgumentException if the HTTP header name is null or empty.
   */
  HttpHeader(String name) {
    Assert.hasText(name, "HTTP protocol request/response header name is required");
    this.name = name;
  }

  /**
   * Returns an {@link HttpHeader} enumerated value for the given name of the HTTP request/response header
   * or null if no match was found.
   *
   * @param name name of the HTTP request/response header.
   * @return an {@link HttpHeader} enumerated value matching the given name of the HTTP request/response header
   * or null if no match was found.
   * @see #name()
   */
  public static HttpHeader valueOfIgnoreCase(String name) {
    for (HttpHeader httpHeader : values()) {
      if (httpHeader.getName().equalsIgnoreCase(StringUtils.trim(name))) {
        return httpHeader;
      }
    }

    return null;
  }

  /**
   * Returns the name of the HTTP request/response header.
   *
   * @return a String with the name of the HTTP request/response header.
   */
  public String getName() {
    return name;
  }
}
