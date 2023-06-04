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

import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * An {@link Enum Enueration} of all HTTP request/response headers.
 *
 * @author John J. Blum
 * @see java.lang.Enum
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

  // Name of the Http request/response header
  private final String name;

  /**
   * Constructs a new {@link HttpHeader} initialized with the given, required HTTP request/response header
   * {@link String name}.
   *
   * @param name {@link String} containing the {@literal name} of the HTTP request/response header.
   * @throws java.lang.IllegalArgumentException if the HTTP request/response header {@link String name}
   * is {@literal null} or {@literal empty}.
   */
  HttpHeader(@NotNull String name) {
    this.name = StringUtils.requireText(name, "HTTP protocol request/response header name is required");
  }

  /**
   * Returns an {@link HttpHeader} enumerated value for the given {@link String name} of the HTTP request/response
   * header or {@literal null} if no match was found.
   *
   * @param name {@link String} containing the {@literal name} of the HTTP request/response header.
   * @return an {@link HttpHeader} enumerated value matching the given {@link String name}
   * of the HTTP request/response header or {@literal null} if no match was found.
   * @see #getName()
   * @see #values()
   */
  public static @Nullable HttpHeader valueOfNameIgnoreCase(@Nullable String name) {

    for (HttpHeader httpHeader : values()) {
      if (httpHeader.getName().equalsIgnoreCase(StringUtils.trim(name))) {
        return httpHeader;
      }
    }

    return null;
  }

  /**
   * Returns the {@link String name} of this HTTP request/response header.
   *
   * @return the {@link String name} of this HTTP request/response header.
   */
  public @NotNull String getName() {
    return this.name;
  }
}
