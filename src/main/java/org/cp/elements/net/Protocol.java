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
package org.cp.elements.net;

import java.net.URI;
import java.util.function.Predicate;

import org.cp.elements.function.FunctionUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * An {@link Enum Enumeration} of constants defining well-known network protocols.
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @see org.cp.elements.net.ServicePort
 * @since 1.0.0
 */
public enum Protocol {

  FTP(ServicePort.FTP, "ftp://", "File Transfer Protocol"),
  HTTP(ServicePort.HTTP, "http://", "Hypertext Transfer Protocol"),
  HTTPS(ServicePort.HTTPS, "https://", "Secure Hypertext Transfer Protocol"),
  LDAP(ServicePort.LDAP, "ldap://", "Lightweight Directory Access Protocol"),
  SFTP(ServicePort.SFTP, "sftp://", "Secure File Transfer Protocol"),
  SMTP(ServicePort.SMTP, "smtp://", "Simple Mail Transfer Protocol");

  private final ServicePort servicePort;

  private final String description;
  private final String scheme;

  /**
   * Constructs a new {@link Protocol} initialized with the given {@link ServicePort}, {@link String scheme}
   * and {@link String description} of the network protocol.
   *
   * @param servicePort {@link ServicePort} used by the network protocol.
   * @param scheme {@link String} declaring the scheme used by the network protocol.
   * @param description {@link String} describing the network protocol.
   * @see org.cp.elements.net.ServicePort
   */
  Protocol(@NotNull ServicePort servicePort, @NotNull String scheme, @NotNull String description) {

    this.servicePort = ObjectUtils.requireObject(servicePort, "ServicePort is required");
    this.scheme = ObjectUtils.requireObject(scheme, "Scheme used by the protocol is required");
    this.description = StringUtils.requireText(description, "A description of the protocol is required");
  }

  /**
   * Factory method used to search for and return an instance of {@link Protocol} matching the given,
   * required {@link Predicate}.
   *
   * @param protocolPredicate {@link Predicate} used to match the {@link Protocol}; must not be {@literal null}.
   * @return a {@link Protocol} enumerated value matching the {@link Predicate} or {@literal null}
   * if no {@link Protocol} matches the given {@link Predicate}.
   * @see java.util.function.Predicate
   * @see #values()
   */
  private static @Nullable Protocol valueOf(@NotNull Predicate<Protocol> protocolPredicate) {

    for (Protocol protocol : values()) {
      if (FunctionUtils.nullSafePredicateMatchNone(protocolPredicate).test(protocol)) {
        return protocol;
      }
    }

    return null;
  }

  /**
   * Returns a {@link Protocol} enumerated value matching the given {@link String name} of the network protocol.
   *
   * The match on {@link String name} is case-insensitive.
   *
   * @param name {@link String} containing the name of the network protocol to match.
   * @return a {@link Protocol} enumerated value matching the given {@link String name} of the network protocol
   * or {@literal null} if no match could be found.
   * @see #valueOf(Predicate)
   * @see #name()
   */
  public static @Nullable Protocol valueOfNameIgnoreCase(@Nullable String name) {
    return valueOf(protocol -> protocol.name().equalsIgnoreCase(StringUtils.trim(name)));
  }

  /**
   * Returns a {@link Protocol} enumerated value matching the given {@link Integer port number}
   * of the network protocol.
   *
   * @param portNumber {@link Integer} declaring the port number of the network protocol to match.
   * @return a {@link Protocol} enumerated value matching the given {@link Integer port number}
   * of the network protocol or {@literal null} if no match could be found.
   * @see #valueOf(Predicate)
   * @see #portNumber()
   */
  public static @Nullable Protocol valueOfPortNumber(int portNumber) {
    return valueOf(protocol -> protocol.portNumber() == portNumber);
  }

  /**
   * Returns a {@link Protocol} enumerated value matching the given {@link String scheme} of the network protocol.
   *
   * @param scheme {@link String} containing the scheme of the network protocol to match.
   * @return a {@link Protocol} enumerated value matching the given {@link String scheme} of the network protocol
   * or {@literal null} if no match could be found.
   * @see #valueOf(Predicate)
   * @see #scheme()
   */
  public static @Nullable Protocol valueOfScheme(@Nullable String scheme) {
    return valueOf(protocol -> protocol.scheme().equals(StringUtils.trim(scheme)));
  }

  /**
   * Returns a {@link Protocol} enumerated value matching the given {@link ServicePort} of the network protocol.
   *
   * @param servicePort {@link ServicePort} of the network protocol to match.
   * @return a {@link Protocol} enumerated value matching the given {@link ServicePort} of the network protocol
   * or {@literal null} if no match could be found.
   * @see #valueOf(Predicate)
   * @see #servicePort()
   */
  public static @Nullable Protocol valueOfServicePort(@Nullable ServicePort servicePort) {
    return valueOf(protocol -> protocol.servicePort().equals(servicePort));
  }

  /**
   * Gets the {@link Integer port number} used by the network protocol.
   *
   * @return the {@link Integer port number} used by the network protocol.
   * @see org.cp.elements.net.ServicePort#portNumber()
   * @see #servicePort()
   */
  public int portNumber() {
    return servicePort().portNumber();
  }

  /**
   * Gets the {@link String scheme} used by this network protocol in the {@link URI}.
   *
   * @return a {@link String value} with the scheme used by this network protocol in the {@link URI}.
   */
  public @NotNull String scheme() {
    return this.scheme;
  }

  /**
   * Gets the {@link ServicePort} used by this network protocol.
   *
   * @return a {@link ServicePort} enumerated value declaring the {@link Integer port number}
   * used by this {@link Protocol}.
   * @see org.cp.elements.net.ServicePort
   */
  public @NotNull ServicePort servicePort() {
    return this.servicePort;
  }

  /**
   * Returns a {@link String} representation of this network protocol.
   *
   * @return a {@link String} describing this network protocol.
   */
  @Override
  public String toString() {
    return this.description;
  }
}
