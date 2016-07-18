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

package org.cp.elements.net;

import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.StringUtils;

/**
 * The Protocol enum defines constants for well-known network protocols.
 *
 * @author John J. Blum
 * @see org.cp.elements.net.ServicePort
 * @since 1.0.0
 */
@SuppressWarnings("unused")
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
   * Constructs an instance of the {@link Protocol} enum initialized with the given {@link ServicePort}, Scheme
   * and Description of the network protocol.
   *
   * @param servicePort {@link ServicePort} used by the network protocol.
   * @param scheme scheme of the network protocol.
   * @param description a String describing the network protocol.
   * @see org.cp.elements.net.ServicePort
   */
  Protocol(ServicePort servicePort, String scheme, String description) {
    this.servicePort = servicePort;
    this.scheme = scheme;
    this.description = description;
  }

  /**
   * Returns a {@link Protocol} enumerated value matching the given name of the network protocol.
   *
   * @param name name of the network protocol to lookup.
   * @return a {@link Protocol} enumerated value matching the given name of the network protocol
   * or null if no match could be found.
   * @see #name()
   */
  @NullSafe
  public static Protocol valueOfIgnoreCase(String name) {
    for (Protocol protocol : values()) {
      if (protocol.name().equalsIgnoreCase(StringUtils.trim(name))) {
        return protocol;
      }
    }

    return null;
  }

  /**
   * Returns a {@link Protocol} enumerated value matching the given network port number.
   *
   * @param portNumber network port number used to match the {@link Protocol}.
   * @return a {@link Protocol} enumerated value matching the given network port number
   * or null if no match could be found.
   * @see #portNumber()
   */
  @NullSafe
  public static Protocol valueOfPortNumber(int portNumber) {
    for (Protocol protocol : values()) {
      if (protocol.portNumber() == portNumber) {
        return protocol;
      }
    }

    return null;
  }

  /**
   * Returns a {@link Protocol} enumerated value for the given scheme.
   *
   * @param scheme scheme used to match the {@link Protocol}.
   * @return a {@link Protocol} enumerated value for the given scheme or null if no match could be found.
   * @see #scheme()
   */
  @NullSafe
  public static Protocol valueOfScheme(String scheme) {
    for (Protocol protocol : values()) {
      if (protocol.scheme().equals(StringUtils.trim(scheme))) {
        return protocol;
      }
    }

    return null;
  }

  /**
   * Returns a {@link Protocol} enumerated value for the given {@link ServicePort}.
   *
   * @param servicePort {@link ServicePort} used to lookup and match the {@link Protocol}.
   * @return a {@link Protocol} enumerated value for the given {@link ServicePort}
   * or null if no match could be found.
   * @see org.cp.elements.net.ServicePort
   * @see #servicePort()
   */
  @NullSafe
  public static Protocol valueOfServicePort(ServicePort servicePort) {
    for (Protocol protocol : values()) {
      if (protocol.servicePort().equals(servicePort)) {
        return protocol;
      }
    }

    return null;
  }

  /**
   * Gets the network port number for the network protocol.
   *
   * @return an integer value with the network port number of this network protocol.
   * @see #servicePort()
   */
  public int portNumber() {
    return servicePort().portNumber();
  }

  /**
   * Gets the scheme used by this network protocol in the URI.
   *
   * @return a String value with the scheme used by this network protocol in the URI.
   */
  public String scheme() {
    return scheme;
  }

  /**
   * Gets the {@link ServicePort} used by this network protocol.
   *
   * @return a {@link ServicePort} enumerated value indicating the network port number used by this {@link Protocol}.
   * @see org.cp.elements.net.ServicePort
   */
  public ServicePort servicePort() {
    return servicePort;
  }

  /**
   * Returns a String representation of this network protocol.
   *
   * @return a String describing this network protocol.
   * @see #description
   */
  @Override
  public String toString() {
    return this.description;
  }
}
