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

import static org.cp.elements.lang.LangExtensions.assertThat;

import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * The ServicePort enum defines an enumeration of well-known named network service ports.
 *
 * @author John J. Blum
 * @see org.cp.elements.net.Protocol
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum ServicePort {
  DNS(53),
  EPHEMERAL(0),
  FTP(21),
  HTTP(80),
  HTTPS(443),
  LDAP(389),
  SFTP(115),
  SMTP(25),
  SSH(22),
  TELNET(23),
  TIME(37),
  WHOIS(43);

  public static final int MIN_PORT = 0;
  public static final int MAX_PORT = 65535;
  public static final int MAX_RESERVED_PORT = 1024;

  private final int portNumber;

  /**
   * Constructs an instance of the {@link ServicePort} enum initialized to the given network port number.
   *
   * @param portNumber well-known network service port number.
   * @throws java.lang.IllegalArgumentException if the port number is outside the valid network port number range
   * [0-65535].
   */
  ServicePort(int portNumber) {
    assertThat(portNumber).throwing(new IllegalArgumentException(String.format(
      "port number [%s] must be greater than equal 0 and less than equal 65535", portNumber)))
        .isGreaterThanEqualToAndLessThanEqualTo(MIN_PORT, MAX_PORT);

    this.portNumber = portNumber;
  }

  /**
   * Returns a {@link ServicePort} enumerated value for the given network service port number.
   *
   * @param portNumber network service port number to lookup.
   * @return a {@link ServicePort} enumerated value for the given network service port number.
   * @see #portNumber()
   */
  public static ServicePort valueOf(int portNumber) {
    for (ServicePort servicePort : values()) {
      if (servicePort.portNumber() == portNumber) {
        return servicePort;
      }
    }

    return null;
  }

  /**
   * Returns a {@link ServicePort} enumerated value matching the given name of the network service.
   *
   * @param name name of the network service.
   * @return a {@link ServicePort} enumerated value matching the given name of the network service.
   * Returns null if no {@link ServicePort} matching the network service by name could be found.
   * @see #name()
   */
  @NullSafe
  public static ServicePort valueOfIgnoreCase(String name) {
    for (ServicePort servicePort : values()) {
      if (servicePort.name().equalsIgnoreCase(StringUtils.trim(name))) {
        return servicePort;
      }
    }

    return null;
  }

  /**
   * Determines whether this ServicePort is reserved by the operating system.  Any port number that is 1024 or below
   * is reserved.
   *
   * @return a boolean indicating whether this ServicePort is reserved.
   * @see #portNumber()
   */
  public boolean isReserved() {
    return (portNumber() <= MAX_RESERVED_PORT);
  }

  /**
   * Gets the port number of this network service.
   *
   * @return an integer value indicating the port number of this network service.
   */
  public int portNumber() {
    return portNumber;
  }
}
