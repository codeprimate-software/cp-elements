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

import static org.cp.elements.lang.LangExtensions.assertThat;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;

import java.util.function.Predicate;

import org.cp.elements.function.FunctionUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * An {@link Enum Enumeration} of well-known {@link String named} network service ports.
 *
 * @author John J. Blum
 * @see java.lang.Enum
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

  private static final String INVALID_PORT_EXCEPTION_MESSAGE =
    "Port number [%s] must be greater than equal to 0 and less than equal to 65535";

  private final int portNumber;

  /**
   * Constructs a new {@link ServicePort} initialized with the given {@link Integer network service port number}.
   *
   * @param portNumber {@link Integer value} declaring the well-known, network service port number.
   * @throws java.lang.IllegalArgumentException if the {@link Integer port number} is outside the range of
   * valid network port numbers {@literal [0-65535]}.
   */
  ServicePort(int portNumber) {

    assertThat(portNumber)
      .throwing(newIllegalArgumentException(INVALID_PORT_EXCEPTION_MESSAGE, portNumber))
      .isGreaterThanEqualToAndLessThanEqualTo(MIN_PORT, MAX_PORT);

    this.portNumber = portNumber;
  }

  /**
   * Factory method used to search for and return an instance of a {@link ServicePort} enumerated value
   * matching the given, required {@link Predicate}.
   *
   * @param servicePortPredicate {@link Predicate} used to find and match a {@link ServicePort}
   * @return an instance of a {@link ServicePort} enumerated value matching the given {@link Predicate}.
   * Returns {@literal null} if no {@link ServicePort} could be found matching the given {@link Predicate}.
   * @see java.util.function.Predicate
   */
  private static @Nullable ServicePort valueOf(@NotNull Predicate<ServicePort> servicePortPredicate) {

    for (ServicePort servicePort : values()) {
      if (FunctionUtils.nullSafePredicateMatchNone(servicePortPredicate).test(servicePort)) {
        return servicePort;
      }
    }

    return null;
  }

  /**
   * Returns a {@link ServicePort} enumerated value for the given {@link Integer network service port number}.
   *
   * @param portNumber {@link Integer} specifying the network service port number to search.
   * @return a {@link ServicePort} enumerated value for the given {@link Integer network service port number}.
   * Returns {@literal null} if no {@link ServicePort} could be found
   * matching the {@link Integer network service port number}.
   * @see #valueOf(Predicate)
   * @see #portNumber()
   */
  public static @Nullable ServicePort valueOf(int portNumber) {
    return valueOf(servicePort -> servicePort.portNumber() == portNumber);
  }

  /**
   * Returns a {@link ServicePort} enumerated value matching the given {@link String name} of the network service.
   *
   * @param name {@link String} containing the name of the network service.
   * @return a {@link ServicePort} enumerated value matching the given {@link String name} of the network service.
   * Returns {@literal null} if no {@link ServicePort} could be found matching the network service
   * by {@link String name}.
   * @see #valueOf(Predicate)
   * @see #name()
   */
  public static @Nullable ServicePort valueOfNameIgnoreCase(@Nullable String name) {
    return valueOf(servicePort -> servicePort.name().equalsIgnoreCase(StringUtils.trim(name)));
  }

  /**
   * Determines whether this {@link ServicePort} is reserved by the operating system (OS).
   *
   * Any port number that is less than or equal to {@literal 1024}  is reserved.
   *
   * @return a boolean indicating whether this {@link ServicePort} is reserved by the operating system (OS).
   * @see #portNumber()
   */
  public boolean isReserved() {
    return portNumber() <= MAX_RESERVED_PORT;
  }

  /**
   * Gets the {@link Integer port number} of this network service.
   *
   * @return the {@link Integer port number} of this network service.
   */
  public int portNumber() {
    return this.portNumber;
  }
}
