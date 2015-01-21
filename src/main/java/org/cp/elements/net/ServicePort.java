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

package org.cp.elements.net;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.RelationalOperator;
import org.cp.elements.lang.StringUtils;

/**
 * The ServicePort enum defines an enumeration of values for well-known network service ports.
 *
 * @author John J. Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum ServicePort {
  DNS(53),
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

  private final int portNumber;

  /**
   * Constructs an instance of the ServicePort enum initialized with the specified network port number of the service.
   *
   * @param portNumber an integer value indicating the network port number of the service.
   * @throws java.lang.IllegalArgumentException if the network port number is outside
   * the valid network port number range (0..65535).
   */
  ServicePort(final int portNumber) {
    Assert.isTrue(RelationalOperator.greaterThanEqualToAndLessThanEqualTo(0, 65535).evaluate(portNumber),
      "The port number must be between 0 and 65535 inclusive!");
    this.portNumber = portNumber;
  }

  /**
   * Returns the ServicePort enumerated value corresponding to the given service port number.
   *
   * @param portNumber an integer value indicating the service port number used ot match the ServicePort.
   * @return a ServicePort enumerated value matching the service port number or null if no match was found.
   * @see org.cp.elements.net.ServicePort#getPortNumber()
   */
  public static ServicePort valueOf(final int portNumber) {
    for (ServicePort servicePort : values()) {
      if (servicePort.getPortNumber() == portNumber) {
        return servicePort;
      }
    }

    return null;
  }

  /**
   * Returns a ServicePort enumerated value matching the given String name or null if no match could be found.  A match
   * is found by ignoring case and trimming leading/trailing whitespace in the String name.
   *
   * @param name the String name used to match the ServicePort.
   * @return a ServicePort enumerated value matching the String name or null if no match was found.
   * @see java.lang.String#equalsIgnoreCase(String)
   * @see org.cp.elements.lang.StringUtils#trim(String)
   * @see org.cp.elements.net.ServicePort#name()
   */
  @NullSafe
  public static ServicePort valueOfIgnoreCase(final String name) {
    for (ServicePort servicePort : values()) {
      if (servicePort.name().equalsIgnoreCase(StringUtils.trim(name))) {
        return servicePort;
      }
    }

    return null;
  }

  /**
   * Gets the port number of this service.
   *
   * @return an integer value indicating the port number of this service.
   */
  public int getPortNumber() {
    return portNumber;
  }

  /**
   * Determines whether this ServicePort is reserved by the operating system.  Any port number that is 1024 or below
   * is reserved.
   *
   * @return a boolean indicating whether this ServicePort is reserved.
   * @see #getPortNumber()
   */
  public boolean isReserved() {
    return (getPortNumber() <= 1024);
  }

}
