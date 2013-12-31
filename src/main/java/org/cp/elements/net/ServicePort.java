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

/**
 * The ServicePort enum defines constants for well-known service ports.
 * </p>
 * @author John J. Blum
 * @since 1.0.0
 */
public enum ServicePort {
  DNS(53),
  FTP(21),
  HTTP(80),
  HTTPS(443),
  LDAP(389),
  SMTP(25),
  SSH(22),
  TELNET(23),
  TIME(37),
  WHOIS(43);

  private final int portNumber;

  ServicePort(final int portNumber) {
    assert portNumber >= 0 && portNumber <= 65535 : "The port number must be between 0 and 65535 inclusive!";
    this.portNumber = portNumber;
  }

  public static ServicePort valueOf(final int portNumber) {
    for (final ServicePort servicePort : values()) {
      if (servicePort.getPortNumber() == portNumber) {
        return servicePort;
      }
    }

    return null;
  }

  public int getPortNumber() {
    return portNumber;
  }

  public boolean isReserved() {
    return (getPortNumber() <= 1024);
  }

}
