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
 * The Protocol enum defines constants for various well-known network protocols.
 * </p>
 * @author John J. Blum
 * @since 1.0.0
 */
public enum Protocol {
  FTP("FTP", ServicePort.FTP, "ftp://", "File Transfer Protocol"),
  HTTP("HTTP", ServicePort.HTTP, "http://", "Hypertext Transfer Protocol"),
  HTTPS("HTTPS", ServicePort.HTTPS, "https://", "Secure Hypertext Transfer Protocol"),
  SMTP("SMTP", ServicePort.SMTP, "smtp://", "Simple Mail Transfer Protocol");

  private final ServicePort port;

  private final String description;
  private final String name;
  private final String scheme;

  Protocol(final String name, final ServicePort port, final String scheme, final String description) {
    this.name = name;
    this.port = port;
    this.scheme = scheme;
    this.description = description;
  }

  public static Protocol valueOfName(final String name) {
    for (final Protocol protocol : values()) {
      if (protocol.getName().equalsIgnoreCase(name)) {
        return protocol;
      }
    }

    return null;
  }

  public static Protocol valueOfPort(final ServicePort port) {
    for (final Protocol protocol : values()) {
      if (protocol.getPort().equals(port)) {
        return protocol;
      }
    }

    return null;
  }

  public static Protocol valueOfScheme(final String scheme) {
    for (final Protocol protocol : values()) {
      if (protocol.getScheme().equalsIgnoreCase(scheme)) {
        return protocol;
      }
    }

    return null;
  }

  public String getName() {
    return name;
  }

  public ServicePort getPort() {
    return port;
  }

  public String getScheme() {
    return scheme;
  }

  @Override
  public String toString() {
    return this.description;
  }

}
