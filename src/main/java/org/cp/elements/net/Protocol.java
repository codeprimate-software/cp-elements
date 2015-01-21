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

import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.StringUtils;

/**
 * The Protocol enum defines constants for various well-known network protocols.
 * 
 * @author John J. Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum Protocol {
  FTP(ServicePort.FTP, "ftp://", "File Transfer Protocol"),
  HTTP(ServicePort.HTTP, "http://", "Hypertext Transfer Protocol"),
  HTTPS(ServicePort.HTTPS, "https://", "Secure Hypertext Transfer Protocol"),
  SFTP(ServicePort.SFTP, "sftp://", "Secure File Transfer Protocol"),
  SMTP(ServicePort.SMTP, "smtp://", "Simple Mail Transfer Protocol");

  private final ServicePort port;

  private final String description;
  private final String scheme;

  /**
   * Constructs an instance of the Protocol enum initialized with the specified ServicePort, Scheme and Description
   * of the network protocol.
   *
   * @param port an integer value indicating the network port number used by the protocol.
   * @param scheme a String value indicating the scheme of the protocol.
   * @param description a String describing the protocol.
   */
  Protocol(final ServicePort port, final String scheme, final String description) {
    this.port = port;
    this.scheme = scheme;
    this.description = description;
  }

  /**
   * Returns a Protocol enumerated value matching the given String name or null if no match could be found.  A match
   * is found by ignoring case and trimming leading/trailing whitespace in the String name.
   *
   * @param name the String name used to match the Protocol.
   * @return a Protocol enumerated value matching the String name or null if no match could be found.
   * @see java.lang.String#equalsIgnoreCase(String)
   * @see org.cp.elements.lang.StringUtils#trim(String)
   * @see org.cp.elements.net.Protocol#name()
   */
  @NullSafe
  public static Protocol valueOfIgnoreCase(final String name) {
    for (Protocol protocol : values()) {
      if (protocol.name().equalsIgnoreCase(StringUtils.trim(name))) {
        return protocol;
      }
    }

    return null;
  }

  /**
   * Returns a Protocol enumerated value having a ServicePort matching the given ServicePort.
   *
   * @param port a ServicePort used to match the Protocol.
   * @return a Protocol enumerated value having a ServicePort matching the given ServicePort
   * or null if no match could be found.
   * @see org.cp.elements.net.Protocol#getPort()
   * @see org.cp.elements.net.ServicePort
   */
  @NullSafe
  public static Protocol valueOfPort(final ServicePort port) {
    for (Protocol protocol : values()) {
      if (protocol.getPort().equals(port)) {
        return protocol;
      }
    }

    return null;
  }

  /**
   * Returns a Protocol enumerated value having a scheme matching the given scheme.
   *
   * @param scheme a String indicating the scheme used to match the Protocol.
   * @return a Protocol enumerated value having a scheme matching the given scheme
   * or null if no match could be found.
   * @see org.cp.elements.net.Protocol#getScheme()
   */
  @NullSafe
  public static Protocol valueOfScheme(final String scheme) {
    for (Protocol protocol : values()) {
      if (protocol.getScheme().equals(StringUtils.trim(scheme))) {
        return protocol;
      }
    }

    return null;
  }

  /**
   * Gets the ServicePort used by this network protocol.
   *
   * @return a ServicePort enum indicating the service port number used by this Protocol.
   * @see org.cp.elements.net.ServicePort
   */
  public ServicePort getPort() {
    return port;
  }

  /**
   * Gets the scheme used by the network protocol in the URI.
   *
   * @return a String value indicating the scheme used by the protocol in the URI.
   */
  public String getScheme() {
    return scheme;
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
