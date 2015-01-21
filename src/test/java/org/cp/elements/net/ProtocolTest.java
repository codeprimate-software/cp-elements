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

import static org.junit.Assert.*;

import org.junit.Test;

/**
 * The ProtocolTest class is a test suite of test cases testing the contract and functionality of the Protocol enum.
 *
 * @author John J. Blum
 * @see org.cp.elements.net.Protocol
 * @see org.junit.Test
 * @since 1.0.0
 */
public class ProtocolTest {

  @Test
  public void testValueOfIgnoreCase() {
    assertEquals(Protocol.FTP, Protocol.valueOfIgnoreCase("FTP"));
    assertEquals(Protocol.HTTP, Protocol.valueOfIgnoreCase("Http"));
    assertEquals(Protocol.HTTPS, Protocol.valueOfIgnoreCase(" HttpS"));
    assertEquals(Protocol.SFTP, Protocol.valueOfIgnoreCase("sFTP"));
    assertEquals(Protocol.SMTP, Protocol.valueOfIgnoreCase(" SmTp  "));
  }

  @Test
  public void testValueOfIgnoreCaseWithInvalidValue() {
    assertNull(Protocol.valueOfIgnoreCase("FTPS"));
    assertNull(Protocol.valueOfIgnoreCase("htp "));
    assertNull(Protocol.valueOfIgnoreCase(" RMI"));
    assertNull(Protocol.valueOfIgnoreCase(" Rpc   "));
    assertNull(Protocol.valueOfIgnoreCase("sCP"));
    assertNull(Protocol.valueOfIgnoreCase("SSH"));
    assertNull(Protocol.valueOfIgnoreCase("  Snmp "));
    assertNull(Protocol.valueOfIgnoreCase("  "));
    assertNull(Protocol.valueOfIgnoreCase(""));
    assertNull(Protocol.valueOfIgnoreCase(null));
  }

  @Test
  public void testValueOfPort() {
    for (Protocol protocol : Protocol.values()) {
      assertEquals(protocol, Protocol.valueOfPort(protocol.getPort()));
    }
  }

  @Test
  public void testValueOfPortWithInvalidValue() {
    assertNull(Protocol.valueOfPort(null));
    assertNull(Protocol.valueOfPort(ServicePort.SSH));
  }

  @Test
  public void testValueOfScheme() {
    for (Protocol protocol : Protocol.values()) {
      assertEquals(protocol, Protocol.valueOfScheme(protocol.getScheme()));
    }

    assertEquals(Protocol.HTTP, Protocol.valueOfScheme(" http://  "));
  }

  @Test
  public void testValueOfSchemeWithInvalidValue() {
    assertNull(Protocol.valueOfScheme("FTP://"));
    assertNull(Protocol.valueOfScheme("hTTp://"));
    assertNull(Protocol.valueOfScheme("shttp://"));
    assertNull(Protocol.valueOfScheme("sFtp://"));
    assertNull(Protocol.valueOfScheme("snmp://"));
    assertNull(Protocol.valueOfScheme("  "));
    assertNull(Protocol.valueOfScheme(""));
    assertNull(Protocol.valueOfScheme(null));
  }

  @Test
  public void testToString() {
    assertEquals("File Transfer Protocol", Protocol.FTP.toString());
    assertEquals("Hypertext Transfer Protocol", Protocol.HTTP.toString());
    assertEquals("Simple Mail Transfer Protocol", Protocol.SMTP.toString());
  }

}
