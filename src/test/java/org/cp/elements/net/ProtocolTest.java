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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

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
