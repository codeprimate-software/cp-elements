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

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;

/**
 * Unit Tests for the {@link Protocol} enum.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.net.Protocol
 * @since 1.0.0
 */
public class ProtocolTests {

  @Test
  public void protocolPortNumber() {

    assertThat(Protocol.FTP.portNumber()).isEqualTo(ServicePort.FTP.portNumber());
    assertThat(Protocol.HTTP.portNumber()).isEqualTo(ServicePort.HTTP.portNumber());
    assertThat(Protocol.HTTPS.portNumber()).isEqualTo(ServicePort.HTTPS.portNumber());
    assertThat(Protocol.LDAP.portNumber()).isEqualTo(ServicePort.LDAP.portNumber());
    assertThat(Protocol.SFTP.portNumber()).isEqualTo(ServicePort.SFTP.portNumber());
    assertThat(Protocol.SMTP.portNumber()).isEqualTo(ServicePort.SMTP.portNumber());
  }

  @Test
  public void protocolServicePort() {

    assertThat(Protocol.FTP.servicePort()).isEqualTo(ServicePort.FTP);
    assertThat(Protocol.HTTP.servicePort()).isEqualTo(ServicePort.HTTP);
    assertThat(Protocol.HTTPS.servicePort()).isEqualTo(ServicePort.HTTPS);
    assertThat(Protocol.LDAP.servicePort()).isEqualTo(ServicePort.LDAP);
    assertThat(Protocol.SFTP.servicePort()).isEqualTo(ServicePort.SFTP);
    assertThat(Protocol.SMTP.servicePort()).isEqualTo(ServicePort.SMTP);
  }

  @Test
  public void protocolScheme() {

    assertThat(Protocol.FTP.scheme()).isEqualTo("ftp://");
    assertThat(Protocol.HTTP.scheme()).isEqualTo("http://");
    assertThat(Protocol.HTTPS.scheme()).isEqualTo("https://");
    assertThat(Protocol.LDAP.scheme()).isEqualTo("ldap://");
    assertThat(Protocol.SFTP.scheme()).isEqualTo("sftp://");
    assertThat(Protocol.SMTP.scheme()).isEqualTo("smtp://");
  }

  @Test
  public void protocolToString() {

    assertThat(Protocol.FTP.toString()).isEqualTo("File Transfer Protocol");
    assertThat(Protocol.HTTP.toString()).isEqualTo("Hypertext Transfer Protocol");
    assertThat(Protocol.HTTPS.toString()).isEqualTo("Secure Hypertext Transfer Protocol");
    assertThat(Protocol.LDAP.toString()).isEqualTo("Lightweight Directory Access Protocol");
    assertThat(Protocol.SFTP.toString()).isEqualTo("Secure File Transfer Protocol");
    assertThat(Protocol.SMTP.toString()).isEqualTo("Simple Mail Transfer Protocol");
  }

  @Test
  public void valueOfIgnoreCaseReturnsProtocol() {

    assertThat(Protocol.valueOfIgnoreCase("FTP")).isEqualTo(Protocol.FTP);
    assertThat(Protocol.valueOfIgnoreCase("Http ")).isEqualTo(Protocol.HTTP);
    assertThat(Protocol.valueOfIgnoreCase(" HtTpS")).isEqualTo(Protocol.HTTPS);
    assertThat(Protocol.valueOfIgnoreCase(" LDaP ")).isEqualTo(Protocol.LDAP);
    assertThat(Protocol.valueOfIgnoreCase("sFTP")).isEqualTo(Protocol.SFTP);
    assertThat(Protocol.valueOfIgnoreCase(" smtp  ")).isEqualTo(Protocol.SMTP);
  }

  @Test
  public void valueOfIgnoreCaseWithInvalidValueReturnsNull() {

    assertThat(Protocol.valueOfIgnoreCase("FTPS")).isNull();
    assertThat(Protocol.valueOfIgnoreCase("Htp ")).isNull();
    assertThat(Protocol.valueOfIgnoreCase(" shttp")).isNull();
    assertThat(Protocol.valueOfIgnoreCase("PLAD")).isNull();
    assertThat(Protocol.valueOfIgnoreCase("RMI")).isNull();
    assertThat(Protocol.valueOfIgnoreCase(" Rpc   ")).isNull();
    assertThat(Protocol.valueOfIgnoreCase("sCP")).isNull();
    assertThat(Protocol.valueOfIgnoreCase("SSH")).isNull();
    assertThat(Protocol.valueOfIgnoreCase("  Snmp ")).isNull();
    assertThat(Protocol.valueOfIgnoreCase("  ")).isNull();
    assertThat(Protocol.valueOfIgnoreCase("")).isNull();
    assertThat(Protocol.valueOfIgnoreCase(null)).isNull();
  }

  @Test
  public void valueOfPortNumberReturnsProtocol() {

    for (Protocol protocol : Protocol.values()) {
      assertThat(Protocol.valueOfPortNumber(protocol.portNumber())).isEqualTo(protocol);
    }
  }

  @Test
  public void valueOfInvalidPortNumberReturnsNull() {

    assertThat(Protocol.valueOfPortNumber(Integer.MIN_VALUE)).isNull();
    assertThat(Protocol.valueOfPortNumber(-1)).isNull();
    assertThat(Protocol.valueOfPortNumber(ServicePort.EPHEMERAL.portNumber())).isNull();
    assertThat(Protocol.valueOfPortNumber(Integer.MAX_VALUE)).isNull();
  }

  @Test
  public void valueOfSchemeReturnsProtocol() {

    for (Protocol protocol : Protocol.values()) {
      assertThat(Protocol.valueOfScheme(protocol.scheme())).isEqualTo(protocol);
    }

    assertThat(Protocol.valueOfScheme(" http://   ")).isEqualTo(Protocol.HTTP);
  }

  @Test
  public void valueOfSchemeWithInvalidValueReturnsNull() {

    assertThat(Protocol.valueOfScheme("FTP://")).isNull();
    assertThat(Protocol.valueOfScheme("hTTp://")).isNull();
    assertThat(Protocol.valueOfScheme("shttp://")).isNull();
    assertThat(Protocol.valueOfScheme("sFtp://")).isNull();
    assertThat(Protocol.valueOfScheme("snmp://")).isNull();
    assertThat(Protocol.valueOfScheme("  ")).isNull();
    assertThat(Protocol.valueOfScheme("")).isNull();
    assertThat(Protocol.valueOfScheme(null)).isNull();
  }

  @Test
  public void valueOfServicePortReturnsProtocol() {

    for (Protocol protocol : Protocol.values()) {
      assertThat(Protocol.valueOfServicePort(protocol.servicePort())).isEqualTo(protocol);
    }
  }

  @Test
  public void valueOfServicePortWithInvalidValueReturnsNull() {

    assertThat(Protocol.valueOfServicePort(null)).isNull();
    assertThat(Protocol.valueOfServicePort(ServicePort.SSH)).isNull();
  }
}
