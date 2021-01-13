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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link Protocol} enum.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.net.Protocol
 * @since 1.0.0
 */
public class ProtocolTests {

  @Test
  public void protocolPortNumber() {
    assertThat(Protocol.FTP.portNumber(), is(equalTo(ServicePort.FTP.portNumber())));
    assertThat(Protocol.HTTP.portNumber(), is(equalTo(ServicePort.HTTP.portNumber())));
    assertThat(Protocol.HTTPS.portNumber(), is(equalTo(ServicePort.HTTPS.portNumber())));
    assertThat(Protocol.LDAP.portNumber(), is(equalTo(ServicePort.LDAP.portNumber())));
    assertThat(Protocol.SFTP.portNumber(), is(equalTo(ServicePort.SFTP.portNumber())));
    assertThat(Protocol.SMTP.portNumber(), is(equalTo(ServicePort.SMTP.portNumber())));
  }

  @Test
  public void protocolServicePort() {
    assertThat(Protocol.FTP.servicePort(), is(equalTo(ServicePort.FTP)));
    assertThat(Protocol.HTTP.servicePort(), is(equalTo(ServicePort.HTTP)));
    assertThat(Protocol.HTTPS.servicePort(), is(equalTo(ServicePort.HTTPS)));
    assertThat(Protocol.LDAP.servicePort(), is(equalTo(ServicePort.LDAP)));
    assertThat(Protocol.SFTP.servicePort(), is(equalTo(ServicePort.SFTP)));
    assertThat(Protocol.SMTP.servicePort(), is(equalTo(ServicePort.SMTP)));
  }

  @Test
  public void protocolScheme() {
    assertThat(Protocol.FTP.scheme(), is(equalTo("ftp://")));
    assertThat(Protocol.HTTP.scheme(), is(equalTo("http://")));
    assertThat(Protocol.HTTPS.scheme(), is(equalTo("https://")));
    assertThat(Protocol.LDAP.scheme(), is(equalTo("ldap://")));
    assertThat(Protocol.SFTP.scheme(), is(equalTo("sftp://")));
    assertThat(Protocol.SMTP.scheme(), is(equalTo("smtp://")));
  }

  @Test
  public void protocolToString() {
    assertThat(Protocol.FTP.toString(), is(equalTo("File Transfer Protocol")));
    assertThat(Protocol.HTTP.toString(), is(equalTo("Hypertext Transfer Protocol")));
    assertThat(Protocol.HTTPS.toString(), is(equalTo("Secure Hypertext Transfer Protocol")));
    assertThat(Protocol.LDAP.toString(), is(equalTo("Lightweight Directory Access Protocol")));
    assertThat(Protocol.SFTP.toString(), is(equalTo("Secure File Transfer Protocol")));
    assertThat(Protocol.SMTP.toString(), is(equalTo("Simple Mail Transfer Protocol")));
  }

  @Test
  public void valueOfIgnoreCaseReturnsProtocol() {
    assertThat(Protocol.valueOfIgnoreCase("FTP"), is(equalTo(Protocol.FTP)));
    assertThat(Protocol.valueOfIgnoreCase("Http "), is(equalTo(Protocol.HTTP)));
    assertThat(Protocol.valueOfIgnoreCase(" HtTpS"), is(equalTo(Protocol.HTTPS)));
    assertThat(Protocol.valueOfIgnoreCase(" LDaP "), is(equalTo(Protocol.LDAP)));
    assertThat(Protocol.valueOfIgnoreCase("sFTP"), is(equalTo(Protocol.SFTP)));
    assertThat(Protocol.valueOfIgnoreCase(" smtp  "), is(equalTo(Protocol.SMTP)));
  }

  @Test
  public void valueOfIgnoreCaseWithInvalidValueReturnsNull() {
    assertThat(Protocol.valueOfIgnoreCase("FTPS"), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfIgnoreCase("Htp "), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfIgnoreCase(" shttp"), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfIgnoreCase("PLAD"), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfIgnoreCase("RMI"), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfIgnoreCase(" Rpc   "), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfIgnoreCase("sCP"), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfIgnoreCase("SSH"), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfIgnoreCase("  Snmp "), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfIgnoreCase("  "), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfIgnoreCase(""), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfIgnoreCase(null), is(nullValue(Protocol.class)));
  }

  @Test
  public void valueOfPortNumberReturnsProtocol() {
    for (Protocol protocol : Protocol.values()) {
      assertThat(Protocol.valueOfPortNumber(protocol.portNumber()), is(equalTo(protocol)));
    }
  }

  @Test
  public void valueOfInvalidPortNumberReturnsNull() {
    assertThat(Protocol.valueOfPortNumber(Integer.MIN_VALUE), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfPortNumber(-1), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfPortNumber(ServicePort.EPHEMERAL.portNumber()), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfPortNumber(Integer.MAX_VALUE), is(nullValue(Protocol.class)));
  }

  @Test
  public void valueOfSchemeReturnsProtocol() {
    for (Protocol protocol : Protocol.values()) {
      assertThat(Protocol.valueOfScheme(protocol.scheme()), is(equalTo(protocol)));
    }

    assertThat(Protocol.valueOfScheme(" http://   "), is(equalTo(Protocol.HTTP)));
  }

  @Test
  public void valueOfSchemeWithInvalidValueReturnsNull() {
    assertThat(Protocol.valueOfScheme("FTP://"), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfScheme("hTTp://"), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfScheme("shttp://"), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfScheme("sFtp://"), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfScheme("snmp://"), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfScheme("  "), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfScheme(""), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfScheme(null), is(nullValue(Protocol.class)));
  }

  @Test
  public void valueOfServicePortReturnsProtocol() {
    for (Protocol protocol : Protocol.values()) {
      assertThat(Protocol.valueOfServicePort(protocol.servicePort()), is(equalTo(protocol)));
    }
  }

  @Test
  public void valueOfServicePortWithInvalidValueReturnsNull() {
    assertThat(Protocol.valueOfServicePort(null), is(nullValue(Protocol.class)));
    assertThat(Protocol.valueOfServicePort(ServicePort.SSH), is(nullValue(Protocol.class)));
  }
}
