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
 * Unit Tests for {@link ServicePort}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.net.ServicePort
 * @since 1.0.0
 */
public class ServicePortTests {

  @Test
  public void isReserved() {

    for (ServicePort servicePort : ServicePort.values()) {
      assertThat(servicePort.isReserved()).isEqualTo(servicePort.portNumber() <= ServicePort.MAX_RESERVED_PORT);
    }
  }

  @Test
  public void portNumber() {

    assertThat(ServicePort.DNS.portNumber()).isEqualTo(53);
    assertThat(ServicePort.EPHEMERAL.portNumber()).isEqualTo(0);
    assertThat(ServicePort.FTP.portNumber()).isEqualTo(21);
    assertThat(ServicePort.HTTP.portNumber()).isEqualTo(80);
    assertThat(ServicePort.HTTPS.portNumber()).isEqualTo(443);
    assertThat(ServicePort.LDAP.portNumber()).isEqualTo(389);
    assertThat(ServicePort.SFTP.portNumber()).isEqualTo(115);
    assertThat(ServicePort.SMTP.portNumber()).isEqualTo(25);
    assertThat(ServicePort.SSH.portNumber()).isEqualTo(22);
    assertThat(ServicePort.TELNET.portNumber()).isEqualTo(23);
    assertThat(ServicePort.TIME.portNumber()).isEqualTo(37);
    assertThat(ServicePort.WHOIS.portNumber()).isEqualTo(43);
  }

  @Test
  public void valueOfIgnoreCaseReturnsServicePort() {

    assertThat(ServicePort.valueOfIgnoreCase("DNS")).isEqualTo(ServicePort.DNS);
    assertThat(ServicePort.valueOfIgnoreCase("Ldap")).isEqualTo(ServicePort.LDAP);
    assertThat(ServicePort.valueOfIgnoreCase(" ssh")).isEqualTo(ServicePort.SSH);
    assertThat(ServicePort.valueOfIgnoreCase(" tiME  ")).isEqualTo(ServicePort.TIME);
    assertThat(ServicePort.valueOfIgnoreCase("WhoIs   ")).isEqualTo(ServicePort.WHOIS);
  }

  @Test
  public void valueOfIgnoreCaseWithInvalidValueReturnsNull() {

    assertThat(ServicePort.valueOfIgnoreCase("SND")).isNull();
    assertThat(ServicePort.valueOfIgnoreCase(" FTTP")).isNull();
    assertThat(ServicePort.valueOfIgnoreCase("FTPS ")).isNull();
    assertThat(ServicePort.valueOfIgnoreCase("  htp ")).isNull();
    assertThat(ServicePort.valueOfIgnoreCase(" shttp  ")).isNull();
    assertThat(ServicePort.valueOfIgnoreCase("PLAD")).isNull();
    assertThat(ServicePort.valueOfIgnoreCase("SH")).isNull();
    assertThat(ServicePort.valueOfIgnoreCase("Telephone")).isNull();
    assertThat(ServicePort.valueOfIgnoreCase("DATE")).isNull();
    assertThat(ServicePort.valueOfIgnoreCase("  ")).isNull();
    assertThat(ServicePort.valueOfIgnoreCase("")).isNull();
    assertThat(ServicePort.valueOfIgnoreCase(null)).isNull();
  }

  @Test
  public void valueOfPortNumberReturnsServicePort() {

    for (ServicePort servicePort : ServicePort.values()) {
      assertThat(ServicePort.valueOf(servicePort.portNumber())).isEqualTo(servicePort);
    }
  }

  @Test
  public void valueOfInvalidPortNumberReturnsNull() {

    assertThat(ServicePort.valueOf(Integer.MIN_VALUE)).isNull();
    assertThat(ServicePort.valueOf(-1)).isNull();
    assertThat(ServicePort.valueOf(65536)).isNull();
    assertThat(ServicePort.valueOf(Integer.MAX_VALUE)).isNull();
  }
}
