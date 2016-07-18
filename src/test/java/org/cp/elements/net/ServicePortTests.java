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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;

import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link ServicePort} class.
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
      assertThat(servicePort.isReserved(), is(servicePort.portNumber() <= ServicePort.MAX_RESERVED_PORT));
    }
  }

  @Test
  public void portNumber() {
    assertThat(ServicePort.DNS.portNumber(), is(equalTo(53)));
    assertThat(ServicePort.EPHEMERAL.portNumber(), is(equalTo(0)));
    assertThat(ServicePort.FTP.portNumber(), is(equalTo(21)));
    assertThat(ServicePort.HTTP.portNumber(), is(equalTo(80)));
    assertThat(ServicePort.HTTPS.portNumber(), is(equalTo(443)));
    assertThat(ServicePort.LDAP.portNumber(), is(equalTo(389)));
    assertThat(ServicePort.SFTP.portNumber(), is(equalTo(115)));
    assertThat(ServicePort.SMTP.portNumber(), is(equalTo(25)));
    assertThat(ServicePort.SSH.portNumber(), is(equalTo(22)));
    assertThat(ServicePort.TELNET.portNumber(), is(equalTo(23)));
    assertThat(ServicePort.TIME.portNumber(), is(equalTo(37)));
    assertThat(ServicePort.WHOIS.portNumber(), is(equalTo(43)));
  }

  @Test
  public void valueOfIgnoreCaseReturnsServicePort() {
    assertThat(ServicePort.valueOfIgnoreCase("DNS"), is(equalTo(ServicePort.DNS)));
    assertThat(ServicePort.valueOfIgnoreCase("Ldap"), is(equalTo(ServicePort.LDAP)));
    assertThat(ServicePort.valueOfIgnoreCase(" ssh"), is(equalTo(ServicePort.SSH)));
    assertThat(ServicePort.valueOfIgnoreCase(" tiME  "), is(equalTo(ServicePort.TIME)));
    assertThat(ServicePort.valueOfIgnoreCase("WhoIs   "), is(equalTo(ServicePort.WHOIS)));
  }

  @Test
  public void valueOfIgnoreCaseWithInvalidValueReturnsNull() {
    assertThat(ServicePort.valueOfIgnoreCase("SND"), is(nullValue(ServicePort.class)));
    assertThat(ServicePort.valueOfIgnoreCase(" FTTP"), is(nullValue(ServicePort.class)));
    assertThat(ServicePort.valueOfIgnoreCase("FTPS "), is(nullValue(ServicePort.class)));
    assertThat(ServicePort.valueOfIgnoreCase("  htp "), is(nullValue(ServicePort.class)));
    assertThat(ServicePort.valueOfIgnoreCase(" shttp  "), is(nullValue(ServicePort.class)));
    assertThat(ServicePort.valueOfIgnoreCase("PLAD"), is(nullValue(ServicePort.class)));
    assertThat(ServicePort.valueOfIgnoreCase("SH"), is(nullValue(ServicePort.class)));
    assertThat(ServicePort.valueOfIgnoreCase("Telephone"), is(nullValue(ServicePort.class)));
    assertNull(ServicePort.valueOfIgnoreCase("DATE"));
    assertNull(ServicePort.valueOfIgnoreCase("  "));
    assertNull(ServicePort.valueOfIgnoreCase(""));
    assertNull(ServicePort.valueOfIgnoreCase(null));
  }

  @Test
  public void valueOfPortNumberReturnsServicePort() {
    for (ServicePort servicePort : ServicePort.values()) {
      assertThat(ServicePort.valueOf(servicePort.portNumber()), is(equalTo(servicePort)));
    }
  }

  @Test
  public void valueOfInvalidPortNumberReturnsNull() {
    assertThat(ServicePort.valueOf(Integer.MIN_VALUE), is(nullValue(ServicePort.class)));
    assertThat(ServicePort.valueOf(-1), is(nullValue(ServicePort.class)));
    assertThat(ServicePort.valueOf(65536), is(nullValue(ServicePort.class)));
    assertThat(ServicePort.valueOf(Integer.MAX_VALUE), is(nullValue(ServicePort.class)));
  }
}
