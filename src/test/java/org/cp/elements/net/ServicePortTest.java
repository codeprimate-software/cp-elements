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
 * The ServicePortTest class is a test suite of test cases testing the contract and functionality
 * of the ServicePort class.
 *
 * @author John J. Blum
 * @see org.cp.elements.net.ServicePort
 * @see org.junit.Test
 * @since 1.0.0
 */
public class ServicePortTest {

  @Test
  public void testIsReserved() {
    for (ServicePort servicePort : ServicePort.values()) {
      assertEquals(servicePort.getPortNumber() <= 1024, servicePort.isReserved());
    }
  }

  @Test
  public void testValueOfIgnoreCase() {
    assertEquals(ServicePort.DNS, ServicePort.valueOfIgnoreCase("DNS"));
    assertEquals(ServicePort.LDAP, ServicePort.valueOfIgnoreCase("Ldap"));
    assertEquals(ServicePort.SSH, ServicePort.valueOfIgnoreCase(" ssh"));
    assertEquals(ServicePort.TIME, ServicePort.valueOfIgnoreCase(" tiME  "));
    assertEquals(ServicePort.WHOIS, ServicePort.valueOfIgnoreCase("WhoIs   "));
  }

  @Test
  public void testValueOfIgnoreCaseWithInvalidValue() {
    assertNull(ServicePort.valueOfIgnoreCase("SND"));
    assertNull(ServicePort.valueOfIgnoreCase(" htTtp "));
    assertNull(ServicePort.valueOfIgnoreCase("Telekinetic"));
    assertNull(ServicePort.valueOfIgnoreCase("date"));
    assertNull(ServicePort.valueOfIgnoreCase("  "));
    assertNull(ServicePort.valueOfIgnoreCase(""));
    assertNull(ServicePort.valueOfIgnoreCase(null));
  }

  @Test
  public void testValueOfPortNumber() {
    for (ServicePort servicePort : ServicePort.values()) {
      assertEquals(servicePort, ServicePort.valueOf(servicePort.getPortNumber()));
    }
  }

  @Test
  public void testValueOfPortNumberNoMatch() {
    assertNull(ServicePort.valueOf(Integer.MAX_VALUE));
    assertNull(ServicePort.valueOf(65536));
    assertNull(ServicePort.valueOf(-1));
    assertNull(ServicePort.valueOf(Integer.MIN_VALUE));
  }

}
