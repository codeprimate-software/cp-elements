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
