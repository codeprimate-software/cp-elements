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

package org.cp.elements.net.protocols.http;

import static org.junit.Assert.*;

import org.cp.elements.dao.CrudOperation;
import org.junit.Test;

/**
 * The HttpMethodTest class is a test suite of test cases testing the contract and functionality of the HttpMethod enum.
 *
 * @author John J. Blum
 * @see org.cp.elements.net.protocols.http.HttpMethod
 * @see org.junit.Test
 * @since 1.0.0
 */
public class HttpMethodTest {

  @Test
  public void getGetCrudOperation() {
    assertEquals(CrudOperation.DELETE, HttpMethod.DELETE.getCrudOperation());
    assertEquals(CrudOperation.READ, HttpMethod.GET.getCrudOperation());
    assertEquals(CrudOperation.CREATE, HttpMethod.POST.getCrudOperation());
    assertEquals(CrudOperation.UPDATE, HttpMethod.PUT.getCrudOperation());
  }

  @Test
  public void testGetCrudOperationReturnsNull() {
    assertNull(HttpMethod.CONNECT.getCrudOperation());
    assertNull(HttpMethod.HEAD.getCrudOperation());
    assertNull(HttpMethod.OPTIONS.getCrudOperation());
    assertNull(HttpMethod.TRACE.getCrudOperation());
  }

  @Test
  public void testValueOfCrudOperation() {
    assertEquals(HttpMethod.POST, HttpMethod.valueOf(CrudOperation.CREATE));
    assertEquals(HttpMethod.GET, HttpMethod.valueOf(CrudOperation.READ));
    assertEquals(HttpMethod.PUT, HttpMethod.valueOf(CrudOperation.UPDATE));
    assertEquals(HttpMethod.DELETE, HttpMethod.valueOf(CrudOperation.DELETE));
  }

  @Test
  public void testValueOfCrudOperationWithNull() {
    assertNull(HttpMethod.valueOf((CrudOperation) null));
  }

  @Test
  public void testValueOfIgnoreCase() {
    assertEquals(HttpMethod.CONNECT, HttpMethod.valueOfIgnoreCase("CONNECT"));
    assertEquals(HttpMethod.DELETE, HttpMethod.valueOfIgnoreCase("Delete"));
    assertEquals(HttpMethod.GET, HttpMethod.valueOfIgnoreCase("get"));
    assertEquals(HttpMethod.HEAD, HttpMethod.valueOfIgnoreCase(" HEad"));
    assertEquals(HttpMethod.OPTIONS, HttpMethod.valueOfIgnoreCase(" opTionS  "));
    assertEquals(HttpMethod.POST, HttpMethod.valueOfIgnoreCase(" POST "));
  }

  @Test
  public void testValueOfIgnoreCaseWithInvalidValue() {
    assertNull(HttpMethod.valueOfIgnoreCase("DISCONNECT"));
    assertNull(HttpMethod.valueOfIgnoreCase("Remove"));
    assertNull(HttpMethod.valueOfIgnoreCase("GETit"));
    assertNull(HttpMethod.valueOfIgnoreCase("grab"));
    assertNull(HttpMethod.valueOfIgnoreCase("BUTT"));
    assertNull(HttpMethod.valueOfIgnoreCase("OPT"));
    assertNull(HttpMethod.valueOfIgnoreCase("Postage"));
    assertNull(HttpMethod.valueOfIgnoreCase("PUTT"));
    assertNull(HttpMethod.valueOfIgnoreCase(" debug  "));
    assertNull(HttpMethod.valueOfIgnoreCase("  "));
    assertNull(HttpMethod.valueOfIgnoreCase(""));
    assertNull(HttpMethod.valueOfIgnoreCase(null));
  }

}
