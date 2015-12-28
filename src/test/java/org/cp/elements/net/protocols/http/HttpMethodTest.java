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

package org.cp.elements.net.protocols.http;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

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
