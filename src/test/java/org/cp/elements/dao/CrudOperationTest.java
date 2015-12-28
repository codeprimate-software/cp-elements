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

package org.cp.elements.dao;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;

/**
 * The CrudOperationTest class is a test suite of test cases testing the contract and functionality
 * of the CrudOperation enum.
 *
 * @author John J. Blum
 * @see org.cp.elements.dao.CrudOperation
 * @see org.junit.Test
 * @since 1.0.0
 */
public class CrudOperationTest {

  @Test
  public void testValueOfIgnoreCase() {
    assertEquals(CrudOperation.CREATE, CrudOperation.valueOfIgnoreCase("CREATE"));
    assertEquals(CrudOperation.READ, CrudOperation.valueOfIgnoreCase("Read"));
    assertEquals(CrudOperation.UPDATE, CrudOperation.valueOfIgnoreCase("update"));
    assertEquals(CrudOperation.DELETE, CrudOperation.valueOfIgnoreCase(" deLeTe  "));
  }

  @Test
  public void testValueOfIgnoreCaseWithInvalidValue() {
    assertNull(CrudOperation.valueOfIgnoreCase("INserT"));
    assertNull(CrudOperation.valueOfIgnoreCase("READING"));
    assertNull(CrudOperation.valueOfIgnoreCase("up"));
    assertNull(CrudOperation.valueOfIgnoreCase("Remove"));
    assertNull(CrudOperation.valueOfIgnoreCase("  "));
    assertNull(CrudOperation.valueOfIgnoreCase(""));
    assertNull(CrudOperation.valueOfIgnoreCase(null));
  }

}
