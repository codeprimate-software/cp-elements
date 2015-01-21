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

package org.cp.elements.dao;

import static org.junit.Assert.*;

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
