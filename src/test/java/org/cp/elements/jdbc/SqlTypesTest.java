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

package org.cp.elements.jdbc;

import static org.junit.Assert.*;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import org.junit.Test;

/**
 * The SqlTypesTest class is a test suite of test cases testing the mapping between the cp-elements JDBC SqlTypes
 * enumerated type and the java.sql.Types constants.
 *
 * @author John J. Blum
 * @see java.sql.Types
 * @see org.cp.elements.jdbc.SqlType
 * @see org.junit.Test
 * @since 1.0.0
 */
public class SqlTypesTest {

  private boolean isPublicStatic(final int modifiers) {
    return (Modifier.isPublic(modifiers) && Modifier.isStatic(modifiers));
  }

  private boolean isJavaSqlTypesConstant(final Field field) {
    return (isPublicStatic(field.getModifiers()) && Integer.TYPE.equals(field.getType()));
  }

  @Test
  public void testValueOfTypes() throws IllegalAccessException {
    int count = 0;

    for (Field field : java.sql.Types.class.getDeclaredFields()) {
      if (isJavaSqlTypesConstant(field)) {
        int value = field.getInt(null);
        SqlType sqlType = SqlType.valueOf(value);
        assertNotNull(sqlType);
        assertEquals(value, sqlType.getType());
        count++;
      }
    }

    assertEquals(count, SqlType.values().length);
  }

  @Test
  public void testValueOfTypesWithInvalidValue() {
    assertNull(SqlType.valueOf(Integer.MIN_VALUE));
    assertNull(SqlType.valueOf(-123456789));
  }

  @Test
  public void testValueOfIgnoreCase() {
    assertEquals(SqlType.ARRAY, SqlType.valueOfIgnoreCase("ARRAY"));
    assertEquals(SqlType.BINARY, SqlType.valueOfIgnoreCase("binary"));
    assertEquals(SqlType.BLOB, SqlType.valueOfIgnoreCase("bLOB"));
    assertEquals(SqlType.CLOB, SqlType.valueOfIgnoreCase(" Clob"));
    assertEquals(SqlType.VARCHAR, SqlType.valueOfIgnoreCase("VARchar"));
    assertEquals(SqlType.TIMESTAMP, SqlType.valueOfIgnoreCase(" TiMeSTamp  "));
  }

  @Test
  public void testValueOfIgnoreCaseNoMatch() {
    assertNull(SqlType.valueOfIgnoreCase("  character"));
    assertNull(SqlType.valueOfIgnoreCase("Fake"));
    assertNull(SqlType.valueOfIgnoreCase("FixedChar "));
    assertNull(SqlType.valueOfIgnoreCase("LONG"));
    assertNull(SqlType.valueOfIgnoreCase("Nil"));
    assertNull(SqlType.valueOfIgnoreCase("Ruby_Object"));
    assertNull(SqlType.valueOfIgnoreCase("  "));
    assertNull(SqlType.valueOfIgnoreCase(""));
    assertNull(SqlType.valueOfIgnoreCase(null));
  }

}
