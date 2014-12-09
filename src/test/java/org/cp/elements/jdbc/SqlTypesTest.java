/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
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
 * @see org.cp.elements.jdbc.SqlType
 * @see org.junit.Test
 * @since 1.0.0
 */
public class SqlTypesTest {

  @Test
  public void testMapping() throws IllegalAccessException {
    int count = 0;

    for (Field field : java.sql.Types.class.getDeclaredFields()) {
      int modifiers = field.getModifiers();

      if (Modifier.isPublic(modifiers) && Modifier.isStatic(modifiers) && Integer.TYPE.equals(field.getType())) {
        count++;
        field.setAccessible(true);
        int value = field.getInt(null);
        SqlType sqlType = SqlType.valueOf(value);
        assertNotNull(sqlType);
        assertEquals(value, sqlType.getType());
      }
    }

    assertEquals(count, SqlType.values().length);
  }

}
