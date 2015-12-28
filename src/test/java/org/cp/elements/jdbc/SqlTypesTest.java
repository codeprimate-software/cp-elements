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

package org.cp.elements.jdbc;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;

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
  @SuppressWarnings("all")
  public void valueOfAllJavaSqlTypes() throws IllegalAccessException {
    int count = 0;

    for (Field field : java.sql.Types.class.getDeclaredFields()) {
      if (isJavaSqlTypesConstant(field)) {
        int value = field.getInt(null);

        SqlType sqlType = SqlType.valueOf(value);

        assertThat(String.format("Expected %1$s for %2$s.%3$s!", SqlType.class.getName(),
          java.sql.Types.class.getName(), field.getName()), sqlType, is(notNullValue()));

        assertThat(sqlType.getType(), is(equalTo(value)));

        count++;
      }
    }

    assertEquals(count, SqlType.values().length);
  }

  @Test
  public void valueOfInvalidValue() {
    assertNull(SqlType.valueOf(Integer.MIN_VALUE));
    assertNull(SqlType.valueOf(-123456789));
  }

  @Test
  public void valueOfInvalidValuesIgnoringCase() {
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

  @Test
  public void valueOfJavaSqlTypesIgnoringCase() {
    assertEquals(SqlType.ARRAY, SqlType.valueOfIgnoreCase("ARRAY"));
    assertEquals(SqlType.BINARY, SqlType.valueOfIgnoreCase("binary"));
    assertEquals(SqlType.BLOB, SqlType.valueOfIgnoreCase("bLOB"));
    assertEquals(SqlType.CLOB, SqlType.valueOfIgnoreCase(" Clob"));
    assertEquals(SqlType.VARCHAR, SqlType.valueOfIgnoreCase("VARchar"));
    assertEquals(SqlType.TIMESTAMP, SqlType.valueOfIgnoreCase(" TiMeSTamp  "));
  }

}
