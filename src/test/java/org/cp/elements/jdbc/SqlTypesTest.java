/*
 * Copyright 2011-Present Author or Authors.
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

import static org.assertj.core.api.Assertions.assertThat;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link SqlType}.
 *
 * @author John J. Blum
 * @see java.sql.Types
 * @see org.cp.elements.jdbc.SqlType
 * @see org.junit.jupiter.api.Test
 * @since 1.0.0
 */
public class SqlTypesTest {

  private boolean isPublicStatic(int modifiers) {
    return Modifier.isPublic(modifiers) && Modifier.isStatic(modifiers);
  }

  private boolean isJavaSqlTypesConstant(Field field) {
    return isPublicStatic(field.getModifiers()) && Integer.TYPE.equals(field.getType());
  }

  @Test
  public void valueOfAllJavaSqlTypes() throws IllegalAccessException {

    int count = 0;

    for (Field field : java.sql.Types.class.getDeclaredFields()) {
      if (isJavaSqlTypesConstant(field)) {

        int value = field.getInt(null);

        SqlType sqlType = SqlType.valueOf(value);

        assertThat(sqlType).as(String.format("Expected %1$s for %2$s.%3$s!", SqlType.class.getName(),
          java.sql.Types.class.getName(), field.getName())).isNotNull();

        assertThat(sqlType.getType()).isEqualTo(value);

        count++;
      }
    }

    assertThat(SqlType.values().length).isEqualTo(count);
  }

  @Test
  public void valueOfInvalidValue() {

    assertThat(SqlType.valueOf(Integer.MIN_VALUE)).isNull();
    assertThat(SqlType.valueOf(-123456789)).isNull();
  }

  @Test
  public void valueOfInvalidValuesIgnoringCase() {

    assertThat(SqlType.valueOfIgnoreCase("  character")).isNull();
    assertThat(SqlType.valueOfIgnoreCase("Fake")).isNull();
    assertThat(SqlType.valueOfIgnoreCase("FixedChar ")).isNull();
    assertThat(SqlType.valueOfIgnoreCase("LONG")).isNull();
    assertThat(SqlType.valueOfIgnoreCase("Nil")).isNull();
    assertThat(SqlType.valueOfIgnoreCase("Ruby_Object")).isNull();
    assertThat(SqlType.valueOfIgnoreCase("  ")).isNull();
    assertThat(SqlType.valueOfIgnoreCase("")).isNull();
    assertThat(SqlType.valueOfIgnoreCase(null)).isNull();
  }

  @Test
  public void valueOfJavaSqlTypesIgnoringCase() {

    assertThat(SqlType.valueOfIgnoreCase("ARRAY")).isEqualTo(SqlType.ARRAY);
    assertThat(SqlType.valueOfIgnoreCase("binary")).isEqualTo(SqlType.BINARY);
    assertThat(SqlType.valueOfIgnoreCase("bLOB")).isEqualTo(SqlType.BLOB);
    assertThat(SqlType.valueOfIgnoreCase(" Clob")).isEqualTo(SqlType.CLOB);
    assertThat(SqlType.valueOfIgnoreCase("VARchar")).isEqualTo(SqlType.VARCHAR);
    assertThat(SqlType.valueOfIgnoreCase(" TiMeSTamp  ")).isEqualTo(SqlType.TIMESTAMP);
  }
}
