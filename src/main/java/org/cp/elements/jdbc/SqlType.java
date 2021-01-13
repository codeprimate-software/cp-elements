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

import java.sql.Types;

import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * The SqlType enum is an enumeration of SQL types from the java.sql.Types class.
 *
 * @author John J. Blum
 * @see java.sql.Types
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum SqlType {
  ARRAY(Types.ARRAY),
  BIGINT(Types.BIGINT),
  BINARY(Types.BINARY),
  BIT(Types.BIT),
  BLOB(Types.BLOB),
  BOOLEAN(Types.BOOLEAN),
  CHAR(Types.CHAR),
  CLOB(Types.CLOB),
  DATALINK(Types.DATALINK),
  DATE(Types.DATE),
  DECIMAL(Types.DECIMAL),
  DISTINCT(Types.DISTINCT),
  DOUBLE(Types.DOUBLE),
  FLOAT(Types.FLOAT),
  INTEGER(Types.INTEGER),
  JAVA_OBJECT(Types.JAVA_OBJECT),
  LONGNVARCHAR(Types.LONGNVARCHAR),
  LONGVARBINARY(Types.LONGVARBINARY),
  LONGVARCHAR(Types.LONGVARCHAR),
  NCHAR(Types.NCHAR),
  NCLOB(Types.NCLOB),
  NULL(Types.NULL),
  NUMERIC(Types.NUMERIC),
  NVARCHAR(Types.NVARCHAR),
  OTHER(Types.OTHER),
  REAL(Types.REAL),
  REF(Types.REF),
  REF_CURSOR(Types.REF_CURSOR),
  ROWID(Types.ROWID),
  SMALLINT(Types.SMALLINT),
  SQLXML(Types.SQLXML),
  STRUCT(Types.STRUCT),
  TIME(Types.TIME),
  TIME_WITH_TIMEZONE(Types.TIME_WITH_TIMEZONE),
  TIMESTAMP(Types.TIMESTAMP),
  TIMESTAMP_WITH_TIMEZONE(Types.TIMESTAMP_WITH_TIMEZONE),
  TINYINT(Types.TINYINT),
  VARBINARY(Types.VARBINARY),
  VARCHAR(Types.VARCHAR);

  private final int type;

  /**
   * Constructs an instance of the SqlType enum initialized with the matching java.sql.Types constant.
   *
   * @param type the java.sql.Types integer constant.
   * @see java.sql.Types
   */
  SqlType(final int type) {
    this.type = type;
  }

  /**
   * Returns the SqlType enumerated value matching the java.sql.Types constant or null if no match was found.
   *
   * @param type the java.sql.Types integer constant.
   * @return the SqlType enumerated value matching the java.sql.Types constant or null if no match was found.
   * @see java.sql.Types
   */
  public static SqlType valueOf(final int type) {
    for (SqlType sqlType : values()) {
      if (sqlType.getType() == type) {
        return sqlType;
      }
    }

    return null;
  }

  /**
   * Returns a SqlType enumerated value matching the given String name or null if no match was found.  A match
   * is found by ignoring case and trimming leading/trailing whitespace in the String name.
   *
   * @param name the String name used ot match the SqlType.
   * @return a SqlType enumerated value matching the String name or null for no match was found.
   * @see java.lang.String#equalsIgnoreCase(String)
   * @see org.cp.elements.jdbc.SqlType#name()
   * @see org.cp.elements.lang.StringUtils#trim(String)
   */
  @NullSafe
  public static SqlType valueOfIgnoreCase(final String name) {
    for (SqlType sqlType : values()) {
      if (sqlType.name().equalsIgnoreCase(StringUtils.trim(name))) {
        return sqlType;
      }
    }

    return null;
  }

  /**
   * Gets the java.sql.Types constant corresponding to this SqlType enumerated value.
   *
   * @return a java.sql.Types constant value corresponding to this SqlType enum.
   * @see java.sql.Types
   */
  public int getType() {
    return type;
  }

}
