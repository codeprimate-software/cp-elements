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

import java.sql.Types;

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
  ROWID(Types.ROWID),
  SMALLINT(Types.SMALLINT),
  SQLXML(Types.SQLXML),
  STRUCT(Types.STRUCT),
  TIME(Types.TIME),
  TIMESTAMP(Types.TIMESTAMP),
  TINYINT(Types.TINYINT),
  VARBINARY(Types.VARBINARY),
  VARCHAR(Types.VARCHAR);

  private final int type;

  /**
   * Constructs an instance of the SqlType enumeration based on the java.sql.Types constant.
   *
   * @param type the java.sql.Types integer constant.
   */
  SqlType(final int type) {
    this.type = type;
  }

  /**
   * Gets the SqlType enumeration based on the java.sql.Types constant, or null if the integer value is not
   * one of the java.sql.Types constants.
   *
   * @param type the java.sql.Types integer constant.
   * @return the SqlType enumeration corresponding to the java.sql.Types constant.
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
   * Gets the java.sql.Types constant corresponding to this SqlType enum.
   *
   * @return a java.sql.Types constant value corresponding to this SqlType enum.
   */
  public int getType() {
    return type;
  }

}
