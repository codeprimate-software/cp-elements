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

package org.cp.elements.lang;

/**
 * The LogicUtils class is a utility class for implementing logic operations.
 * <p/>
 * @author John Blum
 */
@SuppressWarnings("unused")
public abstract class LogicUtils {

  /**
   * Implementation of exclusive OR (XOR), meaning 1 and only 1 value can be true.  If both values are true or both
   * values are false the result of XOR is false.
   * <p/>
   * @param value1 the first value in the XOR expression.
   * @param value2 the second value in the XOR expression.
   * @return true if an only if 1 of the 2 values is true.
   */
  public static boolean xor(final boolean value1, final boolean value2) {
    return ((value1 || value2) && !(value1 && value2));
  }

}
