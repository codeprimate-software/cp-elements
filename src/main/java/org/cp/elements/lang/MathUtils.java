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
 * The MathUtils class is a utility class encapsulating common mathematical operations.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Math
 * @since 1.0.0
 * @link http://www.math.com
 */
public abstract class MathUtils {

  /**
   * Rounds the specified double value to the nearest tenth.
   * <p/>
   * @param value the double value to round to the nearest tenth.
   * @return the double value rounded to the nearest tenth.
   * @see java.lang.Math#round(double)
   */
  public static double roundToNearestTenth(double value) {
    value *= 10.0d;
    value = Math.round(value);
    value /= 10.0d;
    return value;
  }

}
