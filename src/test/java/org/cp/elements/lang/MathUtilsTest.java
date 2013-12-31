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

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * The MathUtilsTest class is a test suite of test cases testing the contract and functionality of the MathUtils 
 * utility class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.MathUtils
 * @see org.junit.Assert
 * @see org.junit.Test
 * @since 1.0.0
 */
public class MathUtilsTest {

  @Test
  public void testRoundToNearestTenth() {
    assertEquals(Double.valueOf(0.0d), Double.valueOf(MathUtils.roundToNearestTenth(0.0d)));
    assertEquals(Double.valueOf(7.0d), Double.valueOf(MathUtils.roundToNearestTenth(7.0d)));
    assertEquals(Double.valueOf(13.1d), Double.valueOf(MathUtils.roundToNearestTenth(13.14d)));
    assertEquals(Double.valueOf(13.2d), Double.valueOf(MathUtils.roundToNearestTenth(13.19d)));
    assertEquals(Double.valueOf(19.4d), Double.valueOf(MathUtils.roundToNearestTenth(19.419d)));
    assertEquals(Double.valueOf(19.4d), Double.valueOf(MathUtils.roundToNearestTenth(19.449d)));
    assertEquals(Double.valueOf(19.6d), Double.valueOf(MathUtils.roundToNearestTenth(19.55d)));
  }

}
