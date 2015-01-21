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

package org.cp.elements.net.protocols.http;

import static org.junit.Assert.*;

import org.junit.Test;

/**
 * The HttpHeaderTest class is a test suite of test cases testing the contract and functionality of the HttpHeader enum.
 *
 * @author John J. Blum
 * @see org.cp.elements.net.protocols.http.HttpHeader
 * @see org.junit.Test
 * @since 1.0.0
 */
public class HttpHeaderTest {

  @Test
  public void testValueOfIgnoreCase() {
    assertEquals(HttpHeader.ACCEPT, HttpHeader.valueOfIgnoreCase("ACCEPT"));
    assertEquals(HttpHeader.ALLOW, HttpHeader.valueOfIgnoreCase("Allow"));
    assertEquals(HttpHeader.CONTENT_LENGTH, HttpHeader.valueOfIgnoreCase("content-length"));
    assertEquals(HttpHeader.DATE, HttpHeader.valueOfIgnoreCase(" dATE  "));
    assertEquals(HttpHeader.ETAG, HttpHeader.valueOfIgnoreCase(" eTag "));
    assertEquals(HttpHeader.HOST, HttpHeader.valueOfIgnoreCase("hOsT "));
  }

  @Test
  public void testValueOfIgnoreCaseWithInvalidValue() {
    assertNull(HttpHeader.valueOfIgnoreCase("EXPIRE"));
    assertNull(HttpHeader.valueOfIgnoreCase("if-not-match"));
    assertNull(HttpHeader.valueOfIgnoreCase("last_modified"));
    assertNull(HttpHeader.valueOfIgnoreCase("locate"));
    assertNull(HttpHeader.valueOfIgnoreCase("loser-agent"));
    assertNull(HttpHeader.valueOfIgnoreCase("  "));
    assertNull(HttpHeader.valueOfIgnoreCase(""));
    assertNull(HttpHeader.valueOfIgnoreCase(null));
  }

}
