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

package org.cp.elements.net.protocols.http;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

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
