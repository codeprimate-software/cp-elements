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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * The HttpStatusTest class is a test suite of test cases testing the contract and functionality of the HttpStatus enum.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.net.protocols.http.HttpStatus
 * @since 1.0.0
 */
public class HttpStatusTest {

  @Test
  public void testValueOfStatusCode() {
    assertEquals(HttpStatus.CONTINUE, HttpStatus.valueOf(HttpStatus.CONTINUE.getCode()));
    assertEquals(HttpStatus.OK, HttpStatus.valueOf(HttpStatus.OK.getCode()));
    assertEquals(HttpStatus.FOUND, HttpStatus.valueOf(HttpStatus.FOUND.getCode()));
    assertEquals(HttpStatus.NOT_FOUND, HttpStatus.valueOf(HttpStatus.NOT_FOUND.getCode()));
    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, HttpStatus.valueOf(HttpStatus.INTERNAL_SERVER_ERROR.getCode()));

    for (HttpStatus httpStatus : HttpStatus.values()) {
      assertEquals(httpStatus, HttpStatus.valueOf(httpStatus.getCode()));
    }
  }

  @Test
  public void testValueOfInvalidStatusCode() {
    assertNull(HttpStatus.valueOf(-404));
    assertNull(HttpStatus.valueOf(600));
    assertNull(HttpStatus.valueOf(1024));
  }

  @Test
  public void testValueOfDescription() {
    assertEquals(HttpStatus.CONTINUE, HttpStatus.valueOfDescription(HttpStatus.CONTINUE.getDescription()));
    assertEquals(HttpStatus.OK, HttpStatus.valueOfDescription("ok"));
    assertEquals(HttpStatus.FOUND, HttpStatus.valueOfDescription("FOUND"));
    assertEquals(HttpStatus.NOT_FOUND, HttpStatus.valueOfDescription("Not Found"));
    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, HttpStatus.valueOfDescription("InTernal ServeR ERRor"));

    for (HttpStatus httpStatus : HttpStatus.values()) {
      assertEquals(httpStatus, HttpStatus.valueOfDescription(httpStatus.getDescription()));
    }
  }

  @Test
  public void testValueOfInvalidDescription() {
    assertNull(HttpStatus.valueOfDescription("pause"));
    assertNull(HttpStatus.valueOfDescription("NOT OK"));
    assertNull(HttpStatus.valueOfDescription("Lost and Found"));
    assertNull(HttpStatus.valueOfDescription("External Server Error"));
    assertNull(HttpStatus.valueOfDescription("invalid description"));
    assertNull(HttpStatus.valueOfDescription("  "));
    assertNull(HttpStatus.valueOfDescription(""));
    assertNull(HttpStatus.valueOfDescription(null));
  }

  @Test
  public void testIsInformational() {
    assertTrue(HttpStatus.SWITCHING_PROTOCOLS.isInformational());
    assertFalse(HttpStatus.NOT_FOUND.isInformational());

    for (HttpStatus httpStatus : HttpStatus.values()) {
      assertEquals(String.format("%1$s is not Informational!", httpStatus),
        String.valueOf(httpStatus.getCode()).startsWith("1"), httpStatus.isInformational());
    }
  }

  @Test
  public void testIsSuccessful() {
    assertTrue(HttpStatus.OK.isSuccessful());
    assertFalse(HttpStatus.INTERNAL_SERVER_ERROR.isSuccessful());

    for (HttpStatus httpStatus : HttpStatus.values()) {
      assertEquals(String.format("%1$s is not Successful!", httpStatus),
        String.valueOf(httpStatus.getCode()).startsWith("2"), httpStatus.isSuccessful());
    }
  }

  @Test
  public void testIsRedirection() {
    assertTrue(HttpStatus.MULTIPLE_CHOICES.isRedirection());
    assertFalse(HttpStatus.CONTINUE.isRedirection());

    for (HttpStatus httpStatus : HttpStatus.values()) {
      assertEquals(String.format("%1$s is not a Redirection!", httpStatus),
        String.valueOf(httpStatus.getCode()).startsWith("3"), httpStatus.isRedirection());
    }
  }

  @Test
  public void testIsClientError() {
    assertTrue(HttpStatus.BAD_REEQUEST.isClientError());
    assertFalse(HttpStatus.BAD_GATEWAY.isClientError());

    for (HttpStatus httpStatus : HttpStatus.values()) {
      assertEquals(String.format("%1$s is not a Client Error!", httpStatus),
        String.valueOf(httpStatus.getCode()).startsWith("4"), httpStatus.isClientError());
    }
  }

  @Test
  public void testIsServerError() {
    assertTrue(HttpStatus.NOT_IMPLEMENTED.isServerError());
    assertFalse(HttpStatus.METHOD_NOT_ALLOWED.isServerError());

    for (HttpStatus httpStatus : HttpStatus.values()) {
      assertEquals(String.format("%1$s is not a Server Error!", httpStatus),
        String.valueOf(httpStatus.getCode()).startsWith("5"), httpStatus.isServerError());
    }
  }

  @Test
  public void testToString() {
    assertEquals("100 - Continue", HttpStatus.CONTINUE.toString());
    assertEquals("200 - OK", HttpStatus.OK.toString());
    assertEquals("302 - Found", HttpStatus.FOUND.toString());
    assertEquals("404 - Not Found", HttpStatus.NOT_FOUND.toString());
    assertEquals("500 - Internal Server Error", HttpStatus.INTERNAL_SERVER_ERROR.toString());
  }

}
