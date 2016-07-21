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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link HttpStatus} enum.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.net.protocols.http.HttpStatus
 * @since 1.0.0
 */
public class HttpStatusTests {

  @Test
  public void valueOfHttpStatusCodeReturnsHttpStatus() {
    for (HttpStatus httpStatus : HttpStatus.values()) {
      assertThat(HttpStatus.valueOf(httpStatus.code()), is(equalTo(httpStatus)));
    }
  }

  @Test
  public void valueOfSelectHttpStatusCodesReturnsHttpStatus() {
    assertThat(HttpStatus.valueOf(100), is(equalTo(HttpStatus.CONTINUE)));
    assertThat(HttpStatus.valueOf(200), is(equalTo(HttpStatus.OK)));
    assertThat(HttpStatus.valueOf(302), is(equalTo(HttpStatus.FOUND)));
    assertThat(HttpStatus.valueOf(404), is(equalTo(HttpStatus.NOT_FOUND)));
    assertThat(HttpStatus.valueOf(500), is(equalTo(HttpStatus.INTERNAL_SERVER_ERROR)));
  }

  @Test
  public void valueOfInvalidHttpStatusCodeReturnsNull() {
    assertThat(HttpStatus.valueOf(-404), is(nullValue(HttpStatus.class)));
    assertThat(HttpStatus.valueOf(0), is(nullValue(HttpStatus.class)));
    assertThat(HttpStatus.valueOf(600), is(nullValue(HttpStatus.class)));
    assertThat(HttpStatus.valueOf(1100), is(nullValue(HttpStatus.class)));
  }

  @Test
  public void valueOfDescriptionReturnsHttpStatus() {
    for (HttpStatus httpStatus : HttpStatus.values()) {
      assertThat(HttpStatus.valueOfDescription(httpStatus.description()), is(equalTo(httpStatus)));
    }
  }

  @Test
  public void valueOfSelectDescriptionsReturnsHttpStatus() {
    assertThat(HttpStatus.valueOfDescription("Continue"), is(equalTo(HttpStatus.CONTINUE)));
    assertThat(HttpStatus.valueOfDescription(" OK"), is(equalTo(HttpStatus.OK)));
    assertThat(HttpStatus.valueOfDescription("found  "), is(equalTo(HttpStatus.FOUND)));
    assertThat(HttpStatus.valueOfDescription(" Not Found  "), is(equalTo(HttpStatus.NOT_FOUND)));
    assertThat(HttpStatus.valueOfDescription("INtErnAl ServeR ERRor"), is(equalTo(HttpStatus.INTERNAL_SERVER_ERROR)));
  }

  @Test
  public void valueOfInvalidDescriptionReturnsNull() {
    assertThat(HttpStatus.valueOfDescription("pause"), is(nullValue(HttpStatus.class)));
    assertThat(HttpStatus.valueOfDescription("NOT OK"), is(nullValue(HttpStatus.class)));
    assertThat(HttpStatus.valueOfDescription("Lost and Found"), is(nullValue(HttpStatus.class)));
    assertThat(HttpStatus.valueOfDescription("NO t F ound"), is(nullValue(HttpStatus.class)));
    assertThat(HttpStatus.valueOfDescription("External Server Error"), is(nullValue(HttpStatus.class)));
    assertThat(HttpStatus.valueOfDescription("invalid description"), is(nullValue(HttpStatus.class)));
  }

  @Test
  public void valueOfNullEmptyAndBlankDescriptionsReturnsNull() {
    assertThat(HttpStatus.valueOfDescription("  "), is(nullValue(HttpStatus.class)));
    assertThat(HttpStatus.valueOfDescription(""), is(nullValue(HttpStatus.class)));
    assertThat(HttpStatus.valueOfDescription(null), is(nullValue(HttpStatus.class)));
  }

  @Test
  public void isInformational() {
    for (HttpStatus httpStatus : HttpStatus.values()) {
      assertThat(String.format("%s is not Informational", httpStatus), httpStatus.isInformational(),
        is(String.valueOf(httpStatus.code()).startsWith("1")));
    }
  }

  @Test
  public void isInformationalForSelectHttpStatuses() {
    assertThat(HttpStatus.CONTINUE.isInformational(), is(true));
    assertThat(HttpStatus.SWITCHING_PROTOCOLS.isInformational(), is(true));
    assertThat(HttpStatus.OK.isInformational(), is(false));
    assertThat(HttpStatus.ACCEPTED.isInformational(), is(false));
    assertThat(HttpStatus.MULTIPLE_CHOICES.isInformational(), is(false));
    assertThat(HttpStatus.FOUND.isInformational(), is(false));
    assertThat(HttpStatus.BAD_REQUEST.isInformational(), is(false));
    assertThat(HttpStatus.NOT_FOUND.isInformational(), is(false));
    assertThat(HttpStatus.INTERNAL_SERVER_ERROR.isInformational(), is(false));
  }

  @Test
  public void isSuccessful() {
    for (HttpStatus httpStatus : HttpStatus.values()) {
      assertThat(String.format("%s is not Successful", httpStatus), httpStatus.isSuccessful(),
        is(String.valueOf(httpStatus.code()).startsWith("2")));
    }
  }

  @Test
  public void isSuccessfulForSelectHttpStatuses() {
    assertThat(HttpStatus.CONTINUE.isSuccessful(), is(false));
    assertThat(HttpStatus.OK.isSuccessful(), is(true));
    assertThat(HttpStatus.MULTIPLE_CHOICES.isSuccessful(), is(false));
    assertThat(HttpStatus.BAD_REQUEST.isSuccessful(), is(false));
    assertThat(HttpStatus.INTERNAL_SERVER_ERROR.isSuccessful(), is(false));
  }

  @Test
  public void isRedirection() {
    for (HttpStatus httpStatus : HttpStatus.values()) {
      assertThat(String.format("%1$s is not a Redirection!", httpStatus), httpStatus.isRedirection(),
        is(String.valueOf(httpStatus.code()).startsWith("3")));
    }
  }

  @Test
  public void isRedirectionForSelectHttpStatuses() {
    assertThat(HttpStatus.CONTINUE.isRedirection(), is(false));
    assertThat(HttpStatus.OK.isRedirection(), is(false));
    assertThat(HttpStatus.MULTIPLE_CHOICES.isRedirection(), is(true));
    assertThat(HttpStatus.BAD_REQUEST.isRedirection(), is(false));
    assertThat(HttpStatus.INTERNAL_SERVER_ERROR.isRedirection(), is(false));
  }

  @Test
  public void isClientError() {
    for (HttpStatus httpStatus : HttpStatus.values()) {
      assertThat(String.format("%1$s is not a Client Error", httpStatus), httpStatus.isClientError(),
        is(String.valueOf(httpStatus.code()).startsWith("4")));
    }
  }

  @Test
  public void isClientErrorForSelectHttpStatuses() {
    assertThat(HttpStatus.CONTINUE.isClientError(), is(false));
    assertThat(HttpStatus.OK.isClientError(), is(false));
    assertThat(HttpStatus.MULTIPLE_CHOICES.isClientError(), is(false));
    assertThat(HttpStatus.BAD_REQUEST.isClientError(), is(true));
    assertThat(HttpStatus.INTERNAL_SERVER_ERROR.isClientError(), is(false));
  }

  @Test
  public void isServerError() {
    for (HttpStatus httpStatus : HttpStatus.values()) {
      assertThat(String.format("%1$s is not a Server Error", httpStatus), httpStatus.isServerError(),
        is(String.valueOf(httpStatus.code()).startsWith("5")));
    }
  }

  @Test
  public void isServerErrorForSelectHttpStatuses() {
    assertThat(HttpStatus.CONTINUE.isServerError(), is(false));
    assertThat(HttpStatus.OK.isServerError(), is(false));
    assertThat(HttpStatus.MULTIPLE_CHOICES.isServerError(), is(false));
    assertThat(HttpStatus.BAD_REQUEST.isServerError(), is(false));
    assertThat(HttpStatus.INTERNAL_SERVER_ERROR.isServerError(), is(true));
  }

  @Test
  public void httpStatusToString() {
    assertEquals("100 - Continue", HttpStatus.CONTINUE.toString());
    assertEquals("201 - Created", HttpStatus.CREATED.toString());
    assertEquals("302 - Found", HttpStatus.FOUND.toString());
    assertEquals("404 - Not Found", HttpStatus.NOT_FOUND.toString());
    assertEquals("500 - Internal Server Error", HttpStatus.INTERNAL_SERVER_ERROR.toString());
  }
}
