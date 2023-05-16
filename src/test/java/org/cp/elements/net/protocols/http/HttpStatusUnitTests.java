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
package org.cp.elements.net.protocols.http;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Arrays;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link HttpStatus}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.net.protocols.http.HttpStatus
 * @since 1.0.0
 */
public class HttpStatusUnitTests {

  @Test
  public void valueOfHttpStatusCodeReturnsHttpStatus() {

    for (HttpStatus httpStatus : HttpStatus.values()) {
      assertThat(HttpStatus.valueOf(httpStatus.code())).isEqualTo(httpStatus);
    }
  }

  @Test
  public void valueOfInvalidHttpStatusCodeReturnsNull() {

    assertThat(HttpStatus.valueOf(Integer.MIN_VALUE)).isNull();
    assertThat(HttpStatus.valueOf(-404)).isNull();
    assertThat(HttpStatus.valueOf(0)).isNull();
    assertThat(HttpStatus.valueOf(600)).isNull();
    assertThat(HttpStatus.valueOf(Integer.MAX_VALUE)).isNull();
  }

  @Test
  public void valueOfSelectHttpStatusCodesReturnsHttpStatus() {

    assertThat(HttpStatus.valueOf(100)).isEqualTo(HttpStatus.CONTINUE);
    assertThat(HttpStatus.valueOf(200)).isEqualTo(HttpStatus.OK);
    assertThat(HttpStatus.valueOf(302)).isEqualTo(HttpStatus.FOUND);
    assertThat(HttpStatus.valueOf(404)).isEqualTo(HttpStatus.NOT_FOUND);
    assertThat(HttpStatus.valueOf(500)).isEqualTo(HttpStatus.INTERNAL_SERVER_ERROR);
  }

  @Test
  public void valueOfDescriptionReturnsHttpStatus() {

    for (HttpStatus httpStatus : HttpStatus.values()) {
      assertThat(HttpStatus.valueOfDescription(httpStatus.description())).isEqualTo(httpStatus);
    }
  }

  @Test
  public void valueOfInvalidDescriptionReturnsNull() {

    assertThat(HttpStatus.valueOfDescription("pause")).isNull();
    assertThat(HttpStatus.valueOfDescription("NOT OK")).isNull();
    assertThat(HttpStatus.valueOfDescription("Lost and Found")).isNull();
    assertThat(HttpStatus.valueOfDescription("NO t F ound")).isNull();
    assertThat(HttpStatus.valueOfDescription("Not Fine")).isNull();
    assertThat(HttpStatus.valueOfDescription("External Server Error")).isNull();
    assertThat(HttpStatus.valueOfDescription("invalid description")).isNull();
  }

  @Test
  public void valueOfNoDescriptionReturnsNull() {

    assertThat(HttpStatus.valueOfDescription("  ")).isNull();
    assertThat(HttpStatus.valueOfDescription("")).isNull();
  }

  @Test
  public void valueOfNullDescriptionIsNullSafeReturnsNull() {
    assertThat(HttpStatus.valueOfDescription(null)).isNull();
  }

  @Test
  public void valueOfSelectDescriptionsReturnsHttpStatus() {

    assertThat(HttpStatus.valueOfDescription("Continue")).isEqualTo(HttpStatus.CONTINUE);
    assertThat(HttpStatus.valueOfDescription(" OK")).isEqualTo(HttpStatus.OK);
    assertThat(HttpStatus.valueOfDescription("found  ")).isEqualTo(HttpStatus.FOUND);
    assertThat(HttpStatus.valueOfDescription(" Not Found  ")).isEqualTo(HttpStatus.NOT_FOUND);
    assertThat(HttpStatus.valueOfDescription("INtErnAl ServeR ERRor")).isEqualTo(HttpStatus.INTERNAL_SERVER_ERROR);
  }

  @Test
  public void isInformational() {

    Arrays.stream(HttpStatus.values()).forEach(httpStatus ->
      assertThat(httpStatus.isInformational())
        .describedAs("%s is not Informational", httpStatus)
        .isEqualTo(String.valueOf(httpStatus.code()).startsWith("1")));
  }

  @Test
  public void isInformationalForSelectHttpStatuses() {

    assertThat(HttpStatus.CONTINUE.isInformational()).isTrue();
    assertThat(HttpStatus.SWITCHING_PROTOCOLS.isInformational()).isTrue();
  }

  @Test
  public void isNotInformationalForSelectHttpStatuses() {

    assertThat(HttpStatus.OK.isInformational()).isFalse();
    assertThat(HttpStatus.ACCEPTED.isInformational()).isFalse();
    assertThat(HttpStatus.MULTIPLE_CHOICES.isInformational()).isFalse();
    assertThat(HttpStatus.FOUND.isInformational()).isFalse();
    assertThat(HttpStatus.BAD_REQUEST.isInformational()).isFalse();
    assertThat(HttpStatus.NOT_FOUND.isInformational()).isFalse();
    assertThat(HttpStatus.REQUEST_TIMEOUT.isInformational()).isFalse();
    assertThat(HttpStatus.INTERNAL_SERVER_ERROR.isInformational()).isFalse();
    assertThat(HttpStatus.BAD_GATEWAY.isInformational()).isFalse();
  }

  @Test
  public void isSuccessful() {

    Arrays.stream(HttpStatus.values()).forEach(httpStatus ->
      assertThat(httpStatus.isSuccessful())
        .describedAs("%s is not Successful", httpStatus)
        .isEqualTo(String.valueOf(httpStatus.code()).startsWith("2")));
  }

  @Test
  public void isSuccessfulForSelectHttpStatuses() {

    assertThat(HttpStatus.OK.isSuccessful()).isTrue();
    assertThat(HttpStatus.ACCEPTED.isSuccessful()).isTrue();
    assertThat(HttpStatus.NO_CONTENT.isSuccessful()).isTrue();
  }

  @Test
  public void isNotSuccessfulForSelectHttpStatuses() {

    assertThat(HttpStatus.CONTINUE.isSuccessful()).isFalse();
    assertThat(HttpStatus.MULTIPLE_CHOICES.isSuccessful()).isFalse();
    assertThat(HttpStatus.BAD_REQUEST.isSuccessful()).isFalse();
    assertThat(HttpStatus.INTERNAL_SERVER_ERROR.isSuccessful()).isFalse();
  }

  @Test
  public void isRedirection() {

    Arrays.stream(HttpStatus.values()).forEach(httpStatus ->
      assertThat(httpStatus.isRedirection())
        .describedAs("%s is not Redirection", httpStatus)
        .isEqualTo(String.valueOf(httpStatus.code()).startsWith("3")));
  }

  @Test
  public void isRedirectionForSelectHttpStatuses() {

    assertThat(HttpStatus.MULTIPLE_CHOICES.isRedirection()).isTrue();
    assertThat(HttpStatus.FOUND.isRedirection()).isTrue();
    assertThat(HttpStatus.NOT_MODIFIED.isRedirection()).isTrue();
    assertThat(HttpStatus.UNUSED.isRedirection()).isTrue();
  }

  @Test
  public void isNotRedirectionForSelectHttpStatuses() {

    assertThat(HttpStatus.CONTINUE.isRedirection()).isFalse();
    assertThat(HttpStatus.OK.isRedirection()).isFalse();
    assertThat(HttpStatus.BAD_REQUEST.isRedirection()).isFalse();
    assertThat(HttpStatus.INTERNAL_SERVER_ERROR.isRedirection()).isFalse();
  }

  @Test
  public void isClientError() {

    Arrays.stream(HttpStatus.values()).forEach(httpStatus ->
      assertThat(httpStatus.isClientError())
        .describedAs("%s is not a Client Error", httpStatus)
        .isEqualTo(String.valueOf(httpStatus.code()).startsWith("4")));
  }

  @Test
  public void isClientErrorForSelectHttpStatuses() {

    assertThat(HttpStatus.BAD_REQUEST.isClientError()).isTrue();
    assertThat(HttpStatus.UNAUTHORIZED.isClientError()).isTrue();
    assertThat(HttpStatus.PAYMENT_REQUIRED.isClientError()).isTrue();
    assertThat(HttpStatus.FORBIDDEN.isClientError()).isTrue();
    assertThat(HttpStatus.NOT_FOUND.isClientError()).isTrue();
    assertThat(HttpStatus.REQUEST_TIMEOUT.isClientError()).isTrue();
    assertThat(HttpStatus.CONFLICT.isClientError()).isTrue();
    assertThat(HttpStatus.UNSUPPORTED_MEDIA_TYPE.isClientError()).isTrue();
  }

  @Test
  public void isNotClientErrorForSelectHttpStatuses() {

    assertThat(HttpStatus.CONTINUE.isClientError()).isFalse();
    assertThat(HttpStatus.OK.isClientError()).isFalse();
    assertThat(HttpStatus.MULTIPLE_CHOICES.isClientError()).isFalse();
    assertThat(HttpStatus.INTERNAL_SERVER_ERROR.isClientError()).isFalse();
  }

  @Test
  public void isServerError() {

    Arrays.stream(HttpStatus.values()).forEach(httpStatus ->
      assertThat(httpStatus.isServerError())
        .describedAs("%s is not a Server Error", httpStatus)
        .isEqualTo(String.valueOf(httpStatus.code()).startsWith("5")));
  }

  @Test
  public void isServerErrorForSelectHttpStatuses() {

    assertThat(HttpStatus.INTERNAL_SERVER_ERROR.isServerError()).isTrue();
    assertThat(HttpStatus.NOT_IMPLEMENTED.isServerError()).isTrue();
    assertThat(HttpStatus.SERVICE_UNAVAILABLE.isServerError()).isTrue();
    assertThat(HttpStatus.HTTP_VERSION_NOT_SUPPORTED.isServerError()).isTrue();
  }

  @Test
  public void isNotServerErrorForSelectHttpStatuses() {

    assertThat(HttpStatus.CONTINUE.isServerError()).isFalse();
    assertThat(HttpStatus.OK.isServerError()).isFalse();
    assertThat(HttpStatus.MULTIPLE_CHOICES.isServerError()).isFalse();
    assertThat(HttpStatus.BAD_REQUEST.isServerError()).isFalse();
  }

  @Test
  public void httpStatusToString() {

    assertThat(HttpStatus.CONTINUE.toString()).isEqualTo("100 - Continue");
    assertThat(HttpStatus.CREATED.toString()).isEqualTo("201 - Created");
    assertThat(HttpStatus.FOUND.toString()).isEqualTo("302 - Found");
    assertThat(HttpStatus.NOT_FOUND.toString()).isEqualTo("404 - Not Found");
    assertThat(HttpStatus.INTERNAL_SERVER_ERROR.toString()).isEqualTo("500 - Internal Server Error");
  }
}
