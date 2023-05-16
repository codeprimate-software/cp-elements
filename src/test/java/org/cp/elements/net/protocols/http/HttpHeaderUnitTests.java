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

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link HttpHeader}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.net.protocols.http.HttpHeader
 * @since 1.0.0
 */
public class HttpHeaderUnitTests {

  @Test
  public void valueOfNameIgnoreCaseForAllNamesReturnsHttpHeader() {

    for (HttpHeader httpHeader : HttpHeader.values()) {
      assertThat(HttpHeader.valueOfNameIgnoreCase(httpHeader.getName())).isEqualTo(httpHeader);
    }
  }

  @Test
  public void valueOfNameIgnoreCaseReturnsHttpHeader() {

    assertThat(HttpHeader.valueOfNameIgnoreCase("ACCEPT")).isEqualTo(HttpHeader.ACCEPT);
    assertThat(HttpHeader.valueOfNameIgnoreCase("Allow")).isEqualTo(HttpHeader.ALLOW);
    assertThat(HttpHeader.valueOfNameIgnoreCase("content-length")).isEqualTo(HttpHeader.CONTENT_LENGTH);
    assertThat(HttpHeader.valueOfNameIgnoreCase(" dATE   ")).isEqualTo(HttpHeader.DATE);
    assertThat(HttpHeader.valueOfNameIgnoreCase(" eTag ")).isEqualTo(HttpHeader.ETAG);
    assertThat(HttpHeader.valueOfNameIgnoreCase("hOsT ")).isEqualTo(HttpHeader.HOST);
  }

  @Test
  public void valueOfNameIgnoreCaseWithInvalidNamesReturnsNull() {

    assertThat(HttpHeader.valueOfNameIgnoreCase("EXPIRE")).isNull();
    assertThat(HttpHeader.valueOfNameIgnoreCase("if-not-match")).isNull();
    assertThat(HttpHeader.valueOfNameIgnoreCase("last_modified")).isNull();
    assertThat(HttpHeader.valueOfNameIgnoreCase("locate")).isNull();
    assertThat(HttpHeader.valueOfNameIgnoreCase("loser-agent")).isNull();
    assertThat(HttpHeader.valueOfNameIgnoreCase("  ")).isNull();
    assertThat(HttpHeader.valueOfNameIgnoreCase("")).isNull();
  }

  @Test
  public void valueOfNameWithNullIsNullSafeReturnsNull() {
    assertThat(HttpHeader.valueOfNameIgnoreCase(null)).isNull();
  }
}
