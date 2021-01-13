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

import org.junit.Test;

/**
 * Unit Tests for the {@link HttpHeader} enum.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.net.protocols.http.HttpHeader
 * @since 1.0.0
 */
public class HttpHeaderTests {

  @Test
  public void valueOfIgnoreCaseForAllNamesReturnsHttpHeader() {

    for (HttpHeader httpHeader : HttpHeader.values()) {
      assertThat(HttpHeader.valueOfIgnoreCase(httpHeader.getName())).isEqualTo(httpHeader);
    }
  }

  @Test
  public void valueOfIgnoreCaseReturnsHttpHeader() {

    assertThat(HttpHeader.valueOfIgnoreCase("ACCEPT")).isEqualTo(HttpHeader.ACCEPT);
    assertThat(HttpHeader.valueOfIgnoreCase("Allow")).isEqualTo(HttpHeader.ALLOW);
    assertThat(HttpHeader.valueOfIgnoreCase("content-length")).isEqualTo(HttpHeader.CONTENT_LENGTH);
    assertThat(HttpHeader.valueOfIgnoreCase(" dATE   ")).isEqualTo(HttpHeader.DATE);
    assertThat(HttpHeader.valueOfIgnoreCase(" eTag ")).isEqualTo(HttpHeader.ETAG);
    assertThat(HttpHeader.valueOfIgnoreCase("hOsT ")).isEqualTo(HttpHeader.HOST);
  }

  @Test
  public void valueOfIgnoreCaseWithInvalidValuesReturnsNull() {

    assertThat(HttpHeader.valueOfIgnoreCase("EXPIRE")).isNull();
    assertThat(HttpHeader.valueOfIgnoreCase("if-not-match")).isNull();
    assertThat(HttpHeader.valueOfIgnoreCase("last_modified")).isNull();
    assertThat(HttpHeader.valueOfIgnoreCase("locate")).isNull();
    assertThat(HttpHeader.valueOfIgnoreCase("loser-agent")).isNull();
    assertThat(HttpHeader.valueOfIgnoreCase("  ")).isNull();
    assertThat(HttpHeader.valueOfIgnoreCase("")).isNull();
    assertThat(HttpHeader.valueOfIgnoreCase(null)).isNull();
  }
}
