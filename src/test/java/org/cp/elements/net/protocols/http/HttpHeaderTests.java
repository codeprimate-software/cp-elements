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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link HttpHeader} enum.
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
      assertThat(HttpHeader.valueOfIgnoreCase(httpHeader.getName()), is(equalTo(httpHeader)));
    }
  }

  @Test
  public void valueOfIgnoreCaseReturnsHttpHeader() {
    assertThat(HttpHeader.valueOfIgnoreCase("ACCEPT"), is(equalTo(HttpHeader.ACCEPT)));
    assertThat(HttpHeader.valueOfIgnoreCase("Allow"), is(equalTo(HttpHeader.ALLOW)));
    assertThat(HttpHeader.valueOfIgnoreCase("content-length"), is(equalTo(HttpHeader.CONTENT_LENGTH)));
    assertThat(HttpHeader.valueOfIgnoreCase(" dATE   "), is(equalTo(HttpHeader.DATE)));
    assertThat(HttpHeader.valueOfIgnoreCase(" eTag "), is(equalTo(HttpHeader.ETAG)));
    assertThat(HttpHeader.valueOfIgnoreCase("hOsT "), is(equalTo(HttpHeader.HOST)));
  }

  @Test
  public void valueOfIgnoreCaseWithInvalidValuesReturnsNull() {
    assertThat(HttpHeader.valueOfIgnoreCase("EXPIRE"), is(nullValue(HttpHeader.class)));
    assertThat(HttpHeader.valueOfIgnoreCase("if-not-match"), is(nullValue(HttpHeader.class)));
    assertThat(HttpHeader.valueOfIgnoreCase("last_modified"), is(nullValue(HttpHeader.class)));
    assertThat(HttpHeader.valueOfIgnoreCase("locate"), is(nullValue(HttpHeader.class)));
    assertThat(HttpHeader.valueOfIgnoreCase("loser-agent"), is(nullValue(HttpHeader.class)));
    assertThat(HttpHeader.valueOfIgnoreCase("  "), is(nullValue(HttpHeader.class)));
    assertThat(HttpHeader.valueOfIgnoreCase(""), is(nullValue(HttpHeader.class)));
    assertThat(HttpHeader.valueOfIgnoreCase(null), is(nullValue(HttpHeader.class)));
  }
}
