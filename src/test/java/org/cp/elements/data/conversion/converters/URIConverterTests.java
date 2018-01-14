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

package org.cp.elements.data.conversion.converters;

import static org.assertj.core.api.Assertions.assertThat;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

import org.cp.elements.data.conversion.ConversionException;
import org.junit.Test;

/**
 * Unit tests for {@link URIConverter}.
 *
 * @author John J. Blum
 * @see java.net.URI
 * @see java.net.URL
 * @see org.junit.Test
 * @see org.cp.elements.data.conversion.converters.URIConverter
 * @since 1.0.0
 */
public class URIConverterTests {

  private final URIConverter converter = new URIConverter();

  @Test
  public void canConvertToUriReturnsTrue() {

    assertThat(this.converter.canConvert(URI.class, URI.class)).isTrue();
    assertThat(this.converter.canConvert(URL.class, URI.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, URI.class)).isTrue();
  }

  @Test
  public void canConvertNullToUriReturnFalse() {
    assertThat(this.converter.canConvert(null, URI.class)).isFalse();
  }

  @Test
  public void cannotConvertUriToNullReturnsFalse() {
    assertThat(this.converter.canConvert(URI.class, null)).isFalse();
  }

  @Test
  public void cannotConvertToUriReturnFalse() {

    assertThat(this.converter.canConvert(URI.class, Object.class)).isFalse();
    assertThat(this.converter.canConvert(URI.class, String.class)).isFalse();
    assertThat(this.converter.canConvert(URI.class, URL.class)).isFalse();
    assertThat(this.converter.canConvert(String.class, URL.class)).isFalse();
    assertThat(this.converter.canConvert(Character.class, URL.class)).isFalse();
    assertThat(this.converter.canConvert(Boolean.class, URL.class)).isFalse();
  }

  @Test
  public void convertSmtpUrlToUri() {

    String smtpUri = "smtp://mail.codeprimate.org";
    URI uri = this.converter.convert(smtpUri);

    assertThat(uri.toString()).isEqualTo(smtpUri);
  }

  @Test
  public void convertStringToUri() {

    String uriValue = "http://spring.io";
    URI uri = this.converter.convert(uriValue);

    assertThat(uri.toString()).isEqualTo(uriValue);
  }

  @Test
  public void convertUriToUri() throws Exception {

    URI expectedUri = URI.create("http://github.com");
    URI actualUri = this.converter.convert(expectedUri);

    assertThat(actualUri).isEqualTo(actualUri);
  }

  @Test
  public void convertUrlToUri() throws Exception {

    URL url = new URL("https://github.com");
    URI uri = this.converter.convert(url);

    assertThat(uri.toURL()).isEqualTo(url);
  }

  @Test(expected = ConversionException.class)
  public void convertMalformedUriThrowsException() {

    try {
      this.converter.convert("$:/where/to\\boldly/\\go ?with=it");
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("[$:/where/to\\boldly/\\go ?with=it] is not a valid URI");
      assertThat(expected).hasCauseInstanceOf(IllegalArgumentException.class);
      assertThat(expected.getCause()).hasCauseInstanceOf(URISyntaxException.class);
      assertThat(expected.getCause().getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ConversionException.class)
  public void convertNullThrowsException() {

    try {
      this.converter.convert(null);
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("[null] is not a valid URI");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }
}
