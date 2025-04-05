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
package org.cp.elements.data.conversion.converters;

import static org.assertj.core.api.Assertions.assertThat;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.lang.ThrowableAssertions;

/**
 * Unit Tests for {@link URLConverter}.
 *
 * @author John J. Blum
 * @see java.net.URI
 * @see java.net.URL
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.data.conversion.converters.URLConverter
 * @since 1.0.0
 */
class URLConverterUnitTests {

  private final URLConverter converter = new URLConverter();

  @Test
  void canConvertToUrlReturnsTrue() {

    assertThat(this.converter.canConvert(URL.class, URL.class)).isTrue();
    assertThat(this.converter.canConvert(URI.class, URL.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, URL.class)).isTrue();
  }

  @Test
  void canConvertNullToUrlReturnFalse() {
    assertThat(this.converter.canConvert(null, URL.class)).isFalse();
  }

  @Test
  void cannotConvertUrlToNullReturnsFalse() {
    assertThat(this.converter.canConvert(URL.class, null)).isFalse();
  }

  @Test
  void cannotConvertToUrlReturnFalse() {

    assertThat(this.converter.canConvert(URL.class, Object.class)).isFalse();
    assertThat(this.converter.canConvert(URL.class, String.class)).isFalse();
    assertThat(this.converter.canConvert(URL.class, URI.class)).isFalse();
    assertThat(this.converter.canConvert(String.class, URI.class)).isFalse();
    assertThat(this.converter.canConvert(Character.class, URI.class)).isFalse();
    assertThat(this.converter.canConvert(Boolean.class, URI.class)).isFalse();
  }

  @Test
  void cannotConvertFromNullToNullIsNullSafe() {
    assertThat(this.converter.canConvert(null, null)).isFalse();
  }


  @Test
  void convertFtpUrlToUrl() {

    String ftpUrl = "ftp://ftp.codeprimate.org";
    URL url = this.converter.convert(ftpUrl);

    assertThat(url.toExternalForm()).isEqualTo(ftpUrl);
  }

  @Test
  void convertStringToUrl() {

    String urlValue = "http://spring.io";
    URL url = this.converter.convert(urlValue);

    assertThat(url.toExternalForm()).isEqualTo(urlValue);
  }

  @Test
  void convertUriToUrl() throws Exception {

    URI uri = URI.create("http://github.com");
    URL url = this.converter.convert(uri);

    assertThat(url).isEqualTo(uri.toURL());
  }

  @Test
  void convertUrlToUrl() throws Exception {

    URL expectedUrl = URI.create("http://github.com").toURL();
    URL actualUrl = this.converter.convert(expectedUrl);

    assertThat(actualUrl).isEqualTo(expectedUrl);
  }

  @Test
  void convertMalformedUrlThrowsException() {

    ThrowableAssertions.assertThatThrowableOfType(ConversionException.class)
      .isThrownBy(args -> this.converter.convert("$:/where/to\\boldly/\\go ?with=it"))
      .havingMessage("[$:/where/to\\boldly/\\go ?with=it] is not a valid URL")
      .causedBy(IllegalArgumentException.class)
      .causedBy(URISyntaxException.class)
      .withNoCause();
  }

  @Test
  void convertNullThrowsException() {

    ThrowableAssertions.assertThatThrowableOfType(ConversionException.class)
      .isThrownBy(args -> this.converter.convert(null))
      .havingMessage("[null] is not a valid URL")
      .withNoCause();
  }
}
