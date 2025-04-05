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
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.lang.ThrowableAssertions;

/**
 * Unit Tests for {@link URIConverter}.
 *
 * @author John J. Blum
 * @see java.net.URI
 * @see java.net.URL
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.data.conversion.converters.URIConverter
 * @since 1.0.0
 */
class URIConverterUnitTests {

  private final URIConverter converter = new URIConverter();

  @Test
  void canConvertToUriReturnsTrue() {

    assertThat(this.converter.canConvert(URI.class, URI.class)).isTrue();
    assertThat(this.converter.canConvert(URL.class, URI.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, URI.class)).isTrue();
  }

  @Test
  void canConvertNullToUriReturnFalse() {
    assertThat(this.converter.canConvert(null, URI.class)).isFalse();
  }

  @Test
  void cannotConvertUriToNullReturnsFalse() {
    assertThat(this.converter.canConvert(URI.class, null)).isFalse();
  }

  @Test
  void cannotConvertToUriReturnFalse() {

    assertThat(this.converter.canConvert(URI.class, Object.class)).isFalse();
    assertThat(this.converter.canConvert(URI.class, String.class)).isFalse();
    assertThat(this.converter.canConvert(URI.class, URL.class)).isFalse();
    assertThat(this.converter.canConvert(String.class, URL.class)).isFalse();
    assertThat(this.converter.canConvert(Character.class, URL.class)).isFalse();
    assertThat(this.converter.canConvert(Boolean.class, URL.class)).isFalse();
  }

  @Test
  void cannotConverterFromNullToNullIsNullSafe() {
    assertThat(this.converter.canConvert(null, null)).isFalse();
  }

  @Test
  void convertSmtpUrlToUri() {

    String smtpUri = "smtp://mail.codeprimate.org";
    URI uri = this.converter.convert(smtpUri);

    assertThat(uri.toString()).isEqualTo(smtpUri);
  }

  @Test
  void convertStringToUri() {

    String uriValue = "http://spring.io";
    URI uri = this.converter.convert(uriValue);

    assertThat(uri.toString()).isEqualTo(uriValue);
  }

  @Test
  @SuppressWarnings("all")
  void convertUriToUri() {

    URI expectedUri = URI.create("http://github.com");
    URI actualUri = this.converter.convert(expectedUri);

    assertThat(actualUri).isEqualTo(actualUri);
  }

  @Test
  void convertUrlToUri() throws Exception {

    URL url = URI.create("https://github.com").toURL();
    URI uri = this.converter.convert(url);

    assertThat(uri.toURL()).isEqualTo(url);
  }

  @Test
  void convertMalformedUriThrowsException() {

    ThrowableAssertions.assertThatThrowableOfType(ConversionException.class)
      .isThrownBy(args -> this.converter.convert("$:/where/to\\boldly/\\go ?with=it"))
      .havingMessage("[$:/where/to\\boldly/\\go ?with=it] is not a valid URI")
      .causedBy(IllegalArgumentException.class)
      .causedBy(URISyntaxException.class)
      .withNoCause();
  }

  @Test
  void convertNullThrowsException() {

    assertThatExceptionOfType(ConversionException.class)
      .isThrownBy(() -> this.converter.convert(null))
      .withMessage("[null] is not a valid URI")
      .withNoCause();
  }
}
