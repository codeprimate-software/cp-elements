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

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;

import org.cp.elements.data.conversion.ConversionException;
import org.junit.Test;

/**
 * Unit tests for {@link URLConverter}.
 *
 * @author John J. Blum
 * @see java.net.URI
 * @see java.net.URL
 * @see org.junit.Test
 * @see org.cp.elements.data.conversion.converters.URLConverter
 * @since 1.0.0
 */
public class URLConverterTests {

  private final URLConverter converter = new URLConverter();

  @Test
  public void canConvertToUrlReturnsTrue() {

    assertThat(this.converter.canConvert(URL.class, URL.class)).isTrue();
    assertThat(this.converter.canConvert(URI.class, URL.class)).isTrue();
    assertThat(this.converter.canConvert(String.class, URL.class)).isTrue();
  }

  @Test
  public void canConvertNullToUrlReturnFalse() {
    assertThat(this.converter.canConvert(null, URL.class)).isFalse();
  }

  @Test
  public void cannotConvertUrlToNullReturnsFalse() {
    assertThat(this.converter.canConvert(URL.class, null)).isFalse();
  }

  @Test
  public void cannotConvertToUrlReturnFalse() {

    assertThat(this.converter.canConvert(URL.class, Object.class)).isFalse();
    assertThat(this.converter.canConvert(URL.class, String.class)).isFalse();
    assertThat(this.converter.canConvert(URL.class, URI.class)).isFalse();
    assertThat(this.converter.canConvert(String.class, URI.class)).isFalse();
    assertThat(this.converter.canConvert(Character.class, URI.class)).isFalse();
    assertThat(this.converter.canConvert(Boolean.class, URI.class)).isFalse();
  }


  @Test
  public void convertFtpUrlToUrl() {

    String ftpUrl = "ftp://ftp.codeprimate.org";
    URL url = this.converter.convert(ftpUrl);

    assertThat(url.toExternalForm()).isEqualTo(ftpUrl);
  }

  @Test
  public void convertStringToUrl() {

    String urlValue = "http://spring.io";
    URL url = this.converter.convert(urlValue);

    assertThat(url.toExternalForm()).isEqualTo(urlValue);
  }

  @Test
  public void convertUriToUrl() throws Exception {

    URI uri = URI.create("http://github.com");
    URL url = this.converter.convert(uri);

    assertThat(url).isEqualTo(uri.toURL());
  }

  @Test
  public void convertUrlToUrl() throws Exception {

    URL expectedUrl = new URL("http://github.com");
    URL actualUrl = this.converter.convert(expectedUrl);

    assertThat(actualUrl).isEqualTo(expectedUrl);
  }

  @Test(expected = ConversionException.class)
  public void convertMalformedUrlThrowsException() {

    try {
      this.converter.convert("$:/where/to\\boldly/\\go ?with=it");
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("[$:/where/to\\boldly/\\go ?with=it] is not a valid URL");
      assertThat(expected).hasCauseInstanceOf(MalformedURLException.class);
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ConversionException.class)
  public void convertNullThrowsException() {

    try {
      this.converter.convert(null);
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("[null] is not a valid URL");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }
}
