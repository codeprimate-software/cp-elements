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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.net.URI;
import java.net.URL;

import org.cp.elements.data.conversion.ConversionException;
import org.junit.Test;

/**
 * The URLConverterTest class is a test suite of test cases testing the contract and functionality of the
 * URLConverter class.
 *
 * @author John J. Blum
 * @see java.net.URI
 * @see java.net.URL
 * @see org.cp.elements.data.conversion.converters.URLConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class URLConverterTest {

  private final URLConverter converter = new URLConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(URL.class, URL.class));
    assertTrue(converter.canConvert(URI.class, URL.class));
    assertTrue(converter.canConvert(String.class, URL.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(null, URL.class));
    assertFalse(converter.canConvert(URL.class, null));
    assertFalse(converter.canConvert(URL.class, Object.class));
    assertFalse(converter.canConvert(URL.class, String.class));
    assertFalse(converter.canConvert(URL.class, URI.class));
    assertFalse(converter.canConvert(String.class, URI.class));
    assertFalse(converter.canConvert(String.class, String.class));
    assertFalse(converter.canConvert(Character.class, URL.class));
    assertFalse(converter.canConvert(Boolean.class, URL.class));
  }

  @Test
  public void testConvertURL() throws Exception {
    URL expectedUrl = new URL("http://java.sun.com");
    URL actualUrl = converter.convert(expectedUrl);

    assertSame(expectedUrl, actualUrl);
  }

  @Test
  public void testConvertURI() throws Exception {
    URI uri = new URI("http://www.codeprimate.org");
    URL url = converter.convert(uri);

    assertNotNull(url);
    assertNotSame(uri, url);
    assertEquals(uri.toURL(), url);
  }

  @Test
  public void testConvertString() {
    String urlValue = "http://spring.io";
    URL url = converter.convert(urlValue);

    assertNotNull(url);
    assertNotSame(urlValue, url);
    assertEquals(urlValue, url.toExternalForm());
  }

  @Test
  public void testConvertFTPURL() {
    String ftpUrl = "ftp://ftp.codeprimate.org";
    URL url = converter.convert(ftpUrl);

    assertNotNull(url);
    assertNotSame(ftpUrl, url);
    assertEquals(ftpUrl, url.toExternalForm());
  }

  @Test(expected = ConversionException.class)
  public void testConvertMalformedURL() {
    try {
      converter.convert("MALFORMED_URL");
    }
    catch (ConversionException expected) {
      assertEquals("The Object value (MALFORMED_URL) is not a valid URL!", expected.getMessage());
      throw expected;
    }
  }

}
