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

package org.cp.elements.data.conversion.support;

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
 * The URIConverterTest class is a test suite of test cases testing the contract and functionality of the
 * URIConverter class.
 *
 * @author John J. Blum
 * @see java.net.URI
 * @see java.net.URL
 * @see org.cp.elements.data.conversion.support.URIConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class URIConverterTest {

  private final URIConverter converter = new URIConverter();

  @Test
  public void testCanConvert() {
    assertTrue(converter.canConvert(URI.class, URI.class));
    assertTrue(converter.canConvert(URL.class, URI.class));
    assertTrue(converter.canConvert(String.class, URI.class));
  }

  @Test
  public void testCannotConvert() {
    assertFalse(converter.canConvert(null, URI.class));
    assertFalse(converter.canConvert(URI.class, null));
    assertFalse(converter.canConvert(URI.class, Object.class));
    assertFalse(converter.canConvert(URI.class, URL.class));
    assertFalse(converter.canConvert(URI.class, String.class));
    assertFalse(converter.canConvert(String.class, URL.class));
    assertFalse(converter.canConvert(Character.class, URL.class));
    assertFalse(converter.canConvert(Boolean.class, URL.class));
  }

  @Test
  public void testConvertURI() throws Exception {
    URI expectedUri = new URI("http://java.sun.com");
    URI actualUri = converter.convert(expectedUri);

    assertSame(expectedUri, actualUri);
  }

  @Test
  public void testConvertURL() throws Exception {
    URL url = new URL("https://www.codeprimate.org");
    URI uri = converter.convert(url);

    assertNotNull(uri);
    assertNotSame(url, uri);
    assertEquals(url.toURI(), uri);
  }

  @Test
  public void testConvertString() {
    String uriValue = "http://spring.io";
    URI uri = converter.convert(uriValue);

    assertNotNull(uri);
    assertNotSame(uriValue, uri);
    assertEquals(uriValue, uri.toString());
  }

  @Test
  public void testConvertSMTPURI() {
    String smtpUri = "smtp://mail.codeprimate.org";
    URI uri = converter.convert(smtpUri);

    assertNotNull(uri);
    assertNotSame(smtpUri, uri);
    assertEquals(smtpUri, uri.toString());
  }

  @Test(expected = ConversionException.class)
  public void testConvertMalformedURI() {
    try {
      converter.convert(null);
    }
    catch (ConversionException expected) {
      assertEquals("The Object value (null) is not a valid URI!", expected.getMessage());
      throw expected;
    }
  }

}
