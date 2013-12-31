/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.util.convert.support;

import static org.junit.Assert.*;

import java.net.URI;
import java.net.URL;

import org.cp.elements.util.convert.ConversionException;
import org.junit.Test;

/**
 * The URLConverterTest class is a test suite of test cases testing the contract and functionality of the
 * URLConverter class.
 * <p/>
 * @author John J. Blum
 * @see java.net.URI
 * @see java.net.URL
 * @see org.cp.elements.util.convert.support.URLConverter
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
