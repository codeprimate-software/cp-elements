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
 * The URIConverterTest class is a test suite of test cases testing the contract and functionality of the
 * URIConverter class.
 * <p/>
 * @author John J. Blum
 * @see java.net.URI
 * @see java.net.URL
 * @see org.cp.elements.util.convert.support.URIConverter
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
