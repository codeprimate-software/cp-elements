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

import java.net.URI;
import java.net.URL;

import org.cp.elements.util.convert.ConversionException;
import org.cp.elements.util.convert.ConverterAdapter;

/**
 * The URIConverter class converts an Object value into a URI.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Object
 * @see java.net.URI
 * @see org.cp.elements.util.convert.ConverterAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class URIConverter extends ConverterAdapter<Object, URI> {

  @Override
  public boolean canConvert(final Class<?> fromType, final Class<?> toType) {
    return (isAssignableTo(fromType, URI.class, URL.class, String.class) && URI.class.equals(toType));
  }

  @Override
  public URI convert(final Object value) {
    try {
      if (value instanceof URI) {
        return (URI) value;
      }
      else if (value instanceof URL) {
        return ((URL) value).toURI();
      }
      else if (value instanceof String) {
        return URI.create(value.toString().trim());
      }
      else {
        throw new ConversionException(String.format("The Object value (%1$s) is not a valid URI!", value));
      }
    }
    catch (Exception e) {
      throw new ConversionException(String.format("The Object value (%1$s) is not a valid URI!", value), e);
    }
  }

}
