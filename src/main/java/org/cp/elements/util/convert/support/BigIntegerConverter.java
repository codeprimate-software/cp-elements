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

import static org.cp.elements.lang.LangExtensions.is;

import java.math.BigInteger;

import org.cp.elements.util.convert.ConversionException;
import org.cp.elements.util.convert.ConverterAdapter;

/**
 * The BigIntegerConverter class converts a String value into a BigInteger.
 * <p/>
 * @author John J. Blum
 * @see java.lang.String
 * @see java.math.BigInteger
 * @see org.cp.elements.util.convert.ConverterAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class BigIntegerConverter extends ConverterAdapter<String, BigInteger> {

  private final Integer radix;

  public BigIntegerConverter() {
    this(null);
  }

  public BigIntegerConverter(final Integer radix) {
    this.radix = radix;
  }

  @Override
  public boolean canConvert(final Class<?> fromType, final Class<?> toType) {
    return (is(String.class).assignableFrom(fromType) && BigInteger.class.equals(toType));
  }

  @Override
  public BigInteger convert(final String value) {
    try {
      return (radix == null ? new BigInteger(String.valueOf(value).trim())
        : new BigInteger(String.valueOf(value).trim(), radix));
    }
    catch (NumberFormatException e) {
      throw new ConversionException(String.format("The String value (%1$s) is not a valid BigInteger!", value), e);
    }
  }

}
