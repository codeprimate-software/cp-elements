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

import java.math.BigDecimal;
import java.math.MathContext;

import org.cp.elements.util.convert.ConversionException;
import org.cp.elements.util.convert.ConverterAdapter;

/**
 * The BigDecimalConverter class converts a String value into a BigDecimal.
 * <p/>
 * @author John J. Blum
 * @see java.lang.String
 * @see java.math.BigDecimal
 * @see org.cp.elements.util.convert.ConverterAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class BigDecimalConverter extends ConverterAdapter<String, BigDecimal> {

  private final MathContext mathContext;

  public BigDecimalConverter() {
    this(null);
  }

  public BigDecimalConverter(final MathContext mathContext) {
    this.mathContext = mathContext;
  }

  @Override
  public boolean canConvert(final Class<?> fromType, final Class<?> toType) {
    return (is(String.class).assignableFrom(fromType) && BigDecimal.class.equals(toType));
  }

  @Override
  public BigDecimal convert(final String value) {
    try {
      return (mathContext == null ? new BigDecimal(String.valueOf(value).trim())
        : new BigDecimal(String.valueOf(value).trim(), mathContext));
    }
    catch (NumberFormatException e) {
      throw new ConversionException(String.format("The String value (%1$s) is not a valid BigDecimal!", value), e);
    }
  }

}
