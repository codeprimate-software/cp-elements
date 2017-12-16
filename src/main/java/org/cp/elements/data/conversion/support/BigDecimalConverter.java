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

import static org.cp.elements.lang.LangExtensions.is;

import java.math.BigDecimal;
import java.math.MathContext;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.ConverterAdapter;

/**
 * The BigDecimalConverter class converts a String value into a BigDecimal.
 *
 * @author John J. Blum
 * @see java.lang.String
 * @see java.math.BigDecimal
 * @see org.cp.elements.data.conversion.ConverterAdapter
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
    return (is(fromType).assignableTo(String.class) && BigDecimal.class.equals(toType));
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
