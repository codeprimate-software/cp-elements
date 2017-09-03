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

package org.cp.elements.data.convert.support;

import static org.cp.elements.lang.LangExtensions.is;

import java.math.BigInteger;

import org.cp.elements.data.convert.ConversionException;
import org.cp.elements.data.convert.ConverterAdapter;

/**
 * The BigIntegerConverter class converts a String value into a BigInteger.
 *
 * @author John J. Blum
 * @see java.lang.String
 * @see java.math.BigInteger
 * @see org.cp.elements.data.convert.ConverterAdapter
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
    return (is(fromType).assignableTo(String.class) && BigInteger.class.equals(toType));
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
