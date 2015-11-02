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

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.cp.elements.util.convert.ConverterAdapter;

/**
 * The BooleanConverter class converts an Object value into a Boolean.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Boolean
 * @see java.lang.Object
 * @see org.cp.elements.util.convert.ConverterAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class BooleanConverter extends ConverterAdapter<Object, Boolean> {

  protected final Set<String> trueValues;

  public BooleanConverter() {
    this(Boolean.TRUE.toString());
  }

  public BooleanConverter(final String... trueValues) {
    this.trueValues = (trueValues == null ? Collections.<String>emptySet()
      : Collections.unmodifiableSet(new HashSet<>(Arrays.asList(trueValues))));
  }

  protected boolean isTrue(final Object value) {
    final String valueString = String.valueOf(value).trim();
    return (Boolean.parseBoolean(valueString) || isTrue(valueString));
  }

  protected boolean isTrue(final String value) {
    for (String trueValue : trueValues) {
      if (trueValue.equalsIgnoreCase(value)) {
        return true;
      }
    }

    return false;
  }

  @Override
  public boolean canConvert(final Class<?> fromType, final Class<?> toType) {
    return (fromType != null && (Boolean.class.equals(toType) || Boolean.TYPE.equals(toType)));
  }

  @Override
  public Boolean convert(final Object value) {
    if (value instanceof Boolean) {
      return (Boolean) value;
    }

    return isTrue(value);
  }

}
