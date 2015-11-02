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

import org.cp.elements.util.convert.ConversionException;
import org.cp.elements.util.convert.ConverterAdapter;

/**
 * The EnumConverter class converts a String value into an Enum of the qualifying enumerated type.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Enum
 * @see java.lang.Object
 * @see org.cp.elements.util.convert.ConverterAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class EnumConverter extends ConverterAdapter<String, Enum> {

  @Override
  public boolean canConvert(final Class<?> fromType, final Class<?> toType) {
    return (String.class.equals(fromType) && isAssignableTo(toType, Enum.class));
  }

  @Override
  public <QT extends Enum> QT convert(final String value, final Class<QT> enumType) {
    try {
      Enum enumInstance = Enum.valueOf(enumType, value);
      return enumType.cast(enumInstance);
    }
    catch (Exception e) {
      throw new ConversionException(String.format("(%1$s) is not a valid enumerated value of Enum (%2$s)",
        value, enumType), e);
    }
  }

}
