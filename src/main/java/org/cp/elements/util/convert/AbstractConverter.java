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

package org.cp.elements.util.convert;

import static org.cp.elements.lang.OperatorUtils.is;

import org.cp.elements.lang.Assert;

/**
 * The AbstractConverter class is an abstract base class encapsulating all common functionality and behavior for all
 * custom Converter implementation.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.convert.ConversionServiceAware
 * @see org.cp.elements.util.convert.Converter
 * @see org.cp.elements.util.convert.ConverterAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractConverter<S, T> implements Converter<S, T> {

  private ConversionService conversionService;

  protected ConversionService getConversionService() {
    Assert.state(conversionService != null, "The reference to the ConversionService was not properly initialized!");
    return conversionService;
  }

  public final void setConversionService(final ConversionService conversionService) {
    this.conversionService = conversionService;
  }

  protected boolean isAssignableTo(final Class<?> fromType, final Class<?>... toTypes) {
    for (Class toType : toTypes) {
      if (is(toType).assignableFrom(fromType)) {
        return true;
      }
    }

    return false;
  }

}
