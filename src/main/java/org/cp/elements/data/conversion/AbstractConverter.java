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

package org.cp.elements.data.conversion;

import static org.cp.elements.lang.LangExtensions.is;

import org.cp.elements.lang.Assert;

/**
 * The AbstractConverter class is an abstract base class encapsulating all common functionality and behavior for all
 * custom Converter implementation.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.conversion.ConversionServiceAware
 * @see org.cp.elements.data.conversion.Converter
 * @see org.cp.elements.data.conversion.ConverterAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractConverter<S, T> implements Converter<S, T> {

  private ConversionService conversionService;

  protected ConversionService getConversionService() {
    Assert.state(conversionService != null, "The ConversionService reference was not properly initialized!");
    return conversionService;
  }

  public final void setConversionService(final ConversionService conversionService) {
    this.conversionService = conversionService;
  }

  protected boolean isAssignableTo(final Class<?> fromType, final Class<?>... toTypes) {
    for (Class toType : toTypes) {
      if (is(fromType).assignableTo(toType)) {
        return true;
      }
    }

    return false;
  }

}
