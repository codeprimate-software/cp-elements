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

/**
 * The ConverterRegistry interface defines a contract for classes that can register Converters.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Iterable
 * @see org.cp.elements.util.convert.ConversionService
 * @see org.cp.elements.util.convert.Converter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface ConverterRegistry extends Iterable<Converter> {

  /**
   * Registers the Converter with the registry making it available to perform type conversions.  Any existing,
   * registered Converter converting from the same source type to the same target type will simply be overridden
   * with the incoming Converter registration.
   * <p/>
   * @param converter the Converter to register with the registry.
   * @see #unregister(Converter)
   * @see org.cp.elements.util.convert.Converter
   */
  void register(Converter<?, ?> converter);

  /**
   * Unregisters the Converter from the registry.
   * <p/>
   * @param converter the Converter to unregister from the registry.
   * @see #register(Converter)
   * @see org.cp.elements.util.convert.Converter
   */
  void unregister(Converter<?, ?> converter);

}
