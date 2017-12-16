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

/**
 * The ConverterRegistry interface defines a contract for classes that can register Converters.
 *
 * @author John J. Blum
 * @see java.lang.Iterable
 * @see org.cp.elements.data.conversion.ConversionService
 * @see org.cp.elements.data.conversion.Converter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface ConverterRegistry extends Iterable<Converter> {

  /**
   * Registers the Converter with the registry making it available to perform type conversions.  Any existing,
   * registered Converter converting from the same source type to the same target type will simply be overridden
   * with the incoming Converter registration.
   *
   * @param converter the Converter to register with the registry.
   * @see #unregister(Converter)
   * @see org.cp.elements.data.conversion.Converter
   */
  void register(Converter<?, ?> converter);

  /**
   * Unregisters the Converter from the registry.
   *
   * @param converter the Converter to unregister from the registry.
   * @see #register(Converter)
   * @see org.cp.elements.data.conversion.Converter
   */
  void unregister(Converter<?, ?> converter);

}
