/*
 * Copyright 2011-Present Author or Authors.
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

import org.cp.elements.lang.Registry;

/**
 * The {@link ConverterRegistry} interface defines a contract for classes
 * that can register {@link Converter Converters}.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.conversion.ConversionService
 * @see org.cp.elements.data.conversion.Converter
 * @see org.cp.elements.lang.Registry
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface ConverterRegistry extends Registry<Converter<?, ?>> {

}
