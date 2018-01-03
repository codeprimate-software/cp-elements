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
 * The ConverterAdapter class is an abstract base class for all Converter implementations implemented as an Adapter
 * throwing UnsupportedOperationExceptions for all unimplemented Converter methods.
 *
 * @author John Blum
 * @see java.lang.UnsupportedOperationException
 * @see org.cp.elements.data.conversion.AbstractConverter
 * @see org.cp.elements.data.conversion.Converter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ConverterAdapter<S, T> extends AbstractConverter<S, T> {

}
