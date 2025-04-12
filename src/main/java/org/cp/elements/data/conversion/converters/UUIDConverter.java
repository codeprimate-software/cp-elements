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
package org.cp.elements.data.conversion.converters;

import java.util.UUID;

import org.cp.elements.data.conversion.AbstractConverter;
import org.cp.elements.data.conversion.Converter;
import org.cp.elements.lang.StringUtils;

/**
 * {@link Converter} used to convert from a {@link String} to a {@link UUID}.
 *
 * @author John Blum
 * @see java.lang.String
 * @see java.util.UUID
 * @since 2.0.0
 */
@SuppressWarnings("all")
public class UUIDConverter extends AbstractConverter<String, UUID> {

  @Override
  public UUID convert(String value) {
    return UUID.fromString(StringUtils.requireText(value, "String [%s] to convert to a UUID is required"));
  }
}
