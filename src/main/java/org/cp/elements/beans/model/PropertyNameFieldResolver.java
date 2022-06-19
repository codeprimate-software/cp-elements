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
package org.cp.elements.beans.model;

import java.lang.reflect.Field;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link FieldResolver} implementation based on the {@link Property Property's} {@link Property#getName() name}.
 *
 * @author John Blum
 * @see java.lang.reflect.Field
 * @see org.cp.elements.beans.model.FieldResolver
 * @see org.cp.elements.beans.model.Property
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PropertyNameFieldResolver implements FieldResolver {

  @NullSafe
  @Override
  public @Nullable Field resolve(@NotNull Property property) {

    if (property != null) {

      Object target = property.getTargetObject();
      String propertyName = property.getName();

      for (Field field : BeanUtils.getAllDeclaredFields(target)) {
        if (field.getName().equals(propertyName)) {
          return field;
        }
      }
    }

    return null;
  }
}
