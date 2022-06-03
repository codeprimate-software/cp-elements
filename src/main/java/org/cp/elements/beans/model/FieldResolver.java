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

/**
 * Interface defining a contract to resolve an {@link Object} {@link Field} from a bean {@link Property}.
 *
 * @author John Blum
 * @see java.lang.reflect.Field
 * @see org.cp.elements.beans.model.Property
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface FieldResolver {

  /**
   * Resolves the {@link Object} {@link Field} from the given bean {@link Property}.
   *
   * @param property bean {@link Property} used to resolve the associated {@link Object} {@link Field}.
   * @return the {@link Object} {@link Field} resolved from the given bean {@link Property}; may be {@literal null}
   * if the bean {@link Property} is derived from another non-backing {@link Object} {@link Field}. Think of a person's
   * age based on his/her data of birth.
   * @see org.cp.elements.beans.model.Property
   * @see java.lang.reflect.Field
   */
  Field resolve(Property property);

}
