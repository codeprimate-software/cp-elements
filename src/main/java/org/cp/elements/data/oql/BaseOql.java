/*
 * Copyright 2017-Present Author or Authors.
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
package org.cp.elements.data.oql;

import org.cp.elements.data.mapping.UndefinedMappingException;
import org.cp.elements.lang.DslExtension;
import org.cp.elements.lang.FluentApiExtension;

/**
 * Base interface for all {@literal Object Query Language (OQL)} definitions.
 *
 * @author John Blum
 * @see org.cp.elements.lang.DslExtension
 * @see org.cp.elements.lang.FluentApiExtension
 * @since 2.0.0
 */
interface BaseOql extends DslExtension, FluentApiExtension {

  /**
   * Interface defining a contract for an {@literal OQL} component capable of mapping an {@link Object}
   * from one {@link Class type} to another {@link Class type}.
   *
   * @param <S> source {@link Class type}.
   * @param <T> target {@link Class type}.
   * @see org.cp.elements.data.oql.QueryContext
   */
  interface ObjectMapper<S, T> {

    default T map(QueryContext<S, T> queryContext, S target) {
      throw UndefinedMappingException.INSTANCE;
    }
  }
}
