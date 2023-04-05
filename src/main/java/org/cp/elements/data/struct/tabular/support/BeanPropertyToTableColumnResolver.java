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
package org.cp.elements.data.struct.tabular.support;

import org.cp.elements.beans.Bean;
import org.cp.elements.beans.model.Property;
import org.cp.elements.data.struct.tabular.Column;
import org.cp.elements.data.struct.tabular.Table;
import org.cp.elements.data.struct.tabular.View;

/**
 * Interface defining a contract for resolving a {@link Table} or {@link View} {@link Column} for a given,
 * required {@link Bean} {@link Property}.
 *
 * @author John Blum
 * @see org.cp.elements.beans.Bean
 * @see org.cp.elements.beans.model.Property
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.Table
 * @see org.cp.elements.data.struct.tabular.View
 * @since 1.0.2
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface BeanPropertyToTableColumnResolver {

  /**
   * Resolves a {@link Table} {@link Column} from an given, required {@link Bean} {@link Property}.
   *
   * @param beanProperty {@link Bean} {@link Property} to resolve into a {@link Table} {@link Column}.
   * @return the resolved {@link Table} {@link Column} from the given, required {@link Bean} {@link Property}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see org.cp.elements.beans.model.Property
   */
  Column<?> resolve(Property beanProperty);

}
