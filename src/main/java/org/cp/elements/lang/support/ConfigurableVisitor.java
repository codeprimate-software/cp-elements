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
package org.cp.elements.lang.support;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Configurable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link Visitor} implementation that visits an {@link Object} graph / hierarchy to configure
 * each {@link Object} visited.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Configurable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ConfigurableVisitor<T> implements Visitor {

  private final T configuration;

  /**
   * Constructs an instance of the ConfigurableVisitor class initialized with the specified configuration used to
   * configure objects visited in the graph/hierarchy.
   *
   * @param configuration the Configuration used to configure visited objects.
   */
  public ConfigurableVisitor(T configuration) {
    Assert.notNull(configuration, "Configuration cannot be null!");
    this.configuration = configuration;
  }

  /**
   * Visits Configurable objects in an object graph/hierarchy in order to apply configuration.
   *
   * @param visitable the Visitable object visited by this Visitor.
   * @see org.cp.elements.lang.Configurable#configure(Object)
   * @see org.cp.elements.lang.Visitable
   */
  @Override
  @SuppressWarnings("unchecked")
  public void visit(@Nullable Visitable visitable) {

    if (visitable instanceof Configurable) {
      ((Configurable<T>) visitable).configure(this.configuration);
    }
  }
}
