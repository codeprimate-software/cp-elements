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
package org.cp.elements.beans.event.support;

import static org.cp.elements.lang.ElementsExceptionsFactory.newImmutableObjectException;

import java.beans.PropertyChangeEvent;
import java.beans.VetoableChangeListener;

import org.cp.elements.beans.event.AbstractVetoableChangeListener;
import org.cp.elements.lang.annotation.NotNull;

/**
 * {@link VetoableChangeListener} implementation that enforces immutability of a JavaBean/POJO.
 *
 * @author John Blum
 * @see java.beans.VetoableChangeListener
 * @see org.cp.elements.beans.event.AbstractVetoableChangeListener
 * @since 1.0.0
 */
@SuppressWarnings("all")
public class ObjectEffectivelyImmutableVetoableChangeListener extends AbstractVetoableChangeListener {

  public static final ObjectEffectivelyImmutableVetoableChangeListener INSTANCE =
    new ObjectEffectivelyImmutableVetoableChangeListener();

  @Override
  protected void doHandle(@NotNull PropertyChangeEvent event) {
    throw newImmutableObjectException("Cannot change property [%s]; Object [%s] is immutable",
      event.getPropertyName(), event.getSource());
  }
}
