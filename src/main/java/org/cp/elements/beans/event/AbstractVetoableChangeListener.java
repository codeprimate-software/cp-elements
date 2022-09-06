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
package org.cp.elements.beans.event;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;

import org.cp.elements.beans.BeansException;
import org.cp.elements.beans.ChainedPropertyVetoException;
import org.cp.elements.lang.annotation.NotNull;

/**
 * {@link AbstractListener} and {@link VetoableChangeListener} implementation used to
 * process {@link PropertyChangeEvent property change events}.
 *
 * @author John Blum
 * @see java.beans.PropertyChangeEvent
 * @see java.beans.VetoableChangeListener
 * @see org.cp.elements.beans.event.AbstractListener
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractVetoableChangeListener extends AbstractListener implements VetoableChangeListener {

  /**
   * Constructs a new instance of {@link AbstractVetoableChangeListener} initialized with
   * the array of {@link String property names}.
   *
   * @param propertyNames array of {@link String property names} for which this listener is interested and processes
   * {@link PropertyChangeEvent property change events}.
   */
  public AbstractVetoableChangeListener(String... propertyNames) {
    super(propertyNames);
  }

  /**
   * Constructs a new instance of {@link AbstractVetoableChangeListener} initialized with
   * the {@link Iterable} of {@link String property names}.
   *
   * @param propertyNames {@link Iterable} of {@link String property names} for which this listener
   * is interested and processes {@link PropertyChangeEvent property change events}.
   * @see java.lang.Iterable
   */
  public AbstractVetoableChangeListener(Iterable<String> propertyNames) {
    super(propertyNames);
  }

  /**
   * Processes the given {@link PropertyChangeEvent} and vetoes any property change
   * that does not satisfy the rules enforced by this listener.
   *
   * @param event {@link PropertyChangeEvent} to process; never {@literal null}.
   * @throws PropertyVetoException if the property change does not satisfy the rules
   * enforced by this listener.
   * @see java.beans.PropertyChangeEvent
   */
  @Override
  public void vetoableChange(@NotNull PropertyChangeEvent event) throws PropertyVetoException {

    try {
      handle(event);
    }
    catch (BeansException cause) {
      String message = String.format("Error occurred while processing event [%s]", event);
      throw new ChainedPropertyVetoException(event, message, cause);
    }
  }
}
