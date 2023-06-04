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
import java.beans.PropertyChangeListener;

import org.cp.elements.lang.annotation.NotNull;

/**
 * An {@link AbstractListener} and {@link PropertyChangeListener} implementation used to
 * process {@link PropertyChangeEvent property change events} from JavaBeans/{@literal POJO}.
 *
 * @author John Blum
 * @see java.beans.PropertyChangeEvent
 * @see java.beans.PropertyChangeListener
 * @see org.cp.elements.beans.event.AbstractListener
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractPropertyChangeListener extends AbstractListener implements PropertyChangeListener {

  /**
   * Constructs a new {@link AbstractPropertyChangeListener} initialized with
   * the array of {@link String property names}.
   *
   * @param propertyNames array of {@link String property names} for which this listener is interested and processes
   * {@link PropertyChangeEvent property change events}.
   */
  public AbstractPropertyChangeListener(String... propertyNames) {
    super(propertyNames);
  }

  /**
   * Constructs a new {@link AbstractPropertyChangeListener} initialized with
   * the {@link Iterable} of {@link String property names}.
   *
   * @param propertyNames {@link Iterable} of {@link String property names} for which this listener
   * is interested and processes {@link PropertyChangeEvent property change events}.
   * @see java.lang.Iterable
   */
  public AbstractPropertyChangeListener(Iterable<String> propertyNames) {
    super(propertyNames);
  }

  /**
   * Event handler method used to process the {@link PropertyChangeEvent}.
   *
   * @param event {@link PropertyChangeEvent} to process.
   * @see java.beans.PropertyChangeEvent
   */
  @Override
  public void propertyChange(@NotNull PropertyChangeEvent event) {
    handle(event);
  }
}
