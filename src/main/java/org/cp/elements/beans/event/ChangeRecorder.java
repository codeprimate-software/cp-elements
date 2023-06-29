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
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

import org.cp.elements.beans.Bean;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.annotation.NotNull;

/**
 * {@link PropertyChangeListener} implementation used by {@link Bean} implementations for recording and monitoring
 * changes in state to the properties of the {@literal observed} {@link Bean}.
 *
 * @author John J. Blum
 * @see java.beans.PropertyChangeEvent
 * @see java.beans.PropertyChangeListener
 * @see java.lang.Iterable
 * @see org.cp.elements.beans.Bean
 * @see org.cp.elements.lang.Visitable
 * @since 1.0.0
 */
public class ChangeRecorder implements Iterable<String>, PropertyChangeListener, Visitable {

  // Map of object state mapping property name to the property value's hash code, ordered (sorted) by property name.
  private final Map<String, Integer> objectStateMap = new TreeMap<>();

  /**
   * Determines whether the {@link Bean} monitored by this {@link ChangeRecorder} has any modified properties.
   *
   * @return {@literal true} if the {@link Bean} monitored by {@literal this} {@link ChangeRecorder} has any
   * modified properties.
   * @see #isModified(String)
   */
  public boolean isModified() {
    return !this.objectStateMap.isEmpty();
  }

  /**
   * Determines whether the {@link String named} property of the {@link Bean} monitored by {@literal this}
   * {@link ChangeRecorder} has been modified.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property on the {@link Bean}
   * monitored by {@literal this} {@link ChangeRecorder}.
   * @return {@literal true} if the {@link String named} property on the {@link Bean} monitored by {@literal this}
   * {@link ChangeRecorder} has been modified.
   * @see #isModified()
   */
  public boolean isModified(@NotNull String propertyName) {
    return this.objectStateMap.containsKey(propertyName);
  }

  /**
   * Event handler method that is fired when a property of the {@link Bean} monitored by this {@link ChangeRecorder}
   * has been modified.
   * <p>
   * The {@link PropertyChangeEvent} encapsulates all the information pertaining to the property change
   * including the {@link String name} of the property, the {@link Object old value} and {@link Object new value}
   * along with the source {@link Bean} of the targeted change.
   *
   * @param event {@link PropertyChangeEvent} encapsulating information about the property change.
   * @see java.beans.PropertyChangeEvent
   * @see #isModified(String)
   */
  public void propertyChange(@NotNull PropertyChangeEvent event) {

    String propertyName = event.getPropertyName();

    if (isModified(propertyName)) {
      if (this.objectStateMap.get(propertyName) == ObjectUtils.hashCode(event.getNewValue())) {
        // NOTE: If the hash code of the property's old (original) value is equal to the hash code of the property's
        // new value, then the state of the property, identified by the event, has been reverted to its old (original),
        // persisted value.
        this.objectStateMap.remove(propertyName);
      }
    }
    else {
      if (!ObjectUtils.equalsIgnoreNull(event.getOldValue(), event.getNewValue())) {
        this.objectStateMap.put(propertyName, ObjectUtils.hashCode(event.getOldValue()));
      }
    }
  }

  /**
   * Iterates over the set of properties on the {@link Bean} monitored by {@literal this} {@link ChangeRecorder}
   * that have been modified.
   *
   * @return an {@link Iterator} over the modified properties on the {@link Bean} monitored by {@literal this}
   * {@link ChangeRecorder}.
   * @see java.util.Iterator
   */
  public @NotNull Iterator<String> iterator() {
    return Collections.unmodifiableSet(this.objectStateMap.keySet()).iterator();
  }

  /**
   * Clears the recorded, dirty state of the {@link Object}.
   *
   * @return {@literal true} if the dirty state of the {@link Object} was successfully cleared.
   */
  @SuppressWarnings("all")
  public boolean clear() {
    this.objectStateMap.clear();
    return this.objectStateMap.isEmpty();
  }
}
