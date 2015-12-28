/*
 * Copyright 2016 Author or Authors.
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

import org.cp.elements.lang.ObjectUtils;

/**
 * The ChangeTracker class is a ChangeListener class used by org.cp.elements.beans.Bean implementations
 * for recording and tracking changes in state to the properties of the "observed" Bean.
 *
 * @author John J. Blum
 * @see java.beans.PropertyChangeEvent
 * @see java.beans.PropertyChangeListener
 * @see java.lang.Iterable
 * @see java.util.Iterator
 * @since 1.0.0
 */
public class ChangeTracker implements Iterable<String>, PropertyChangeListener {

  // The object state Map is a mapping between the property name and property value's hash code.
  private final Map<String, Integer> objectStateMap = new TreeMap<String, Integer>();

  /**
   * Determines whether the Bean tracked by this ChangeTracker has any modified properties.
   *
   * @return a boolean value indicating whether the Bean tracked by this ChangeTracker has any modified properties.
   * @see #isModified(String)
   */
  public boolean isModified() {
    return !objectStateMap.isEmpty();
  }

  /**
   * Determines whether the specified property on the Bean tracked by this ChangeTracker has been modified.
   *
   * @param propertyName a String specifying the name of the property on the Bean.
   * @return a boolean value indicating whether the specified property on the Bean tracked by this ChangeTracker
   * has been modified.
   * @see #isModified()
   */
  public boolean isModified(final String propertyName) {
    return objectStateMap.containsKey(propertyName);
  }

  /**
   * The event handler method that gets fired when a property of the Bean tracked by this ChangeTracker has been
   * modified.  The PropertyChangeEvent encapsulates all the information pertaining to the property change
   * including the name of the property, it's old and new value and the source Bean of the targeted change.
   *
   * @param event the PropertyChangeEvent encapsulating information about the property change.
   * @see java.beans.PropertyChangeEvent
   */
  public void propertyChange(final PropertyChangeEvent event) {
    if (objectStateMap.containsKey(event.getPropertyName())) {
      if (objectStateMap.get(event.getPropertyName()) == ObjectUtils.hashCode(event.getNewValue())) {
        // NOTE the state of the property identified by the event has been reverted to it's original, persisted value.
        objectStateMap.remove(event.getPropertyName());
      }
    }
    else {
      if (!ObjectUtils.equalsIgnoreNull(event.getOldValue(), event.getNewValue())) {
        objectStateMap.put(event.getPropertyName(), ObjectUtils.hashCode(event.getOldValue()));
      }
    }
  }

  /**
   * Iterates over the set of properties on the Bean tracked by this ChangeTracker that have been modified.
   *
   * @return a Iterator over the modified properties on the Bean tracked by this ChangeTracker.
   * @see java.util.Iterator
   */
  public Iterator<String> iterator() {
    return Collections.unmodifiableSet(objectStateMap.keySet()).iterator();
  }

}
