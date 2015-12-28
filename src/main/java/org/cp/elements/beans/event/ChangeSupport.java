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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.cp.elements.lang.Assert;

/**
 * The ChangeSupport class is a support class used to register and unregister ChangeListeners and notify listeners
 * of state change events happening on the source Object associated with this change support class.
 *
 * @author John J. Blum
 * @see java.io.Serializable
 * @see java.lang.Iterable
 * @see java.util.Iterator
 * @see org.cp.elements.beans.AbstractBean
 * @see org.cp.elements.beans.event.ChangeEvent
 * @see org.cp.elements.beans.event.ChangeListener
 * @since 1.0.0
 */
public class ChangeSupport implements Iterable<ChangeListener>, Serializable {

  // collection of registered ChangeListeners listening for change events on the source Object
  private transient final List<ChangeListener> changeListeners = new ArrayList<ChangeListener>();

  // an Object reference to the source of the change events
  private final Object source;

  /**
   * Creates an instance of the ChangeSupport class initialized with the specified Object as the source
   * of change events.
   *
   * @param source the Object that is the source of the change events sent to listeners as notifications by
   * this support class.
   * @throws NullPointerException if the source Object reference is null.
   */
  public ChangeSupport(final Object source) {
    Assert.notNull(source, "The source of the change events cannot be null!");
    this.source = source;
  }

  /**
   * Gets a reference to the Object that is the source of change events fired by this change support class.
   *
   * @return an Object reference to the source of the change events.
   */
  protected Object getSource() {
    return source;
  }

  /**
   * Adds the specified ChangeListener to the collection of listeners managed by this support class that get notified
   * of change events occurring for the given source Object.
   *
   * @param listener the ChangeListener to add to the collection of listeners managed by this support class.
   * @return a boolean indicating if the ChangeListener was successfully added to the collection of listeners.
   * Null ChangeListeners cannot be added and so this method will return false if the ChangeListener object reference
   * is null.
   * @see #remove(ChangeListener)
   */
  public boolean add(final ChangeListener listener) {
    return (listener != null && changeListeners.add(listener));
  }

  /**
   * Determines whether the support class contains the specified ChangeListener.
   *
   * @param listener the ChangeListener parameter being tested for registration with this support class.
   * @return a boolean value indication whether the ChangeListener has been registered with this support class.
   */
  public boolean contains(final ChangeListener listener) {
    return changeListeners.contains(listener);
  }

  /**
   * Creates an instance of the ChangeEvent class initialized with the specified source Object for which
   * change events occur.
   *
   * @param source a reference to the source Object for the change events.
   * @return a ChangeEvent object wrapping the specified Object parameter as the source of the change events.
   * @see org.cp.elements.beans.event.ChangeEvent
   */
  protected ChangeEvent createChangeEvent(final Object source) {
    return new ChangeEvent(source);
  }

  /**
   * Fires a ChangeEvent for the source Object notifying each registered ChangeListener of the change.
   *
   * @see org.cp.elements.beans.event.ChangeEvent
   * @see org.cp.elements.beans.event.ChangeListener
   * @see #iterator()
   */
  public void fireChangeEvent() {
    final ChangeEvent event = createChangeEvent(getSource());

    for (final ChangeListener listener : this) {
      listener.stateChanged(event);
    }
  }

  /**
   * Determines whether this support class has any registered ChangeListeners.
   *
   * @return a boolean value indicating whether this support class has any registered ChangeListeners.
   */
  public boolean hasListeners() {
    return !changeListeners.isEmpty();
  }

  /**
   * Iterates over the ChangeListeners registered on this support class for the specified event source Object.
   *
   * @return an Iterator over the ChangeListeners registered with this support class instance for the source Object.
   * @see java.util.Iterator
   */
  public Iterator<ChangeListener> iterator() {
    return Collections.unmodifiableList(changeListeners).iterator();
  }

  /**
   * Removes the specified ChangeListener from the collection of listeners on this support class that are notified
   * of change events occurring for the source Object.
   *
   * @param listener the ChangeListener to remove from this support class.
   * @return a boolean indicating if the ChangeListener was successfully removed from the collection of listeners.
   * @see #add(ChangeListener)
   */
  public boolean remove(final ChangeListener listener) {
    return changeListeners.remove(listener);
  }

  /**
   * Determines the number of registered ChangeListeners registered on this support class.
   *
   * @return an integer value indicating the number of ChangeListener registered on this support class.
   */
  public int size() {
    return changeListeners.size();
  }

}
