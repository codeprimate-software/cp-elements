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

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Support class used to register and unregister {@link ChangeListener ChangeListeners} interested in state changes to
 * the {@literal source} {@link Object} managed and monitored by {@literal this} {@link ChangeSupport} object.
 *
 * @author John J. Blum
 * @see java.lang.Iterable
 * @see org.cp.elements.beans.event.ChangeEvent
 * @see org.cp.elements.beans.event.ChangeListener
 * @since 1.0.0
 */
public class ChangeSupport implements Iterable<ChangeListener> {

  // List of registered ChangeListeners listening for change events on the source Object.
  private final transient List<ChangeListener> changeListeners = new CopyOnWriteArrayList<>();

  // Reference to the source Object of the change events.
  private final Object source;

  /**
   * Constructs a new {@link ChangeSupport} initialized with the given, required {@link Object}
   * used as the source of the {@link ChangeEvent change events}.
   *
   * @param source {@link Object} used as the source of the {@link ChangeEvent change events};
   * must not be {@literal null}.
   * @throws IllegalArgumentException if {@link Object source} is {@literal null}.
   */
  public ChangeSupport(@NotNull Object source) {
    this.source = ObjectUtils.requireObject(source, "A source object is required");
  }

  /**
   * Returns the {@link List} of {@link ChangeListener} objects currently registered and managed by {@literal this}
   * {@link ChangeSupport} object.
   *
   * @return the {@link List} of {@link ChangeListener} objects currently registered and managed by {@literal this}
   * {@link ChangeSupport} object.
   * @see org.cp.elements.beans.event.ChangeListener
   * @see java.util.List
   */
  protected @NotNull List<ChangeListener> getChangeListeners() {
    return this.changeListeners;
  }

  /**
   * Gets a reference to the {@link Object} used as the source of the {@link ChangeEvent change events} fired by
   * {@literal this} {@link ChangeSupport} object.
   *
   * @return an {@link Object} reference to the source of the {@link ChangeEvent change events}; never {@literal null}.
   * @see java.lang.Object
   */
  protected @NotNull Object getSource() {
    return this.source;
  }

  /**
   * Determines whether the given {@link ChangeListener} is registered with
   * {@literal this} {@link ChangeSupport} object.
   *
   * @param listener {@link ChangeListener} to evaluate.
   * @return {@literal true} if the given {@link ChangeListener} is not {@literal null} and has been registered with
   * {@literal this} {@link ChangeSupport} object.
   * @see org.cp.elements.beans.event.ChangeListener
   * @see #unregister(ChangeListener)
   * @see #register(ChangeListener)
   * @see #getChangeListeners()
   */
  public boolean contains(@Nullable ChangeListener listener) {
    return listener != null && getChangeListeners().contains(listener);
  }

  /**
   * Computes the number of {@link ChangeListener ChangeListeners} registered with {@literal this}
   * {@link ChangeSupport} object.
   *
   * @return the number of {@link ChangeListener ChangeListeners} registered with {@literal this}
   * {@link ChangeSupport} object.
   * @see #getChangeListeners()
   * @see #hasListeners()
   */
  public int count() {
    return getChangeListeners().size();
  }

  /**
   * Fires a {@link ChangeEvent} notifying each registered {@link ChangeListener} of the change in state to
   * the {@link #getSource() source} {@link Object}.
   *
   * @see org.cp.elements.beans.event.ChangeListener
   * @see org.cp.elements.beans.event.ChangeEvent
   * @see #newChangeEvent(Object)
   * @see #getSource()
   * @see #iterator()
   */
  public void fireChangeEvent() {

    if (hasListeners()) {

      ChangeEvent event = newChangeEvent(getSource());

      this.forEach(changeListener -> changeListener.stateChanged(event));
    }
  }

  /**
   * Constructs a new {@link ChangeEvent} initialized with the given {@link #getSource() source}
   * {@link Object} for which {@link ChangeEvent change event} occurred.
   *
   * @param source reference to the {@literal source} {@link Object} for the {@link ChangeEvent}.
   * @return a new {@link ChangeEvent} object wrapping the given {@link Object} as the {@literal source}
   * of the {@link ChangeEvent}.
   * @see org.cp.elements.beans.event.ChangeEvent
   * @see java.lang.Object
   */
  protected @NotNull ChangeEvent newChangeEvent(@NotNull Object source) {
    return new ChangeEvent(source);
  }

  /**
   * Determines whether {@literal this} {@link ChangeSupport} object has any registered
   * {@link ChangeListener ChangeListeners}.
   *
   * @return {@literal true} if {@literal this} {@link ChangeSupport} object has any registered
   * {@link ChangeListener ChangeListeners}.
   * @see #count()
   */
  public boolean hasListeners() {
    return count() > 0;
  }

  /**
   * Iterates over the {@link ChangeListener ChangeListeners} registered with {@literal this}
   * {@link ChangeSupport} object.
   *
   * @return an {@link Iterator} over the {@link ChangeListener ChangeListeners} registered with
   * {@literal this} {@link ChangeSupport} object.
   * @see org.cp.elements.beans.event.ChangeListener
   * @see #getChangeListeners()
   * @see java.util.Iterator
   */
  public @NotNull Iterator<ChangeListener> iterator() {
    return Collections.unmodifiableList(getChangeListeners()).iterator();
  }

  /**
   * Registers the given {@link ChangeListener} to the {@link List} of listeners managed by {@literal this}
   * {@link ChangeSupport} object.
   *
   * The registered {@link ChangeListener} will be notified of all {@link ChangeEvent change events} occurring on
   * the {@link #getSource() source} {@link Object}.
   *
   * @param listener {@link ChangeListener} to register; must not be {@literal null}.
   * @return {@literal this} {@link ChangeSupport} object.
   * @see #unregister(ChangeListener)
   * @see #contains(ChangeListener)
   * @see #getChangeListeners()
   */
  @SuppressWarnings("all")
  public @NotNull ChangeSupport register(@NotNull ChangeListener listener) {

    if (listener != null) {
      getChangeListeners().add(listener);
    }

    return this;
  }

  /**
   * Unregisters the given {@link ChangeListener} from {@literal this} {@link ChangeSupport} object.
   *
   * The unregistered {@link ChangeListener} will no longer be notified of any {@link ChangeEvent change events}
   * occurring on the {@link #getSource() source} {@link Object}.
   *
   * @param listener {@link ChangeListener} to unregister; must not be {@literal null}.
   * @return {@literal this} {@link ChangeSupport} object.
   * @see #register(ChangeListener)
   * @see #contains(ChangeListener)
   * @see #getChangeListeners()
   */
  public @NotNull ChangeSupport unregister(@NotNull ChangeListener listener) {

    if (listener != null) {
      getChangeListeners().remove(listener);
    }

    return this;
  }
}
