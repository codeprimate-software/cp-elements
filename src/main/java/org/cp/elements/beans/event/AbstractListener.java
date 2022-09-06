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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EventListener;
import java.util.Iterator;
import java.util.List;

import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionUtils;

/**
 * Abstract base class and {@link EventListener} implementation used to listen for JavaBean, {@literal POJO}
 * {@link PropertyChangeEvent property change events}.
 *
 * @author John Blum
 * @see java.beans.PropertyChangeEvent
 * @see java.lang.Iterable
 * @see java.util.EventListener
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractListener implements EventListener, Iterable<String> {

  private final List<String> propertyNames;

  /**
   * Constructs a new instance of {@link AbstractListener} initialized with the array of {@link String property names}.
   *
   * @param propertyNames array of {@link String property names} for which this listener is interested and processes
   * {@link PropertyChangeEvent property change events}.
   * @see #AbstractListener(Iterable)
   */
  public AbstractListener(String... propertyNames) {
    this(Arrays.asList(ArrayUtils.nullSafeArray(propertyNames, String.class)));
  }

  /**
   * Constructs a new instance of {@link AbstractListener} initialized with the {@link Iterable}
   * of {@link String property names}.
   *
   * @param propertyNames {@link Iterable} of {@link String property names} for which this listener
   * is interested and processes {@link PropertyChangeEvent property change events}.
   * @see java.lang.Iterable
   */
  public AbstractListener(Iterable<String> propertyNames) {
    this.propertyNames = Collections.unmodifiableList(CollectionUtils.addAll(new ArrayList<>(),
      CollectionUtils.nullSafeIterable(propertyNames)));
  }

  /**
   * Gets the {@link List} of {@link String property names} processed (handled) by this listener.
   *
   * @return the {@link List} of {@link String property names} processed (handled) by this listener.
   * @see java.util.List
   */
  protected List<String> getPropertyNames() {
    return this.propertyNames;
  }

  /**
   * Iterates over the {@link String property names} of this listener.
   *
   * @return an {@link Iterator} over the {@link String property names} of this listener.
   * @see #getPropertyNames()
   * @see java.util.Iterator
   */
  @Override
  public Iterator<String> iterator() {
    return getPropertyNames().iterator();
  }

  /**
   * Processes (handles) the {@link PropertyChangeEvent} if this listener is interested in the property
   * that is the source of the given {@link PropertyChangeEvent}.
   *
   * @param event {@link PropertyChangeEvent} to process.
   * @see #canHandle(PropertyChangeEvent)
   * @see java.beans.PropertyChangeEvent
   * @see #doHandle(PropertyChangeEvent)
   */
  @NullSafe
  protected void handle(@NotNull PropertyChangeEvent event) {

    if (canHandle(event)) {
      doHandle(event);
    }
  }

  /**
   * Determines whether this listener is interested in the JavaBean/POJO property
   * that is the source of the given {@link PropertyChangeEvent}.
   *
   * @param event {@link PropertyChangeEvent} to evaluate.
   * @return a boolean value to determine whether this listener is interested in the JavaBean/POJO property
   * that is the source of the given {@link PropertyChangeEvent}.
   * @see #canHandle(String)
   * @see java.beans.PropertyChangeEvent
   */
  @NullSafe
  protected boolean canHandle(@Nullable PropertyChangeEvent event) {
    return event != null && canHandle(event.getPropertyName());
  }

  /**
   * Determines whether this listener is interested in the {@link String named} JavaBean/POJO property.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property.
   * @return a boolean value indicating whether this listener is interested in the {@link String named}
   * JavaBean/POJO property.
   * @see #getPropertyNames()
   */
  @NullSafe
  protected boolean canHandle(@Nullable String propertyName) {

    List<String> propertyNames = getPropertyNames();

    return StringUtils.hasText(propertyName)
      && (propertyNames.isEmpty() || propertyNames.contains(propertyName));
  }

  /**
   * Processes (handles) the given {@link PropertyChangeEvent}.
   *
   * @param event {@link PropertyChangeEvent} to process; never {@literal null}.
   * @see java.beans.PropertyChangeEvent
   */
  protected abstract void doHandle(@NotNull PropertyChangeEvent event);

}
