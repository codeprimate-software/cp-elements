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
package org.cp.elements.beans;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.beans.VetoableChangeSupport;
import java.lang.reflect.Field;
import java.time.Instant;
import java.util.Map;
import java.util.TreeMap;

import org.cp.elements.beans.event.ChangeEvent;
import org.cp.elements.beans.event.ChangeListener;
import org.cp.elements.beans.event.ChangeSupport;
import org.cp.elements.beans.event.ChangeTracker;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.Visitor;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.support.AuditableSupport;
import org.cp.elements.util.ComparatorUtils;

/**
 * {@link AbstractBean} is an abstract base class for modeling {@literal Abstract Data Types (ADT)} and application data
 * as application domain model objects and entity types.
 *
 * @author John J. Blum
 * @param <ID> {@link Comparable} type for the identifier uniquely identifying {@literal this} {@link Bean}.
 * @param <USER> {@link Class type} used to model the object identifying the user for auditing information.
 * @param <PROCESS> {@link Class type} used to model the object identifying the process for auditing information.
 * @see java.beans.PropertyChangeEvent
 * @see java.beans.PropertyChangeListener
 * @see java.beans.PropertyChangeSupport
 * @see java.beans.VetoableChangeListener
 * @see java.beans.VetoableChangeSupport
 * @see java.lang.Comparable
 * @see java.lang.reflect.Field
 * @see java.time.Instant
 * @see org.cp.elements.beans.Bean
 * @see org.cp.elements.beans.event.ChangeEvent
 * @see org.cp.elements.beans.event.ChangeListener
 * @see org.cp.elements.beans.event.ChangeSupport
 * @see org.cp.elements.beans.event.ChangeTracker
 * @see org.cp.elements.lang.Visitor
 * @see org.cp.elements.lang.support.AuditableSupport
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractBean<ID extends Comparable<ID>, USER, PROCESS> extends AuditableSupport<USER, PROCESS, ID>
    implements Bean<ID, USER, PROCESS> {

  private static final boolean DEFAULT_EVENT_DISPATCH_ENABLED = true;

  private volatile boolean eventDispatchEnabled = DEFAULT_EVENT_DISPATCH_ENABLED;

  private final ChangeSupport changeSupport = new ChangeSupport(this);

  private final ChangeTracker changeTracker = new ChangeTracker();

  private final Map<String, String> propertyNameToFieldNameMapping = new TreeMap<>();
  private final Map<String, StateChangeCallback<Object>> propertyNameToStateChangeCallbackMapping = new TreeMap<>();

  private transient final PropertyChangeEvent propertyChangeEvent =
    new PropertyChangeEvent(this, null, null, null);

  private final PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);

  private final VetoableChangeSupport vetoableChangeSupport = new VetoableChangeSupport(this);

  /**
   * Constructs a new instance of {@literal this} {@link AbstractBean}.
   */
  public AbstractBean() {
    addPropertyChangeListener(this.changeTracker);
  }

  /**
   * Constructs a new instance of {@link AbstractBean} initialized with the specified typed identifier
   * uniquely identifying {@literal this} {@link Bean}.
   *
   * It is recommended that the identifier uniquely identify {@link Object Objects}
   * within {@literal this} {@link Bean} {@link Class}.
   *
   * @param id generically typed identifier uniquely identifying {@link Object Objects}
   * within {@literal this} {@link Bean} {@link Class}.
   * @see org.cp.elements.lang.Identifiable
   */
  public AbstractBean(@Nullable ID id) {
    this();
    identifiedBy(id);
  }

  /**
   * Sets the user responsible for creating this object.
   *
   * @param createdBy {@link Object} modeling the user responsible for creating this object.
   * @see org.cp.elements.lang.Auditable
   */
  public void setCreatedBy(USER createdBy) {
    processChange("createdBy", getCreatedBy(), createdBy);
  }

  /**
   * Sets the date and time when this object was created.
   *
   * @param createdOn {@link Instant} with the date and time when this object was created.
   * @see org.cp.elements.lang.Auditable
   * @see java.time.Instant
   */
  public void setCreatedOn(Instant createdOn) {
    processChange("createdOn", getCreatedOn(), createdOn);
  }

  /**
   * Sets the process (application) used by the user to create this object.
   *
   * @param createdWith {@link Object} modeling the process used by the user to create this object.
   * @see org.cp.elements.lang.Auditable
   */
  public void setCreatedWith(PROCESS createdWith) {
    processChange("createdWith", getCreatedWith(), createdWith);
  }

  /**
   * Determines whether event dispatching is enabled for property changes on {@literal this} {@link Bean}.
   *
   * @return a boolean value indicating whether event dispatching is enabled for property changes
   * on {@literal this} {@link Bean}.
   */
  public boolean isEventDispatchEnabled() {
    return this.eventDispatchEnabled;
  }

  /**
   * Enables or disables event dispatching for property changes on {@literal this} {@link Bean}.
   *
   * @param eventDispatchEnabled a boolean value indicating whether to enable or disable event dispatching
   * for property changes on {@literal this} {@link Bean}.
   */
  protected final void setEventDispatchEnabled(boolean eventDispatchEnabled) {
    this.eventDispatchEnabled = eventDispatchEnabled;
  }

  /**
   * Gets the {@link String name} of the {@link Field} mapped to the given {@literal property}.
   *
   * If no such mapping exists, then the given property name is returned.
   *
   * @param propertyName {@link String} value specifying the name of the property to retrieve
   * the corresponding {@link Field} name.
   * @return a {@link String} containing the {@link Field} name corresponding to the specified {@literal property}.
   */
  protected @NotNull String getFieldName(@NotNull String propertyName) {
    return ObjectUtils.returnFirstNonNullValue(this.propertyNameToFieldNameMapping.get(propertyName), propertyName);
  }

  /**
   * Sets the identifier uniquely identifying {@literal this} {@link Bean}.
   *
   * @param id a value of {@link Class type T} assigned as {@literal this} {@link Bean Bean's} unique identifier.
   * @see org.cp.elements.lang.Identifiable
   */
  public final void setId(ID id) {
    processChange("id", getId(), id);
  }

  /**
   * Determines whether {@literal this} {@link Bean} has been modified.
   *
   * One particular implementation suggests that if the last modified date and time does not match
   * the current modified date and time then {@literal this} {@link Bean} has been modified.
   *
   * @return a boolean value indicating whether {@literal this} {@link Bean} has been modified or not.
   * @see org.cp.elements.lang.Auditable
   */
  public boolean isModified() {
    return this.changeTracker.isModified();
  }

  /**
   * Determines whether the specified property of {@literal this} {@link Bean} has been modified.
   *
   * The property has been changed if the old value is not equal in value to the new value.
   *
   * @param propertyName {@link String} containing the name of the property to check for modification.
   * @return a boolean value indicating whether the specified property of {@literal this} {@link Bean},
   * identified by name, has been modified.
   */
  public boolean isModified(String propertyName) {
    return this.changeTracker.isModified(propertyName);
  }

  /**
   * Sets the user responsible for modifying this object.
   *
   * @param modifiedBy {@link Object} modeling the user who modified this object.
   * @see org.cp.elements.lang.Auditable
   */
  public void setModifiedBy(USER modifiedBy) {
    processChange("modifiedBy", getModifiedBy(), modifiedBy);
  }

  /**
   * Sets the date and time when this object was modified.
   *
   * @param modifiedOn {@link Instant} with the date and time when this object was modified.
   * @see org.cp.elements.lang.Auditable
   * @see java.time.Instant
   */
  public void setModifiedOn(Instant modifiedOn) {
    processChange("modifiedOn", getModifiedOn(), modifiedOn);
  }

  /**
   * Sets the process (application) used by the user to modify this object.
   *
   * @param modifiedWith {@link Object} modeling the process used by the user to modify this object.
   * @see org.cp.elements.lang.Auditable
   */
  public void setModifiedWith(PROCESS modifiedWith) {
    processChange("modifiedWith", getModifiedWith(), modifiedWith);
  }

  /**
   * Accepts a {@link Visitor} used to perform some operation or inspection on {@literal this} {@link Bean}.
   *
   * @param visitor {@link Visitor} used to {@literal visit} {@literal this} {@link Bean} to perform an opeation.
   * @see org.cp.elements.lang.Visitable
   * @see org.cp.elements.lang.Visitor
   */
  public void accept(@NotNull Visitor visitor) {
    visitor.visit(this);
  }

  /**
   * Adds the specified ChangeListener to the list of listeners to be notified of change events (or changes)
   * to this Bean.
   *
   * @param listener the ChangeListener to add to the list of listeners being notified of change events on
   * this Bean.
   * @see org.cp.elements.beans.event.ChangeListener
   * @see org.cp.elements.beans.event.ChangeSupport
   */
  protected void addChangeListener(@Nullable ChangeListener listener) {
    this.changeSupport.add(listener);
  }

  /**
   * Adds the specified PropertyChangeListener to listen for and be notified of all property change events on
   * this Bean.
   *
   * @param listener the PropertyChangeListener listening for all property change events on this Bean.
   * @see java.beans.PropertyChangeListener
   * @see java.beans.PropertyChangeSupport
   */
  protected void addPropertyChangeListener(@Nullable PropertyChangeListener listener) {
    this.propertyChangeSupport.addPropertyChangeListener(listener);
  }

  /**
   * Adds the specified PropertyChangeListener to listen for and receive notifications for changes to the specified
   * property of this Bean.
   *
   * @param propertyName a String value specifying the name of the property on this Bean for which the specified
   * listener will receive property change events.
   * @param listener the PropertyChangeListener listening for property changes events on the specified property.
   * @see java.beans.PropertyChangeListener
   * @see java.beans.PropertyChangeSupport
   */
  protected void addPropertyChangeListener(@Nullable String propertyName, @Nullable PropertyChangeListener listener) {
    this.propertyChangeSupport.addPropertyChangeListener(propertyName, listener);
  }

  /**
   * Adds the specified VetoableChangeListener to this Bean listening for property change events in order to monitor
   * and veto any undesired change to this Bean's state.
   *
   * @param listener the VetoableChangeListener listening for and potentially vetoing any undesired property change
   * events to this Bean.
   * @see java.beans.VetoableChangeListener
   * @see java.beans.VetoableChangeSupport
   */
  protected void addVetoableChangeListener(@Nullable VetoableChangeListener listener) {
    this.vetoableChangeSupport.addVetoableChangeListener(listener);
  }

  /**
   * Adds the specified VetoableChangeListener to this Bean listening for change events on the specified property
   * in order to monitor and veto any undesired changes.
   *
   * @param propertyName a String value specifying the name of the property on this Bean for wihch the specified
   * listener will receive property change events.
   * @param listener the VetoableChangeListener listening for and potentially vetoing any undesired property change
   * events to this Bean.
   * @see java.beans.VetoableChangeListener
   * @see java.beans.VetoableChangeSupport
   */
  protected void addVetoableChangeListener(@Nullable String propertyName, @Nullable VetoableChangeListener listener) {
    this.vetoableChangeSupport.addVetoableChangeListener(propertyName, listener);
  }

  /**
   * Changes the old {@link Object value} of the given property, referenced by {@link String name},
   * to the new {@link Object value}, effectively modifying the internal state of {@literal this} {@link Bean}.
   *
   * This method uses the Java Reflection API to set the {@link Field} for the corresponding {@literal property}.
   *
   * @param propertyName {@link String} containing the name of the property ({@link Field}) to set.
   * @param newValue {@link Object} containing the new value for the given property ({@link Field}).
   * @throws PropertyNotFoundException if property referenced by {@link String name} is not a property
   * of {@literal this} {@link Bean}. A {@link PropertyNotFoundException} is thrown when the {@link Field}
   * for the corresponding property does not exist.
   * @see #getFieldName(String)
   */
  void changeState(@NotNull String propertyName, @Nullable Object newValue) {

    try {
      ObjectUtils.setField(this, getFieldName(propertyName), newValue);
    }
    catch (IllegalArgumentException cause) {

      String message = String.format("The property [%1$s] for field [%2$s] was not found in this Bean [%3$s]!",
        propertyName, getFieldName(propertyName), getClass().getName());

      throw new PropertyNotFoundException(message, cause);
    }
  }

  /**
   * Notifies all {@link ChangeListener ChangeListeners} of a {@link ChangeEvent} happening to
   * {@literal this} {@link Bean}.
   *
   * @see org.cp.elements.beans.event.ChangeEvent
   * @see org.cp.elements.beans.event.ChangeListener
   * @see org.cp.elements.beans.event.ChangeSupport#fireChangeEvent()
   * @see org.cp.elements.beans.event.ChangeSupport#hasListeners()
   */
  protected void fireChange() {

    boolean triggerFireChange = isEventDispatchEnabled()
      && this.changeSupport.hasListeners()
      && isModified();

    if (triggerFireChange) {
      this.changeSupport.fireChangeEvent();
    }
  }

  /**
   * Notifies all {@link PropertyChangeListener PropertyChangeListeners} of a {@link PropertyChangeEvent} happening to
   * {@literal this} {@link Bean}.
   *
   * @param event {@link PropertyChangeEvent} capturing the property that is changing.
   * @throws IllegalArgumentException if the {@link PropertyChangeEvent} is {@literal null}.
   * @see java.beans.PropertyChangeSupport#firePropertyChange(java.beans.PropertyChangeEvent)
   * @see java.beans.PropertyChangeSupport#hasListeners(String)
   * @see java.beans.PropertyChangeListener
   * @see java.beans.PropertyChangeEvent
   */
  protected void firePropertyChange(@NotNull PropertyChangeEvent event) {

    if (isEventDispatchEnabled()) {
      Assert.notNull(event, "PropertyChangeEvent is required");
      if (this.propertyChangeSupport.hasListeners(event.getPropertyName())) {
        this.propertyChangeSupport.firePropertyChange(event);
      }
    }
  }

  /**
   * Notifies all {@link VetoableChangeListener VetoableChangeListeners} of a {@link PropertyChangeEvent} happening to
   * {@literal this} {@link Bean}.
   *
   * The change to the property may be vetoed by one of the {@link VetoableChangeListener VetoableChangeListeners}
   * listening to property changes occuring on {@literal this} {@link Bean}. If the property change is vetoed by
   * any listener, the veto prevents the value of the property from changing.
   *
   * @param event {@link PropertyChangeEvent} capturing the property that is changing.
   * @throws IllegalArgumentException if the {@link PropertyChangeEvent} is {@literal null}.
   * @throws PropertyVetoException if the property change is vetoed by a {@link VetoableChangeListener}
   * listening to property changes happening to {@literal this} {@link Bean}.
   * @see java.beans.VetoableChangeSupport#fireVetoableChange(java.beans.PropertyChangeEvent)
   * @see java.beans.VetoableChangeSupport#hasListeners(String)
   * @see java.beans.VetoableChangeListener
   * @see java.beans.PropertyChangeEvent
   */
  protected void fireVetoableChange(@NotNull PropertyChangeEvent event) throws PropertyVetoException {

    if (isEventDispatchEnabled()) {
      Assert.notNull(event, "PropertyChangeEvent is required");
      if (this.vetoableChangeSupport.hasListeners(event.getPropertyName())) {
        this.vetoableChangeSupport.fireVetoableChange(event);
      }
    }
  }

  /**
   * Maps the specified property with {@link String name} of {@literal this} {@link Bean}
   * to one of the {@link Bean Bean's} {@link Field Fields}.
   *
   * @param propertyName {@link String} containing the name of the property on {@link this} {@link Bean}.
   * @param fieldName {@link String} containing the name of the {@link Bean Bean's} {@link Field Fields}.
   * @return a boolean value indicating whether the mapping between the property and {@link Field}
   * was created successfully.
   * @throws IllegalArgumentException if the {@link String name} of the property or {@link Field}
   * is not specified.
   */
  protected boolean mapPropertyNameToFieldName(@NotNull String propertyName, @NotNull String fieldName) {

    Assert.hasText(propertyName, "Property name [%s] is required", propertyName);
    Assert.hasText(fieldName, "Field name [%s] is required", fieldName);

    this.propertyNameToFieldNameMapping.put(propertyName, fieldName);

    return this.propertyNameToFieldNameMapping.containsKey(propertyName);
  }

  /**
   * Maps the specified property with {@link String name} of {@literal this} {@link Bean}
   * to the given {@link StateChangeCallback} used to affect state changes for the specified property.
   *
   * If the {@link StateChangeCallback} is {@literal null}, then the mapping for the specified property
   * is removed if any such mapping exists.
   *
   * @param propertyName {@link String} containing the name of the property on {@link this} {@link Bean}.
   * @param callback {@link StateChangeCallback} used to affect state changes for the given property.
   * @return a boolean indicating the mapping was added successfully.
   * @throws IllegalArgumentException if the property is not specified.
   */
  protected boolean mapPropertyNameToStateChangeCallback(@NotNull String propertyName,
      @Nullable StateChangeCallback<Object> callback) {

    Assert.hasText(propertyName, "Property name [%s] is required", propertyName);

    if (callback != null) {
      this.propertyNameToStateChangeCallbackMapping.put(propertyName, callback);
    }
    else {
      this.propertyNameToStateChangeCallbackMapping.remove(propertyName);
    }

    return this.propertyNameToStateChangeCallbackMapping.containsKey(propertyName);
  }

  /**
   * Constructs a new instance of {@link PropertyChangeEvent} initialized with {@literal this} {@link Bean}
   * as the source of the event as well as the {@link String name} of the property that is changing
   * along with the property's {@link Object old value} and {@link Object new values}.
   *
   * A {@link PropertyChangeEvent} will only be created if event dispatching for registered listeners
   * is {@link #isEventDispatchEnabled() enabled} and there are either
   * {@link PropertyChangeListener PropertyChangeListeners} or {@link VetoableChangeListener VetoableChangeListeners}
   * registered with {@literal this} {@link Bean}.
   *
   * @param propertyName {@link String} containing the name of the property that is changing.
   * @param oldValue {@link Object} containing the old value for the specified property.
   * @param newValue {@link Object} containing the new value for the specified property.
   * @return a new {@link PropertyChangeEvent} for {@literal this} {@link Bean} specifying the name of the property
   * that is changing along with the property's {@link Object old value} and {@link Object new value}.
   * @see java.beans.PropertyChangeEvent
   */
  protected PropertyChangeEvent newPropertyChangeEvent(@NotNull String propertyName,
      @Nullable Object oldValue, @Nullable Object newValue) {

    return isEventDispatchingEnabledWithRegisteredListeners(propertyName)
      ? new PropertyChangeEvent(this, propertyName, oldValue, newValue)
      : this.propertyChangeEvent;
  }

  private boolean isEventDispatchingEnabledWithRegisteredListeners(@Nullable String propertyName) {

    return isEventDispatchEnabled()
      && isPropertySpecified(propertyName)
      && hasListeners(propertyName);
  }

  private boolean isPropertySpecified(@Nullable String propertyName) {
    return StringUtils.hasText(propertyName);
  }

  private boolean hasListeners(@NotNull String propertyName) {
    return this.propertyChangeSupport.hasListeners(propertyName)
      || this.vetoableChangeSupport.hasListeners(propertyName);
  }

  /**
   * Processes the change in state to the specified property of {@literal this} {@link Bean}.
   *
   * A {@link PropertyChangeEvent} is created to notify all listeners of the state change.
   *
   * First, all {@link VetoableChangeListener VetoableChangeListeners} are notified of the pending state change
   * in order to validate the change and veto any undesirable change to the specified property.
   *
   * Next, the state change is effected followed by notification of
   * all {@link PropertyChangeListener PropertyChangeListeners}.
   *
   * Finally, a {@link ChangeEvent} is sent to all {@link ChangeListener ChangeListeners} notifying the listeners
   * that {@literal this} {@link Bean} has been changed.
   *
   * This particular implementation employs a callback style and approach to effectively changing the state
   * of {@literal this} {@link Bean}. As it is not always possible to access the particular {@link Field Fields}
   * of an {@link Object Object's} class, the callback allows the subclass to effect the change, naturally.
   *
   * @param propertyName {@link String} containing the name of the property on {@literal this} {@link Bean}
   * to which the change will be applied.
   * @param oldValue {@link Object} containing the old value for the specified property.
   * @param newValue {@link Object} containing the new value for the specified property.
   * @throws IllegalPropertyValueException if the property change violates a constraint or invariant imposed by
   * one of the {@link VetoableChangeListener VetoableChangeListeners} listening to property change events
   * on {@literal this} {@link Bean}.
   * @see #processChange(String, Object, Object, org.cp.elements.beans.AbstractBean.StateChangeCallback)
   */
  protected void processChange(String propertyName, Object oldValue, Object newValue) {
    processChange(propertyName, oldValue, newValue, null);
  }

  /**
   * Processes the change in state to the specified property of {@literal this} {@link Bean}.
   *
   * A {@link PropertyChangeEvent} is created to notify all listeners of the state change.
   *
   * First, all {@link VetoableChangeListener VetoableChangeListeners} are notified of the pending state change
   * in order to validate the change and veto any undesirable change to the specified property.
   *
   * Next, the state change is effected followed by notification of
   * all {@link PropertyChangeListener PropertyChangeListeners}.
   *
   * Finally, a {@link ChangeEvent} is sent to all {@link ChangeListener ChangeListeners} notifying the listeners
   * that {@literal this} {@link Bean} has been changed.
   *
   * This particular implementation employs a callback style and approach to effectively changing the state
   * of {@literal this} {@link Bean}. As it is not always possible to access the particular {@link Field Fields}
   * of an {@link Object Object's} class, the callback allows the subclass to effect the change, naturally.
   *
   * @param <T> {@link Class type} of the {@link Object old value} and {@link Object new value}.
   * @param propertyName {@link String} containing the name of the property on {@literal this} {@link Bean}
   * to which the change will be applied.
   * @param oldValue {@link Object} containing the old value for the specified property.
   * @param newValue {@link Object} containing the new value for the specified property.
   * @param callback {@link StateChangeCallback} used to actually effect the state transition on the specified property
   * of {@literal this} {@link Bean}.
   * @throws IllegalPropertyValueException if the property change violates a constraint or invariant imposed by
   * one of the {@link VetoableChangeListener VetoableChangeListeners} listening to property change events
   * on {@literal this} {@link Bean}.
   * @see #processChange(String, Object, Object)
   */
  protected <T> void processChange(@NotNull String propertyName, @Nullable T oldValue, @Nullable T newValue,
      @Nullable StateChangeCallback<T> callback) {

    try {

      PropertyChangeEvent event = newPropertyChangeEvent(propertyName, oldValue, newValue);

      fireVetoableChange(event);

      if (callback != null) {
        callback.changeState(newValue);
      }
      else if (this.propertyNameToStateChangeCallbackMapping.containsKey(propertyName)) {
        this.propertyNameToStateChangeCallbackMapping.get(propertyName).changeState(newValue);
      }
      else {
        changeState(propertyName, newValue);
      }

      firePropertyChange(event);
      fireChange();
    }
    catch (PropertyVetoException cause) {

      String message = String.format("The new value [%1$s] for property [%2$s] of Bean [%3$s] is not valid",
        newValue, propertyName, getClass().getName());

      throw new IllegalPropertyValueException(message, cause);
    }
  }

  /**
   * Removes the specified ChangeListener from the list of listeners being notified of change events to this Bean.
   *
   * @param listener the ChangeListener to to remove from the list of listeners being notified of change events on
   * this Bean.
   * @see org.cp.elements.beans.event.ChangeListener
   * @see org.cp.elements.beans.event.ChangeSupport
   */
  protected void removeChangeListener(@Nullable ChangeListener listener) {
    this.changeSupport.remove(listener);
  }

  /**
   * Removes the specified PropertyChangeListener from listening and being notified of property change events on
   * this Bean.
   *
   * @param listener the PropertyChangeListener to remove and stop notifying of property change events on this Bean.
   * @see java.beans.PropertyChangeListener
   * @see java.beans.PropertyChangeSupport
   */
  protected void removePropertyChangeListener(@Nullable PropertyChangeListener listener) {
    this.propertyChangeSupport.removePropertyChangeListener(listener);
  }

  /**
   * Removes the specified PropertyChangeListener from listening and being notified of property change events
   * for the specified property on this Bean.
   *
   * @param propertyName a String value specifying the name of the property on this Bean.
   * @param listener the PropertyChangeListener to remove and stop notifying of property change events for
   * the specified property on this Bean.
   * @see java.beans.PropertyChangeListener
   * @see java.beans.PropertyChangeSupport
   */
  protected void removePropertyChangeListener(@Nullable String propertyName, @Nullable PropertyChangeListener listener) {
    this.propertyChangeSupport.removePropertyChangeListener(propertyName, listener);
  }

  /**
   * Removes the specified VetoableChangeListener from listening and being notified of property change events
   * to this Bean.  In addition, the listeners vote in vetoing potentially undesirable property changes is now
   * void.
   *
   * @param listener the VetoableChangeListener to remove and stop notifying of property change events on this Bean.
   * @see java.beans.VetoableChangeListener
   * @see java.beans.VetoableChangeSupport
   */
  protected void removeVetoableChangeListener(@Nullable VetoableChangeListener listener) {
    this.vetoableChangeSupport.removeVetoableChangeListener(listener);
  }

  /**
   * Removes the specified VetoableChangeListener from listening and being notified of property change events
   * from the specified property on this Bean.  In addition, the listeners vote in vetoing potentially undesirable
   * property changes to the specified property is now void.
   *
   * @param propertyName a String value specifying the name of the property on this Bean.
   * @param listener the VetoableChangeListener to remove and stop notifying of property change events on this Bean.
   * @see java.beans.VetoableChangeListener
   * @see java.beans.VetoableChangeSupport
   */
  protected void removeVetoableChangeListener(@Nullable String propertyName, @Nullable VetoableChangeListener listener) {
    this.vetoableChangeSupport.removeVetoableChangeListener(propertyName, listener);
  }

  /**
   * Removes the mapping from the specified property to the corresponding {@link Field} of {@literal this} {@link Bean}.
   *
   * @param propertyName {@link String} containing the name of the property of {@literal this} {@link Bean}
   * for which the mapping will be removed.
   * @return {@link String} containing the name of the {@link Bean Bean's} {@link Field} that was mapped to
   * the specified property.
   */
  protected String unmapPropertyNameToFieldName(@Nullable String propertyName) {
    return this.propertyNameToFieldNameMapping.remove(propertyName);
  }

  /**
   * Removes the mapping from the specified property of {@literal this} {@link Bean}
   * to the registered {@link StateChangeCallback} used to affect state changes to the specified property.
   *
   * @param propertyName {@link String} containing the name of the property of {@literal this} {@link Bean}.
   * @return a boolean indicating that whether the mapping was removed successfully.
   */
  protected boolean unmapPropertyNameToStateChangeCallback(@Nullable String propertyName) {
    return this.propertyNameToStateChangeCallbackMapping.remove(propertyName) != null;
  }

  /**
   * Compares the given {@link Bean} with {@literal this} {@link Bean} to determine sort order.
   *
   * The default implementation is to order by identifier, where {@literal null} identifiers
   * are ordered before {@literal non-null} identifiers.
   *
   * @param bean {@link Bean} to compare with {@literal this} {@link Bean}.
   * @return an integer value indicating the relative sort order of the given {@link Bean}
   * with {@literal this} {@link Bean}. A negative value implies {@literal this} {@link Bean}
   * logically comes before the given {@link Bean}. A zero value indicates the given {@link Bean}
   * and {@literal this} {@link Bean} are comparatively equal. And, a positive value indicates
   * {@literal this} {@link Bean} logically comes after the given {@link Bean}.
   * @throws ClassCastException if the given {@link Bean} is not an instance of
   * {@literal this} {@link Bean} {@link Class type}.
   * @see java.lang.Comparable#compareTo(Object)
   */
  @SuppressWarnings("all")
  public int compareTo(@NotNull Bean<ID, USER, PROCESS> bean) {

    String message = String.format("The Bean to compare with this Bean must be an instance of [%s]!",
      getClass().getName());

    Assert.isInstanceOf(bean, getClass(), new ClassCastException(message));

    return ComparatorUtils.compareIgnoreNull(getId(), bean.getId());
  }

  /**
   * Determines whether {@literal this} {@link Bean} and the given {@link Bean} are equal.
   *
   * {@literal This} {@link Bean} and the given {@link Bean} are considered equal if their {@link #getId() identifiers}
   * match. A {@literal null} {@link #getId() identifier} is not logically equal to any other {@link Bean}, even other
   * {@link Bean Beans} with a {@literal null} {@link #getId() identifier} regardless of the {@link Bean Bean's}
   * other state.
   *
   * @param obj {@link Bean} to compare for equality with {@literal this} {@link Bean}.
   * @return a boolean value indicating whether {@literal this} {@link Bean} and the given {@link Bean}
   * are logically equal.
   * @see org.cp.elements.lang.ObjectUtils#equals(Object, Object)
   * @see java.lang.Object#equals(Object)
   */
  @Override
  @SuppressWarnings("unchecked")
  public boolean equals(@Nullable Object obj) {

    if (this == obj) {
      return true;
    }

    if (!(obj instanceof Bean)) {
      return false;
    }

    Bean<ID, USER, PROCESS> that = (Bean<ID, USER, PROCESS>) obj;

    return ObjectUtils.equals(this.getId(), that.getId());
  }

  /**
   * Computes the {@link Integer#TYPE hash code value} for {@literal this} {@link Bean}
   * as determined by {@literal this} {@link Bean Bean's} internal state.
   *
   * @return an integer value representing the computed hash code of {@literal this} {@link Bean}.
   * @see org.cp.elements.lang.ObjectUtils#hashCode(Object)
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    int hashValue = 17;
    hashValue = 37 * hashValue + ObjectUtils.hashCode(getId());
    return hashValue;
  }

  /**
   * The {@link StateChangeCallback} interface defines a callback enabling the invoked accessor (setter) method
   * for the property of {@literal this} {@link Bean} to change state.
   *
   * @param <T> {@link Class type} of the {@link Object new value}.
   */
  @FunctionalInterface
  protected interface StateChangeCallback<T> {
    void changeState(T newValue);
  }
}
