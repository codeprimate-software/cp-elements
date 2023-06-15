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
import java.util.concurrent.atomic.AtomicReference;

import org.cp.elements.beans.event.ChangeEvent;
import org.cp.elements.beans.event.ChangeListener;
import org.cp.elements.beans.event.ChangeRecorder;
import org.cp.elements.beans.event.ChangeSupport;
import org.cp.elements.beans.event.support.RequiredPropertyVetoableChangeListener;
import org.cp.elements.beans.model.BeanAdapter;
import org.cp.elements.beans.model.BeanUtils;
import org.cp.elements.function.BiFeederFunction;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.Visitor;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.reflect.FieldNotFoundException;
import org.cp.elements.lang.support.AuditableSupport;
import org.cp.elements.util.ComparatorUtils;

/**
 * Abstract base class used to model {@literal Abstract Data Types (ADT)} and data as application domain model objects
 * and entity types.
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
 * @see org.cp.elements.beans.event.ChangeRecorder
 * @see org.cp.elements.beans.event.ChangeSupport
 * @see org.cp.elements.beans.model.BeanAdapter
 * @see org.cp.elements.lang.Visitor
 * @see org.cp.elements.lang.support.AuditableSupport
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractBean<ID extends Comparable<ID>, USER, PROCESS> extends AuditableSupport<USER, PROCESS, ID>
    implements Bean<ID, USER, PROCESS> {

  public static final boolean EVENT_DISPATCH_DISABLED = false;
  public static final boolean EVENT_DISPATCH_ENABLED = true;

  private static final boolean DEFAULT_EVENT_DISPATCH_ENABLED = EVENT_DISPATCH_ENABLED;

  private volatile boolean eventDispatchEnabled = DEFAULT_EVENT_DISPATCH_ENABLED;

  private final AtomicReference<BeanAdapter> beanAdapterReference = new AtomicReference<>(null);

  private final ChangeRecorder changeRecorder = new ChangeRecorder();

  private final ChangeSupport changeSupport = new ChangeSupport(this);

  private final Map<String, String> propertyNameToFieldNameMapping = new TreeMap<>();
  private final Map<String, StateChangeCallback<Object>> propertyNameToStateChangeCallbackMapping = new TreeMap<>();

  private final PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);

  private final VetoableChangeSupport vetoableChangeSupport = new VetoableChangeSupport(this);

  /**
   * Constructs a new {@link AbstractBean}.
   * <p>
   * This default, no-arg constructor simply registers a {@link ChangeRecorder} {@link PropertyChangeListener}
   * to record and keep track of changes in state to {@literal this} {@link AbstractBean}.
   *
   * @see org.cp.elements.beans.event.ChangeRecorder
   * @see #register(PropertyChangeListener)
   */
  public AbstractBean() {
    register(this.changeRecorder);
    register(RequiredPropertyVetoableChangeListener.INSTANCE);
  }

  /**
   * Constructs a new instance of {@link AbstractBean} initialized with the given typed {@link ID identifier}
   * to uniquely identify {@literal this} {@link Bean}.
   * <p>
   * It is recommended that the {@link ID identifier} uniquely identify {@link Object Objects}
   * within {@literal this} {@link Bean} {@link Class type}.
   *
   * @param id {@link ID generically typed identifier} uniquely identifying {@link Object Objects}
   * within {@literal this} {@link Bean} {@link Class type}.
   * @see org.cp.elements.lang.Identifiable
   * @see #identifiedBy(Comparable)
   * @see #AbstractBean()
   */
  public AbstractBean(@Nullable ID id) {
    this();
    identifiedBy(id);
    register(RequiredPropertyVetoableChangeListener.INSTANCE);
  }

  /**
   * Gets a {@link BeanAdapter} adapting this {@link Object POJO} as a {@literal JavaBean}.
   *
   * @return a {@link BeanAdapter} adapting this {@link Object POJO} as a {@literal JavaBean}.
   * @see org.cp.elements.beans.model.BeanAdapter
   */
  public @NotNull BeanAdapter getAdapter() {
    return this.beanAdapterReference.updateAndGet(beanAdapter ->
      beanAdapter != null ? beanAdapter : new BeanAdapter(this));
  }

  /**
   * Sets the {@link USER} responsible for creating this object.
   *
   * @param createdBy {@link Object} modeling the {@literal user} responsible for creating this object.
   * @see org.cp.elements.lang.Auditable
   */
  public void setCreatedBy(USER createdBy) {
    processChange("createdBy", getCreatedBy(), createdBy, super::setCreatedBy);
  }

  /**
   * Sets the {@link Instant date and time} when this object was created.
   *
   * @param createdOn {@link Instant} with the {@literal date and time} when this object was created.
   * @see org.cp.elements.lang.Auditable
   * @see java.time.Instant
   */
  public void setCreatedOn(Instant createdOn) {
    processChange("createdOn", getCreatedOn(), createdOn, super::setCreatedOn);
  }

  /**
   * Sets the {@link PROCESS} (application) used by the user to create this object.
   *
   * @param createdWith {@link Object} modeling the {@literal process} used by the user to create this object.
   * @see org.cp.elements.lang.Auditable
   */
  public void setCreatedWith(PROCESS createdWith) {
    processChange("createdWith", getCreatedWith(), createdWith, super::setCreatedWith);
  }

  /**
   * Determines whether event dispatching is enabled for property changes on {@literal this} {@link Bean}.
   *
   * @return a boolean value indicating whether event dispatching is enabled for property changes
   * on {@literal this} {@link Bean}.
   * @see #setEventDispatchEnabled(boolean)
   */
  public boolean isEventDispatchEnabled() {
    return this.eventDispatchEnabled;
  }

  /**
   * Enables or disables event dispatching for property changes on {@literal this} {@link Bean}.
   *
   * @param eventDispatchEnabled a boolean value indicating whether to enable or disable event dispatching
   * for property changes on {@literal this} {@link Bean}.
   * @see #isEventDispatchEnabled()
   */
  protected final void setEventDispatchEnabled(boolean eventDispatchEnabled) {
    this.eventDispatchEnabled = eventDispatchEnabled;
  }

  /**
   * Gets the {@link String name} of the {@link Field} mapped to the given {@literal property}
   * of {@literal this} {@link Bean}.
   *
   * If no such mapping exists, then the given {@link String named} property is returned.
   *
   * @param propertyName {@link String} value containing the {@literal name} of the property
   * from {@literal this} {@link Bean} for which to return the corresponding {@link Field} {@link String name}.
   * @return a {@link String} containing the {@link Field} name corresponding to the {@link String named}
   * {@literal property} of {@literal this} {@link Bean}.
   */
  protected @NotNull String getFieldName(@NotNull String propertyName) {
    return ObjectUtils.returnFirstNonNullValue(this.propertyNameToFieldNameMapping.get(propertyName), propertyName);
  }

  /**
   * Sets the {@link ID identifier} uniquely identifying {@literal this} {@link Bean}.
   *
   * @param id a value of {@link Class type ID} assigned as {@literal this} {@link Bean} unique identifier.
   * @see org.cp.elements.lang.Identifiable
   */
  public final void setId(ID id) {
    processChange("id", getId(), id, super::setId);
  }

  /**
   * Determines whether {@literal this} {@link Bean} has been modified.
   *
   * One implementation suggests that if the {@link Instant last modified date and time} does not match
   * the {@link Instant current modified date and time} then {@literal this} {@link Bean} has been modified.
   *
   * @return a boolean value indicating whether {@literal this} {@link Bean} has been modified or not.
   * @see org.cp.elements.lang.Auditable
   */
  public boolean isModified() {
    return this.changeRecorder.isModified();
  }

  /**
   * Determines whether the {@link String named} property of {@literal this} {@link Bean} has been modified.
   *
   * The property has been changed if the old value is not equal in value to the new value.
   *
   * @param propertyName {@link String} containing the name of the property to check for modification.
   * @return a boolean value indicating whether the {@link String named} property of {@literal this} {@link Bean}
   * has been modified.
   */
  public boolean isModified(String propertyName) {
    return this.changeRecorder.isModified(propertyName);
  }

  /**
   * Sets the {@link USER} responsible for modifying this object.
   *
   * @param modifiedBy {@link Object} modeling the {@literal user} who modified this object.
   * @see org.cp.elements.lang.Auditable
   */
  public void setModifiedBy(USER modifiedBy) {
    processChange("modifiedBy", getModifiedBy(), modifiedBy, super::setModifiedBy);
  }

  /**
   * Sets the {@link Instant date and time} when this object was modified.
   *
   * @param modifiedOn {@link Instant} with the {@literal date and time} when this object was modified.
   * @see org.cp.elements.lang.Auditable
   * @see java.time.Instant
   */
  public void setModifiedOn(Instant modifiedOn) {
    processChange("modifiedOn", getModifiedOn(), modifiedOn, super::setModifiedOn);
  }

  /**
   * Sets the {@link PROCESS} (application) used by the user to modify this object.
   *
   * @param modifiedWith {@link Object} modeling the {@literal process} used by the user to modify this object.
   * @see org.cp.elements.lang.Auditable
   */
  public void setModifiedWith(PROCESS modifiedWith) {
    processChange("modifiedWith", getModifiedWith(), modifiedWith, super::setModifiedWith);
  }

  /**
   * Accepts a {@link Visitor} used to perform some operation or inspection on {@literal this} {@link Bean}.
   *
   * @param visitor {@link Visitor} used to {@literal visit} {@literal this} {@link Bean} to perform an operation.
   * @see org.cp.elements.lang.Visitable
   * @see org.cp.elements.lang.Visitor
   */
  public void accept(@NotNull Visitor visitor) {
    visitor.visit(this);
  }

  /**
   * Changes the {@link Object old value} of the given property, referenced by {@link String name},
   * to the {@link Object new value}, effectively modifying the internal state of {@literal this} {@link Bean}.
   *
   * This method uses the Java Reflection API to set the {@link Field} for the corresponding {@literal property}.
   *
   * @param propertyName {@link String} containing the name of the property ({@link Field}) to set.
   * @param newValue {@link Object} containing the new value for the given property ({@link Field}).
   * @throws FieldNotFoundException if property referenced by {@link String name} has no {@link Field}
   * on {@literal this} {@link Bean}.
   * @see #getFieldName(String)
   */
  void changeState(@NotNull String propertyName, @Nullable Object newValue) {

    try {
      ObjectUtils.setField(this, getFieldName(propertyName), newValue);
    }
    catch (IllegalArgumentException cause) {

      String message = String.format("No field [%1$s] for property [%2$s] was found on this Bean [%3$s]",
        getFieldName(propertyName), propertyName, getClass().getName());

      throw new FieldNotFoundException(message, cause);
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
  protected void fireChangeEvent() {

    boolean fireChangeEvent = isEventDispatchEnabled()
      && this.changeSupport.hasListeners()
      && isModified();

    if (fireChangeEvent) {
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
  protected void firePropertyChangeEvent(@NotNull PropertyChangeEvent event) {

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
  protected void fireVetoableChangeEvent(@NotNull PropertyChangeEvent event) throws PropertyVetoException {

    if (isEventDispatchEnabled()) {

      Assert.notNull(event, "PropertyChangeEvent is required");

      if (this.vetoableChangeSupport.hasListeners(event.getPropertyName())) {
        this.vetoableChangeSupport.fireVetoableChange(event);
      }
    }
  }

  /**
   * Maps the given, required {@link String named} property of {@literal this} {@link Bean}
   * to one of the {@link Bean} {@link Field Fields} by {@link String name}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property on {@literal this} {@link Bean}.
   * @param fieldName {@link String} containing the {@literal name} of a {@link Field} on {@literal this} {@link Bean}.
   * @return the previously mapped {@link String named} {@link Field} of this {@link Bean}.
   * @throws IllegalArgumentException if the {@link String name} of the property or {@link Field} is not specified.
   */
  protected String mapPropertyNameToFieldName(@NotNull String propertyName, @NotNull String fieldName) {

    Assert.hasText(propertyName, "Property name [%s] is required", propertyName);
    Assert.hasText(fieldName, "Field name [%s] is required", fieldName);

    return this.propertyNameToFieldNameMapping.put(propertyName, fieldName);
  }

  /**
   * Maps the given, required, {@link String named} property of {@literal this} {@link Bean}
   * to the given {@link StateChangeCallback}, which is used to affect changes in state
   * to the given {@link String named} property.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property on {@literal this} {@link Bean}.
   * @param callback {@link StateChangeCallback} used to affect changes in state
   * to the given {@link String named} property.
   * @return the previously mapped {@link StateChangeCallback}.
   * @throws IllegalArgumentException if the {@link String named} property is not specified.
   * @see StateChangeCallback
   */
  protected @Nullable StateChangeCallback<Object> mapPropertyNameToStateChangeCallback(@NotNull String propertyName,
      @Nullable StateChangeCallback<Object> callback) {

    Assert.hasText(propertyName, "Property name [%s] is required", propertyName);
    Assert.notNull(callback, "The StateChangeCallback to map to property [%s] is required", propertyName);

    return this.propertyNameToStateChangeCallbackMapping.put(propertyName, callback);
  }

  /**
   * Constructs a new instance of {@link PropertyChangeEvent} initialized with {@literal this} {@link Bean}
   * as the source of the event as well as the {@link String name} of the property that is changing
   * along with the property's {@link Object old value} and {@link Object new value}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property that is changing.
   * @param oldValue {@link Object} containing the old value for the {@link String named} property.
   * @param newValue {@link Object} containing the new value for the {@link String named} property.
   * @return a new {@link PropertyChangeEvent} for {@literal this} {@link Bean} specifying the {@link String name}
   * of the property that is changing along with the property's {@link Object old value} and {@link Object new value}.
   * @see java.beans.PropertyChangeEvent
   */
  protected PropertyChangeEvent newPropertyChangeEvent(@NotNull String propertyName,
      @Nullable Object oldValue, @Nullable Object newValue) {

    return BeanUtils.newPropertyChangeEvent(this, propertyName, oldValue, newValue);
  }

  /**
   * Processes the change in state to the {@link String named} property of {@literal this} {@link Bean}.
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
   * Processes the change in state to the {@link String named} property of {@literal this} {@link Bean}.
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

      StateChangeFunction function = resolveStateChangeFunction(callback);

      fireVetoableChangeEvent(event);
      function.apply(event, false);
      firePropertyChangeEvent(event);
      fireChangeEvent();
    }
    catch (PropertyVetoException cause) {

      String message = String.format("The new value [%1$s] for property [%2$s] of Bean [%3$s] is not valid",
        newValue, propertyName, getClass().getName());

      throw new IllegalPropertyValueException(message, cause);
    }
  }

  private @NotNull <T> StateChangeFunction resolveStateChangeFunction(@Nullable StateChangeCallback<T> callback) {

    StateChangeFunction resolvedFunction = callback == null
      ? StateChangeFunction.noStateChangeFunction()
      : newStateChangeFunction(callback);

    for (StateChangeFunction function : StateChangeFunctionStrategies.values()) {
      resolvedFunction = resolvedFunction.andThen(function);
    }

    return resolvedFunction;
  }

  @SuppressWarnings("unchecked")
  private @NotNull <T> StateChangeFunction newStateChangeFunction(@NotNull StateChangeCallback<T> callback) {

    return (event, returnValue) -> {
      callback.changeState((T) event.getNewValue());
      return true;
    };
  }

  /**
   * Registers the given {@link ChangeListener} to the list of listeners notified of {@link ChangeEvent ChangeEvents},
   * or changes to {@literal this} {@link Bean}.
   *
   * @param listener {@link ChangeListener} to add to the list of listeners being notified of changes
   * to {@literal this} {@link Bean}.
   * @see org.cp.elements.beans.event.ChangeSupport#register(ChangeListener)
   * @see org.cp.elements.beans.event.ChangeListener
   */
  protected void register(@Nullable ChangeListener listener) {
    this.changeSupport.register(listener);
  }

  /**
   * Registers the given {@link PropertyChangeListener} to listen for and be notified of all
   * {@link PropertyChangeEvent PropertyChangeEvents} on {@literal this} {@link Bean}.
   *
   * @param listener {@link PropertyChangeListener} to register and listen for all
   * {@link PropertyChangeEvent PropertyChangeEvents} on {@literal this} {@link Bean}.
   * @see java.beans.PropertyChangeSupport#addPropertyChangeListener(PropertyChangeListener)
   * @see #register(String, PropertyChangeListener)
   * @see java.beans.PropertyChangeListener
   */
  protected void register(@Nullable PropertyChangeListener listener) {
    this.propertyChangeSupport.addPropertyChangeListener(listener);
  }

  /**
   * Registers the given {@link PropertyChangeListener} to listen for and ben notified of changes to
   * the given {@link String named} property of {@literal this} {@link Bean}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property on {@literal this} {@link Bean}
   * for which the listener will be notified of {@link PropertyChangeEvent PropertyChangeEvents}.
   * @param listener {@link PropertyChangeListener} listening for changes to the given {@link String named} property
   * of {@literal this} {@link Bean}.
   * @see java.beans.PropertyChangeSupport#addPropertyChangeListener(String, PropertyChangeListener)
   * @see #register(PropertyChangeListener)
   * @see java.beans.PropertyChangeListener
   */
  protected void register(@Nullable String propertyName, @Nullable PropertyChangeListener listener) {
    this.propertyChangeSupport.addPropertyChangeListener(propertyName, listener);
  }

  /**
   * Registers the given {@link VetoableChangeListener} to listen for {@link PropertyChangeEvent PropertyChangeEvents}
   * in order to monitor and veto any undesired changes to {@literal this} {@link Bean} state.
   *
   * @param listener {@link VetoableChangeListener} used to listen for and possibly veto any undesired changes
   * to {@literal this} {@link Bean} state.
   * @see java.beans.VetoableChangeSupport#addVetoableChangeListener(VetoableChangeListener)
   * @see #register(String, VetoableChangeListener)
   * @see java.beans.VetoableChangeListener
   */
  protected void register(@Nullable VetoableChangeListener listener) {
    this.vetoableChangeSupport.addVetoableChangeListener(listener);
  }

  /**
   * Registers the given {@link VetoableChangeListener} to listen for {@link PropertyChangeEvent PropertyChangeEvents}
   * on the given {@link String named} property in order to monitor and veto any undesired changes to {@literal this}
   * {@link Bean} state.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property on {@literal this} {@link Bean}
   * for which the listener will be notified of changes.
   * @param listener {@link VetoableChangeListener} used to listen for and possibly veto any undesired changes
   * to {@literal this} {@link Bean} state.
   * @see java.beans.VetoableChangeSupport#addVetoableChangeListener(String, VetoableChangeListener)
   * @see #register(VetoableChangeListener)
   * @see java.beans.VetoableChangeListener
   */
  protected void register(@Nullable String propertyName, @Nullable VetoableChangeListener listener) {
    this.vetoableChangeSupport.addVetoableChangeListener(propertyName, listener);
  }

  /**
   * Unregisters the given {@link ChangeListener} from the list of listeners being notified of changes
   * to {@literal this} {@link Bean}.
   *
   * @param listener {@link ChangeListener} to remove from the list of listeners being notified of changes
   * to {@literal this} {@link Bean}.
   * @see org.cp.elements.beans.event.ChangeSupport#unregister(ChangeListener)
   * @see org.cp.elements.beans.event.ChangeListener
   */
  protected void unregister(@Nullable ChangeListener listener) {
    this.changeSupport.unregister(listener);
  }

  /**
   * Unregisters the given {@link PropertyChangeListener} from listening to and being notified of
   * {@link PropertyChangeEvent PropertyChangeEvents} on {@literal this} {@link Bean}.
   *
   * @param listener {@link PropertyChangeListener} to remove.
   * @see java.beans.PropertyChangeSupport#removePropertyChangeListener(PropertyChangeListener)
   * @see #unregister(String, PropertyChangeListener)
   * @see java.beans.PropertyChangeListener
   */
  protected void unregister(@Nullable PropertyChangeListener listener) {
    this.propertyChangeSupport.removePropertyChangeListener(listener);
  }

  /**
   * Unregisters the given {@link PropertyChangeListener} from listening to and being notified of
   * {@link PropertyChangeEvent PropertyChangeEvents} to the given {@link String named} property
   * on {@literal this} {@link Bean}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property
   * on {@literal this} {@link Bean}.
   * @param listener {@link PropertyChangeListener} to remove.
   * @see java.beans.PropertyChangeSupport#removePropertyChangeListener(String, PropertyChangeListener)
   * @see #unregister(PropertyChangeListener)
   * @see java.beans.PropertyChangeListener
   */
  protected void unregister(@Nullable String propertyName, @Nullable PropertyChangeListener listener) {
    this.propertyChangeSupport.removePropertyChangeListener(propertyName, listener);
  }

  /**
   * Unregisters the given {@link VetoableChangeListener} from listening to and being notified of
   * {@link PropertyChangeEvent PropertyChangeEvents} to the given {@link String named} property
   * of {@literal this} {@link Bean}.
   *
   * In addition, the listener's vote in vetoing possibly undesirable changes to {@literal this} {@link Bean}
   * state is now void.
   *
   * @param listener {@link VetoableChangeListener} to remove.
   * @see java.beans.VetoableChangeSupport#removeVetoableChangeListener(VetoableChangeListener)
   * @see #unregister(String, VetoableChangeListener)
   * @see java.beans.VetoableChangeListener
   */
  protected void unregister(@Nullable VetoableChangeListener listener) {
    this.vetoableChangeSupport.removeVetoableChangeListener(listener);
  }

  /**
   * Unregisters the given {@link VetoableChangeListener} from listening to and being notified of
   * {@link PropertyChangeEvent PropertyChangeEvents} to the given {@link String named} property
   * of {@literal this} {@link Bean}.
   *
   * In addition, the listener's vote in vetoing possibly undesirable changes to {@literal this} {@link Bean} state
   * is now void.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property on {@literal this} {@link Bean}.
   * @param listener {@link VetoableChangeListener} to remove.
   * @see java.beans.VetoableChangeSupport#removeVetoableChangeListener(String, VetoableChangeListener)
   * @see #unregister(VetoableChangeListener)
   * @see java.beans.VetoableChangeListener
   */
  protected void unregister(@Nullable String propertyName, @Nullable VetoableChangeListener listener) {
    this.vetoableChangeSupport.removeVetoableChangeListener(propertyName, listener);
  }

  /**
   * Removes the mapping from the {@link String named} property to the corresponding {@link Field}
   * of {@literal this} {@link Bean}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property on {@literal this} {@link Bean}.
   * @return {@link String} containing the {@literal name} of the {@link Bean Bean} {@link Field} that was mapped to
   * the {@link String named} property.
   */
  protected @Nullable String unmapPropertyNameToFieldName(@Nullable String propertyName) {

    return StringUtils.hasText(propertyName)
      ? this.propertyNameToFieldNameMapping.remove(propertyName)
      : null;
  }

  /**
   * Removes the mapping from the {@link String named} property of {@literal this} {@link Bean} to the registered
   * {@link StateChangeCallback} used to affect changes in state to the given {@link String named} property
   * of {@literal this} {@link Bean}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property of {@literal this} {@link Bean}.
   * @return the mapped {@link StateChangeCallback} to the given {@link String named} property.
   * @see StateChangeCallback
   */
  protected @Nullable StateChangeCallback<Object> unmapPropertyNameToStateChangeCallback(
      @Nullable String propertyName) {

    return StringUtils.hasText(propertyName)
      ? this.propertyNameToStateChangeCallbackMapping.remove(propertyName)
      : null;
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

    String message = String.format("The Bean to compare with this Bean must be an instance of [%s]",
      getClass().getName());

    Assert.isInstanceOf(bean, getClass(), new ClassCastException(message));

    return ComparatorUtils.compareIgnoreNull(getId(), bean.getId());
  }

  /**
   * Determines whether {@literal this} {@link Bean} and the given {@link Bean} are equal.
   *
   * {@literal This} {@link Bean} and the given {@link Bean} are considered equal if their {@link #getId() identifiers}
   * match. A {@literal null} {@link #getId() identifier} is not logically equal to any other {@link Bean}, even other
   * {@link Bean Beans} with a {@literal null} {@link #getId() identifier} regardless of the {@link Bean} other state.
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
   * as determined by {@literal this} {@link Bean} internal state.
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

  /**
   * {@link BiFeederFunction} implementation encapsulating logic used to change the state
   * of {@literal this} {@link Bean}.
   *
   * @see org.cp.elements.function.BiFeederFunction
   * @see java.beans.PropertyChangeEvent
   */
  protected interface StateChangeFunction extends BiFeederFunction<PropertyChangeEvent, Boolean> {

    /**
     * Default {@link StateChangeFunction} that does not change the state of {@literal this} {@link Bean}.
     *
     * @return a default, no-op {@link StateChangeFunction}.
     */
    static @NotNull StateChangeFunction noStateChangeFunction() {
      return (event, returnValue) -> false;
    }

    /**
     * Composes {@literal this} {@link StateChangeFunction} with the given, required {@link StateChangeFunction}.
     *
     * @param function {@link StateChangeFunction} to compose; must not be {@literal null}.
     * @return a new {@link StateChangeFunction} composed of {@literal this} {@link StateChangeFunction}
     * and the given, required {@link StateChangeFunction}.
     * @throws IllegalArgumentException if the given {@link StateChangeFunction} is {@literal null}.
     */
    default @NotNull StateChangeFunction andThen(@NotNull StateChangeFunction function) {

      Assert.notNull(function, "The StateChangeFunction to compose with this StateChangeFunction is required");

      return (event, returnValue) -> {
        boolean thisReturnValue = Boolean.TRUE.equals(this.apply(event, returnValue));
        return function.apply(event, merge(returnValue, thisReturnValue));
      };
    }
  }

  /**
   * Enumeration of {@link StateChangeFunction StateChangeFunctions} implementing different {@literal Strategies}
   * for changing the state of {@literal this} {@link Bean}.
   *
   * @see <a href="https://en.wikipedia.org/wiki/Strategy_pattern">Strategy Software Design Pattern</a>
   * @see StateChangeCallback
   * @see StateChangeFunction
   */
  private enum StateChangeFunctionStrategies implements StateChangeFunction {

    REGISTERED_STATE_CHANGE_CALLBACK {

      @Override
      public @Nullable Boolean apply(@NotNull PropertyChangeEvent event, @Nullable Boolean returnValue) {

        if (!Boolean.TRUE.equals(returnValue)) {

          AbstractBean<?, ?, ?> bean = (AbstractBean<?, ?, ?>) event.getSource();

          StateChangeCallback<Object> registeredStateChangeCallback =
            bean.propertyNameToStateChangeCallbackMapping.get(event.getPropertyName());

          if (registeredStateChangeCallback != null) {
            registeredStateChangeCallback.changeState(event.getNewValue());
            return true;
          }
        }

        return returnValue;
      }
    },

    REFLECTION_BASED_STATE_CHANGE {

      @Override
      @SuppressWarnings("all")
      public @Nullable Boolean apply(@NotNull PropertyChangeEvent event, @Nullable Boolean returnValue) {

        if (!Boolean.TRUE.equals(returnValue)) {

          AbstractBean<?, ?, ?> bean = (AbstractBean<?, ?, ?>) event.getSource();

          bean.changeState(event.getPropertyName(), event.getNewValue());

          return true;
        }

        return returnValue;
      }
    }
  }
}
