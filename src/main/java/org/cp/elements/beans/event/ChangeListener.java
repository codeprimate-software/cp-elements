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

import java.util.EventListener;

/**
 * Interface to define a contract for implementing classes interested in {@link ChangeEvent ChangeEvents}.
 *
 * @author John J. Blum
 * @see java.lang.FunctionalInterface
 * @see java.util.EventListener
 * @see org.cp.elements.beans.event.ChangeEvent
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface ChangeListener extends EventListener {

  /**
   * Event handler method notifying {@literal this} listener that a {@link ChangeEvent}, such as a change
   * in the {@link Object Object's} state has occurred.
   *
   * @param event {@link ChangeEvent} encapsulating the change in the {@link Object Object's} state.
   * @see org.cp.elements.beans.event.ChangeEvent
   */
  void stateChanged(ChangeEvent event);

}
