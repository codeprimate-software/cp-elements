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
 * The {@link ChangeListener} interface defines a contract for implementing classes interested in change events.
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
   * The ChangeListener callback method notifying the listener that a change event, such as a Object state transition
   * or change has occurred.
   *
   * @param event the ChangeEvent object encapsulating the state change event.
   * @see org.cp.elements.beans.event.ChangeEvent
   */
  void stateChanged(ChangeEvent event);

}
