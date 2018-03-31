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

package org.cp.elements.lang;

/**
 * The {@link Lifecycle} interface defines a contract for implementing {@link Object objects} that live out
 * a cycle of events from instantiation (construction) to configuration and initialization, then transitioning
 * to an executable, runnable (running) state and finally reaching its end-of-life by being destroyed.
 *
 * @author John J. Blum
 * @param <T> {@link Class type} of configuration meta-data and settings.
 * @see org.cp.elements.lang.Configurable
 * @see org.cp.elements.lang.Destroyable
 * @see org.cp.elements.lang.Executable
 * @see org.cp.elements.lang.Initable
 * @see org.cp.elements.lang.Interruptable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Lifecycle<T> extends Configurable<T>, Initable, Executable<T>, Interruptable, Destroyable {

}
