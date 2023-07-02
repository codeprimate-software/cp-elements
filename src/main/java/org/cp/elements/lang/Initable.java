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
package org.cp.elements.lang;

/**
 * Interface defining a contract for {@link Object Objects} that can be initialized.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Destroyable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Initable {

  /**
   * Determines whether {@literal this} {@link Object} has been initialized.
   * <p>
   * This {@link Object} gets initialized when its {@link #init} method is invoked.
   *
   * @return a boolean value indicating whether {@literal this} {@link Object} has been initialized or not.
   */
  boolean isInitialized();

  /**
   * Initializes {@literal this} {@link Object} and prepares any required resources.
   */
  void init();

}
