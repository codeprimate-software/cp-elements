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
 * The {@link Initable} interface defines a contract for objects that can be initialized.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Destroyable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Initable {

  /**
   * Determines whether this object has been initialized.  This object gets initialized when it's init method
   * is invoked.
   *
   * @return a boolean value indicating whether this object has been initialized or not.
   */
  boolean isInitialized();

  /**
   * Initializes this object and prepares any required resources.
   */
  void init();

}
