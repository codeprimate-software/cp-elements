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

import java.util.Map;

/**
 * The {@link ParameterizedInitable} interface extends the {@link Initable} interface and defines a contract
 * for implementing objects that can be initialized using arguments.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Initable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface ParameterizedInitable extends Initable {

  /**
   * Initializes this object using the provided arguments.
   *
   * @param args an array of Object arguments used to initialize this object.
   * @see org.cp.elements.lang.Initable#init()
   * @see #init(java.util.Map)
   */
  void init(Object... args);

  /**
   * Initialized this object using the provide parameters, a mapping of key-value pairs.
   *
   * @param parameters a Map of initialization parameters to initialize this object.
   * @see org.cp.elements.lang.Initable#init()
   * @see #init(Object...)
   * @see java.util.Map
   */
  void init(Map<?, ?> parameters);

}
