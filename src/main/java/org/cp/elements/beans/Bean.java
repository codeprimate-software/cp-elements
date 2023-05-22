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

import java.io.Serializable;

import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Visitable;

/**
 * The {@link Bean} interface defines a contract for an application domain model objects representing/modeling data
 * in a software application.
 *
 * @author John J. Blum
 * @param <ID> {@link Comparable} type for the identifier uniquely identifying {@literal this} {@link Bean}.
 * @param <USER> {@link Class type} used to model the object identifying the user for auditing information.
 * @param <PROCESS> {@link Class type} used to model the object identifying the process for auditing information.
 * @see java.lang.Cloneable
 * @see java.lang.Comparable
 * @see java.io.Serializable
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.Visitable
 * @since 1.0.0
 */
public interface Bean<ID extends Comparable<ID>, USER, PROCESS> extends Auditable<USER, PROCESS, ID>,
  Cloneable, Comparable<Bean<ID, USER, PROCESS>>, Serializable, Visitable {

}
