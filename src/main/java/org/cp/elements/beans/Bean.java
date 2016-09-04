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

package org.cp.elements.beans;

import java.io.Serializable;

import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Visitable;

/**
 * The Bean interface defines a contract for an application domain model object representing/modeling data
 * in a software application.
 * 
 * @author John J. Blum
 * @param <ID> the Comparable class type of the identifier uniquely identifying this Bean.
 * @param <USER> the class type of the object signifying the user for auditing information.
 * @param <PROCESS> the class type of the object signifying the process for auditing information.
 * @see java.lang.Cloneable
 * @see java.lang.Comparable
 * @see java.io.Serializable
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.Identifiable
 * @see org.cp.elements.lang.Visitable
 * @since 1.0.0
 */
public interface Bean<ID extends Comparable<ID>, USER, PROCESS> extends Auditable<USER, PROCESS, ID>,
  Cloneable, Comparable<Bean<ID, USER, PROCESS>>, Serializable, Visitable {

}
