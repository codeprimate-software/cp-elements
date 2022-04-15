/*
 * Copyright 2016 Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package org.cp.elements.service;

/**
 * The {@link ServiceTemplate} interface is a marker {@link Class interface} declaring an application service component.
 *
 * Additionally, this {@link ServiceTemplate} interface defines a contract for application service components
 * encapsulating business logic and other service operations common to all services.
 *
 * @author John J. Blum
 * @see <a href="https://en.wikipedia.org/wiki/Template_method_pattern">Template Method Software Design Pattern</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface ServiceTemplate<T> {

}
