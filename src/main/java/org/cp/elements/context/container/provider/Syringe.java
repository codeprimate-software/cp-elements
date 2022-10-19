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
package org.cp.elements.context.container.provider;

import org.cp.elements.context.configure.ConfigurationServiceAware;
import org.cp.elements.context.container.DependencyInjection;
import org.cp.elements.data.conversion.ConversionServiceAware;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.factory.ObjectFactoryAware;
import org.cp.elements.lang.factory.provider.PrototypeObjectFactory;
import org.cp.elements.service.ServiceTemplate;

/**
 * Elements implementation of the {@link DependencyInjection} interface.
 *
 * @author John Blum
 * @see org.cp.elements.context.container.DependencyInjection
 * @since 1.0.0
 */
public class Syringe implements DependencyInjection, ServiceTemplate<Syringe> {

  /**
   * Performs dependency injection to automatically configure, auto-wire and initialize the given {@link Object target}.
   *
   * @param <T> {@link Class type} of the {@link Object target}.
   * @param target {@link Object} on which to inject dependencies.
   * @return the given {@link Object}.
   */
  @Override
  public @Nullable <T> T inject(@Nullable T target) {
    return applyStandardDependencyInjections(target);
  }

  private @Nullable <T> T applyStandardDependencyInjections(@Nullable T target) {

    if (target instanceof ConfigurationServiceAware) {
      getConfigurationService().ifPresent(((ConfigurationServiceAware) target)::setConfigurationService);
    }
    if (target instanceof ConversionServiceAware) {
      getConversionService().ifPresent(((ConversionServiceAware) target)::setConversionService);
    }
    if (target instanceof ObjectFactoryAware) {
      ((ObjectFactoryAware) target).setObjectFactory(new PrototypeObjectFactory());
    }

    return target;
  }
}
