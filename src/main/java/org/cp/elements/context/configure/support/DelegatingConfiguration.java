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
package org.cp.elements.context.configure.support;

import java.util.Iterator;

import org.cp.elements.context.configure.Configuration;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;

/**
 * {@link Configuration} implementation that delegates to underlying {@link Configuration} instance.
 *
 * @author John Blum
 * @see org.cp.elements.context.configure.Configuration
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class DelegatingConfiguration implements Configuration {

  protected static final String DELEGATE_NAME_SUFFIX = "Delegate";

  private final Configuration delegate;

  /**
   * Constructs a new instance of {@link DelegatingConfiguration} initialized with the given,
   * required {@link Configuration} object used as the {@literal delegate}.
   *
   * @param delegate {@link Configuration} object used as a {@literal delegate}
   * backing all property access operations of this {@link Configuration}; must not be {@literal null}.
   * @throws IllegalArgumentException if the {@link Configuration} is {@literal null}.
   */
  public DelegatingConfiguration(@NotNull Configuration delegate) {
    this.delegate = ObjectUtils.requireObject(delegate, "Configuration to be used as the delegate is required");
  }

  protected @NotNull Configuration getDelegate() {
    return this.delegate;
  }

  @Override
  public Descriptor<?> getDescriptor() {
    return getDelegate().getDescriptor();
  }

  @Override
  public String getName() {
    return getDelegate().getName().concat(DELEGATE_NAME_SUFFIX);
  }

  @Override
  public String[] getProfiles() {
    return getDelegate().getProfiles();
  }

  @Override
  public String getPropertyValue(String propertyName, boolean required) {
    return getDelegate().getPropertyValue(propertyName, required);
  }

  @Override
  public Iterator<String> iterator() {
    return getDelegate().iterator();
  }
}
