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

package org.cp.elements.lang.support;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.cp.elements.context.configure.Configuration;
import org.cp.elements.lang.Configurable;
import org.cp.elements.lang.Visitable;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for {@link ConfigurableVisitor}.
 *
 * @author John J. Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.mockito.Mockito
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.lang.Configurable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.support.ConfigurableVisitor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ConfigurableVisitorTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Test
  public void construct() {
    new ConfigurableVisitor<>(mock(Configuration.class));
  }

  @Test
  public void constructWithNullConfiguration() {

    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Configuration cannot be null");

    new ConfigurableVisitor<Configuration>(null);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void visit() {

    Configuration mockConfiguration = mock(Configuration.class);

    VisitableConfigurable<Configuration> mockVisitableConfigurable = mock(VisitableConfigurable.class);

    ConfigurableVisitor<Configuration> visitor = new ConfigurableVisitor<>(mockConfiguration);

    visitor.visit(mockVisitableConfigurable);

    verify(mockVisitableConfigurable, times(1)).configure(eq(mockConfiguration));
  }

  @Test
  public void visitWithNonConfigurableVisitable() {
    new ConfigurableVisitor<>(mock(Configuration.class)).visit(mock(Visitable.class));
  }

  protected interface VisitableConfigurable<T> extends Configurable<T>, Visitable { }

}
