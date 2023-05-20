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
package org.cp.elements.lang.support;

import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;

import org.cp.elements.context.configure.Configuration;
import org.cp.elements.lang.Configurable;
import org.cp.elements.lang.Visitable;

/**
 * Unit Tests for {@link ConfigurableVisitor}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.lang.Configurable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.support.ConfigurableVisitor
 * @see org.cp.elements.test.TestUtils
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ConfigurableVisitorTests {

  @Test
  public void construct() {
    new ConfigurableVisitor<>(mock(Configuration.class));
  }

  @Test
  public void constructWithNullConfiguration() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ConfigurableVisitor<Configuration>(null))
      .withMessage("Configuration cannot be null!")
      .withNoCause();
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

  private interface VisitableConfigurable<T> extends Configurable<T>, Visitable { }

}
