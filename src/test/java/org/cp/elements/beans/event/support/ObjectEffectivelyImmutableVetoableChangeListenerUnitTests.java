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
package org.cp.elements.beans.event.support;

import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.beans.PropertyChangeEvent;

import org.cp.elements.beans.model.BeanUtils;
import org.cp.elements.lang.ImmutableObjectException;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link ObjectEffectivelyImmutableVetoableChangeListener}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.event.support.ObjectEffectivelyImmutableVetoableChangeListener
 * @since 1.0.0
 */
public class ObjectEffectivelyImmutableVetoableChangeListenerUnitTests {

  @Test
  public void doHandleAlwaysThrowsImmutableObjectException() {

    Object source = new Object();

    PropertyChangeEvent event =
      spy(BeanUtils.newPropertyChangeEvent(source, "mockProperty", 1, 2));

    assertThatExceptionOfType(ImmutableObjectException.class)
      .isThrownBy(() -> ObjectEffectivelyImmutableVetoableChangeListener.INSTANCE.vetoableChange(event))
      .withMessage("Cannot change property [mockProperty]; Object [%s] is immutable", source)
      .withNoCause();

    verify(event, atLeastOnce()).getPropertyName();
    verify(event, times(1)).getSource();
    verifyNoMoreInteractions(event);
  }
}
