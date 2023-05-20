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

import static org.cp.elements.lang.LangExtensions.assertThat;

import java.beans.PropertyVetoException;

import org.junit.jupiter.api.Test;

import org.cp.elements.beans.AbstractBean;
import org.cp.elements.beans.IllegalPropertyValueException;
import org.cp.elements.beans.PropertyNotSetException;
import org.cp.elements.beans.annotation.Required;
import org.cp.elements.lang.Nameable;
import org.cp.elements.lang.ThrowableAssertions;
import org.cp.elements.lang.ThrowableOperation;
import org.cp.elements.lang.annotation.NotNull;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

/**
 * Integration Tests for {@link RequiredPropertyVetoableChangeListener}.
 *
 * @author John Blum
 * @see java.beans.PropertyChangeEvent
 * @see java.beans.VetoableChangeListener
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.beans.AbstractBean
 * @see org.cp.elements.beans.event.support.RequiredPropertyVetoableChangeListener
 * @since 1.0.0
 */
public class RequiredPropertyVetoableChangeListenerIntegrationTests {

  @Test
  public void setNonRequiredPropertyToNullValue() {

    TestUser jonBloom = TestUser.as("jonBloom").identifiedBy(1);

    assertThat(jonBloom).isNotNull();
    assertThat(jonBloom.getId()).isEqualTo(1);
    assertThat(jonBloom.getName()).isEqualTo("jonBloom");

    jonBloom.setId(null);

    assertThat(jonBloom.getId()).isNull();
    assertThat(jonBloom.getName()).isEqualTo("jonBloom");
  }

  @Test
  public void setRequiredPropertyToNonNullValue() {

    TestUser jonDoe = TestUser.as("jonDoe");

    assertThat(jonDoe).isNotNull();
    assertThat(jonDoe.getName()).isEqualTo("jonDoe");

    jonDoe.setName("janeDoe");

    assertThat(jonDoe.getName()).isEqualTo("janeDoe");
  }

  @Test
  public void setRequiredPropertyToNullValue() {

    TestUser janeDoe = TestUser.as("janeDoe");

    assertThat(janeDoe).isNotNull();
    assertThat(janeDoe.getName()).isEqualTo("janeDoe");
    assertThat(janeDoe.isEventDispatchEnabled()).isTrue();

    ThrowableAssertions.assertThatThrowableOfType(IllegalPropertyValueException.class)
      .isThrownBy(ThrowableOperation.fromConsumer(args -> janeDoe.setName(null)))
      .havingMessage("The new value [null] for property [name] of Bean [%s] is not valid", janeDoe.getClass().getName())
      .causedBy(PropertyVetoException.class)
      .havingMessageStartingWith("Failed to process event [")
      .causedBy(PropertyNotSetException.class)
      .havingMessage("Property [name] not set")
      .withNoCause();

    assertThat(janeDoe.getName()).isEqualTo("janeDoe");
  }

  @Getter
  @ToString(of = "name")
  @EqualsAndHashCode(callSuper = false, of = "name")
  @RequiredArgsConstructor(staticName = "as")
  static class TestUser extends AbstractBean<Integer, Object, Object> implements Nameable<String> {

    @NotNull
    @lombok.NonNull
    private String name;

    @Required
    public void setName(@NotNull String name) {
      processChange("name", getName(), name, arg -> this.name = arg);
    }
  }
}
