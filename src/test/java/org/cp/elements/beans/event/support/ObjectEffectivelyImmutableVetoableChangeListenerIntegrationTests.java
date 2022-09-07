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

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.lang.ThrowableAssertions.assertThatThrowableOfType;

import java.beans.VetoableChangeListener;

import org.cp.elements.beans.AbstractBean;
import org.cp.elements.lang.ImmutableObjectException;
import org.cp.elements.lang.Nameable;
import org.cp.elements.lang.ThrowableOperation;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.security.model.User;

import org.junit.Test;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

/**
 * Integration Tests for {@link ObjectEffectivelyImmutableVetoableChangeListener}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.beans.AbstractBean
 * @see org.cp.elements.beans.event.support.ObjectEffectivelyImmutableVetoableChangeListener
 * @see org.cp.elements.lang.ImmutableObjectException
 * @since 1.0.0
 */
public class ObjectEffectivelyImmutableVetoableChangeListenerIntegrationTests {

  @Test
  @SuppressWarnings("all")
  public void setPropertiesThrowsImmutableObjectException() {

    TestBean bean = TestBean.as("testBean");

    assertThat(bean).isNotNull();
    assertThat(bean.getId()).isNull();
    assertThat(bean.getName()).isEqualTo("testBean");

    bean.setId(1);
    bean.setName("mockBean");

    assertThat(bean.getId()).isEqualTo(1);
    assertThat(bean.getName()).isEqualTo("mockBean");

    bean.register(ObjectEffectivelyImmutableVetoableChangeListener.INSTANCE);

    assertThatThrowableOfType(ImmutableObjectException.class)
      .isThrownBy(ThrowableOperation.from(args -> bean.setId(2)))
      .havingMessage("Cannot change property [id]; Object [%s] is immutable", bean)
      .withNoCause();

    assertThatThrowableOfType(ImmutableObjectException.class)
      .isThrownBy(ThrowableOperation.from(args -> bean.setName(null)))
      .havingMessage("Cannot change property [name]; Object [%s] is immutable", bean)
      .withNoCause();

    assertThat(bean.getId()).isEqualTo(1);
    assertThat(bean.getName()).isEqualTo("mockBean");

    bean.unregister(ObjectEffectivelyImmutableVetoableChangeListener.INSTANCE);

    bean.setId(3);
    bean.setName("junkBean");

    assertThat(bean.getId()).isEqualTo(3);
    assertThat(bean.getName()).isEqualTo("junkBean");
  }

  @Getter
  @ToString(of = "name")
  @EqualsAndHashCode(callSuper = false, of = "name")
  @RequiredArgsConstructor(staticName = "as")
  static class TestBean extends AbstractBean<Integer, User<Integer>, Object> implements Nameable<String> {

    @lombok.NonNull
    private String name;

    public void setName(@NotNull String name) {
      processChange("name", getName(), name, arg -> this.name = arg);
    }

    @Override
    protected void register(VetoableChangeListener listener) {
      super.register(listener);
    }

    @Override
    protected void unregister(VetoableChangeListener listener) {
      super.unregister(listener);
    }
  }
}
