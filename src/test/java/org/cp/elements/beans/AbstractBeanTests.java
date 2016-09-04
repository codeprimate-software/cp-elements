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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link AbstractBean} class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.beans.AbstractBean
 * @see org.cp.elements.beans.Bean
 * @since 1.0.0
 */
public class AbstractBeanTests {

  @Test
  public void constructAbstractBean() {
    TestBean<Long> bean = new TestBean<>();

    assertThat(bean, is(notNullValue(AbstractBean.class)));
    assertThat(bean.getId(), is(nullValue(Long.class)));
  }

  @Test
  public void constructAbstractBeanWithId() {
    TestBean<Long> bean = new TestBean<>(1L);

    assertThat(bean, is(notNullValue(AbstractBean.class)));
    assertThat(bean.getId(), is(equalTo(1L)));
  }

  @Test
  public void setAndGetId() {
    TestBean<Long> bean = new TestBean<>();

    assertThat(bean, is(notNullValue(AbstractBean.class)));
    assertThat(bean.getId(), is(nullValue(Long.class)));

    bean.setId(1L);

    assertThat(bean.getId(), is(equalTo(1L)));
  }

  protected static final class TestBean<ID extends Comparable<ID>> extends AbstractBean<ID, String, String> {

    public TestBean() {
    }

    public TestBean(ID id) {
      super(id);
    }
  }
}
