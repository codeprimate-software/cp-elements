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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;

import org.junit.Test;

/**
 * The AbstractBeanTest class is a test suite of test cases testing the contract and functionality
 * of the AbstractBean class.
 *
 * @author John J. Blum
 * @see org.cp.elements.beans.AbstractBean
 * @see org.cp.elements.beans.Bean
 * @see org.junit.Test
 * @since 1.0.0
 */
public class AbstractBeanTest {

  @Test
  public void createAbstractBean() {
    TestBean<Long> bean = new TestBean<>();

    assertThat(bean, is(notNullValue()));
    assertThat(bean.getId(), is(nullValue()));
  }

  @Test
  public void createAbstractBeanWithId() {
    TestBean<Long> bean = new TestBean<>(1L);

    assertThat(bean, is(notNullValue()));
    assertThat(bean.getId(), is(equalTo(1l)));
  }

  @Test
  public void setAndGetId() {
    TestBean<Long> bean = new TestBean<>();

    assertThat(bean, is(notNullValue()));
    assertThat(bean.getId(), is(nullValue()));

    bean.setId(1L);

    assertThat(bean.getId(), is(equalTo(1l)));
  }

  protected static final class TestBean<ID extends Comparable<ID>> extends AbstractBean<ID, String, String> {

    public TestBean() {
    }

    public TestBean(final ID id) {
      super(id);
    }
  }

}
