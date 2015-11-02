/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.beans;

import static org.hamcrest.CoreMatchers.*;
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
