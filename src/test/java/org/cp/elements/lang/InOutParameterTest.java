/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 *
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 *
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 *
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 *
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;

import org.junit.Test;

/**
 * The InOutParameterTest class is a test suite of test cases testing the contract and functionality
 * of the InOutParameter class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @since 1.0.0
 */
public class InOutParameterTest {

  @Test
  public void constructUninitializedInOutParameter() {
    InOutParameter<Object> parameter = new InOutParameter<>();

    assertThat(parameter, is(notNullValue()));
    assertThat(parameter.getValue(), is(nullValue()));
  }

  @Test
  public void constructInOutParameterWithValue() {
    InOutParameter<String> parameter = new InOutParameter<>("test");

    assertThat(parameter, is(notNullValue()));
    assertThat(parameter.getValue(), is(equalTo("test")));
  }

  @Test
  public void setAndGetValue() {
    InOutParameter<Object> parameter = new InOutParameter<>();

    assertThat(parameter.getValue(), is(nullValue()));

    parameter.setValue("test");

    assertThat(parameter.getValue(), is(equalTo("test")));

    parameter.setValue(Math.PI);

    assertThat(parameter.getValue(), is(equalTo(Math.PI)));

    parameter.setValue(2);

    assertThat(parameter.getValue(), is(equalTo(2)));

    parameter.setValue('A');

    assertThat(parameter.getValue(), is(equalTo('A')));

    parameter.setValue(Boolean.TRUE);

    assertThat(parameter.getValue(), is(true));

    parameter.setValue(null);

    assertThat(parameter.getValue(), is(nullValue()));
  }

  @Test
  public void equalsDifferentParameterWithSameValue() {
    InOutParameter<Object> parameterOne = new InOutParameter<>("test");
    InOutParameter<Object> parameterTwo = new InOutParameter<>("test");

    assertThat(parameterOne, is(not(sameInstance(parameterTwo))));
    assertThat(parameterOne.equals(parameterTwo), is(true));
  }

  @Test
  @SuppressWarnings("all")
  public void equalsItself() {
    InOutParameter<Object> parameter = new InOutParameter<>("test");

    assertThat(parameter.equals(parameter), is(true));
  }

  @Test
  @SuppressWarnings("all")
  public void equalsValue() {
    assertThat(new InOutParameter<>("test").equals("test"), is(true));
  }

  @Test
  @SuppressWarnings("all")
  public void equalsWithNull() {
    assertThat(new InOutParameter<>().equals(new InOutParameter<>(null)), is(true));
    assertThat(new InOutParameter<>().equals(null), is(true));
  }

  @Test
  @SuppressWarnings("all")
  public void unequalParameters() {
    InOutParameter<Object> parameter = new InOutParameter<>();

    assertThat(parameter.equals(new InOutParameter<>("null")), is(false));
    assertThat(parameter.equals("test"), is(false));
  }

  @Test
  public void hashCodeWithNull() {
    assertThat(new InOutParameter<>().hashCode(), is(equalTo(0)));
  }

  @Test
  public void hashCodeWithValue() {
    assertThat(new InOutParameter<>("test").hashCode(), is(equalTo("test".hashCode())));
  }

  @Test
  public void toStringWithNull() {
    assertThat(new InOutParameter<>().toString(), is(equalTo("null")));
  }

  @Test
  public void toStringWithValue() {
    assertThat(new InOutParameter<>("test").toString(), is(equalTo("test")));
  }

  @Test
  public void inOutParameterIsMutable() {
    assertThat(increment(new InOutParameter<>(0)).getValue(), is(equalTo(1)));
  }

  protected InOutParameter<Integer> increment(InOutParameter<Integer> parameter) {
    parameter.setValue(parameter.getValue() + 1);
    return parameter;
  }

  @Test
  public void inOutParameterIsThreadSafe() throws Exception {
    InOutParameter<Integer> parameter = new InOutParameter<>(0);

    Thread incrementThread = new Thread(() -> {
      parameter.setValue(parameter.getValue() + 1);
    });

    incrementThread.setDaemon(true);
    incrementThread.setName("Parameter Increment Thread");
    incrementThread.setPriority(Thread.NORM_PRIORITY);

    assertThat(parameter.getValue(), is(equalTo(0)));

    incrementThread.start();
    incrementThread.join();

    assertThat(incrementThread.isAlive(), is(false));
    assertThat(parameter.getValue(), is(equalTo(1)));
  }

}
