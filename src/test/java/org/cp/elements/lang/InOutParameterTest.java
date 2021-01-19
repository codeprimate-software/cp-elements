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
package org.cp.elements.lang;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;

/**
 * Unit Tests for {@link InOutParameter}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.lang.InOutParameter
 * @since 1.0.0
 */
public class InOutParameterTest {

  @Test
  public void constructUninitializedInOutParameter() {

    InOutParameter<Object> parameter = new InOutParameter<>();

    assertThat(parameter).isNotNull();
    assertThat(parameter.getValue()).isNull();
  }

  @Test
  public void constructInOutParameterWithValue() {

    InOutParameter<String> parameter = new InOutParameter<>("test");

    assertThat(parameter).isNotNull();
    assertThat(parameter.getValue()).isEqualTo("test");
  }

  @Test
  public void setAndGetValue() {

    InOutParameter<Object> parameter = new InOutParameter<>();

    assertThat(parameter.getValue()).isNull();

    parameter.setValue("test");

    assertThat(parameter.getValue()).isEqualTo("test");

    parameter.setValue(Math.PI);

    assertThat(parameter.getValue()).isEqualTo(Math.PI);

    parameter.setValue(2);

    assertThat(parameter.getValue()).isEqualTo(2);

    parameter.setValue('A');

    assertThat(parameter.getValue()).isEqualTo('A');

    parameter.setValue(Boolean.TRUE);

    assertThat(parameter.getValue()).isEqualTo(true);

    parameter.setValue(null);

    assertThat(parameter.getValue()).isNull();
  }

  @Test
  public void equalsDifferentParameterWithSameValue() {

    InOutParameter<Object> parameterOne = new InOutParameter<>("test");
    InOutParameter<Object> parameterTwo = new InOutParameter<>("test");

    assertThat(parameterOne).isNotSameAs(parameterTwo);
    assertThat(parameterOne.equals(parameterTwo)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void equalsItself() {

    InOutParameter<Object> parameter = new InOutParameter<>("test");

    assertThat(parameter.equals(parameter)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void equalsValue() {
    assertThat(new InOutParameter<>("test").equals("test")).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void equalsWithNull() {

    assertThat(new InOutParameter<>().equals(new InOutParameter<>(null))).isTrue();
    assertThat(new InOutParameter<>().equals(null)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void unequalParameters() {

    InOutParameter<Object> parameter = new InOutParameter<>();

    assertThat(parameter.equals(new InOutParameter<>("null"))).isFalse();
    assertThat(parameter.equals("test")).isFalse();
  }

  @Test
  public void hashCodeWithNull() {
    assertThat(new InOutParameter<>().hashCode()).isEqualTo(0);
  }

  @Test
  public void hashCodeWithValue() {
    assertThat(new InOutParameter<>("test").hashCode()).isEqualTo("test".hashCode());
  }

  @Test
  public void toStringWithNull() {
    assertThat(new InOutParameter<>().toString()).isEqualTo("null");
  }

  @Test
  public void toStringWithValue() {
    assertThat(new InOutParameter<>("test").toString()).isEqualTo("test");
  }

  @Test
  public void inOutParameterIsMutable() {
    assertThat(increment(new InOutParameter<>(0)).getValue()).isEqualTo(1);
  }

  private InOutParameter<Integer> increment(InOutParameter<Integer> parameter) {

    parameter.setValue(parameter.getValue() + 1);

    return parameter;
  }

  @Test
  public void inOutParameterIsThreadSafe() throws Exception {

    InOutParameter<Integer> parameter = new InOutParameter<>(0);

    Thread incrementThread = new Thread(() -> parameter.setValue(parameter.getValue() + 1));

    incrementThread.setDaemon(true);
    incrementThread.setName("Parameter Increment Thread");
    incrementThread.setPriority(Thread.NORM_PRIORITY);

    assertThat(parameter.getValue()).isEqualTo(0);

    incrementThread.start();
    incrementThread.join();

    assertThat(incrementThread.isAlive()).isFalse();
    assertThat(parameter.getValue()).isEqualTo(1);
  }
}
