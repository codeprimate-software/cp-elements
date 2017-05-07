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

package org.cp.elements.util.stream;

import static org.cp.elements.util.ArrayUtils.asIterable;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Test suite of test cases testing the contract and functionality of the {@link StreamUtils} class.
 *
 * @author John J. Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.cp.elements.util.stream.StreamUtils
 * @since 1.0.0
 */
@SuppressWarnings("unchecked")
public class StreamUtilsTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Test
  public void streamFromArray() {
    String[] array = { "test", "testing", "tested" };
    Stream<String> stream = StreamUtils.stream(array);

    assertThat(stream, is(notNullValue(Stream.class)));

    List<String> list = stream.collect(Collectors.toList());

    assertThat(list, is(notNullValue(List.class)));
    assertThat(list.size(), is(equalTo(array.length)));
    assertThat(list.containsAll(Arrays.asList(array)), is(true));
  }

  @Test
  public void streamFromSingleElementArray() {
    Stream<String> stream = StreamUtils.stream("test");

    assertThat(stream, is(notNullValue(Stream.class)));

    List<String> list = stream.collect(Collectors.toList());

    assertThat(list, is(notNullValue(List.class)));
    assertThat(list.size(), is(equalTo(1)));
    assertThat(list.get(0), is(equalTo("test")));
  }

  @Test
  public void streamFromEmptyArray() {
    Stream<Object> stream = StreamUtils.stream();

    assertThat(stream, is(notNullValue(Stream.class)));
    assertThat(stream.count(), is(equalTo(0l)));
  }

  @Test(expected = NullPointerException.class)
  public void streamFromNullArray() {
    StreamUtils.stream((Object[]) null);
  }

  @Test
  public void streamFromIterable() {
    Stream<Integer> stream = StreamUtils.stream(asIterable(1, 2, 3));

    assertThat(stream, is(notNullValue(Stream.class)));

    Object[] array = stream.toArray();

    assertThat(array, is(notNullValue(Object[].class)));
    assertThat(array.length, is(equalTo(3)));

    for (int index = 0; index < array.length; index++) {
      assertThat(array[index], is(equalTo(index + 1)));
    }
  }

  @Test
  public void streamFromSingleElementIterable() {
    Stream<Integer> stream = StreamUtils.stream(asIterable(1));

    assertThat(stream, is(notNullValue(Stream.class)));

    Object[] array = stream.toArray();

    assertThat(array, is(notNullValue(Object[].class)));
    assertThat(array.length, is(equalTo(1)));
    assertThat(array[0], is(equalTo(1)));
  }

  @Test
  public void streamFromEmptyIterable() {
    Stream<Object> stream = StreamUtils.stream(asIterable());

    assertThat(stream, is(notNullValue(Stream.class)));
    assertThat(stream.count(), is(equalTo(0l)));
  }

  @Test
  public void streamFromNullIterable() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage(is(equalTo("Iterable cannot be null")));

    StreamUtils.stream((Iterable) null);
  }

  @Test
  public void streamFromCollection() {
    List<Integer> list = Arrays.asList(1, 2, 3);
    Stream<Integer> stream = StreamUtils.stream(list);

    assertThat(stream, is(notNullValue(Stream.class)));

    List<Integer> result = stream.collect(Collectors.toList());

    assertThat(result, is(notNullValue(List.class)));
    assertThat(result.size(), is(equalTo(list.size())));
    assertThat(result.containsAll(list), is(true));
  }
}
