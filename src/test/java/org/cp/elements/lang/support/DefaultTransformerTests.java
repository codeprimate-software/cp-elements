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

package org.cp.elements.lang.support;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import org.junit.Test;

/**
 * The DefaultTransformerTests class is a test suite of test cases testing the contract and functionality
 * of the {@link DefaultTransformer} class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.lang.support.DefaultTransformer
 * @since 1.0.0
 */
public class DefaultTransformerTests {

  private DefaultTransformer<Object> transformer = DefaultTransformer.INSTANCE;

  @Test
  public void transformReturnsObjectUnaltered() {
    assertThat(transformer.transform(null), is(nullValue()));
    assertThat(transformer.transform(Boolean.TRUE), is(equalTo(Boolean.TRUE)));
    assertThat(transformer.transform('x'), is(equalTo('x')));
    assertThat(transformer.transform(2), is(equalTo(2)));
    assertThat(transformer.transform(Math.PI), is(equalTo(Math.PI)));
    assertThat(transformer.transform("test"), is(equalTo("test")));
  }
}
