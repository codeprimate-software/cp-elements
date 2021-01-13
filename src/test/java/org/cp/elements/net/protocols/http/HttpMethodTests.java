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

package org.cp.elements.net.protocols.http;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import org.cp.elements.dao.CrudOperation;
import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link HttpMethod} enum.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.net.protocols.http.HttpMethod
 * @since 1.0.0
 */
public class HttpMethodTests {

  @Test
  public void httpMethodWithCrudOperation() {
    assertThat(HttpMethod.POST.crudOperation(), is(equalTo(CrudOperation.CREATE)));
    assertThat(HttpMethod.GET.crudOperation(), is(equalTo(CrudOperation.READ)));
    assertThat(HttpMethod.PUT.crudOperation(), is(equalTo(CrudOperation.UPDATE)));
    assertThat(HttpMethod.DELETE.crudOperation(), is(equalTo(CrudOperation.DELETE)));
  }

  @Test
  public void httpMethodWithNoCrudOperationReturnsNull() {
    assertThat(HttpMethod.CONNECT.crudOperation(), is(nullValue(CrudOperation.class)));
    assertThat(HttpMethod.HEAD.crudOperation(), is(nullValue(CrudOperation.class)));
    assertThat(HttpMethod.OPTIONS.crudOperation(), is(nullValue(CrudOperation.class)));
    assertThat(HttpMethod.TRACE.crudOperation(), is(nullValue(CrudOperation.class)));
  }

  @Test
  public void valueOfCrudOperationReturnsHttpMethod() {
    assertThat(HttpMethod.valueOf(CrudOperation.CREATE), is(equalTo(HttpMethod.POST)));
    assertThat(HttpMethod.valueOf(CrudOperation.READ), is(equalTo(HttpMethod.GET)));
    assertThat(HttpMethod.valueOf(CrudOperation.UPDATE), is(equalTo(HttpMethod.PUT)));
    assertThat(HttpMethod.valueOf(CrudOperation.DELETE), is(equalTo(HttpMethod.DELETE)));
  }

  @Test
  public void valueOfCrudOperationReturnsNull() {
    assertThat(HttpMethod.valueOf((CrudOperation) null), is(nullValue(HttpMethod.class)));
  }

  @Test
  public void valueOfIgnoreCaseReturnsHttpMethod() {
    assertThat(HttpMethod.valueOfIgnoreCase("CONNECT"), is(equalTo(HttpMethod.CONNECT)));
    assertThat(HttpMethod.valueOfIgnoreCase(" Delete"), is(equalTo(HttpMethod.DELETE)));
    assertThat(HttpMethod.valueOfIgnoreCase("get  "), is(equalTo(HttpMethod.GET)));
    assertThat(HttpMethod.valueOfIgnoreCase("  HEad   "), is(equalTo(HttpMethod.HEAD)));
    assertThat(HttpMethod.valueOfIgnoreCase("   OPTionS "), is(equalTo(HttpMethod.OPTIONS)));
    assertThat(HttpMethod.valueOfIgnoreCase(" POST "), is(equalTo(HttpMethod.POST)));
  }

  @Test
  public void valueOfIgnoreCaseReturnsNull() {
    assertThat(HttpMethod.valueOfIgnoreCase("DISCONNECT"), is(nullValue(HttpMethod.class)));
    assertThat(HttpMethod.valueOfIgnoreCase("Remove"), is(nullValue(HttpMethod.class)));
    assertThat(HttpMethod.valueOfIgnoreCase("GETit"), is(nullValue(HttpMethod.class)));
    assertThat(HttpMethod.valueOfIgnoreCase("giveMe"), is(nullValue(HttpMethod.class)));
    assertThat(HttpMethod.valueOfIgnoreCase("grab"), is(nullValue(HttpMethod.class)));
    assertThat(HttpMethod.valueOfIgnoreCase("PUTS"), is(nullValue(HttpMethod.class)));
    assertThat(HttpMethod.valueOfIgnoreCase("OPT"), is(nullValue(HttpMethod.class)));
    assertThat(HttpMethod.valueOfIgnoreCase("Postage"), is(nullValue(HttpMethod.class)));
    assertThat(HttpMethod.valueOfIgnoreCase("PUTT"), is(nullValue(HttpMethod.class)));
    assertThat(HttpMethod.valueOfIgnoreCase(" debug  "), is(nullValue(HttpMethod.class)));
  }

  @Test
  public void valueOfIgnoreCaseWithNullEmptyAndBlankReturnsNull() {
    assertThat(HttpMethod.valueOfIgnoreCase("  "), is(nullValue(HttpMethod.class)));
    assertThat(HttpMethod.valueOfIgnoreCase(""), is(nullValue(HttpMethod.class)));
    assertThat(HttpMethod.valueOfIgnoreCase(null), is(nullValue(HttpMethod.class)));
  }
}
