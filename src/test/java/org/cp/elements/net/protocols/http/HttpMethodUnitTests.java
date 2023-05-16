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

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import org.cp.elements.dao.CrudOperation;

/**
 * Unit Tests for {@link HttpMethod}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.net.protocols.http.HttpMethod
 * @since 1.0.0
 */
public class HttpMethodUnitTests {

  @Test
  public void httpMethodWithCrudOperation() {

    assertThat(HttpMethod.POST.crudOperation()).isEqualTo(CrudOperation.CREATE);
    assertThat(HttpMethod.GET.crudOperation()).isEqualTo(CrudOperation.READ);
    assertThat(HttpMethod.PUT.crudOperation()).isEqualTo(CrudOperation.UPDATE);
    assertThat(HttpMethod.DELETE.crudOperation()).isEqualTo(CrudOperation.DELETE);
  }

  @Test
  public void httpMethodWithNoCrudOperationReturnsNull() {

    assertThat(HttpMethod.CONNECT.crudOperation()).isNull();
    assertThat(HttpMethod.HEAD.crudOperation()).isNull();
    assertThat(HttpMethod.OPTIONS.crudOperation()).isNull();
    assertThat(HttpMethod.TRACE.crudOperation()).isNull();
  }

  @Test
  public void valueOfCrudOperationReturnsHttpMethod() {

    assertThat(HttpMethod.valueOf(CrudOperation.CREATE)).isEqualTo(HttpMethod.POST);
    assertThat(HttpMethod.valueOf(CrudOperation.READ)).isEqualTo(HttpMethod.GET);
    assertThat(HttpMethod.valueOf(CrudOperation.UPDATE)).isEqualTo(HttpMethod.PUT);
    assertThat(HttpMethod.valueOf(CrudOperation.DELETE)).isEqualTo(HttpMethod.DELETE);
  }

  @Test
  public void valueOfCrudOperationWithNullIsNullSafeReturnsNull() {
    assertThat(HttpMethod.valueOf((CrudOperation) null)).isNull();
  }

  @Test
  public void valueOfNameIgnoreCaseReturnsHttpMethod() {

    assertThat(HttpMethod.valueOfNameIgnoreCase("CONNECT")).isEqualTo(HttpMethod.CONNECT);
    assertThat(HttpMethod.valueOfNameIgnoreCase(" Delete")).isEqualTo(HttpMethod.DELETE);
    assertThat(HttpMethod.valueOfNameIgnoreCase("get  ")).isEqualTo(HttpMethod.GET);
    assertThat(HttpMethod.valueOfNameIgnoreCase("  HEad   ")).isEqualTo(HttpMethod.HEAD);
    assertThat(HttpMethod.valueOfNameIgnoreCase("   OPTionS ")).isEqualTo(HttpMethod.OPTIONS);
    assertThat(HttpMethod.valueOfNameIgnoreCase(" PuT")).isEqualTo(HttpMethod.PUT);
    assertThat(HttpMethod.valueOfNameIgnoreCase(" POST ")).isEqualTo(HttpMethod.POST);
    assertThat(HttpMethod.valueOfNameIgnoreCase("tRACE")).isEqualTo(HttpMethod.TRACE);
    assertThat(HttpMethod.valueOfNameIgnoreCase("trACE")).isEqualTo(HttpMethod.TRACE);
  }

  @Test
  public void valueOfNameIgnoreCaseWithInvalidNamesReturnsNull() {

    assertThat(HttpMethod.valueOfNameIgnoreCase("DISCONNECT")).isNull();
    assertThat(HttpMethod.valueOfNameIgnoreCase("Remove")).isNull();
    assertThat(HttpMethod.valueOfNameIgnoreCase("GETit")).isNull();
    assertThat(HttpMethod.valueOfNameIgnoreCase("giveMe")).isNull();
    assertThat(HttpMethod.valueOfNameIgnoreCase("grab")).isNull();
    assertThat(HttpMethod.valueOfNameIgnoreCase("PUTS")).isNull();
    assertThat(HttpMethod.valueOfNameIgnoreCase("OPT")).isNull();
    assertThat(HttpMethod.valueOfNameIgnoreCase("OPTS")).isNull();
    assertThat(HttpMethod.valueOfNameIgnoreCase("OPTNS")).isNull();
    assertThat(HttpMethod.valueOfNameIgnoreCase("Postage")).isNull();
    assertThat(HttpMethod.valueOfNameIgnoreCase("PUTT")).isNull();
    assertThat(HttpMethod.valueOfNameIgnoreCase("debug")).isNull();
    assertThat(HttpMethod.valueOfNameIgnoreCase("info")).isNull();
    assertThat(HttpMethod.valueOfNameIgnoreCase("warn")).isNull();
    assertThat(HttpMethod.valueOfNameIgnoreCase("error")).isNull();
  }

  @Test
  public void valueOfNameIgnoreCaseWithNoNamesReturnsNull() {

    assertThat(HttpMethod.valueOfNameIgnoreCase("  ")).isNull();
    assertThat(HttpMethod.valueOfNameIgnoreCase("")).isNull();
  }

  @Test
  public void valueOfNameIgnoreCaseWithNullIsNullSafeReturnsNull() {
    assertThat(HttpMethod.valueOfNameIgnoreCase(null)).isNull();
  }
}
