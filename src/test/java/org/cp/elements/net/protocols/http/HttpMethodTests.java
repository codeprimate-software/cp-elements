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

import org.cp.elements.dao.CrudOperation;
import org.junit.Test;

/**
 * Unit Tests for the {@link HttpMethod} enum.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.net.protocols.http.HttpMethod
 * @since 1.0.0
 */
public class HttpMethodTests {

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
  public void valueOfCrudOperationReturnsNull() {
    assertThat(HttpMethod.valueOf((CrudOperation) null)).isNull();
  }

  @Test
  public void valueOfIgnoreCaseReturnsHttpMethod() {

    assertThat(HttpMethod.valueOfIgnoreCase("CONNECT")).isEqualTo(HttpMethod.CONNECT);
    assertThat(HttpMethod.valueOfIgnoreCase(" Delete")).isEqualTo(HttpMethod.DELETE);
    assertThat(HttpMethod.valueOfIgnoreCase("get  ")).isEqualTo(HttpMethod.GET);
    assertThat(HttpMethod.valueOfIgnoreCase("  HEad   ")).isEqualTo(HttpMethod.HEAD);
    assertThat(HttpMethod.valueOfIgnoreCase("   OPTionS ")).isEqualTo(HttpMethod.OPTIONS);
    assertThat(HttpMethod.valueOfIgnoreCase(" POST ")).isEqualTo(HttpMethod.POST);
  }

  @Test
  public void valueOfIgnoreCaseReturnsNull() {

    assertThat(HttpMethod.valueOfIgnoreCase("DISCONNECT")).isNull();
    assertThat(HttpMethod.valueOfIgnoreCase("Remove")).isNull();
    assertThat(HttpMethod.valueOfIgnoreCase("GETit")).isNull();
    assertThat(HttpMethod.valueOfIgnoreCase("giveMe")).isNull();
    assertThat(HttpMethod.valueOfIgnoreCase("grab")).isNull();
    assertThat(HttpMethod.valueOfIgnoreCase("PUTS")).isNull();
    assertThat(HttpMethod.valueOfIgnoreCase("OPT")).isNull();
    assertThat(HttpMethod.valueOfIgnoreCase("Postage")).isNull();
    assertThat(HttpMethod.valueOfIgnoreCase("PUTT")).isNull();
    assertThat(HttpMethod.valueOfIgnoreCase(" debug  ")).isNull();
  }

  @Test
  public void valueOfIgnoreCaseWithNullEmptyAndBlankReturnsNull() {

    assertThat(HttpMethod.valueOfIgnoreCase("  ")).isNull();
    assertThat(HttpMethod.valueOfIgnoreCase("")).isNull();
    assertThat(HttpMethod.valueOfIgnoreCase(null)).isNull();
  }
}
