/*
 * Copyright 2017-Present Author or Authors.
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
package org.cp.elements.data.oql.functions;

import org.cp.elements.data.oql.QueryFunction;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.util.ArrayUtils;

/**
 * {@link QueryFunction} used to {@literal count} the elements in the result set derived from a query.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.QueryFunction
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class Count implements QueryFunction<Object, Long> {

  private static final Count INSTANCE = new Count();

  public static Count all() {
    return INSTANCE;
  }

  private String name;

  @Override
  public String getName() {
    return StringUtils.defaultIfBlank(this.name, Constants.UNKNOWN);
  }

  @Override
  public Long apply(Object... resultSet) {
    return Integer.valueOf(ArrayUtils.nullSafeArray(resultSet).length).longValue();
  }

  public Count named(String name) {
    this.name = name;
    return this;
  }
}
