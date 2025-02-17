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
package org.cp.elements.data.oql.support;

import java.util.function.BiPredicate;
import java.util.function.Predicate;

import org.cp.elements.data.oql.QueryArguments;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Abstract utility class for OQL.
 *
 * @author John Blum
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public abstract class OqlUtils {

  public static final BiPredicate<QueryArguments, ?> ACCEPT_ALL_QUERY_PREDICATE = (queryArguments, target) -> true;

  public static <T> BiPredicate<QueryArguments, T> asBiPredicate(@NotNull Predicate<T> predicate) {
    return (queryArguments, target) -> predicate.test(target);
  }

  public static <S> Predicate<S> asPredicate(@NotNull BiPredicate<QueryArguments, S> predicate) {
    return asPredicate(predicate, QueryArguments.empty());
  }

  public static <S> Predicate<S> asPredicate(@NotNull BiPredicate<QueryArguments, S> predicate, QueryArguments arguments) {
    Assert.notNull(predicate, "Predicate is required");
    return target -> predicate.test(arguments, target);
  }
}
