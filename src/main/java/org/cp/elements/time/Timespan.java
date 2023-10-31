/*
 * Copyright 2023-Present Author or Authors.
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
package org.cp.elements.time;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.Month;
import java.time.Year;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.function.Supplier;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Renderable;
import org.cp.elements.lang.annotation.Dsl;
import org.cp.elements.lang.annotation.FluentApi;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract Data Type (ADT) modeling a span of time.
 *
 * @author John Blum
 * @see java.lang.Comparable
 * @see java.time.LocalDate
 * @see java.time.LocalDateTime
 * @see java.time.LocalTime
 * @see java.time.YearMonth
 * @see java.time.Month
 * @see java.time.Year
 * @see org.cp.elements.lang.Builder
 * @see org.cp.elements.lang.Renderable
 * @see org.cp.elements.lang.annotation.Dsl
 * @see org.cp.elements.lang.annotation.FluentApi
 * @since 2.0.0
 */
@FluentApi
@SuppressWarnings("unused")
public class Timespan implements Comparable<Timespan>, Renderable {

	protected static final String BEGINNING_OF_TIME = "Beginning of Time";
	protected static final String DATE_PATTERN = "yyyy-MM-dd HH:mm:ss:S";
	protected static final String DATE_TIME_PATTERN = DATE_PATTERN.concat(" HH:mm:ss:S");
	protected static final String END_OF_TIME = "End of Time";
	protected static final String TIMESPAN_TO_STRING = "%s-%s";

	protected static final Predicate<LocalDateTime> TRUE_PREDICATE = dateTime -> true;

	protected static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern(DATE_PATTERN);
	protected static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern(DATE_TIME_PATTERN);

	public static @NotNull Timespan beginning(@NotNull Year year) {
		Assert.notNull(year, "Begin Year is required");
		return beginning(year.atMonth(Month.JANUARY));
	}

	public static @NotNull Timespan beginning(@NotNull YearMonth yearMonth) {
		Assert.notNull(yearMonth, "Begin Year/Month are required");
		return beginning(yearMonth.atDay(1));
	}

	public static @NotNull Timespan beginning(@NotNull LocalDate date) {
		Assert.notNull(date, "Begin date is required");
		return beginning(date.atStartOfDay());
	}

	public static @NotNull Timespan beginning(@NotNull LocalDateTime dateTime) {
		Assert.notNull(dateTime, "Begin date/time is required");
		return new Timespan(dateTime, null);
	}

	public static @NotNull Timespan beginning(@NotNull LocalTime time) {
		Assert.notNull(time, "Start time is required");
		return beginning(time.atDate(LocalDate.now()));
	}

	/**
	 * Factory method used to construct a new {@link Timespan} between the given {@link LocalDateTime begin}
	 * and {@link LocalDateTime end} date/times.
	 *
	 * @param begin {@link LocalDateTime} declaring the {@literal beginning}, or start of the {@link Timespan};
	 * must not be {@literal null}.
	 * @param end {@link LocalDateTime} declaring the {@literal ending}, or start of the {@link Timespan};
	 * must not be {@literal null}.
	 * @return a new {@link Timespan} between the given {@link LocalDateTime begin}
	 * and {@link LocalDateTime end} date/times.
	 * @throws IllegalArgumentException if the {@link LocalDateTime begin} is on or after the {@link LocalDateTime end}.
	 * @see java.time.LocalDateTime
	 */
	@SuppressWarnings("all")
	public static @NotNull Timespan between(@NotNull LocalDateTime begin, @NotNull LocalDateTime end) {

		Assert.notNull(begin, "Begin date/time is required");
		Assert.notNull(end, "End date/time is required");

		Supplier<Boolean> timePredicate = () -> begin == null || end == null || begin.isBefore(end);

		Assert.isTrue(timePredicate.get(), "Beginning [%s] must occur before the Ending [%s]",
			begin.format(DATE_TIME_FORMATTER), end.format(DATE_TIME_FORMATTER));

		return new Timespan(begin, end);
	}

	public static @NotNull Timespan ending(@NotNull Year year) {
		Assert.notNull(year, "End Year is required");
		return ending(year.atMonth(Month.DECEMBER));
	}

	public static @NotNull Timespan ending(@NotNull YearMonth yearMonth) {
		Assert.notNull(yearMonth, "End Year/Month is required");
		return ending(yearMonth.atEndOfMonth());
	}

	public static @NotNull Timespan ending(@NotNull LocalDate date) {
		Assert.notNull(date, "End date is required");
		return ending(date.atTime(LocalTime.MAX));
	}

	public static @NotNull Timespan ending(@NotNull LocalDateTime dateTime) {
		Assert.notNull(dateTime, "End date/time is required");
		return new Timespan(null, dateTime);
	}

	public static @NotNull Timespan ending(@NotNull LocalTime time) {
		Assert.notNull(time, "End time is required");
		return ending(time.atDate(LocalDate.now()));
	}

	@Dsl
	public static @NotNull WithTo from(@Nullable Year beginning) {
		return from(beginning != null ? beginning.atMonth(Month.JANUARY) : null);
	}

	@Dsl
	public static @NotNull WithTo from(@Nullable YearMonth beginning) {
		return from(beginning != null ? beginning.atDay(1) : null);
	}

	@Dsl
	public static @NotNull WithTo from(@Nullable LocalDate beginning) {
		return from(beginning != null ? beginning.atStartOfDay() : null);
	}

	@Dsl
	public static @NotNull WithTo from(@Nullable LocalDateTime beginning) {
		return new WithTo(beginning);
	}

	@Dsl
	public static @NotNull WithTo from(@Nullable LocalTime beginning) {
		return from(beginning != null ? beginning.atDate(LocalDate.now()) : null);
	}

	/**
	 * Factory method used to construct a new {@link Timespan} that starts {@link LocalDateTime#now() now}
	 * and extends until the end of time.
	 *
	 * @return a new {@link Timespan} that starts {@link LocalDateTime#now() now} and extends until the end of time.
	 */
	public static @NotNull Timespan fromNow() {
		return new Timespan(LocalDateTime.now(), null);
	}

	/**
	 * Factory method used to construct a new {@link Timespan} that is {@literal infinite} across time, extending
	 * into the infinite past and into the infinite future.
	 *
	 * @return a new {@link Timespan} that is {@literal infinite} across time, extending into the infinite past
	 * and into the infinite future.
	 */
	public static @NotNull Timespan infinite() {
		return new Timespan(null, null);
	}

	/**
	 * Factory method used to construct a new {@link Timespan} that ends {@link LocalDateTime#now() now}
	 * and extends into the infinite past.
	 *
	 * @return a new {@link Timespan} that ends {@link LocalDateTime#now() now} and extends into the infinite past.
	 */
	public static @NotNull Timespan untilNow() {
		return new Timespan(null, LocalDateTime.now());
	}

	@Nullable
	private final LocalDateTime begin;

	@Nullable
	private final LocalDateTime end;

	/**
	 * Constructs a new {@link Timespan} between the given {@link LocalDateTime begin} and {@link LocalDateTime end}
	 * date/times.
	 *
	 * @param begin {@link LocalDateTime} declaring the {@literal beginning}, or start of the {@link Timespan};
	 * must not be {@literal null}.
	 * @param end {@link LocalDateTime} declaring the {@literal ending}, or start of the {@link Timespan};
	 * must not be {@literal null}.
	 * @throws IllegalArgumentException if the {@link LocalDateTime begin} is on or after the {@link LocalDateTime end}.
	 * @see java.time.LocalDateTime
	 */
	@SuppressWarnings("all")
	private Timespan(@Nullable LocalDateTime begin, @Nullable LocalDateTime end) {
		this.begin = begin;
		this.end = end;
	}

	public @NotNull LocalDateTime getBegin() {
		return this.begin;
	}

	public @NotNull LocalDateTime getEnd() {
		return this.end;
	}

	public Optional<LocalDateTime> getOptionalBegin() {
		return Optional.ofNullable(getBegin());
	}

	public Optional<LocalDateTime> getOptionalEnd() {
		return Optional.ofNullable(getEnd());
	}

	// Determine whether the given year is after this Timespan
	public boolean isAfter(@NotNull Year year) {
		return year != null && isAfter(year.atMonth(Month.JANUARY));
	}

	// Determine whether the given year/month is after this Timespan
	public boolean isAfter(@NotNull YearMonth yearMonth) {
		return yearMonth != null && isAfter(yearMonth.atDay(1));
	}

	// Determine whether the given date is after this Timespan
	public boolean isAfter(@NotNull LocalDate date) {
		return date != null && isAfter(date.atStartOfDay());
	}

	// Determine whether the given date/time is after this Timespan
	public boolean isAfter(@NotNull LocalDateTime dateTime) {
		return dateTime != null && getOptionalEnd().map(dateTime::isAfter).orElse(false);
	}

	// Determine whether the given time is after this Timespan
	public boolean isAfter(@NotNull LocalTime time) {
		return time != null && isAfter(time.atDate(LocalDate.now()));
	}

	// Determine whether the given year is before this Timespan
	public boolean isBefore(@NotNull Year year) {
		return year != null && isBefore(year.atMonth(Month.DECEMBER));
	}

	// Determine whether the given year/month is before this Timespan
	public boolean isBefore(@NotNull YearMonth yearMonth) {
		return yearMonth != null && isBefore(yearMonth.atEndOfMonth());
	}

	// Determine whether the given date occurs is this Timespan
	public boolean isBefore(@NotNull LocalDate date) {
		return date != null && isBefore(date.atTime(LocalTime.MAX));
	}

	// Determine whether the given date/time is before this Timespan
	public boolean isBefore(@NotNull LocalDateTime dateTime) {
		return dateTime != null && getOptionalBegin().map(dateTime::isBefore).orElse(false);
	}

	// Determine whether the given time occurs is this Timespan
	public boolean isBefore(@NotNull LocalTime time) {
		return time != null && isBefore(time.atDate(LocalDate.now()));
	}

	// Determine whether the entire year is during this Timespan
	public boolean isDuring(@NotNull Year year) {
		return year != null && isDuring(year.atMonth(Month.JANUARY)) && isDuring(year.atMonth(Month.DECEMBER));
	}

	// Determine whether the entire month of the year is during this Timespan
	public boolean isDuring(@NotNull YearMonth yearMonth) {
		return yearMonth != null && isDuring(yearMonth.atDay(1)) && isDuring(yearMonth.atEndOfMonth());
	}

	// Determine whether the entire day is during this Timespan
	public boolean isDuring(@NotNull LocalDate date) {
		return date != null && isDuring(date.atStartOfDay()) && isDuring(date.atTime(LocalTime.MAX));
	}

	// Determine whether the given date/time (instant in time) is during this Timespan
	public boolean isDuring(@NotNull LocalDateTime dateTime) {

		return dateTime != null
			&& getOptionalBegin().map(begin -> !dateTime.isBefore(begin)).orElse(true)
			&& getOptionalEnd().map(end -> !dateTime.isAfter(end)).orElse(true);
	}

	// Determine whether the given time occurs is this Timespan
	public boolean isDuring(@NotNull LocalTime time) {
		return time != null && isDuring(time.atDate(LocalDate.now()));
	}

	public boolean hasBeginning() {
		return getOptionalBegin().isPresent();
	}

	public boolean hasEnding() {
		return getOptionalEnd().isPresent();
	}

	public boolean hasNoBeginning() {
		return getOptionalBegin().isEmpty();
	}

	public boolean hasNoEnding() {
		return getOptionalEnd().isEmpty();
	}

	public boolean isAllEternity() {
		return hasNoBeginning() && hasNoEnding();
	}

	public boolean isFinite() {
		return hasBeginning() && hasEnding();
	}

	public boolean isInfinite() {
		return hasNoBeginning() || hasNoEnding();
	}

	public boolean isOpenEnded() {
		return hasNoBeginning() || hasNoEnding();
	}

	@SuppressWarnings("all")
	public boolean isContained(@NotNull Timespan timespan) {

		boolean containedBeginning = this.hasNoBeginning() || isDuring(timespan.getBegin());
		boolean containedEnding = this.hasNoEnding() || isDuring(timespan.getEnd());
		boolean allTrue = !timespan.isAllEternity() && containedBeginning && containedEnding;

		return timespan != null && (this.isAllEternity() || allTrue);
	}

	public boolean isContainedBy(@NotNull Timespan timespan) {
		return timespan != null && timespan.isContained(this);
	}

	public boolean isOverlapping(@NotNull Timespan timespan) {

		boolean anyTrue = this.isAllEternity() || timespan.isAllEternity()
			|| (timespan.hasNoBeginning() && isDuring(timespan.getEnd()))
			|| (timespan.hasNoEnding() && isDuring(timespan.getBegin()));

		return timespan != null && anyTrue;
	}

	public boolean isNotOverlapping(@NotNull Timespan timespan) {
		return !isOverlapping(timespan);
	}

	@Override
	@SuppressWarnings("all")
	public int compareTo(@NotNull Timespan that) {

		return this.getBegin() == null ? -1
			: that.getBegin() == null ? 1
			: this.getBegin().isBefore(that.getBegin()) ? -1
			: that.getEnd() == null ? -1
			: this.getEnd() == null ? 1
			: this.getEnd().isBefore(that.getEnd()) ? -1
			: 0;
	}

	@Override
	public String toString() {

		String beginningString = getOptionalBegin()
			.map(begin -> begin.format(DATE_TIME_FORMATTER))
			.orElse(BEGINNING_OF_TIME);

		String endingString = getOptionalEnd()
			.map(end -> end.format(DATE_TIME_FORMATTER))
			.orElse(END_OF_TIME);

		return TIMESPAN_TO_STRING.formatted(beginningString, endingString);
	}

	public static class WithTo {

		private final LocalDateTime beginning;

		protected WithTo(@NotNull LocalDateTime beginning) {
			this.beginning = ObjectUtils.requireObject(beginning, "Begin date/time is required");
		}

		protected @NotNull LocalDateTime getBeginning() {
			return this.beginning;
		}

		@Dsl
		public @NotNull Builder to(@Nullable Year ending) {
			return to(ending != null ? ending.atMonth(Month.DECEMBER) : null);
		}

		@Dsl
		public @NotNull Builder to(@Nullable YearMonth ending) {
			return to(ending != null ? ending.atEndOfMonth() : null);
		}

		@Dsl
		public @NotNull Builder to(@Nullable LocalDate ending) {
			return to(ending != null ? ending.atTime(LocalTime.MAX) : null);
		}

		@Dsl
		public @NotNull Builder to(@Nullable LocalDateTime ending) {
			return new Builder(getBeginning(), ending);
		}

		@Dsl
		public @NotNull Builder to(@Nullable LocalTime ending) {
			return to(ending != null ? ending.atDate(LocalDate.now()) : null);
		}
	}

	public static class Builder implements org.cp.elements.lang.Builder<Timespan> {

		private final LocalDateTime beginning;
		private final LocalDateTime ending;

		protected Builder(@NotNull LocalDateTime beginning, @NotNull LocalDateTime ending) {
			this.beginning = ObjectUtils.requireObject(beginning, "Begin date/time is required");
			this.ending = ObjectUtils.requireObject(ending, "End date/time is required");
		}

		protected @NotNull LocalDateTime getBeginning() {
			return this.beginning;
		}

		protected @NotNull LocalDateTime getEnding() {
			return this.ending;
		}

		@Override
		public Timespan build() {
			return new Timespan(getBeginning(), getEnding());
		}
	}
}
