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
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.text.CharacterIterator;
import java.text.StringCharacterIterator;
import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.IntStream;

import org.junit.jupiter.api.Test;

import org.cp.elements.util.ArrayUtils;

/**
 * Unit Tests for {@link StringUtils}.
 *
 * @author John J. Blum
 * @see java.lang.String
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.StringUtils
 * @since 1.0.0
 */
public class StringUtilsTests {

  private <T> Iterable<T> toIterable(Iterator<T> iterator) {
    return () -> iterator;
  }

  @Test
  public void capitalizeCharacter() {
    assertThat(StringUtils.capitalize("x")).isEqualTo("X");
  }

  @Test
  public void capitalizeNumberOrSymbol() {

    assertThat(StringUtils.capitalize("1")).isEqualTo("1");
    assertThat(StringUtils.capitalize("$")).isEqualTo("$");
  }

  @Test
  public void capitalizeString() {
    assertThat(StringUtils.capitalize("test")).isEqualTo("Test");
  }

  @Test
  public void capitalizeInvalidString() {

    Arrays.asList("  ", "", null).forEach(invalidString ->
      assertThatIllegalArgumentException()
        .isThrownBy(() -> StringUtils.capitalize(invalidString))
        .withMessage("Value [%s] is required", invalidString)
        .withNoCause());
  }

  @Test
  public void capitalizeUpperCaseString() {
    assertThat(StringUtils.capitalize("TEST")).isEqualTo("TEST");
  }

  @Test
  public void concatWithStrings() {

    assertThat(StringUtils.concat("one", "two", "three")).isEqualTo("one, two, three");
    assertThat(StringUtils.concat(new String[] { "org", "cp", "elements" }, StringUtils.DOT_SEPARATOR)).isEqualTo(
      "org.cp.elements");
    assertThat(StringUtils.concat("void", null, "nil")).isEqualTo("void, null, nil");
  }

  @Test
  public void concatWithNonValueStrings() {

    Arrays.asList("  ", "").forEach(string ->
      assertThat(StringUtils.concat(string)).isEqualTo(string));
  }

  @Test
  public void testConcatWithNullStringArray() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> StringUtils.concat((String[]) null))
      .withMessage("An array of String values to concatenate is required")
      .withNoCause();
  }

  @Test
  public void testContains() {

    assertThat(StringUtils.contains("test", "test")).isTrue();
    assertThat(StringUtils.contains("testing", "test")).isTrue();
    assertThat(StringUtils.contains("tested", "test")).isTrue();
    assertThat(StringUtils.contains("null", "null")).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void testContainsWithNonContainedText() {

    assertThat(StringUtils.contains("null", "nil")).isFalse();
    assertThat(StringUtils.contains("test", "TEST")).isFalse();
    assertThat(StringUtils.contains("TEST", "test")).isFalse();
    assertThat(StringUtils.contains("test", "testing")).isFalse();
    assertThat(StringUtils.contains(null, "test")).isFalse();
    assertThat(StringUtils.contains(null, "null")).isFalse();
    assertThat(StringUtils.contains("null", null)).isFalse();
  }

  @Test
  public void testContainsDigits() {

    assertThat(StringUtils.containsDigits("0123456789")).isTrue();
    assertThat(StringUtils.containsDigits("Ol2E4SG7B9")).isTrue();
    assertThat(StringUtils.containsDigits("!1#$%*")).isTrue();
    assertThat(StringUtils.containsDigits("$100.00")).isTrue();
    assertThat(StringUtils.containsDigits("50%")).isTrue();
    assertThat(StringUtils.containsDigits("(503) 555-1234")).isTrue();
    assertThat(StringUtils.containsDigits("00")).isTrue();
    assertThat(StringUtils.containsDigits("0")).isTrue();
  }

  @Test
  public void testContainsNoDigits() {

    assertThat(StringUtils.containsDigits("abcdefghijklmnopqrstuvwxyz")).isFalse();
    assertThat(StringUtils.containsDigits("lOlOl")).isFalse();
    assertThat(StringUtils.containsDigits("$###.##")).isFalse();
    assertThat(StringUtils.containsDigits("one")).isFalse();
    assertThat(StringUtils.containsDigits("  ")).isFalse();
    assertThat(StringUtils.containsDigits("")).isFalse();
    assertThat(StringUtils.containsDigits(null)).isFalse();
  }

  @Test
  public void testContainsLetters() {

    assertThat(StringUtils.containsLetters("abcdefghijklmnopqrstuvwxyz")).isTrue();
    assertThat(StringUtils.containsLetters("lOlOl")).isTrue();
    assertThat(StringUtils.containsLetters("l0l0l")).isTrue();
    assertThat(StringUtils.containsLetters("1O1O1")).isTrue();
    assertThat(StringUtils.containsLetters("O0")).isTrue();
    assertThat(StringUtils.containsLetters("O")).isTrue();
    assertThat(StringUtils.containsLetters("XyZ")).isTrue();
    assertThat(StringUtils.containsLetters("ABC123")).isTrue();
    assertThat(StringUtils.containsLetters("fifty%")).isTrue();
  }

  @Test
  public void testContainsNoLetters() {

    assertThat(StringUtils.containsLetters("0123456789")).isFalse();
    assertThat(StringUtils.containsLetters("10101")).isFalse();
    assertThat(StringUtils.containsLetters("@554013")).isFalse();
    assertThat(StringUtils.containsLetters("$$$")).isFalse();
    assertThat(StringUtils.containsLetters("  ")).isFalse();
    assertThat(StringUtils.containsLetters("")).isFalse();
    assertThat(StringUtils.containsLetters(null)).isFalse();
  }

  @Test
  public void testContainsWhitespace() {

    assertThat(StringUtils.containsWhitespace(" ")).isTrue();
    assertThat(StringUtils.containsWhitespace("   ")).isTrue();
    assertThat(StringUtils.containsWhitespace(" text ")).isTrue();
    assertThat(StringUtils.containsWhitespace(" test")).isTrue();
    assertThat(StringUtils.containsWhitespace("test ")).isTrue();
    assertThat(StringUtils.containsWhitespace("t e s t")).isTrue();
    assertThat(StringUtils.containsWhitespace("Mc Fly")).isTrue();
  }

  @Test
  public void testContainsNoWhitespace() {

    assertThat(StringUtils.containsWhitespace("")).isFalse();
    assertThat(StringUtils.containsWhitespace(" text ".trim())).isFalse();
    assertThat(StringUtils.containsWhitespace(" test".trim())).isFalse();
    assertThat(StringUtils.containsWhitespace("test ".trim())).isFalse();
    assertThat(StringUtils.containsWhitespace("t_e_s_t".trim())).isFalse();
    assertThat(StringUtils.containsWhitespace("McFly".trim())).isFalse();
  }

  @Test
  public void defaultIfBlankWithBlankValues() {

    assertThat(StringUtils.defaultIfBlank(null, "test", "testing", "tested")).isEqualTo("test");
    assertThat(StringUtils.defaultIfBlank("", null, "<empty/>", "  ", "tested")).isEqualTo("<empty/>");
    assertThat(StringUtils.defaultIfBlank("  ", null, "", "  ", "___")).isEqualTo("___");
  }

  @Test
  public void defaultIfBlankWithNonBlankValues() {

    assertThat(StringUtils.defaultIfBlank("test", "1", "2", "3")).isEqualTo("test");
    assertThat(StringUtils.defaultIfBlank("-1", "1", "2", "3")).isEqualTo("-1");
    assertThat(StringUtils.defaultIfBlank("0", "1", "2", "3")).isEqualTo("0");
    assertThat(StringUtils.defaultIfBlank("<empty/>", "1", "2", "3")).isEqualTo("<empty/>");
    assertThat(StringUtils.defaultIfBlank("___", "1", "2", "3")).isEqualTo("___");
    assertThat(StringUtils.defaultIfBlank("-", "1", "2", "3")).isEqualTo("-");
    assertThat(StringUtils.defaultIfBlank("nil", null, null, null)).isEqualTo("nil");
    assertThat(StringUtils.defaultIfBlank("null", null, null, null)).isEqualTo("null");
  }

  @Test
  public void testEqualsIgnoreCase() {

    assertThat(StringUtils.equalsIgnoreCase("test", "test")).isTrue();
    assertThat(StringUtils.equalsIgnoreCase("test", "TEST")).isTrue();
    assertThat(StringUtils.equalsIgnoreCase("titlecase", "Titlecase")).isTrue();
    assertThat(StringUtils.equalsIgnoreCase("null", "null")).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void testEqualsIgnoreCaseWithUnequalStrings() {

    assertThat(StringUtils.equalsIgnoreCase("test", "testing")).isFalse();
    assertThat(StringUtils.equalsIgnoreCase("seam", "seem")).isFalse();
    assertThat(StringUtils.equalsIgnoreCase("null", null)).isFalse();
    assertThat(StringUtils.equalsIgnoreCase(null, "null")).isFalse();
  }

  @Test
  public void getDigitsFromDigitStringsIsCorrect() {

    assertThat(StringUtils.getDigits("123")).isEqualTo("123");
    assertThat(StringUtils.getDigits("abc123")).isEqualTo("123");
    assertThat(StringUtils.getDigits("l0l0l")).isEqualTo("00");
    assertThat(StringUtils.getDigits("1O1O1")).isEqualTo("111");
    assertThat(StringUtils.getDigits("n0a1bc2defg4hijklmno8p0qrstuvwxzy")).isEqualTo("012480");
    assertThat(StringUtils.getDigits("localhost:12480")).isEqualTo("12480");
    assertThat(StringUtils.getDigits("10.234.77.120:12480")).isEqualTo("102347712012480");
    assertThat(StringUtils.getDigits("$100.50")).isEqualTo("10050");
    assertThat(StringUtils.getDigits("50%")).isEqualTo("50");
    assertThat(StringUtils.getDigits("(503) 555-1234 x5")).isEqualTo("50355512345");
  }

  @Test
  public void getDigitsFromNonDigitStrings() {

    assertThat(StringUtils.getDigits("abc")).isEqualTo("");
    assertThat(StringUtils.getDigits("lOlOl")).isEqualTo("");
    assertThat(StringUtils.getDigits("oneTwoThree")).isEqualTo("");
    assertThat(StringUtils.getDigits("$###.##")).isEqualTo("");
    assertThat(StringUtils.getDigits(".lS%")).isEqualTo("");
    assertThat(StringUtils.getDigits("  ")).isEqualTo("");
    assertThat(StringUtils.getDigits("")).isEqualTo("");
  }

  @Test
  public void getDigitsFromNullIsNullSafe() {
    assertThat(StringUtils.getDigits(null)).isEmpty();
  }

  @Test
  public void getLettersFromLetterStrings() {

    assertThat(StringUtils.getLetters("abc")).isEqualTo("abc");
    assertThat(StringUtils.getLetters("abc123")).isEqualTo("abc");
    assertThat(StringUtils.getLetters("1A2BC3")).isEqualTo("ABC");
    assertThat(StringUtils.getLetters("l0l0l")).isEqualTo("lll");
    assertThat(StringUtils.getLetters("1O1O1")).isEqualTo("OO");
    assertThat(StringUtils.getLetters("localhost:12480")).isEqualTo("localhost");
    assertThat(StringUtils.getLetters("n0a1bc2defg4hijklmno8p0qrstuvwxyz")).isEqualTo("nabcdefghijklmnopqrstuvwxyz");
    assertThat(StringUtils.getLetters("(503) 555-1234 x520")).isEqualTo("x");
  }

  @Test
  public void getLettersFromNonLetterStrings() {

    assertThat(StringUtils.getLetters("123")).isEqualTo("");
    assertThat(StringUtils.getLetters("10101")).isEqualTo("");
    assertThat(StringUtils.getLetters("8007")).isEqualTo("");
    assertThat(StringUtils.getLetters("$100.50")).isEqualTo("");
    assertThat(StringUtils.getLetters("50%")).isEqualTo("");
    assertThat(StringUtils.getLetters("@$$4013")).isEqualTo("");
    assertThat(StringUtils.getLetters("  ")).isEqualTo("");
    assertThat(StringUtils.getLetters("")).isEqualTo("");
  }

  @Test
  public void getLettersFromNullIsNullSafe() {
    assertThat(StringUtils.getLetters(null)).isEmpty();
  }

  @Test
  public void getSpacesIsCorrect() {

    assertThat(StringUtils.getSpaces(0)).isEqualTo("");
    assertThat(StringUtils.getSpaces(1)).isEqualTo(" ");
    assertThat(StringUtils.getSpaces(2)).isEqualTo("  ");
    assertThat(StringUtils.getSpaces(3)).isEqualTo("   ");
    assertThat(StringUtils.getSpaces(4)).isEqualTo("    ");
    assertThat(StringUtils.getSpaces(5)).isEqualTo("     ");
    assertThat(StringUtils.getSpaces(6)).isEqualTo("      ");
    assertThat(StringUtils.getSpaces(7)).isEqualTo("       ");
    assertThat(StringUtils.getSpaces(8)).isEqualTo("        ");
    assertThat(StringUtils.getSpaces(9)).isEqualTo("         ");
    assertThat(StringUtils.getSpaces(10)).isEqualTo("          ");
    assertThat(StringUtils.getSpaces(11)).isEqualTo("           ");
    assertThat(StringUtils.getSpaces(12)).isEqualTo("            ");
    assertThat(StringUtils.getSpaces(13)).isEqualTo("             ");
    assertThat(StringUtils.getSpaces(14)).isEqualTo("              ");
    assertThat(StringUtils.getSpaces(15)).isEqualTo("               ");
    assertThat(StringUtils.getSpaces(16)).isEqualTo("                ");
    assertThat(StringUtils.getSpaces(17)).isEqualTo("                 ");
    assertThat(StringUtils.getSpaces(18)).isEqualTo("                  ");
    assertThat(StringUtils.getSpaces(19)).isEqualTo("                   ");
    assertThat(StringUtils.getSpaces(20)).isEqualTo("                    ");
    assertThat(StringUtils.getSpaces(21)).isEqualTo("                     ");
    assertThat(StringUtils.getSpaces(51)).isEqualTo("                                                   ");
    assertThat(StringUtils.getSpaces(99)).isEqualTo(
      "                                                                                                   ");
  }

  @Test
  public void testGetSpacesWithInvalidNumber() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> StringUtils.getSpaces(-1))
      .withMessage("The number [-1] of desired spaces must be greater than equal to 0")
      .withNoCause();
  }

  @Test
  public void testHasText() {

    assertThat(StringUtils.hasText("test")).isTrue();
    assertThat(StringUtils.hasText("0123456789")).isTrue();
    assertThat(StringUtils.hasText("$@$!")).isTrue();
    assertThat(StringUtils.hasText("X")).isTrue();
    assertThat(StringUtils.hasText("-")).isTrue();
    assertThat(StringUtils.hasText("null")).isTrue();
    assertThat(StringUtils.hasText("nill")).isTrue();
    assertThat(StringUtils.hasText("empty")).isTrue();
    assertThat(StringUtils.hasText("blank")).isTrue();
    assertThat(StringUtils.hasText("_")).isTrue();
  }

  @Test
  public void testHasTextWithNoText() {

    assertThat(StringUtils.hasText(null)).isFalse();
    assertThat(StringUtils.hasText("")).isFalse();
    assertThat(StringUtils.hasText(" ")).isFalse();
    assertThat(StringUtils.hasText("   ")).isFalse();
  }

  @Test
  public void testIndexOf() {

    assertThat(StringUtils.indexOf(null, "test")).isEqualTo(-1);
    assertThat(StringUtils.indexOf("test", null)).isEqualTo(-1);
    assertThat(StringUtils.indexOf("", " ")).isEqualTo(-1);
    assertThat(StringUtils.indexOf("", "text")).isEqualTo(-1);
    assertThat(StringUtils.indexOf("null", "nil")).isEqualTo(-1);
    assertThat(StringUtils.indexOf("", "")).isEqualTo(0);
    assertThat(StringUtils.indexOf("  ", "")).isEqualTo(0);
    assertThat(StringUtils.indexOf("  ", " ")).isEqualTo(0);
    assertThat(StringUtils.indexOf("null", "null")).isEqualTo(0);
    assertThat(StringUtils.indexOf("This is example text!", " ")).isEqualTo(4);
    assertThat(StringUtils.indexOf("This is example text!", "ex")).isEqualTo(8);
    assertThat(StringUtils.indexOf("This is example text!", "text")).isEqualTo(16);
    assertThat(StringUtils.indexOf("This is example text!", "ext")).isEqualTo(17);
    assertThat(StringUtils.indexOf("This is example text!", "test")).isEqualTo(-1);
  }

  @Test
  public void testIsBlank() {

    assertThat(StringUtils.isBlank(null)).isTrue();
    assertThat(StringUtils.isBlank("")).isTrue();
    assertThat(StringUtils.isBlank(" ")).isTrue();
    assertThat(StringUtils.isBlank("   ")).isTrue();
    assertThat(StringUtils.isBlank("\0")).isTrue();
  }

  @Test
  public void testIsBlankWithNonBlankStrings() {

    assertThat(StringUtils.isBlank("_")).isFalse();
    assertThat(StringUtils.isBlank("___")).isFalse();
    assertThat(StringUtils.isBlank("null")).isFalse();
    assertThat(StringUtils.isBlank("nil")).isFalse();
    assertThat(StringUtils.isBlank("false")).isFalse();
    assertThat(StringUtils.isBlank(".")).isFalse();
    assertThat(StringUtils.isBlank("0")).isFalse();
    assertThat(StringUtils.isBlank("0.0")).isFalse();
    assertThat(StringUtils.isBlank("space")).isFalse();
    assertThat(StringUtils.isBlank("empty")).isFalse();
    assertThat(StringUtils.isBlank("blank")).isFalse();
    assertThat(StringUtils.isBlank("test")).isFalse();
  }

  @Test
  public void testIsDigits() {

    assertThat(StringUtils.isDigits("012")).isTrue();
    assertThat(StringUtils.isDigits("123")).isTrue();
    assertThat(StringUtils.isDigits("0123456789")).isTrue();
    assertThat(StringUtils.isDigits("112358")).isTrue();
    assertThat(StringUtils.isDigits("012480")).isTrue();
    assertThat(StringUtils.isDigits("0122444488888888")).isTrue();
  }

  @Test
  public void testIsDigitsWithNonDigitStrings() {

    assertThat(StringUtils.isDigits(null)).isFalse();
    assertThat(StringUtils.isDigits("")).isFalse();
    assertThat(StringUtils.isDigits("  ")).isFalse();
    assertThat(StringUtils.isDigits("abc")).isFalse();
    assertThat(StringUtils.isDigits("abc123")).isFalse();
    assertThat(StringUtils.isDigits("l0l0l")).isFalse();
    assertThat(StringUtils.isDigits("B00B")).isFalse();
    assertThat(StringUtils.isDigits("$1024.64")).isFalse();
    assertThat(StringUtils.isDigits("50%")).isFalse();
    assertThat(StringUtils.isDigits("(503) 555-0123")).isFalse();
    assertThat(StringUtils.isDigits("####")).isFalse();
    assertThat(StringUtils.isDigits(" 0123 ")).isFalse();
  }

  @Test
  public void isEmptyWithEmptyStringReturnsTrue() {
    assertThat(StringUtils.isEmpty("")).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void testIsEmptyWithNonEmptyStrings() {

    assertThat(StringUtils.isEmpty(null)).isFalse();
    assertThat(StringUtils.isEmpty(" ")).isFalse();
    assertThat(StringUtils.isEmpty("   ")).isFalse();
    assertThat(StringUtils.isEmpty("false")).isFalse();
    assertThat(StringUtils.isEmpty("\0")).isFalse();
    assertThat(StringUtils.isEmpty("0")).isFalse();
    assertThat(StringUtils.isEmpty("0.0")).isFalse();
    assertThat(StringUtils.isEmpty("-")).isFalse();
    assertThat(StringUtils.isEmpty("_")).isFalse();
    assertThat(StringUtils.isEmpty("x")).isFalse();
    assertThat(StringUtils.isEmpty("empty")).isFalse();
  }

  @Test
  public void testIsLetters() {

    assertThat(StringUtils.isLetters("abcdefghijklmnopqrstuvwxyz")).isTrue();
    assertThat(StringUtils.isLetters("ABC")).isTrue();
    assertThat(StringUtils.isLetters("lOlO")).isTrue();
    assertThat(StringUtils.isLetters("oneTwoThree")).isTrue();
  }

  @Test
  public void testIsLettersWithNonLetterStrings() {

    assertThat(StringUtils.isLetters(null)).isFalse();
    assertThat(StringUtils.isLetters("")).isFalse();
    assertThat(StringUtils.isLetters("  ")).isFalse();
    assertThat(StringUtils.isLetters("123")).isFalse();
    assertThat(StringUtils.isLetters("abc123")).isFalse();
    assertThat(StringUtils.isLetters("A1BC23")).isFalse();
    assertThat(StringUtils.isLetters("$oneHundred.fifty")).isFalse();
    assertThat(StringUtils.isLetters("fifty%")).isFalse();
    assertThat(StringUtils.isLetters("@$")).isFalse();
    assertThat(StringUtils.isLetters("$0$")).isFalse();
    assertThat(StringUtils.isLetters("localhost:8080")).isFalse();
  }

  @Test
  public void testLastIndexOf() {

    assertThat(StringUtils.lastIndexOf(null, "test")).isEqualTo(-1);
    assertThat(StringUtils.lastIndexOf("test", null)).isEqualTo(-1);
    assertThat(StringUtils.lastIndexOf("", " ")).isEqualTo(-1);
    assertThat(StringUtils.lastIndexOf("", "text")).isEqualTo(-1);
    assertThat(StringUtils.lastIndexOf("null", "nil")).isEqualTo(-1);
    assertThat(StringUtils.lastIndexOf("", "")).isEqualTo(0);
    assertThat(StringUtils.lastIndexOf("  ", "")).isEqualTo(2);
    assertThat(StringUtils.lastIndexOf("  ", " ")).isEqualTo(1);
    assertThat(StringUtils.lastIndexOf("null", "null")).isEqualTo(0);
    assertThat(StringUtils.lastIndexOf("This is example text!", " ")).isEqualTo(15);
    assertThat(StringUtils.lastIndexOf("This is example text!", "ex")).isEqualTo(17);
    assertThat(StringUtils.lastIndexOf("This is example text!", "text")).isEqualTo(16);
    assertThat(StringUtils.lastIndexOf("This is example text!", "exam")).isEqualTo(8);
    assertThat(StringUtils.lastIndexOf("This is example text!", "test")).isEqualTo(-1);
  }

  @Test
  public void testLength() {

    assertThat(StringUtils.length(null)).isEqualTo(0);
    assertThat(StringUtils.length("")).isEqualTo(0);
    assertThat(StringUtils.length(" ")).isEqualTo(1);
    assertThat(StringUtils.length("\0")).isEqualTo(1);
    assertThat(StringUtils.length("   ")).isEqualTo(3);
    assertThat(StringUtils.length("nil")).isEqualTo(3);
    assertThat(StringUtils.length("null")).isEqualTo(4);
    assertThat(StringUtils.length("test")).isEqualTo(4);
  }

  @Test
  public void padWithCharacter() {

    assertThat(StringUtils.pad("", 'x', 5)).isEqualTo("xxxxx");
    assertThat(StringUtils.pad(" ", 'x', 5)).isEqualTo(" xxxx");
    assertThat(StringUtils.pad("   ", 'x', 5)).isEqualTo("   xx");
    assertThat(StringUtils.pad("xxxxx", 'x', 5)).isEqualTo("xxxxx");
    assertThat(StringUtils.pad("x", 'x', 5)).isEqualTo("xxxxx");
    assertThat(StringUtils.pad("xxX", 'x', 5)).isEqualTo("xxXxx");
    assertThat(StringUtils.pad("xxxxx", 'X', 10)).isEqualTo("xxxxxXXXXX");
  }

  @Test
  public void padWithIllegalLength() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> StringUtils.pad("test", -10))
      .withMessage("[-10] must be greater than equal to 0")
      .withNoCause();
  }

  @Test
  public void padWithNull() {

    assertThat(StringUtils.pad(null, 0)).isNull();
    assertThat(StringUtils.pad(null, 2)).isEqualTo("  ");
    assertThat(StringUtils.pad(null, 'x', 5)).isEqualTo("xxxxx");
  }

  @Test
  public void padWithSpaces() {

    assertThat(StringUtils.pad("test", ' ', 10)).isEqualTo("test      ");
    assertThat(StringUtils.pad("test", ' ', 5)).isEqualTo("test ");
    assertThat(StringUtils.pad("test", ' ', 4)).isEqualTo("test");
    assertThat(StringUtils.pad("test", ' ', 1)).isEqualTo("test");
    assertThat(StringUtils.pad("x", ' ', 0)).isEqualTo("x");
    assertThat(StringUtils.pad("null", ' ', 2)).isEqualTo("null");
    assertThat(StringUtils.pad("nil", ' ', 2)).isEqualTo("nil");
    assertThat(StringUtils.pad("", ' ', 2)).isEqualTo("  ");
    assertThat(StringUtils.pad(" ", ' ', 2)).isEqualTo("  ");
    assertThat(StringUtils.pad("   ", ' ', 2)).isEqualTo("   ");
  }

  @Test
  public void requireText() {
    assertThat(StringUtils.requireText("mock", "This is a test!")).isEqualTo("mock");
  }

  @Test
  public void requireTextWithNoText() {

    Arrays.stream(ArrayUtils.asArray("  ", "", null)).forEach(text ->
      assertThatIllegalArgumentException()
        .isThrownBy(() -> StringUtils.requireText(text, "String [%s] must have {1}", text, "text"))
        .withMessage("String [%s] must have text", text)
        .withNoCause());
  }

  @Test
  public void requireTextWithNoTextDefaultsMessageArguments() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> StringUtils.requireText("  ", "String [%s] must not be null or empty"))
      .withMessage("String [  ] must not be null or empty")
      .withNoCause();
  }

  @Test
  public void replaceAllModifiesString() {

    assertThat(StringUtils.replaceAll("testing", "test", "mock")).isEqualTo("mocking");
    assertThat(StringUtils.replaceAll("test", "test", "mock")).isEqualTo("mock");
    assertThat(StringUtils.replaceAll("xxx", "x", "X")).isEqualTo("XXX");
    assertThat(StringUtils.replaceAll("xxx", "x", "xxx")).isEqualTo("xxxxxxxxx");
    assertThat(StringUtils.replaceAll("mmm", "m", "w")).isEqualTo("www");
    assertThat(StringUtils.replaceAll("//absolute//path//to//file.txt", "//", "/"))
      .isEqualTo("/absolute/path/to/file.txt");
    assertThat(StringUtils.replaceAll("///absolute/////path/to//file.txt", "/+", "/"))
      .isEqualTo("/absolute/path/to/file.txt");
    assertThat(StringUtils.replaceAll("//////////", "/+", "/")).isEqualTo("/");
  }

  @Test
  public void replaceAllReturnsStringUnmodified() {

    assertThat(StringUtils.replaceAll("test", "testing", "mock")).isEqualTo("test");
    assertThat(StringUtils.replaceAll("test", "tset", "mock")).isEqualTo("test");
    assertThat(StringUtils.replaceAll("v", "x", "X")).isEqualTo("v");
    assertThat(StringUtils.replaceAll("v", "vv", "x")).isEqualTo("v");
    assertThat(StringUtils.replaceAll("www", "W", "m")).isEqualTo("www");
    assertThat(StringUtils.replaceAll("XOXO", "0", "x")).isEqualTo("XOXO");
  }

  @Test
  public void replaceAllReturnsNullWithNullValue() {
    assertThat(StringUtils.replaceAll(null, "x", "X")).isNull();
  }

  @Test
  public void replaceAllReturnsValueWithNullReplacement() {
    assertThat(StringUtils.replaceAll("test", "x", null)).isEqualTo("test");
  }

  @Test
  public void replaceAllReturnsValueWithNullPattern() {
    assertThat(StringUtils.replaceAll("test", null, "X")).isEqualTo("test");
  }

  @Test
  public void replaceAllGuardsAgainsOutOfMemoryAndStackOverflowErrors() {
    StringUtils.replaceAll("x", "x", "xx");
  }

  @Test
  public void stringReplaceAllIsNotComplete() {

    assertThat("ttesttting".replaceAll("tt", "t")).isEqualTo("testting");
    assertThat("tttesttttting".replaceAll("t+", "t")).isEqualTo("testing");
  }

  @Test
  public void testSingleSpaceObjects() {

    assertThat(StringUtils.singleSpaceObjects(true, false)).isEqualTo("true false");
    assertThat(StringUtils.singleSpaceObjects('t', 'e', 's', 't')).isEqualTo("t e s t");
    assertThat(StringUtils.singleSpaceObjects(1, 0, 1)).isEqualTo("1 0 1");
    assertThat(StringUtils.singleSpaceObjects(3.14159d)).isEqualTo("3.14159");
    assertThat(StringUtils.singleSpaceObjects(false, '\0', 'c', 0, 3.14159d, "mock")).isEqualTo(
      "false \0 c 0 3.14159 mock");
    assertThat(StringUtils.singleSpaceObjects("test")).isEqualTo("test");
    assertThat(StringUtils.singleSpaceObjects("  null ")).isEqualTo("null");
    assertThat(StringUtils.singleSpaceObjects("  this", "  is ", "a", "    test!   ")).isEqualTo(
      "this   is  a     test!");
    assertThat(StringUtils.singleSpaceObjects("this", "is", "a", "test", "with",
      "a", null, "value")).isEqualTo("this is a test with a null value");
  }

  @Test
  public void testSingleSpaceString() {

    assertThat(StringUtils.singleSpaceString(" This is  a          test!  ")).isEqualTo("This is a test!");
    assertThat(StringUtils.singleSpaceString("This_is_another_test!")).isEqualTo("This_is_another_test!");
    assertThat(StringUtils.singleSpaceString("null")).isEqualTo("null");
  }

  @Test
  public void singleSpaceStringWithInvalidString() {

    Arrays.asList("  ", "", null).forEach(invalidString ->
        assertThatIllegalArgumentException()
          .isThrownBy(() -> StringUtils.singleSpaceString(invalidString))
          .withMessage("String value must contain text")
          .withNoCause());
  }

  @Test
  public void testToCharArray() {

    char[] charArray = StringUtils.toCharArray(null);

    assertThat(charArray).isNotNull();
    assertThat(charArray.length).isEqualTo(0);

    charArray = StringUtils.toCharArray("abc");

    assertThat(charArray).isNotNull();
    assertThat(charArray[0]).isEqualTo('a');
    assertThat(charArray[1]).isEqualTo('b');
    assertThat(charArray[2]).isEqualTo('c');
  }

  @Test
  public void toIteratorFromCharacterIterator() {

    AtomicInteger index = new AtomicInteger(0);

    CharacterIterator mockCharacterIterator = mock(CharacterIterator.class);

    doReturn(3).when(mockCharacterIterator).getEndIndex();

    doAnswer(invocation -> index.get()).when(mockCharacterIterator).getIndex();

    doAnswer(invocation -> {

      index.set(invocation.getArgument(0));

      return switch (index.get()) {
        case 0 -> 'A';
        case 1 -> 'B';
        case 2 -> 'C';
        default -> CharacterIterator.DONE;
      };
    }).when(mockCharacterIterator).setIndex(anyInt());

    Iterator<Character> iterator = StringUtils.toIterator(mockCharacterIterator);

    assertThat(iterator).isNotNull();
    assertThat(iterator).hasNext();
    assertThat(toIterable(iterator)).containsExactly('A', 'B', 'C');
    assertThat(iterator).isExhausted();

    verify(mockCharacterIterator, times(6)).getIndex();
    verify(mockCharacterIterator, times(6)).getEndIndex();

    IntStream.range(0, 3).forEach(idx ->
      verify(mockCharacterIterator, times(1)).setIndex(eq(idx)));

    verifyNoMoreInteractions(mockCharacterIterator);
  }

  @Test
  public void toIteratorFromEmptyCharacterIteratorThrowsNoSuchElementExceptionOnNext() {

    CharacterIterator mockCharacterIterator = mock(CharacterIterator.class);

    doReturn(0).when(mockCharacterIterator).getIndex();
    doReturn(0).when(mockCharacterIterator).getEndIndex();
    doReturn(CharacterIterator.DONE).when(mockCharacterIterator).setIndex(anyInt());

    Iterator<Character> characterIterator = StringUtils.toIterator(mockCharacterIterator);

    assertThat(characterIterator).isNotNull();
    assertThat(characterIterator).isExhausted();

    assertThatExceptionOfType(NoSuchElementException.class)
      .isThrownBy(characterIterator::next)
      .withMessage("No more characters available")
      .withNoCause();

    verify(mockCharacterIterator, times(1)).getIndex();
    verify(mockCharacterIterator, times(1)).getEndIndex();
    verify(mockCharacterIterator, times(1)).setIndex(eq(0));
    verifyNoMoreInteractions(mockCharacterIterator);
  }

  @Test
  public void toIteratorFromStringCharacterIterator() {

    String alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    StringCharacterIterator stringCharacterIterator = new StringCharacterIterator(alphabet);

    IntStream.range(0, alphabet.length()).forEach(index -> {
      char actualChar = index == 0 ? stringCharacterIterator.first() : stringCharacterIterator.next();
      char expectedChar = alphabet.charAt(index);
      assertThat(actualChar).isEqualTo(expectedChar);
    });

    assertThat(stringCharacterIterator.first()).isEqualTo('A');

    Iterator<Character> characterIterator = StringUtils.toIterator(stringCharacterIterator);

    assertThat(characterIterator).isNotNull();
    assertThat(characterIterator).hasNext();

    StringBuilder stringBuilder = new StringBuilder();

    characterIterator.forEachRemaining(stringBuilder::append);

    assertThat(characterIterator).isExhausted();
    assertThat(stringBuilder.toString()).isEqualTo(alphabet);
  }

  @Test
  public void toIteratorFromNullCharacterIterator() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> StringUtils.toIterator(null))
      .withMessage("CharacterIterator is required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("all")
  public void testToLowerCase() {

    assertThat(StringUtils.toLowerCase(null)).isNull();
    assertThat(StringUtils.toLowerCase("")).isEqualTo("");
    assertThat(StringUtils.toLowerCase("  ")).isEqualTo("  ");
    assertThat(StringUtils.toLowerCase("test")).isEqualTo("test");
    assertThat(StringUtils.toLowerCase("TEST")).isEqualTo("test");
    assertThat(StringUtils.toLowerCase("Captain Hook")).isEqualTo("captain hook");
    assertThat(StringUtils.toLowerCase("80013@N")).isEqualTo("80013@n");
  }

  @Test
  public void toStringArrayWithCommaDelimiter() {

    assertThat(StringUtils.toStringArray(" test,testing,  tested")).containsExactly("test", "testing", "tested");
    assertThat(StringUtils.toStringArray("1, 2, 3")).containsExactly("1", "2", "3");
    assertThat(StringUtils.toStringArray(", ,  ")).containsExactly("", "", "");
    assertThat(StringUtils.toStringArray("null,_,  nil ")).containsExactly("null", "_", "nil");
    assertThat(StringUtils.toStringArray("  1: 2 :3  ")).containsExactly("1: 2 :3");
  }

  @Test
  public void toStringArrayWithNullDelimitedValue() {
    assertThat(StringUtils.toStringArray(null)).containsExactly("");
  }

  @Test
  public void toStringArrayWithNullDelimiter() {
    assertThat(StringUtils.toStringArray("1, 2, 3", null)).containsExactly("1", "2", "3");
  }

  @Test
  public void toStringArrayWithOtherDelimiter() {

    assertThat(StringUtils.toStringArray("test; testing; tested", ";")).containsExactly("test", "testing", "tested");
    assertThat(StringUtils.toStringArray("1-2-3", "-")).containsExactly("1", "2", "3");
    assertThat(StringUtils.toStringArray("a:b:c", ":")).containsExactly("a", "b", "c");
  }

  @Test
  @SuppressWarnings("all")
  public void testToUpperCase() {

    assertThat(StringUtils.toUpperCase(null)).isNull();
    assertThat(StringUtils.toUpperCase("")).isEqualTo("");
    assertThat(StringUtils.toUpperCase("  ")).isEqualTo("  ");
    assertThat(StringUtils.toUpperCase("TEST")).isEqualTo("TEST");
    assertThat(StringUtils.toUpperCase("test")).isEqualTo("TEST");
    assertThat(StringUtils.toUpperCase("Captain Hook")).isEqualTo("CAPTAIN HOOK");
    assertThat(StringUtils.toUpperCase("80013@n")).isEqualTo("80013@N");
  }

  @Test
  @SuppressWarnings("all")
  public void testTrim() {

    assertThat(StringUtils.trim(null)).isNull();
    assertThat(StringUtils.trim("")).isEqualTo("");
    assertThat(StringUtils.trim(" ")).isEqualTo("");
    assertThat(StringUtils.trim("   ")).isEqualTo("");
    assertThat(StringUtils.trim("abc")).isEqualTo("abc");
    assertThat(StringUtils.trim(" 123")).isEqualTo("123");
    assertThat(StringUtils.trim("abc123 ")).isEqualTo("abc123");
    assertThat(StringUtils.trim(" xyz ")).isEqualTo("xyz");
    assertThat(StringUtils.trim("   xyz ")).isEqualTo("xyz");
    assertThat(StringUtils.trim("x y z")).isEqualTo("x y z");
    assertThat(StringUtils.trim("  x y z ")).isEqualTo("x y z");
    assertThat(StringUtils.trim("_TT_")).isEqualTo("_TT_");
    assertThat(StringUtils.trim(" _ TT _ ")).isEqualTo("_ TT _");
    assertThat(StringUtils.trim("spaceXspace")).isEqualTo("spaceXspace");
  }

  @Test
  public void testTrimAll() {

    assertThat(StringUtils.trimAll(null)).isNull();
    assertThat(StringUtils.trimAll("")).isEqualTo("");
    assertThat(StringUtils.trimAll("  ")).isEqualTo("");
    assertThat(StringUtils.trimAll("abc")).isEqualTo("abc");
    assertThat(StringUtils.trimAll(" abc")).isEqualTo("abc");
    assertThat(StringUtils.trimAll(" abc  ")).isEqualTo("abc");
    assertThat(StringUtils.trimAll(" a  b    c   ")).isEqualTo("abc");
  }

  @Test
  public void truncateWithString() {

    assertThat(StringUtils.truncate("test", 4)).isEqualTo("test");
    assertThat(StringUtils.truncate("tested", 4)).isEqualTo("test");
    assertThat(StringUtils.truncate("testing", 4)).isEqualTo("test");
    assertThat(StringUtils.truncate("test", 8)).isEqualTo("test");
  }

  @Test
  public void truncateWithNonTextualString() {

    assertThat(StringUtils.truncate(null, 10)).isNull();
    assertThat(StringUtils.truncate("", 5)).isEmpty();
    assertThat(StringUtils.truncate("  ", 5)).isEqualTo("  ");
    assertThat(StringUtils.truncate("     ", 5)).isEqualTo("     ");
    assertThat(StringUtils.truncate("          ", 5)).isEqualTo("     ");
  }

  @Test
  public void truncateWithIllegalLength() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> StringUtils.truncate("test", -2))
      .withMessage("[-2] must be greater than equal to 0")
      .withNoCause();
  }

  @Test
  public void testWrap() {

    String text = "This example block of text will be wrapped at no more than 40 characters."
      .concat(" This is a test of the wrap(..) method in the org.cp.elements.StringUtils utility class.")
      .concat(" This is a test and only a test. If this were an actual emergency then this freakin test will fail")
      .concat(" and will require a fix.");

    String[] lines = StringUtils.wrap(text, 40, null).split(StringUtils.LINE_SEPARATOR);

    assertThat(lines).isNotNull();
    assertThat(lines).hasSize(8);
    assertThat(lines[0]).isEqualTo("This example block of text will be");
    assertThat(lines[1]).isEqualTo("wrapped at no more than 40 characters.");
    assertThat(lines[2]).isEqualTo("This is a test of the wrap(..) method");
    assertThat(lines[3]).isEqualTo("in the org.cp.elements.StringUtils");
    assertThat(lines[4]).isEqualTo("utility class. This is a test and only");
    assertThat(lines[5]).isEqualTo("a test. If this were an actual");
    assertThat(lines[6]).isEqualTo("emergency then this freakin test will");
    assertThat(lines[7]).isEqualTo("fail and will require a fix.");
  }
}
