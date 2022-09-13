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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Test;

import org.cp.elements.util.ArrayUtils;

/**
 * Unit Tests for {@link StringUtils}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.lang.StringUtils
 * @since 1.0.0
 */
public class StringUtilsTest {

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

  @Test(expected = IllegalArgumentException.class)
  public void capitalizeBlankString() {

    try {
      StringUtils.capitalize("  ");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Value [  ] is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void capitalizeEmptyString() {

    try {
      StringUtils.capitalize("");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Value [] is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void capitalizeNullString() {

    try {
      StringUtils.capitalize(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Value [null] is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void capitalizeUpperCaseString() {
    assertThat(StringUtils.capitalize("TEST")).isEqualTo("TEST");
  }

  @Test
  public void testConcat() {

    assertEquals("one, two, three", StringUtils.concat("one", "two", "three"));
    assertEquals("org.cp.elements", StringUtils.concat(new String[] { "org", "cp", "elements" }, StringUtils.DOT_SEPARATOR));
    assertEquals("void, null, nil", StringUtils.concat("void", null, "nil"));
  }

  @Test
  public void testConcatWithBlankString() {
    assertEquals("  ", StringUtils.concat("  "));
  }

  @Test
  public void testConcatWithEmptyString() {
    assertEquals(StringUtils.EMPTY_STRING, StringUtils.concat(StringUtils.EMPTY_STRING));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testConcatWithNullStringArray() {
    StringUtils.concat((String[]) null);
  }

  @Test
  public void testContains() {

    assertTrue(StringUtils.contains("test", "test"));
    assertTrue(StringUtils.contains("testing", "test"));
    assertTrue(StringUtils.contains("tested", "test"));
    assertTrue(StringUtils.contains("null", "null"));
  }

  @Test
  public void testContainsWithNonContainedText() {

    assertFalse(StringUtils.contains("null", "nil"));
    assertFalse(StringUtils.contains("test", "TEST"));
    assertFalse(StringUtils.contains("TEST", "test"));
    assertFalse(StringUtils.contains("test", "testing"));
    assertFalse(StringUtils.contains(null, "test"));
    assertFalse(StringUtils.contains(null, "null"));
    assertFalse(StringUtils.contains("null", null));
  }

  @Test
  public void testContainsDigits() {

    assertTrue(StringUtils.containsDigits("0123456789"));
    assertTrue(StringUtils.containsDigits("Ol2E4SG7B9"));
    assertTrue(StringUtils.containsDigits("!1#$%*"));
    assertTrue(StringUtils.containsDigits("$100.00"));
    assertTrue(StringUtils.containsDigits("50%"));
    assertTrue(StringUtils.containsDigits("(503) 555-1234"));
    assertTrue(StringUtils.containsDigits("00"));
    assertTrue(StringUtils.containsDigits("0"));
  }

  @Test
  public void testContainsNoDigits() {

    assertFalse(StringUtils.containsDigits("abcdefghijklmnopqrstuvwxyz"));
    assertFalse(StringUtils.containsDigits("lOlOl"));
    assertFalse(StringUtils.containsDigits("$###.##"));
    assertFalse(StringUtils.containsDigits("one"));
    assertFalse(StringUtils.containsDigits("  "));
    assertFalse(StringUtils.containsDigits(""));
    assertFalse(StringUtils.containsDigits(null));
  }

  @Test
  public void testContainsLetters() {

    assertTrue(StringUtils.containsLetters("abcdefghijklmnopqrstuvwxyz"));
    assertTrue(StringUtils.containsLetters("lOlOl"));
    assertTrue(StringUtils.containsLetters("l0l0l"));
    assertTrue(StringUtils.containsLetters("1O1O1"));
    assertTrue(StringUtils.containsLetters("O0"));
    assertTrue(StringUtils.containsLetters("O"));
    assertTrue(StringUtils.containsLetters("XyZ"));
    assertTrue(StringUtils.containsLetters("ABC123"));
    assertTrue(StringUtils.containsLetters("fifty%"));
  }

  @Test
  public void testContainsNoLetters() {

    assertFalse(StringUtils.containsLetters("0123456789"));
    assertFalse(StringUtils.containsLetters("10101"));
    assertFalse(StringUtils.containsLetters("@554013"));
    assertFalse(StringUtils.containsLetters("$$$"));
    assertFalse(StringUtils.containsLetters("  "));
    assertFalse(StringUtils.containsLetters(""));
    assertFalse(StringUtils.containsLetters(null));
  }

  @Test
  public void testContainsWhitespace() {

    assertTrue(StringUtils.containsWhitespace(" "));
    assertTrue(StringUtils.containsWhitespace("   "));
    assertTrue(StringUtils.containsWhitespace(" text "));
    assertTrue(StringUtils.containsWhitespace(" test"));
    assertTrue(StringUtils.containsWhitespace("test "));
    assertTrue(StringUtils.containsWhitespace("t e s t"));
    assertTrue(StringUtils.containsWhitespace("Mc Fly"));
  }

  @Test
  public void testContainsNoWhitespace() {

    assertFalse(StringUtils.containsWhitespace(""));
    assertFalse(StringUtils.containsWhitespace(" text ".trim()));
    assertFalse(StringUtils.containsWhitespace(" test".trim()));
    assertFalse(StringUtils.containsWhitespace("test ".trim()));
    assertFalse(StringUtils.containsWhitespace("t_e_s_t".trim()));
    assertFalse(StringUtils.containsWhitespace("McFly".trim()));
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

    assertTrue(StringUtils.equalsIgnoreCase("test", "test"));
    assertTrue(StringUtils.equalsIgnoreCase("test", "TEST"));
    assertTrue(StringUtils.equalsIgnoreCase("titlecase", "Titlecase"));
    assertTrue(StringUtils.equalsIgnoreCase("null", "null"));
  }

  @Test
  @SuppressWarnings("all")
  public void testEqualsIgnoreCaseWithUnequalStrings() {

    assertFalse(StringUtils.equalsIgnoreCase("test", "testing"));
    assertFalse(StringUtils.equalsIgnoreCase("seam", "seem"));
    assertFalse(StringUtils.equalsIgnoreCase("null", null));
    assertFalse(StringUtils.equalsIgnoreCase(null, "null"));
  }

  @Test
  public void testGetDigits() {

    assertEquals("", StringUtils.getDigits(""));
    assertEquals("", StringUtils.getDigits("  "));
    assertEquals("", StringUtils.getDigits("abc"));
    assertEquals("", StringUtils.getDigits("lOlOl"));
    assertEquals("", StringUtils.getDigits("oneTwoThree"));
    assertEquals("", StringUtils.getDigits("$###.##"));
    assertEquals("", StringUtils.getDigits(".lS%"));
    assertEquals("123", StringUtils.getDigits("123"));
    assertEquals("123", StringUtils.getDigits("abc123"));
    assertEquals("00", StringUtils.getDigits("l0l0l"));
    assertEquals("111", StringUtils.getDigits("1O1O1"));
    assertEquals("012480", StringUtils.getDigits("n0a1bc2defg4hijklmno8p0qrstuvwxzy"));
    assertEquals("12480", StringUtils.getDigits("localhost:12480"));
    assertEquals("102347712012480", StringUtils.getDigits("10.234.77.120:12480"));
    assertEquals("10050", StringUtils.getDigits("$100.50"));
    assertEquals("50", StringUtils.getDigits("50%"));
    assertEquals("50355512345", StringUtils.getDigits("(503) 555-1234 x5"));
  }

  @Test
  public void testGetLetters() {

    assertEquals("", StringUtils.getLetters(""));
    assertEquals("", StringUtils.getLetters("  "));
    assertEquals("", StringUtils.getLetters("123"));
    assertEquals("", StringUtils.getLetters("10101"));
    assertEquals("", StringUtils.getLetters("8007"));
    assertEquals("", StringUtils.getLetters("$100.50"));
    assertEquals("", StringUtils.getLetters("50%"));
    assertEquals("", StringUtils.getLetters("@$$4013"));
    assertEquals("abc", StringUtils.getLetters("abc"));
    assertEquals("abc", StringUtils.getLetters("abc123"));
    assertEquals("ABC", StringUtils.getLetters("1A2BC3"));
    assertEquals("lll", StringUtils.getLetters("l0l0l"));
    assertEquals("OO", StringUtils.getLetters("1O1O1"));
    assertEquals("localhost", StringUtils.getLetters("localhost:12480"));
    assertEquals("nabcdefghijklmnopqrstuvwxyz", StringUtils.getLetters("n0a1bc2defg4hijklmno8p0qrstuvwxyz"));
    assertEquals("x", StringUtils.getLetters("(503) 555-1234 x520"));
  }

  @Test
  public void testGetSpaces() {

    assertEquals("", StringUtils.getSpaces(0));
    assertEquals(" ", StringUtils.getSpaces(1));
    assertEquals("  ", StringUtils.getSpaces(2));
    assertEquals("   ", StringUtils.getSpaces(3));
    assertEquals("    ", StringUtils.getSpaces(4));
    assertEquals("     ", StringUtils.getSpaces(5));
    assertEquals("      ", StringUtils.getSpaces(6));
    assertEquals("       ", StringUtils.getSpaces(7));
    assertEquals("        ", StringUtils.getSpaces(8));
    assertEquals("         ", StringUtils.getSpaces(9));
    assertEquals("          ", StringUtils.getSpaces(10));
    assertEquals("           ", StringUtils.getSpaces(11));
    assertEquals("            ", StringUtils.getSpaces(12));
    assertEquals("             ", StringUtils.getSpaces(13));
    assertEquals("              ", StringUtils.getSpaces(14));
    assertEquals("               ", StringUtils.getSpaces(15));
    assertEquals("                ", StringUtils.getSpaces(16));
    assertEquals("                 ", StringUtils.getSpaces(17));
    assertEquals("                  ", StringUtils.getSpaces(18));
    assertEquals("                   ", StringUtils.getSpaces(19));
    assertEquals("                    ", StringUtils.getSpaces(20));
    assertEquals("                     ", StringUtils.getSpaces(21));
    assertEquals("                                                   ", StringUtils.getSpaces(51));
    assertEquals("                                                                                                   ",
      StringUtils.getSpaces(99));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetSpacesWithInvalidNumber() {

    try {
      StringUtils.getSpaces(-1);
    }
    catch (IllegalArgumentException expected) {
      assertEquals("The number [-1] of desired spaces must be greater than equal to 0", expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testHasText() {

    assertTrue(StringUtils.hasText("test"));
    assertTrue(StringUtils.hasText("0123456789"));
    assertTrue(StringUtils.hasText("$@$!"));
    assertTrue(StringUtils.hasText("X"));
    assertTrue(StringUtils.hasText("-"));
    assertTrue(StringUtils.hasText("null"));
    assertTrue(StringUtils.hasText("nill"));
    assertTrue(StringUtils.hasText("empty"));
    assertTrue(StringUtils.hasText("blank"));
    assertTrue(StringUtils.hasText("_"));
  }

  @Test
  public void testHasTextWithNoText() {

    assertFalse(StringUtils.hasText(null));
    assertFalse(StringUtils.hasText(""));
    assertFalse(StringUtils.hasText(" "));
    assertFalse(StringUtils.hasText("   "));
  }

  @Test
  public void testIndexOf() {

    assertEquals(-1, StringUtils.indexOf(null, "test"));
    assertEquals(-1, StringUtils.indexOf("test", null));
    assertEquals(-1, StringUtils.indexOf("", " "));
    assertEquals(-1, StringUtils.indexOf("", "text"));
    assertEquals(-1, StringUtils.indexOf("null", "nil"));
    assertEquals(0, StringUtils.indexOf("", ""));
    assertEquals(0, StringUtils.indexOf("  ", ""));
    assertEquals(0, StringUtils.indexOf("  ", " "));
    assertEquals(0, StringUtils.indexOf("null", "null"));
    assertEquals(4, StringUtils.indexOf("This is example text!", " "));
    assertEquals(8, StringUtils.indexOf("This is example text!", "ex"));
    assertEquals(16, StringUtils.indexOf("This is example text!", "text"));
    assertEquals(17, StringUtils.indexOf("This is example text!", "ext"));
    assertEquals(-1, StringUtils.indexOf("This is example text!", "test"));
  }

  @Test
  public void testIsBlank() {

    assertTrue(StringUtils.isBlank(null));
    assertTrue(StringUtils.isBlank(""));
    assertTrue(StringUtils.isBlank(" "));
    assertTrue(StringUtils.isBlank("   "));
    assertTrue(StringUtils.isBlank("\0"));
  }

  @Test
  public void testIsBlankWithNonBlankStrings() {

    assertFalse(StringUtils.isBlank("_"));
    assertFalse(StringUtils.isBlank("___"));
    assertFalse(StringUtils.isBlank("null"));
    assertFalse(StringUtils.isBlank("nil"));
    assertFalse(StringUtils.isBlank("false"));
    assertFalse(StringUtils.isBlank("."));
    assertFalse(StringUtils.isBlank("0"));
    assertFalse(StringUtils.isBlank("0.0"));
    assertFalse(StringUtils.isBlank("space"));
    assertFalse(StringUtils.isBlank("empty"));
    assertFalse(StringUtils.isBlank("blank"));
    assertFalse(StringUtils.isBlank("test"));
  }

  @Test
  public void testIsDigits() {

    assertTrue(StringUtils.isDigits("012"));
    assertTrue(StringUtils.isDigits("123"));
    assertTrue(StringUtils.isDigits("0123456789"));
    assertTrue(StringUtils.isDigits("112358"));
    assertTrue(StringUtils.isDigits("012480"));
    assertTrue(StringUtils.isDigits("0122444488888888"));
  }

  @Test
  public void testIsDigitsWithNonDigitStrings() {

    assertFalse(StringUtils.isDigits(null));
    assertFalse(StringUtils.isDigits(""));
    assertFalse(StringUtils.isDigits("  "));
    assertFalse(StringUtils.isDigits("abc"));
    assertFalse(StringUtils.isDigits("abc123"));
    assertFalse(StringUtils.isDigits("l0l0l"));
    assertFalse(StringUtils.isDigits("B00B"));
    assertFalse(StringUtils.isDigits("$1024.64"));
    assertFalse(StringUtils.isDigits("50%"));
    assertFalse(StringUtils.isDigits("(503) 555-0123"));
    assertFalse(StringUtils.isDigits("####"));
    assertFalse(StringUtils.isDigits(" 0123 "));
  }

  @Test
  public void testIsEmpty() throws Exception {
    assertTrue(StringUtils.isEmpty(""));
  }

  @Test
  @SuppressWarnings("all")
  public void testIsEmptyWithNonEmptyStrings() {

    assertFalse(StringUtils.isEmpty(null));
    assertFalse(StringUtils.isEmpty(" "));
    assertFalse(StringUtils.isEmpty("   "));
    assertFalse(StringUtils.isEmpty("false"));
    assertFalse(StringUtils.isEmpty("\0"));
    assertFalse(StringUtils.isEmpty("0"));
    assertFalse(StringUtils.isEmpty("0.0"));
    assertFalse(StringUtils.isEmpty("-"));
    assertFalse(StringUtils.isEmpty("_"));
    assertFalse(StringUtils.isEmpty("x"));
    assertFalse(StringUtils.isEmpty("empty"));
  }

  @Test
  public void testIsLetters() {

    assertTrue(StringUtils.isLetters("abcdefghijklmnopqrstuvwxyz"));
    assertTrue(StringUtils.isLetters("ABC"));
    assertTrue(StringUtils.isLetters("lOlO"));
    assertTrue(StringUtils.isLetters("oneTwoThree"));
  }

  @Test
  public void testIsLettersWithNonLetterStrings() {

    assertFalse(StringUtils.isLetters(null));
    assertFalse(StringUtils.isLetters(""));
    assertFalse(StringUtils.isLetters("  "));
    assertFalse(StringUtils.isLetters("123"));
    assertFalse(StringUtils.isLetters("abc123"));
    assertFalse(StringUtils.isLetters("A1BC23"));
    assertFalse(StringUtils.isLetters("$oneHundred.fifty"));
    assertFalse(StringUtils.isLetters("fifty%"));
    assertFalse(StringUtils.isLetters("@$"));
    assertFalse(StringUtils.isLetters("$0$"));
    assertFalse(StringUtils.isLetters("localhost:8080"));
  }

  @Test
  public void testLastIndexOf() {

    assertEquals(-1, StringUtils.lastIndexOf(null, "test"));
    assertEquals(-1, StringUtils.lastIndexOf("test", null));
    assertEquals(-1, StringUtils.lastIndexOf("", " "));
    assertEquals(-1, StringUtils.lastIndexOf("", "text"));
    assertEquals(-1, StringUtils.lastIndexOf("null", "nil"));
    assertEquals(0, StringUtils.lastIndexOf("", ""));
    assertEquals(2, StringUtils.lastIndexOf("  ", ""));
    assertEquals(1, StringUtils.lastIndexOf("  ", " "));
    assertEquals(0, StringUtils.lastIndexOf("null", "null"));
    assertEquals(15, StringUtils.lastIndexOf("This is example text!", " "));
    assertEquals(17, StringUtils.lastIndexOf("This is example text!", "ex"));
    assertEquals(16, StringUtils.lastIndexOf("This is example text!", "text"));
    assertEquals(8, StringUtils.lastIndexOf("This is example text!", "exam"));
    assertEquals(-1, StringUtils.lastIndexOf("This is example text!", "test"));
  }

  @Test
  public void testLength() {

    assertEquals(0, StringUtils.length(null));
    assertEquals(0, StringUtils.length(""));
    assertEquals(1, StringUtils.length(" "));
    assertEquals(1, StringUtils.length("\0"));
    assertEquals(3, StringUtils.length("   "));
    assertEquals(3, StringUtils.length("nil"));
    assertEquals(4, StringUtils.length("null"));
    assertEquals(4, StringUtils.length("test"));
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

  @Test(expected = IllegalArgumentException.class)
  public void padWithIllegalLength() {

    try {
      StringUtils.pad("test", -10);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("[-10] must be greater than equal to 0");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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
  public void requiresTextWithNoText() {

    Arrays.stream(ArrayUtils.asArray("  ", "", null)).forEach(text ->
      assertThatIllegalArgumentException()
        .isThrownBy(() -> StringUtils.requireText(text, "String [%s] must have {1}", text, "text"))
        .withMessage("String [%s] must have text", text)
        .withNoCause());
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

    assertEquals("true false", StringUtils.singleSpaceObjects(true, false));
    assertEquals("t e s t", StringUtils.singleSpaceObjects('t', 'e', 's', 't'));
    assertEquals("1 0 1", StringUtils.singleSpaceObjects(1, 0, 1));
    assertEquals("3.14159", StringUtils.singleSpaceObjects(3.14159d));
    assertEquals("false \0 c 0 3.14159 mock", StringUtils.singleSpaceObjects(false, '\0', 'c', 0, 3.14159d, "mock"));
    assertEquals("test", StringUtils.singleSpaceObjects("test"));
    assertEquals("null", StringUtils.singleSpaceObjects("  null "));
    assertEquals("this   is  a     test!", StringUtils.singleSpaceObjects("  this", "  is ", "a", "    test!   "));
    assertEquals("this is a test with a null value", StringUtils.singleSpaceObjects("this", "is", "a", "test", "with",
      "a", null, "value"));
  }

  @Test
  public void testSingleSpaceString() {

    assertEquals("This is a test!", StringUtils.singleSpaceString(" This is  a          test!  "));
    assertEquals("This_is_another_test!", StringUtils.singleSpaceString("This_is_another_test!"));
    assertEquals("null", StringUtils.singleSpaceString("null"));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSingleSpaceStringWithBlankString() {
    StringUtils.singleSpaceString("   ");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSingleSpaceStringWithEmptyString() {
    StringUtils.singleSpaceString("");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSingleSpaceStringWithNullString() {
    StringUtils.singleSpaceString(null);
  }

  @Test
  public void testToCharArray() {

    char[] charArray = StringUtils.toCharArray(null);

    assertNotNull(charArray);
    assertEquals(0, charArray.length);

    charArray = StringUtils.toCharArray("abc");

    assertNotNull(charArray);
    assertEquals('a', charArray[0]);
    assertEquals('b', charArray[1]);
    assertEquals('c', charArray[2]);
  }

  @Test
  public void testToLowerCase() {

    assertNull(StringUtils.toLowerCase(null));
    assertEquals("", StringUtils.toLowerCase(""));
    assertEquals("  ", StringUtils.toLowerCase("  "));
    assertEquals("test", StringUtils.toLowerCase("test"));
    assertEquals("test", StringUtils.toLowerCase("TEST"));
    assertEquals("captain hook", StringUtils.toLowerCase("Captain Hook"));
    assertEquals("80013@n", StringUtils.toLowerCase("80013@N"));
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

    assertNull(StringUtils.toUpperCase(null));
    assertEquals("", StringUtils.toUpperCase(""));
    assertEquals("  ", StringUtils.toUpperCase("  "));
    assertEquals("TEST", StringUtils.toUpperCase("TEST"));
    assertEquals("TEST", StringUtils.toUpperCase("test"));
    assertEquals("CAPTAIN HOOK", StringUtils.toUpperCase("Captain Hook"));
    assertEquals("80013@N", StringUtils.toUpperCase("80013@n"));
  }

  @Test
  @SuppressWarnings("all")
  public void testTrim() {

    assertNull(StringUtils.trim(null));
    assertEquals("", StringUtils.trim(""));
    assertEquals("", StringUtils.trim(" "));
    assertEquals("", StringUtils.trim("   "));
    assertEquals("abc", StringUtils.trim("abc"));
    assertEquals("123", StringUtils.trim(" 123"));
    assertEquals("abc123", StringUtils.trim("abc123 "));
    assertEquals("xyz", StringUtils.trim(" xyz "));
    assertEquals("xyz", StringUtils.trim("   xyz "));
    assertEquals("x y z", StringUtils.trim("x y z"));
    assertEquals("x y z", StringUtils.trim("  x y z "));
    assertEquals("_TT_", StringUtils.trim("_TT_"));
    assertEquals("_ TT _", StringUtils.trim(" _ TT _ "));
    assertEquals("spaceXspace", StringUtils.trim("spaceXspace"));
  }

  @Test
  public void testTrimAll() {

    assertNull(StringUtils.trimAll(null));
    assertEquals("", StringUtils.trimAll(""));
    assertEquals("", StringUtils.trimAll("  "));
    assertEquals("abc", StringUtils.trimAll("abc"));
    assertEquals("abc", StringUtils.trimAll(" abc"));
    assertEquals("abc", StringUtils.trimAll(" abc  "));
    assertEquals("abc", StringUtils.trimAll(" a  b    c   "));
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

  @Test(expected = IllegalArgumentException.class)
  public void truncateWithIllegalLength() {

    try {
      StringUtils.truncate("test", -2);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("[-2] must be greater than equal to 0");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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
