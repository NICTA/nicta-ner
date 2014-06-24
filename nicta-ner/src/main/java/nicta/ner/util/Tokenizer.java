/*
 * #%L
 * NICTA Named Entity Recogniser library
 * %%
 * Copyright (C) 2010 - 2014 NICTA
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/gpl-3.0.html>.
 * #L%
 */
package nicta.ner.util;

import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;

import java.io.IOException;
import java.text.BreakIterator;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import static java.lang.Character.isLetterOrDigit;
import static java.lang.Character.isUpperCase;
import static nicta.ner.util.Strings.endsWith;
import static nicta.ner.util.Strings.equalss;
import static nicta.ner.util.Tokenizer.Mode.WITHOUT_PUNCTUATE;
import static nicta.ner.util.Tokenizer.Mode.WITH_PUNCTUATE;

/** This class utilizes a Java standard class to token the input sentence. */
public class Tokenizer {

    public enum Mode {
        WITH_PUNCTUATE,
        WITHOUT_PUNCTUATE
    }

    private static final ImmutableCollection<String> ABBREVIATION_EXCEPTION;

    private final Mode mode;

    static {
        try { ABBREVIATION_EXCEPTION = ImmutableList.copyOf(IO.lines(Tokenizer.class, "TokenizerAbbreviation")); }
        catch (final IOException e) { throw new RuntimeException("Could not load the TokenizerAbbreviation file.", e); }
    }

    public Tokenizer(final Mode mode) { this.mode = mode; }

    public List<List<String>> process(final String text) {
        final List<List<String>> paragraph = new ArrayList<>();

        final Locale currentLocale = new Locale("en", "US");
        final BreakIterator wordIterator = BreakIterator.getWordInstance(currentLocale);
        wordIterator.setText(text);

        int sPtr = wordIterator.first();
        int ePtr = wordIterator.next();

        List<String> currentSentence = new ArrayList<>();

        while (ePtr != BreakIterator.DONE) {
            final String word = text.substring(sPtr, ePtr);
            if (!"".equals(word.trim())
                && ((mode == WITH_PUNCTUATE)
                    || (mode == WITHOUT_PUNCTUATE && isLetterOrDigit(word.charAt(0))))) {
                boolean canBreakSentence = true;
                if (word.contains("'")) {
                    if (word.endsWith("n't")) {
                        final String w1 = word.substring(0, word.length() - 3);
                        if (!w1.isEmpty()) currentSentence.add(w1);
                        currentSentence.add("n't");
                    }
                    else if (endsWith(word, "'s", "'ll", "'re", "'m", "'ve", "'d")) {
                        final int p = word.indexOf("'");
                        final String w1 = word.substring(0, p);
                        final String w2 = word.substring(p);
                        if (!w1.isEmpty()) currentSentence.add(w1);
                        if (!w2.isEmpty()) currentSentence.add(w2);
                    }
                    else currentSentence.add(word);
                }
                else if (".".equals(word.trim())) {
                    final int formerIndex = currentSentence.size() - 1;
                    if (formerIndex == -1) currentSentence.add(word);
                    else {
                        final String formerWord = currentSentence.get(formerIndex);
                        if (ABBREVIATION_EXCEPTION.contains(formerWord)
                            || (formerWord.length() == 1
                                && isUpperCase(formerWord.charAt(0)))
                            || formerWord.contains(".")) {
                            currentSentence.remove(formerIndex);
                            currentSentence.add(formerWord + ".");
                            // do not break the sentence
                            canBreakSentence = false;
                        }
                        else currentSentence.add(word);
                    }
                }
                else if (":".equals(word.trim())) {
                    try {
                        if (text.charAt(ePtr) != ' '
                            && text.charAt(sPtr - 1) != ' '
                            && !currentSentence.isEmpty()) {
                            final int formerIndex = currentSentence.size() - 1;
                            final String formerWord = currentSentence.get(formerIndex);
                            sPtr = ePtr;
                            ePtr = wordIterator.next();
                            if (ePtr == BreakIterator.DONE) {
                                currentSentence.add(word);
                                continue;
                            }
                            else {
                                final String nextWord = text.substring(sPtr, ePtr);
                                if (isLetterOrDigit(nextWord.charAt(0))
                                    && isLetterOrDigit(formerWord.charAt(0))) {
                                    // merge
                                    currentSentence.remove(formerIndex);
                                    //noinspection StringConcatenationMissingWhitespace
                                    currentSentence.add(formerWord + word + nextWord);
                                }
                                else {
                                    currentSentence.add(word);
                                    currentSentence.add(nextWord);
                                }
                            }
                        }
                        else currentSentence.add(word);
                    }
                    catch (final StringIndexOutOfBoundsException ignored) {
                        currentSentence.add(word);
                    }
                }
                else currentSentence.add(word);
                // handling the end of a sentence
                if (equalss(word.trim(), ".", ";", "?", "!") && canBreakSentence) {
                    paragraph.add(currentSentence);
                    currentSentence = new ArrayList<>();
                }
            }
            sPtr = ePtr;
            ePtr = wordIterator.next();
        }

        if (!currentSentence.isEmpty()) paragraph.add(currentSentence);
        return paragraph;
    }
}
