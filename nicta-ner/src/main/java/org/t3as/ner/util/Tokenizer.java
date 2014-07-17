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
package org.t3as.ner.util;

import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import org.t3as.ner.data.Token;

import java.io.IOException;
import java.text.BreakIterator;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;

import static java.lang.Character.isLetterOrDigit;
import static java.lang.Character.isSpaceChar;
import static java.text.BreakIterator.DONE;
import static org.t3as.ner.util.Strings.endsWith;
import static org.t3as.ner.util.Strings.equalss;
import static org.t3as.ner.util.Strings.initChar;
import static org.t3as.ner.util.Strings.isSingleUppercaseChar;
import static org.t3as.ner.util.Strings.lastChar;
import static org.t3as.ner.util.Tokenizer.Mode.WITHOUT_PUNCTUATION;
import static org.t3as.ner.util.Tokenizer.Mode.WITH_PUNCTUATION;

/**
 * This class utilizes a Java standard class to token the input sentence.
 * <p/>
 * This class is not thread safe.
 */
public class Tokenizer {

    public enum Mode {
        WITH_PUNCTUATION,
        WITHOUT_PUNCTUATION
    }

    private static final ImmutableCollection<String> ABBREVIATION_EXCEPTIONS;

    static {
        try { ABBREVIATION_EXCEPTIONS = ImmutableList.copyOf(IO.lines(Tokenizer.class, "TokenizerAbbreviation")); }
        catch (final IOException e) { throw new RuntimeException("Could not load the TokenizerAbbreviation file.", e); }
    }

    private final Mode mode;
    private List<Token> currentSentence;

    public Tokenizer(final Mode mode) { this.mode = mode; }

    /**
     * Tokenize some text - not thread safe.
     * @param text tokenize this
     * @return tokenized text
     */
    public List<List<Token>> process(final String text) {
        final List<List<Token>> paragraph = new ArrayList<>();
        currentSentence = new ArrayList<>();
        final Tokens tokens = splitText(text);

        while (tokens.hasNext()) {
            final Token t = tokens.next();
            final String trimmedWord = t.str.trim();

            // skip spaces
            if (trimmedWord.isEmpty()) continue;

            if (((mode == WITH_PUNCTUATION) || (mode == WITHOUT_PUNCTUATION && isLetterOrDigit(initChar(t.str))))) {
                boolean canBreakSentence = true;
                if (t.str.contains("'")) {
                    wordContainsApostrophe(t);
                }
                else if (".".equals(trimmedWord)) {
                    canBreakSentence = wordIsFullStop(t);
                }
                else if (":".equals(trimmedWord)) {
                    wordIsColon(tokens, t);
                }
                else currentSentence.add(t);

                // handling the end of a sentence
                if (canBreakSentence && equalss(trimmedWord, ".", ";", "?", "!")) {
                    paragraph.add(currentSentence);
                    currentSentence = new ArrayList<>();
                }
            }
        }

        if (!currentSentence.isEmpty()) paragraph.add(currentSentence);
        return paragraph;
    }

    private void wordIsColon(final Tokens tokens, final Token t) {
        // check we can get a previous and next word to merge together
        if (!currentSentence.isEmpty() && tokens.hasNext()) {
            // if the colon does not have a space on either side
            if (!isSpaceChar(lastChar(tokens.peekPrev().str)) && !isSpaceChar(initChar(tokens.peekNext().str))) {
                // try to merge the 3 tokens back together again
                final int prevWordIndex = currentSentence.size() - 1;
                final Token prevSentenceWord = currentSentence.get(prevWordIndex);
                mergeWordsIntoSentence(prevSentenceWord, t, tokens.next(), prevWordIndex);
            }
            else currentSentence.add(t);
        }
        else currentSentence.add(t);
    }

    private static Tokens splitText(final String text) {
        final List<Token> l = new LinkedList<>();

        // use a BreakIterator to iterate our way through the words of the text
        final BreakIterator wordIterator = BreakIterator.getWordInstance(new Locale("en", "US"));
        wordIterator.setText(text);

        // simply iterate through the text, keeping track of a start and end index of the current word
        int startIdx = wordIterator.first();
        for (int endIdx = wordIterator.next(); endIdx != DONE; startIdx = endIdx, endIdx = wordIterator.next()) {
            final String word = text.substring(startIdx, endIdx);
            l.add(new Token(startIdx, word));
        }

        return new Tokens(l);
    }

    private void mergeWordsIntoSentence(final Token previousWord, final Token word, final Token nextWord,
                                        final int previousWordIndex) {
        // make sure the previous and next words both start with a letter or digit
        if (isLetterOrDigit(initChar(previousWord.str)) && isLetterOrDigit(initChar(nextWord.str))) {
            // merge the 3 words again and add the result, replace previous word from sentence
            currentSentence.set(previousWordIndex,
                                new Token(previousWord.startIndex, previousWord.str + word.str + nextWord.str));
        }
        // otherwise just add the word and next word
        else {
            currentSentence.add(word);
            currentSentence.add(nextWord);
        }
    }

    private boolean wordIsFullStop(final Token t) {
        boolean canBreakSentence = true;
        if (currentSentence.isEmpty()) currentSentence.add(t);
        else {
            final int previousWordIndex = currentSentence.size() - 1;
            final Token previousWord = currentSentence.get(previousWordIndex);
            if (ABBREVIATION_EXCEPTIONS.contains(previousWord.str)
                || isSingleUppercaseChar(previousWord.str)
                || previousWord.str.contains(".")) {
                currentSentence.set(previousWordIndex, new Token(previousWord.startIndex, previousWord.str + "."));
                // do not break the sentence
                canBreakSentence = false;
            }
            else currentSentence.add(t);
        }
        return canBreakSentence;
    }

    private void wordContainsApostrophe(final Token t) {
        if (t.str.endsWith("n't")) {
            final String w1 = t.str.substring(0, t.str.length() - 3);
            if (!w1.isEmpty()) currentSentence.add(new Token(t.startIndex, w1));
            currentSentence.add(new Token(t.startIndex + w1.length(), "n't"));
        }
        else if (endsWith(t.str, "'s", "'ll", "'re", "'m", "'ve", "'d")) {
            final int p = t.str.indexOf("'");
            final String w1 = t.str.substring(0, p);
            final String w2 = t.str.substring(p);
            if (!w1.isEmpty()) currentSentence.add(new Token(t.startIndex, w1));
            if (!w2.isEmpty()) currentSentence.add(new Token(t.startIndex + w1.length(), w2));
        }
        else currentSentence.add(t);
    }

    private static class Tokens {

        private final List<Token> l;
        private int index = -1;

        private Tokens(final List<Token> l) { this.l = l; }

        public boolean hasNext() { return index + 1 < l.size(); }

        public Token next() { return l.get(++index); }

        public Token peekPrev() { return l.get(index - 1); }

        public Token peekNext() { return l.get(index + 1); }
    }
}
