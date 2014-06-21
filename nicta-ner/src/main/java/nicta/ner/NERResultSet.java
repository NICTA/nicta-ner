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
package nicta.ner;

import nicta.ner.data.Phrase;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static nicta.ner.util.Strings.NL;

/**
 * This class encapsulated the two result set used in the NameExtractor class
 * into one single class.
 * <p/>
 * The map result can be got from this class as well.
 */
public class NERResultSet {

    private final List<List<String>> tokens;
    public final List<List<Phrase>> phrases;

    public NERResultSet(final List<List<Phrase>> phrases, final List<List<String>> tokens) {
        // TODO: need these to be deep immutable lists
        this.phrases = phrases;
        this.tokens = tokens;
    }

    /** This method returns a map format set of the result. */
    public Map<String, String> getMappedResult() {
        final Map<String, String> r = new HashMap<>();
        for (final List<Phrase> pa : phrases) {
            for (final Phrase p : pa) {
                final String ps = p.phraseString();
                r.put(ps, p.phraseType.toString());
            }
        }
        return r;
    }

    public String toString() {
        final StringBuilder sb = new StringBuilder();

        for (int si = 0; si < tokens.size(); si++) {
            final List<String> sentence = tokens.get(si);
            final List<Phrase> ps = this.phrases.get(si);
            for (final String aSentence : sentence) sb.append(aSentence).append(" ");
            sb.append(NL).append("===============================================").append(NL);
            for (final Phrase np : ps) {
                String ptext = "";
                for (int wi = 0; wi < np.phrase.length; wi++) {
                    ptext += (np.phrase[wi] + " ");
                }
                ptext = ptext.trim();

                ///*
                String stext = "";
                for (int sci = 0; sci < np.score.length; sci++) {
                    if (sci != 0) stext += ", ";
                    stext += np.score[sci];
                }
                sb.append(np.phrasePosition).append(": ").append(ptext).append("\t").append(np.phraseType.toString())
                  .append("\t").append(stext).append("\t").append(np.attachedWordMap.get("prep")).append("\t")
                  .append(np.phrasePosition).append(":").append(np.phraseStubPosition).append(":")
                  .append(np.phraseStubLength).append(":").append(np.phraseLength).append(NL);
            }
            sb.append(NL).append(NL);
        }
        return sb.toString();
    }
}
