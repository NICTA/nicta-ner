/*
 * #%L
 * NICTA t3as NER CoNLL 2003 evaluation
 * %%
 * Copyright (C) 2014 NICTA
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
package org.t3as.ner.conll2003.cmdline;

import com.beust.jcommander.JCommander;
import com.beust.jcommander.Parameter;

import java.io.File;
import java.io.IOException;

public final class Main {

    private Main() {}

    @SuppressWarnings("MethodNamesDifferingOnlyByCase")
    public static void main(final String[] args) throws IOException {
        final Options opts = getOptions(args);

        System.out.println(opts.file);
    }

    @SuppressWarnings("CallToSystemExit")
    private static Options getOptions(final String[] args) {
        final Options opts = new Options();
        JCommander jc = null;
        try { jc = new JCommander(opts, args); }
        catch (final RuntimeException e) {
            System.err.println("Could not parse the options: " + e.getMessage());
            System.exit(1);
        }
        if (opts.showUsage) {
            jc.usage();
            System.exit(0);
        }
        if (opts.file == null) {
            System.out.println("Please pass a CoNLL 2003 test file.");
        }
        return opts;
    }

    private static class Options {
        @Parameter(help = true, names = {"-h", "--help"}, description = "Show this help message.")
        boolean showUsage;

        @Parameter(description = "<CoNLL 2003 test file>")
        File file;
    }
}
