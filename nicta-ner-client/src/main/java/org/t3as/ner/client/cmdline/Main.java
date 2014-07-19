/*
 * #%L
 * NICTA t3as Named-Entity Recognition client
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
package org.t3as.ner.client.cmdline;

import com.beust.jcommander.JCommander;
import com.beust.jcommander.Parameter;
import com.google.common.base.Charsets;
import com.google.common.io.Files;
import org.t3as.ner.client.NerClient;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

public final class Main {

    private Main() {}

    @SuppressWarnings("MethodNamesDifferingOnlyByCase")
    public static void main(final String[] args) throws IOException {
        final Options opts = getOptions(args);

        // create a web service client
        final NerClient client = new NerClient(opts.url);

        // call the webservice with the text and any passed options
        if (opts.text.isEmpty()) processFiles(opts, client);
        else System.out.println(client.call(opts.text));
    }

    private static void processFiles(final Options opts, final NerClient client) throws IOException {
        // read each file and call the web service
        for (final File f : opts.files) {
            final String input = Files.toString(f, Charsets.UTF_8);
            System.out.printf("%s:\n", f);
            System.out.println(client.call(input));
        }
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
        if (opts.text.isEmpty() && opts.files.isEmpty()) {
            System.err.println("Pass either text to analyse or files to read text from.");
            System.exit(1);
        }
        if (!opts.text.isEmpty() && !opts.files.isEmpty()) {
            System.err.println("Pass either text to analyse or files to read text from, not both.");
            System.exit(1);
        }
        // do nothing unless all files are readable
        for (final File f : opts.files) {
            if (!f.canRead()) {
                System.err.printf("Can not read file '%s'\n", f);
                System.exit(1);
            }
        }
        return opts;
    }

    private static class Options {
        @Parameter(help = true, names = {"-h", "--help"}, description = "Show this help message.")
        boolean showUsage;

        @Parameter(names = "-url", description = "The base URL of the NICTA NER web service.")
        String url = NerClient.DEFAULT_BASE_URL;

        @Parameter(names = "-text", description = "The text to submit to the web service.")
        String text = "";

        @Parameter(description = "[files]")
        Collection<File> files = new ArrayList<>();
    }
}
