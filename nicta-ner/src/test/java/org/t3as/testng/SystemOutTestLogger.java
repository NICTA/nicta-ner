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
package org.t3as.testng;

import org.testng.IInvokedMethod;
import org.testng.IInvokedMethodListener;
import org.testng.ITestNGMethod;
import org.testng.ITestResult;

import static nicta.ner.util.Strings.NL;

public class SystemOutTestLogger implements IInvokedMethodListener {

    @Override
    public void beforeInvocation(final IInvokedMethod method, final ITestResult testResult) {}

    @Override
    public void afterInvocation(final IInvokedMethod m, final ITestResult r) {
        if (!m.isTestMethod()) return;

        final ITestNGMethod method = m.getTestMethod();
        final String testName = method.getTestClass().getRealClass().getSimpleName();
        final ITestResult testResult = m.getTestResult();
        final long elapsed = testResult.getEndMillis() - testResult.getStartMillis();
        //noinspection resource
        System.out.printf("%s.%s(): %s, %d millis%s",
                          testName, method.getMethodName(), Status.forCode(testResult.getStatus()), elapsed, NL);
    }

    /** {@link org.testng.ITestResult} */
    @SuppressWarnings("MagicNumber")
    private enum Status {
        OK(1),
        FAIL(2),
        SKIP(3),
        OK_PCT_FAIL(4),
        STARTED(16),
        UNKNOWN(-1);

        final int code;

        Status(final int code) { this.code = code; }

        static Status forCode(final int c) {
            for (final Status s : Status.values()) {
                if (s.code == c) return s;
            }
            return UNKNOWN;
        }
    }
}
