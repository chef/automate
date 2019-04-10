#jmeter
1. run compliance-api locally
2. install jmeter (brew install jmeter)
3. run jmeter (from command line just type: jmeter)
4. open compliance-test-plan.jmx from jmeter
5. hit green arrow to start test

#view reports
- navigate to the "aggregate report" in jmeter under the thread group/loop controller to see timing
- navigate to the "view reports tree" to see the individual request/response info for each call made.

#clearing old runs
- menu->run->clearall will delete prior run data and start showing live run data for current run

# run headless
- headless mode will allow us to run this in a ci/cd pipeline and fail based on exceeded thresholds.
