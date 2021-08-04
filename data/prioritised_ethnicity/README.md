# Prioritised Ethnicity derivation

This data file does not contain the truth.

Instead, it is patched together based on publically available sources.

## Problem

Ministry of Health uses Prioritised ethnicity to report COVID-19 vaccination counts.

These need population counts to be contextualised, but Statistics NZ provide only Total
Response ethnicity counts.

In Total Response ethnicity counts, people that report multiple ethnicities are counted in
each of them.

Prioritised ethnicity instead prioritises certain ethnicities, and each person only ends up
in one of them. e.g. Māori are prioritised first, followed by Pacific peoples etc. European
is the last prioritised ethnicity.

Thus, the Prioritised count for Māori is the same as the Total count. Whereas the Prioritised
count for Europeans is only people whose level 1 ethnicity is only "European".

Unfortunately Statistics NZ doesn't publish data on Prioritised count (presumably they do
provide this to Ministry of Health though!)

## Hacked solution

To hack up a Prioritised Ethnicity count data for 2021, I did the following:

1. Find this paper which compares the 2013 census Total Ethnicity and Prioritised Ethnicity age distributions. See Appendix Figure 1: https://doi.org/10.1080/1177083X.2019.1657912

2. Convert the figure to data via point and clicking A LOT. http://graphreader.com

3. Compared the Total Response age distributions in 2013 and 2021. They weren't too different.

4. ASSUME that the difference between Total Response (TR) and Prioritised (P) age distributions in 2013 and 2021 are the same. They probably aren't!

5. Apply the difference between TR and P age distributions from 2013 to the 2021 TR age distribution to compute the 2021 P age distribution.

6. ASSUME that the proportion of those reporting more than one ethnicity in each ethnic group in 2021
stay in that ethnic group after prioritisation the same way as they do in 2013, and that the proportion of that reporting is the same as the 2018 census.

7. Use the proportion of each ethnicity that report multiple ethnicities versus one ethnicity in 2018(available here: https://www.stats.govt.nz/tools/2018-census-ethnic-group-summaries) and the total reported counts to compute the total prioritised counts.

8. Compute the counts by age for each prioritised ethnicity using the 2021 P distribution and 2021 P counts.

9. ASSUME that the breakdown by gender within age and ethnicity is the same between P and TR ethnicities.

10. Use the proportion male and female to break the counts by age and prioritised ethnicity into age, prioritised ethnicity and sex.

11. Check that the prioritised counts are less than the total counts. They were, except for 90+ Europeans (likely as I didn't extend the age distributions enough), which we're not using here anyway.
