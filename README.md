[EPoCH Explorer](https://gcsharp.shinyapps.io/EPoCH/) is a web app built in R Shiny that allows you to explore and download the results of the Exploring Prenatal Influences on Child Health (EPoCH) study.

### About the EPoCH study

EPoCH is an epidemiological study that takes a systematic approach to explore associations between prenatal parental health behaviours and multiple child health outcomes.

The study combines data from four longitudinal cohort studies (ALSPAC, BiB, MoBa, and MCS), with a combined maximum sample size of over 230,000. Regression analyses were performed within each cohort, and then, where possible, results were meta-analysed to produce average effect estimates across the cohorts.

EPoCH generated results from over 500,000 models, exploring associations between variables describing 99 parental health behaviours (reflecting different types and timings of exposure to parental **smoking**, **alcohol** consumption, **caffeine** consumption), and 218 child health outcomes (reflecting different traits relating to **body size and composition**, **psychosocial** and **cognitive** factors, **immunology**, **blood pressure**, and serum **biomarkers**, measured in different ways at several ages). Where possible, health behaviours from both parents are studied. Associations between parental **socioeconomic position** and child health outcomes are also presented for comparison.

### What can I do with EPoCH Explorer?

EPoCH explorer is a web app providing interactive and customisable data visualisations, interpretations, and downloads to disseminate results from the EPoCH study. It is a useful tool for research and it can also serve as an aid for teaching about causal inference and triangulation of evidence garnered from observational data.

### Walkthrough

The opening screen of EPoCH Explorer:

![](www/openingscreen.png){width="384"}

On selecting ‘Click to get started’ the data is loaded, and a modal appears to explain that this  will take a few seconds. The main screen of the app then appears with the navigation bar (below) at the top. The user can click the tabs to gain insights into the EPoCH results.

![](www/navigationbar.png){width="548"}

[Welcome/About]{.underline}

A description of each tab is provided on the welcome screen:

![](www/whatcanido.png){width="468"}

On sub-tabs within the welcome/about page, users can view basic information about each cohort that contributed to EPoCH, followed by an interactive table describing the distribution of variables (the same tables that are used in Supplementary File 6 of the paper). An example for ALSPAC is shown below.

![](www/cohortinfo.png){width="443"}

The statistical models are also described on a separate sub-tab.

[Manhattan plots]{.underline}

Manhattan plots (below) allow the user to visualise trends by summarising the results (P values or effect estimates) across all exposures and outcomes. They can select the data to plot using the dropdown boxes and, once generated, they can filter results by parent-of-interest and select whether to view -log10 P-values or standardised effect estimates (Cohen’s D) on the Y axis, and exposures or outcome on X. All plots (created using Plotly) are interactive, so the user can zoom in on certain areas. Hovering the cursor over a point will reveal pertinent information about the association (e.g. exposure, outcome, effect estimate, p-value, sample size, contributing cohorts).

![](www/manhattan1.png){width="321"} ![](www/manhattan2.png){width="321"} ![](www/manhattan3.png){width="321"} ![](www/manhattan4.png){width="321"}

*Example Manhattan plots showing the same set of results (smoking, psychosocial/cognitive outcomes, standard unstratified model without adjustment for the co-parent’s exposure, i.e. model 2a). Top left: Y=-log10P, X=outcomes; top right: Y=effect estimates, X=outcomes; bottom left: Y=-log10P, X=exposures; bottom right: Y=effect estimates, X=exposures.*

[Volcano plots]{.underline}

Volcano plots allow the user to visualise results by both precision (-log10 P) and magnitude of association (standardised effect estimate Cohen’s D). Plots for each parent-of-interest are plotted side-by-side. Users can select the data to plots using the dropdown boxes and, once generated, they can select whether the Y axes display the raw -log10 P-values or the ranked versions. Raw values allow users to visualise differences between parents-of-interest more clearly, whereas ranked values spread the data out along the Y-axis more equally, allowing easier identification of points with larger P-values. As with the Manhattan plots, the user can hover over a point to reveal more information.

![](www/volcano1.png){width="531"} ![](www/volcano2.png){width="531"}

*Example Volcano plots showing the same set of results (smoking, psychosocial/cognitive outcomes, standard unstratified model without adjustment for the co-parent’s exposure, i.e. model 2a). Top: Y= raw -log10P, mother on left, partner on right; bottom: Y= ranked -log10P, mother on left, partner on right. The top panel shows an example of the information displayed on hovering over a point.*

[Coefficient plots]{.underline}

Coefficient plots allow the user to compare effect estimates and 95% confidence intervals from up to four different exposure/model combinations side-by-side. The user selects the outcome class and type (i.e. binary or continuous) to view.

![](www/coefplots.png){width="482"}

*Example Coefficient plots comparing the observational results (i.e. self-reported maternal smoking ever in pregnancy) vs results from the genetic risk score for smoking initiation.*

[Deep Dive]{.underline}

On the ‘Deep Dive’ tab, users can select a specific association (i.e. select the exposure subclass and timing, the outcome subclass and timing, and the model) and generate a detailed description of the result, including an interactive forest plot displaying the effect estimates and 95% CIs for each cohort along with the meta-analysis result. The verbal interpretation summarises the same size, number and % exposed, effect estimate and 95% CIs, number of contributing cohorts, and (where n cohorts \>1) the heterogeneity statistics I2 and heterogeneity P-value. These are integrated with a verbal summary of the results that attempts to translate the results into terms that can be more easily understood by students or other interested non-experts.

![](www/deepdive.png){width="523"}

[Download]{.underline}

The user can download all results by first agreeing to three conditions (around appropriately interpreting and citing our work), and then clicking the download button that will appear.

![](www/download.png){width="539"}

[Causal inference report]{.underline}

This tab allows users to select a health behaviour and parent of interest, and a specific outcome, and generate a series of plots designed to help them evaluate and interpret the evidence around whether their exposure *in pregnancy* has a causal effect on their outcome. An example of a causal inference report is provided in the next section of this file.

The following screenshots show the various sections of a causal inference report generated to evaluate evidence for an effect of maternal smoking in pregnancy on externalising traits at age 5 to 7.

![](www/causalinferencereport1.png){width="486"} ![](www/causalinferencereport2.png){width="486"} ![](www/causalinferencereport3.png){width="486"} ![](www/causalinferencereport4.png){width="486"} ![](www/causalinferencereport5.png){width="486"} ![](www/causalinferencereport6.png){width="486"} ![](www/causalinferencereport7.png){width="486"} ![](www/causalinferencereport8.png){width="486"} ![](www/causalinferencereport9.png){width="486"} ![](www/causalinferencereport10.png){width="486"} ![](www/causalinferencereport11.png){width="486"}
