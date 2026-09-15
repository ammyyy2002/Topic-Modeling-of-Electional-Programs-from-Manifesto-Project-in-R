# Manifesto Topic Modeling with R

This project analyzes party manifestos from the Manifesto Project for the German parties SPD, FDP, and Bündnis 90/Die Grünen. The goal is to identify recurring thematic patterns in party programs over time and compare the parties' political priorities using topic modeling.

The analysis is implemented in R and follows a standard text-mining pipeline: data retrieval, preprocessing, POS-based filtering, document-term matrix creation, LDA topic modeling, and visualization of topic proportions by election year and party.

## Project objective

The project investigates:

- which themes dominate party manifestos across time,
- how the salience of topics differs between SPD, FDP, and the Greens,
- how topical emphasis shifts across election cycles,
- which latent topics can be derived from the manifestos using an unsupervised LDA model.

## Research context

The data source is the Manifesto Project database:

- https://manifesto-project.wzb.eu/

The study focuses on German party manifestos from 1983 onward, using the relevant party codes for:

- SPD (41320)
- FDP (41420)
- Bündnis 90/Die Grünen (41111, 41112, 41113)

## Prerequisites

Before running the scripts, make sure the following are available:

### R and packages

Install R (preferably recent version) and the required packages:

```r
install.packages(c(
  "manifestoR",
  "dplyr",
  "tidytext",
  "quanteda",
  "udpipe",
  "topicmodels",
  "LDAvis",
  "tsne",
  "reshape2",
  "ggplot2",
  "ldatuning",
  "tidyverse"
))
```

### Manifesto Project API key

You need a valid Manifesto Project API key stored at:

- `other_docs/manifesto_apikey.txt`


### Additional stopword files

The preprocessing step reads custom German stopwords and topic-related words from:

- `other_docs/stopwords_deutsch.txt`
- `other_docs/topwords_deutsch.txt`

The scripts currently point to absolute Windows paths and may need to be adjusted on another machine.

### Optional: UDPipe model

The POS-tagging script downloads the German UDPipe model automatically if it is not already available.

## Repository structure

```text
manifesto-topic-modeling-r/
├── README.md
├── R_scripts/
│   ├── 1_Corpus_vorbereiten.R
│   ├── 2_POS-Tagging.R
│   ├── 3_DTM.R
│   ├── 4_Topic Modelling.R
│   ├── 5_Viz_Heatmap_PartyDistributions.R
│   └── CalculateTopicNumber.R
├── other_docs/
│   ├── manifesto_apikey.txt
│   ├── stopwords_deutsch.txt
│   └── topwords_deutsch.txt
├── results/
│   ├── TopicLabels.txt
│   └── Top20Terms.txt
└── workspaces/
    ├── DTM_05.03..RData
    └── tm3_51_06.03.RData
```

## Workflow used

### 1. Corpus preparation

File: `R_scripts/1_Corpus_vorbereiten.R`

This script:

- loads the `manifestoR` package,
- authenticates using the personal API key,
- downloads the selected party manifestos,
- filters the corpus by relevant party IDs and date range,
- splits long texts into chunks of 99 tokens,
- creates a tidy dataframe with one section per row.

This step reduces memory pressure and makes later text processing more manageable.

### 2. POS tagging and lemmatization

File: `R_scripts/2_POS-Tagging.R`

This script:

- downloads the German UDPipe language model,
- annotates each text section,
- keeps nouns and proper nouns,
- replaces special characters and normalizes the text,
- stores only lemma-based noun terms for topic modeling.

The motivation is to reduce noisy text and keep conceptually relevant terms.

### 3. DTM creation

File: `R_scripts/3_DTM.R`

This script:

- converts the prepared data to a `quanteda` corpus,
- removes German stopwords and custom topwords,
- identifies frequent collocations,
- compounds multi-word phrases,
- creates a document-term matrix,
- removes empty rows and sparse non-representative terms.

The final DTM is used as input to the latent topic model.

### 4. Topic modeling

File: `R_scripts/4_Topic Modelling.R`

This file:

- defines the topic number `K = 51`,
- runs an LDA model with Gibbs sampling,
- calculates topic distributions (`theta`) and term distributions (`beta`),
- derives topic labels from the top terms,
- exports JSON for LDAvis-based interactive exploration.

The script also ranks topics by overall proportion and stores the topic summary.

### 5. Visualization of party-specific topic distributions

File: `R_scripts/5_Viz_Heatmap_PartyDistributions.R`

This script:

- loads the labels generated for each topic,
- aggregates topic proportions by election year for each party,
- creates party-specific heatmaps,
- saves topic terms and labels to `results/`.

The visual output shows how topic emphasis varies by year and party.

### 6. Topic number selection

File: `R_scripts/CalculateTopicNumber.R`

This script uses `ldatuning` and several internal metrics such as:

- Griffiths2004
- CaoJuan2009
- Arun2010
- Deveaud2014

It helps determine an appropriate number of latent topics before final modeling.

## How to run the project

1. Place your Manifesto Project API key in the path expected by the scripts.
2. Adjust absolute file paths in the scripts if you are running the project outside the original setup.
3. Run the scripts in order:

```r
source("R_scripts/1_Corpus_vorbereiten.R")
source("R_scripts/2_POS-Tagging.R")
source("R_scripts/3_DTM.R")
source("R_scripts/4_Topic Modelling.R")
source("R_scripts/5_Viz_Heatmap_PartyDistributions.R")
```

4. If you want to test the topic-number tuning step first, run:

```r
source("R_scripts/CalculateTopicNumber.R")
```

## Results summary

The final LDA model identifies 51 latent topics. The model groups the party manifestos into recurring political themes such as:

- labor market and employment policy,
- foreign and defense policy,
- welfare and social policy,
- education and family policy,
- environmental policy,
- science, digitalization, and data protection,
- equal rights and discrimination,
- democratic institutions and transparency,
- urban development and mobility,
- memory culture and historical justice.

The topic labels generated in `results/TopicLabels.txt` show the thematic structure of the corpus. The most prominent topics include:

- labor market policy,
- foreign policy,
- housing and urban development,
- social policy,
- environmental protection,
- education and training,
- internal security,
- democracy and governance.

Across the examined parties, the visualizations suggest clear differences in emphasis:

- the Greens show strong signals in environmental policy, equality, and sustainability issues,
- the FDP emphasizes market regulation, innovation, digitalization, and liberal rights,
- the SPD often highlights welfare, social policy, labor market policy, and public services.

## Output files

The project produces several result artifacts:

- `results/TopicLabels.txt` — manually assigned labels for each topic
- `results/Top20Terms.txt` — top terms for the labeled topics
- `workspaces/*.RData` — saved workspace objects from the modeling pipeline

## Notes and caveats

- The scripts currently contain hard-coded Windows paths; they need to be adapted for Linux or macOS environments.
- The workflow is research-oriented and designed for exploratory analysis rather than production-grade automation.
- The LDA parameter choice (`K = 51`) is based on topic-number tuning and domain judgment; results may vary with preprocessing and the chosen stopword list.

## Conclusion

This project demonstrates how topic modeling can be used in political text analysis to extract latent themes from party manifestos and compare the ideological structure of major German parties over time. It is a useful workflow for digital humanities and comparative politics research.
