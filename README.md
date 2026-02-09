<h1 align="center">Student Stress Analysis & Segmentation Dashboard</h1>

<p align="center">
  <strong>An end-to-end data project understanding student stress personas with Machine Learning and Interactive Visualization.</strong>
  <br>
  ⭐<a href="https://p6fch7-minh0hoang-dau0tran.shinyapps.io/Student_Stress_App_final/"><strong>RShiny View App</strong></a> | 
  📜<a href="https://drive.google.com/file/d/1bp3Cyq3BEgepjY70h_mqFc4TH2KremJX/view"><strong>Full Analysis PDF</strong></a>
</p>

<hr>

## 📌 Project Overview
Researchers from Tribhuvan University gathered data from 1,100 students across Nepal to understand the intersection of academic pressure and mental health. This project segments students into same-feature groups to provide more specific help and solutions.

## 🚀 Key EDA Features
* <b>Machine Learning:</b> Implemented K-means++ clustering, evaluated using the Elbow Method and Silhouette Scores to find the most intuitive segmentation -> decided to use 3 clusters.
* <b>Feature Engineering:</b> Used 21 features reflecting student mental responses across different aspects: Psychological + Physiological (combined in Health), Academic, Social, and Environmental. 
* <b>Dimensionality Reduction:</b> Applied Principal Component Analysis (PCA) to identify "Teacher-Student Relationship" as a feature holding the highest variances.
* <b>Interactive R Shiny UI:</b> Built a custom dashboard using `tabBox`, `valueBox`, `ggplot2`, and `plotly` to visualize complex correlations between features to find correlations to stress.

## 📊 Critical Insights
* <b>Psychological Impact:</b> 49.27% of students have a mental health history. Among them, only 27.31% maintain high self-esteem.
* <b>Academic Findings:</b> Study load was found to correlate negatively with academic performance, suggesting that high pressure does not equal better results.
* <b>Environmental Stress:</b> Living conditions significantly affect stress levels; 79.67% of students in bad living conditions also live in unsafe environments.
* <b>The Role of Support:</b> Students experiencing extreme bullying report receiving only minimal social support.

## 🛠️ Tech Stack
* <b>Data Analysis:</b> Python (Pandas, Scikit-learn) 
* <b>Web Framework:</b> R Shiny
* <b>Visualizations:</b> Web(R, Rshiny), PDF Visuals (PowerBI)
* <b>Version Control:</b> Git (feature-branch workflow)

<hr>

<p align="center">
  [cite_start]<em>"Take care of the youth is take care of the future"</em> [cite: 28]
</p>
